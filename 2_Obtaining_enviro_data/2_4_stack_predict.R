# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
library(dplyr)
library(beepr)
library(raster)

#-------------------------------------------------------------------------------
# 2_4_stack_predict: Stack environmental data for predict
#-------------------------------------------------------------------------------

# 1. Set static data repository -------------------------------------------------------
# path to environmental static data
static_data <- paste0(input_data, "/summary_static/")

# path to output
outdir <- paste0(temp_data, "/stack_daily/") 
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# 2. Create oceanmask-----------------------------------------------------------
# Set raster resolution and extent
res <- 0.25
e <- extent(-2.125, 4.125, 35.875, 43.125)

# create base raster
m <- raster(e, res = res, crs = crs("+proj=longlat +datum=WGS84"))
m[] <- 1

# 3. Stack static environmental data-------------------------------------------
## Run only once:
# import static maps
bathy <- raster(paste0(static_data, "/bathy.tif"))  # bathymetry
bathy <- bathy+0
print(bathy)
plot(bathy)

# Keep only values between 0 and -199
bathy_shallow <- bathy
bathy_shallow[!(bathy_shallow <= 0 & bathy_shallow > -200)] <- NA
print(bathy_shallow)
plot(bathy_shallow)

# Keep only values between -200 and -499
bathy_med <- bathy
bathy_med[!(bathy_med <= -200 & bathy_med > -500)] <- NA
print(bathy_med)
plot(bathy_med)

# Keep only values between 500 and -800
bathy_deep <- bathy
bathy_deep[!(bathy_deep <= -500 & bathy_deep >= -800)] <- NA
print(bathy_deep)
plot(bathy_deep)


# prepare function for stack
prepareGrid <- function(r, m, method, name){
  rc <- raster::crop(r, m)  # crop by extent
  rs <- raster::resample(r, m, method=method)  # resample
  rm <- raster::mask(rs, m)  # mask
  names(rm) <- name
  return(rm)
}

# create stack with static variables
#bat <- prepareGrid(bathy_shallow, m, method="bilinear", name="hauling_depth")
#bat <- prepareGrid(bathy_med, m, method="bilinear", name="hauling_depth")
bat <- prepareGrid(bathy_deep, m, method="bilinear", name="hauling_depth")
plot(bat)
print(bat)
# Stack them all:
stack_static <- stack(bat)
plot(stack_static)
#output_file <- file.path(static_data, paste0("stack_static_shallow.grd"))
#output_file <- file.path(static_data, paste0("stack_static_med.grd"))
output_file <- file.path(static_data, paste0("stack_static_deep.grd"))
writeRaster(stack_static, output_file, format = "raster", overwrite = TRUE)

#file <- file.path(static_data, paste0("stack_static_shallow.grd"))
#file <- file.path(static_data, paste0("stack_static_med.grd"))
file <- file.path(static_data, paste0("stack_static_deep.grd"))
stack_static <- raster::stack(file)
plot(stack_static)


# 4. Prepare dynamic variables for stack --------------------------------------------------
# Function to prepare and stack raster files for each day
prepareStackForDay <- function(day_folder, variables, res, e, output_folder) {
  # Define extent and resolution
  e <- extent(e)
  
  # Create an empty stack
  stack_dynamic <- stack()
  
  # Mapping of original variable names to new names
  variable_names_map <- list(
    t2m = "atm_temp")
  for (variable in variables) {
    # example to test code: variable <- variables[1]
    
    # Construct the file pattern for the variable
    file_pattern <- paste0("*.nc")
    
    # List netCDF files for the given variable
    nc_files <- list.files(path = day_folder, pattern = file_pattern, full.names = TRUE)
    
    if (length(nc_files) == 0) {
      next
    }
    
    # Read each netCDF file and prepare the raster
    for (nc_file in nc_files) {
      # example for testing code: nc_file <- nc_files[1]
      
      # Open the netCDF file
      r <- raster(nc_file)
      
      # Calculate the number of columns and rows
      ncol <- (e@xmax - e@xmin) / res
      nrow <- (e@ymax - e@ymin) / res
      
      # Create an empty raster with the specified extent and resolution
      target_raster <- raster(ncol = ncol, nrow = nrow, 
                              xmn = e@xmin, xmx = e@xmax, 
                              ymn = e@ymin, ymx = e@ymax)
      
      r_resampled <- resample(r, target_raster, method = "bilinear")
      
      # Resamplear stack_static para que coincida con r_resampled
      stack_static_resampled <- resample(stack_static, r_resampled, method = "bilinear")
      
      # Stack the raster
      stack_dynamic <- stack(stack_dynamic, r_resampled)
      
      # Rename the latest raster layer in the stack with the desired name
      layer_name <- variable_names_map[[variable]]
      names(stack_dynamic)[nlayers(stack_dynamic)] <- layer_name
      
      
      # Close the netCDF file
      rm(r)
    }
  }
  
  # Now add the static layers once, resampled to the same resolution as the dynamic stack
  if (!is.null(stack_static)) {
    
    # Resample the static stack to match the dynamic raster
    stack_static_resampled <- resample(stack_static, target_raster, method = "bilinear")
    
    # Combine the dynamic stack with the resampled static stack (only once)
    stack_final <- stack(stack_dynamic, stack_static_resampled)
    
  } else {
    # If no static stack is provided, just return the dynamic stack
    stack_final <- stack_dynamic
  }
  
  # Save the final stack to file
  if (nlayers(stack_final) > 0) {
    # Extract the base directory and split by '/'
    components <- unlist(strsplit(day_folder, "/"))
    
    # Assumes that folder structure includes year, month, day in the specified positions
    year <- components[4]
    month <- components[5]
    day <- components[6]
    
    # Create a date string for the file name
    date_string <- paste0(year, month, day)
    
    # Define the output file path
    output_file <- file.path(output_folder, paste0("stack_", date_string, "_bathy_deep.grd")) ###########
    
    # Save the final stack
    writeRaster(stack_final, output_file, format = "raster", overwrite = TRUE)
    
    cat("Stack saved to", output_file, "\n")
  }
  
  # Return the final stacked raster
  return(stack_final)
}


# Function to process each dayly stack:
processDailyStacks <- function(base_folder, variables, res, e) {
  
  # Get list of all month folders
  month_folders <- list.dirs(base_folder, full.names = TRUE, recursive = FALSE)
  
  for (month_folder in month_folders) {
    
    # Get list of all day folders within the current month folder
    # example to test code: month_folder <- month_folders[1]
    day_folders <- list.dirs(month_folder, full.names = TRUE, recursive = FALSE)
    
    for (day_folder in day_folders) {
      # example to test code: day_folder <- day_folders[1]
      
      # Extract date from folder name
      date_folder <- basename(day_folder)
      
      # Define output folder for the stack
      output_folder <- day_folder
      if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
      }
      
      # Prepare and stack rasters
      prepareStackForDay(day_folder, variables, res, e, output_folder)
    }
  }
}


# Stack you documents:
# General path:
base_folder <- "output/ERA5/AT_Reanalysis/2021"

# Select the dynamic variables to extract (same names as catalog):
path <- paste0(input_data, "/Catalog_ERA5.csv")
catalogERA <- read.csv(path, sep=";")
head(catalogERA)
#catalog$variable
variables <- c("t2m")

# Set the resolution and extent:
res <- 0.25
e <- extent(-2.125, 4.125, 35.875, 43.125)

# Process the stacks
processDailyStacks(base_folder, variables, res, e)
beep()


# 5. Eliminate your stack ------------------------------------------------------
# I had to remove a copy of all the .grd and .gri files generated (made a mistake)
# List all files in the directory ending with ".grd" and ".gri"
# Define the base directory
base_folder <- paste0("output/ERA5/AT_Reanalysis/2021")

# Get list of all month folders
month_folders <- list.dirs(base_folder, full.names = TRUE, recursive = FALSE)

# Loop through each month folder
# month_folder <- month_folders[1]
for (month_folder in month_folders) {
  
  # Get list of all day folders within the current month folder
  day_folders <- list.dirs(month_folder, full.names = TRUE, recursive = FALSE)
  
  # Loop through each day folder
  # day_folder <- day_folders[1]
  for (day_folder in day_folders) {
    
    # List all files in the day folder ending with "_eke_3d.nc"
    files_to_remove <- list.files(path = day_folder, pattern = "bathy_shallow.*\\.grd$", full.names = TRUE) #"\\.gri$"
    
    # Print files to be removed (for confirmation)
    print(files_to_remove)
    
    # Remove all matching files
    file.remove(files_to_remove)
  }
}

