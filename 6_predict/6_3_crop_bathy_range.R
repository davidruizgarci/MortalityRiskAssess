# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_3_crop_bathy_range  Crop the rasters by species depth range
#-------------------------------------------------------------------------------
library(dplyr)
library(beepr)

#1. Set data repository---------------------------------------------------------

# 1.1. species depth ranges:
depthranges <- read.csv("input/depthranges.csv", sep = ";") 
head(depthranges)

# 1.2. subset:
sp <- "Scanicula"
mins <- "Mins55" #Mins55 #Mins41 #Mins10
trawl <- "Trawl4.1" #Trawl4.1 #Trawl3.4 #Trawl2.9

# 1.3. paths:
indir <- paste0(output_data, paste0("/predict/", season, "/predict_mean/", season, "/", mins, "_", trawl))
outdir <- paste0(output_data, paste0("/predict_crop/", season, "/", sp,  "/", mins, "_", trawl))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# 1.4. Create dates
date_start <- as.Date("2021-01-01") 
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="day")  
# Convert date sequences to dataframes
year_df <- data.frame(date = dates)
# Prepare your date list and other necessary variables
dates <- year_df #spring_df, winter_df, summer_df, autumn_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

#2.Select-----------------------------------------------------------------------

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern (either for values or 95% CI)
  pat <- paste0("mean_bathys_X", format(date, "%Y%m%d"), "_", sp, "_", mins, "_", trawl, "_pred.tif")
  #pat <- paste0(format(date, "%Y%m%d"), "_", sp, "_", bathy, "_", mins, "_", trawl, "_pred_cir.tif")
  
  # Construct the path to the directory containing TIFF files
  stack_repo <- paste0(indir, "/", MM)
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  print(tiffile)  # Check the output
  
  # Debugging print
  print(paste("Found TIFF files:", length(tiffile)))
  
  if (length(tiffile) > 0) {
    s <- tryCatch({
      raster::stack(tiffile)
    }, error = function(e) {
      cat("Error in stacking raster files:", e$message, "\n")
      NULL
    })
    
    if (!is.null(s)) {
      stack_list[[i]] <- s
    }
  }
}

# Print a message indicating completion
print("Processing completed.")

# Identify which elements in the list are NULL
null_indices <- which(sapply(stack_list, is.null))
null_indices
stack_list_sp <- stack_list[!sapply(stack_list, is.null)]
head(stack_list_sp)


# 3. Stack with depth range-----------------------------------------------------
# 3.1. Make depth range mask----------------------------------------------------
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)

# Get the depth range for the selected species
depth_min <- depthranges$usualMin[depthranges$species == sp]
depth_max <- depthranges$usalMax[depthranges$species == sp]

# 3. Filter bathymetry to only retain suitable depth range
bathy_filtered <- calc(bathy, function(x) {
  x[x > -depth_min | x < -depth_max] <- NA  # Note: depths are negative
  return(x)
})
#print(bathy_filtered)
#plot(bathy_filtered)

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})
#print(bathy_mask)
#plot(bathy_mask)


# 3.2. Stack depth and AVM risk--------------------------------------------------
# Create a new list to store masked stacks
masked_stack_list <- list()

# Loop through each stack
for (i in seq_along(stack_list_sp)) {
  #i=1
  # Extract the stack
  s <- stack_list_sp[[i]]
  
  # Print progress
  cat("Processing stack", i, "of", length(stack_list_sp), "| Layers:", nlayers(s), "\n")
  
  # Resample bathy_mask to match the raster stack
  bathy_mask_resampled <- resample(bathy_mask, s, method = "ngb")  # nearest neighbor (best for masks)
  
  # Now apply the mask
  s_masked <- mask(s, bathy_mask_resampled)
  #plot(s_masked)
  #print(s_masked)
  
  # Store the masked stack
  masked_stack_list[[i]] <- s_masked
}
beep()

# Save them:
for (i in seq_along(masked_stack_list)) {
  #i=1
  # Extract masked stack
  s_masked <- masked_stack_list[[i]]
  
  # Extract month (MM) from the corresponding date
  date_i <- dates$date[!sapply(stack_list, is.null)][i]
  MM <- sprintf("%02d", month(date_i))
  
  # Set/create product folder
  product_folder <- file.path(output_data, "predict_crop", season, sp, paste0(mins, "_", trawl), MM)
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)
  
  # Get the name of the layer (remove "_bathy_shallow", etc., if needed)
  layer_name <- names(stack_list_sp[[i]])
  clean_name <- sub("^mean", "crop", layer_name)
  
  # Create output filename
  output_filename <- file.path(product_folder, paste0("mean_bathys_", clean_name, ".tif"))
  
  # Print progress
  cat("Saving stack", i, "to", output_filename, "\n")
  
  # Write the masked raster to file
  writeRaster(s_masked, output_filename, format = "GTiff", overwrite = TRUE)
}
