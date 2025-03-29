# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_4_season_year_means  Calculate seasonal and yearly means for each species  
#-------------------------------------------------------------------------------
library(viridis)
library(raster)
library(sf)
library(lubridate)
library(beepr)



#1. Set data repository---------------------------------------------------------
# 1.1. dataset
data <- read.csv("temp/final/AVM_allEnviro.csv", sep = ";") 

# Constants and fixed values
season <- "2021"
mins <- "Mins55" #Mins55 #Mins41 #Mins10
trawl <- "Trawl4.1" #Trawl4.1 #Trawl3.4 #Trawl2.9
sp_list <- unique(data$Species)
sp_list


# paths
indir <- paste0(output_data, paste0("/predict_crop/2021/", sp, "/", mins, "_", trawl))


# Import landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# crop it:
e <- c(-2.125, 4.125, 35.875, 43.125)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)


# Create date sequences that you wish:
## Year:
#date_start <- as.Date("2021-01-01")
#date_end <- as.Date("2021-12-31")
#dates <- seq.Date(date_start, date_end, by="day")  # define sequence
#
## Convert date sequences to dataframes
#year_df <- data.frame(date = dates)
#head(year_df)
#
## Define a function to assign seasons based on the date
#get_season <- function(date) {
#  # Extract the month and day
#  month <- as.numeric(format(date, "%m"))
#  day <- as.numeric(format(date, "%d"))
#  
#  # Assign seasons based on month and day
#  if ((month == 12 && day >= 21) || (month %in% c(1, 2)) || (month == 3 && day < 21)) {
#    return("Winter")
#  } else if ((month == 3 && day >= 21) || (month %in% c(4, 5)) || (month == 6 && day < 21)) {
#    return("Spring")
#  } else if ((month == 6 && day >= 21) || (month %in% c(7, 8)) || (month == 9 && day < 21)) {
#    return("Summer")
#  } else {
#    return("Fall")
#  }
#}
#
## Apply the function to all dates
#seasons <- sapply(dates, get_season)
#
## Combine dates and their corresponding seasons into a data frame
#season_data <- data.frame(date = dates, season = seasons)
#
## Create separate data frames for each season
#winter_df <- subset(season_data, season == "Winter")
#spring_df <- subset(season_data, season == "Spring")
#summer_df <- subset(season_data, season == "Summer")
#fall_df <- subset(season_data, season == "Fall")
#
## View a sample from each season
#head(winter_df)
#head(spring_df)
#head(summer_df)
#head(fall_df)

# 4. Create season dataframe
date_seq <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by="day")
get_season <- function(date) {
  m <- month(date)
  d <- day(date)
  if ((m == 12 && d >= 21) || (m %in% c(1,2)) || (m == 3 && d < 21)) return("Winter")
  else if ((m == 3 && d >= 21) || (m %in% c(4,5)) || (m == 6 && d < 21)) return("Spring")
  else if ((m == 6 && d >= 21) || (m %in% c(7,8)) || (m == 9 && d < 21)) return("Summer")
  else return("Fall")
}
season_data <- data.frame(date = date_seq, season = sapply(date_seq, get_season))


# 2. Merge maps to create seasonal means----------------------------------------
# Prepare your date list and other necessary variables
#dates <- year_df #spring_df, winter_df, summer_df, fall_df
#stack_list <- vector("list", nrow(dates))  # Pre-allocate list

season_vals <- c("2021", "Spring", "Winter", "Fall", "Summer")


for (sp in sp_list) {
  message("Processing species: ", sp)
  
  for (season in season_vals) {
    
    message("  Season: ", season)
    
    # Define date subset
    if (season == "2021") {
      dates_df <- season_data
    } else {
      dates_df <- subset(season_data, season == season)
    }
    
    # Set input/output dirs
    indir <- file.path(output_data, "predict_crop/2021", sp, paste0(mins, "_", trawl))
    outdir <- file.path(output_data, "predict_overall/2021", sp, paste0(mins, "_", trawl))
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    
    # Preallocate stack list
    stack_list <- vector("list", nrow(dates_df))
    
    # Loop through dates
    for (i in 1:nrow(dates_df)) {
      date <- dates_df$date[i]
      MM <- sprintf("%02d", month(date))
      pat <- paste0("mean_bathys_crop_bathys_X", format(date, "%Y%m%d"), "_", sp, "_", mins, "_", trawl, "_pred.tif")
      stack_repo <- file.path(indir, MM)
      
      tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
      if (length(tiffile) > 0) {
        s <- tryCatch({
          raster::stack(tiffile)
        }, error = function(e) NULL)
        
        if (!is.null(s)) stack_list[[i]] <- s
      }
    }
    
    # Clean nulls
    stack_list <- stack_list[!sapply(stack_list, is.null)]
    if (length(stack_list) == 0) next  # Skip if no data
    
    # Stack and calculate median
    pred_stack <- raster::stack(stack_list)
    pred_med <- raster::calc(pred_stack, fun = median)
    
    # Save TIFF
    tifffile <- file.path(outdir, paste0(season, "_pred_median.tif"))
    writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)
    
    # Save PNG
    #pngfile <- file.path(outdir, paste0(season, "_pred_median.png"))
    #png(pngfile, width = 560, height = 600, res = 100)
    #plot(pred_med, main = paste(sp, "Model:", "\n", season), col = viridis(100))
    #plot(mask, col = "grey80", border = "grey60", add = TRUE)
    #box()
    #dev.off()
    
    message("    Saved: ", season)
  }
  
  beep()  # optional alert per species
}
beep()














































#outdir path:
outdir <- paste0(output_data, paste0("/predict_overall/2021/", sp,  "/", mins, "_", trawl))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0("mean_bathys_crop_bathys_X", format(date, "%Y%m%d"), "_", sp, "_", mins, "_", trawl, "_pred.tif")
  #pat <- paste0("mean_bathys_crop_bathys_X", format(date, "%Y%m%d"), "_", sp, "_", mins, "_", trawl, "_pred_cir.tif")
  
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
stack_list <- stack_list[!sapply(stack_list, is.null)]

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = median)
#plot(pred_med)
#print(pred_med)
beep()

# Define output paths
tifffile <- paste0(outdir, "/", season, "_pred_median.tif")
pngfile <- paste0(outdir, "/", season, "_pred_median.png")

# Save the median raster as TIFF
writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_med, main = paste(sp, "Model:", "\n", season), col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()

