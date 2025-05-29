# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

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
mins <- "Mins55" #Mins55 #Mins41 #Mins29 #Mins10
trawl <- "Trawl4.1" #Trawl4.1 #Trawl3.4 #Trawl2.9
sp_list <- unique(data$Species)
sp_list


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

# 4. Create season dataframe
date_seq <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by="30 days")
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
season_vals <- c("2021", "Spring", "Winter", "Fall", "Summer")

for (sp in sp_list) {
  #sp <- sp_list[1]
  message("========================================")
  message("Processing species: ", sp)
  message("========================================")
  
  for (season_val in season_vals) {
    #season_val <- season_vals[1]
    message("  --- Season: ", season_val)
    
    # Define date subset
    if (season_val == "2021") {
      dates_df <- season_data
    } else {
      dates_df <- subset(season_data, season == season_val)
    }
    
    # Set input/output dirs
    indir <- file.path(output_data, "predict_crop", "2021", paste0(mins, "_", trawl), sp)
    outdir <- file.path(output_data, "predict_overall/2021", sp, paste0(mins, "_", trawl))
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    
    # Preallocate stack list
    stack_list <- vector("list", nrow(dates_df))
    
    message("    Total dates to process: ", nrow(dates_df))
    
    # Loop through dates
    for (i in 1:nrow(dates_df)) {
      #i=1
      date <- dates_df$date[i]
      MM <- sprintf("%02d", month(date))
      pat <- paste0("crop_bathys_X", format(date, "%Y%m%d"), "_", sp, "_", mins, "_", trawl, "_pred_INTER2.tif")
      stack_repo <- file.path(indir, MM)
      
      message("      [", i, "/", nrow(dates_df), "] Date: ", date, " | Looking in: ", stack_repo)
      
      tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
      message("         Files found: ", length(tiffile))
      
      if (length(tiffile) > 0) {
        s <- tryCatch({
          raster::stack(tiffile)
        }, error = function(e) {
          message("         ERROR stacking: ", e$message)
          NULL
        })
        
        if (!is.null(s)) {
          stack_list[[i]] <- s
          message("         Raster stacked successfully.")
        }
      }
    }
    
    # Clean nulls
    stack_list <- stack_list[!sapply(stack_list, is.null)]
    message("    Total rasters successfully stacked: ", length(stack_list))
    
    if (length(stack_list) == 0) {
      message("    >>> No valid rasters for this season. Skipping.")
      next
    }
    
    # Stack and calculate median
    pred_stack <- raster::stack(stack_list)
    pred_med <- raster::calc(pred_stack, fun = median)
    message("    >>> Median raster calculated.")
    
    # Save TIFF
    tifffile <- file.path(outdir, paste0(season_val, "_pred_median_INTER2.tif"))
    writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)
    message("    >>> TIFF saved: ", tifffile)
    
    # Optional PNG save (uncomment to use)
    pngfile <- file.path(outdir, paste0(season_val, "_pred_median_INTER2.png"))
    png(pngfile, width = 560, height = 600, res = 100)
    plot(pred_med, main = paste(sp, "Model:", "\n", season_val), col = viridis(100))
    plot(mask, col = "grey80", border = "grey60", add = TRUE)
    box()
    dev.off()
    message("    >>> PNG saved: ", pngfile)
    
    message("    >>> Season complete: ", season_val)
  }
  
  beep()
  message("========================================")
  message("Finished species: ", sp)
  message("========================================\n")
}
beep()
