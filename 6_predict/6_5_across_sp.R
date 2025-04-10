# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_5_across_spp  Calculate means and SD across species
#-------------------------------------------------------------------------------
#1. Set data repository---------------------------------------------------------
# 1.1. dataset
data <- read.csv("temp/final/AVM_allEnviro.csv", sep = ";") 

# Constants and fixed values
mins <- "Mins29" #Mins55 #Mins41 #Mins10
trawl <- "Trawl2.9" #Trawl4.1 #Trawl3.4 #Trawl2.9
sp_list <- unique(data$Species)
sp_list
season_vals <- c("2021", "Spring", "Winter", "Fall", "Summer")


# Import landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)
e <- c(-2.125, 4.125, 35.875, 43.125)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
st_crs(bbox) <- st_crs(mask) 
mask <- st_intersection(mask, bbox)
print(mask)


# Calculate mean +- SD across species for all seasons:
for (season_val in season_vals) {
  message("📅 --- Season:", season_val)
  
  stack_list <- list()  # Collect rasters for ALL species here
  
  for (sp in sp_list) {
    message("🔁 Processing species: ", sp)
    
    indir <- file.path(output_data, "predict_overall", "2021", sp, paste0(mins, "_", trawl))
    pat <- paste0(season_val, "_pred_median.tif")
    
    tiffile <- list.files(indir, recursive = TRUE, full.names = TRUE, pattern = pat)
    
    if (length(tiffile) > 0) {
      r <- tryCatch({
        raster(tiffile)
      }, error = function(e) {
        message("❌ Error loading ", sp, ": ", e$message)
        NULL
      })
      if (!is.null(r)) {
        stack_list[[sp]] <- r
        message("✅ Loaded raster for: ", sp)
      }
    } else {
      message("⚠️ No raster found for: ", sp)
    }
  }
  
  message("📦 Total valid rasters for season '", season_val, "': ", length(stack_list))
  
  if (length(stack_list) == 0) {
    message("🚫 No rasters to process for season ", season_val, ". Skipping.")
    next
  }
  
  # Stack and calculate mean and SD
  pred_stack <- stack(stack_list)
  pred_mean <- calc(pred_stack, fun = mean, na.rm = TRUE)
  pred_sd   <- calc(pred_stack, fun = sd, na.rm = TRUE)
  message("✅ Mean and SD calculated.")
  
  # Output path
  outdir <- file.path(output_data, "predict_across_sp", "2021", paste0(mins, "_", trawl))
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Save TIFFs
  writeRaster(pred_mean, filename = file.path(outdir, paste0(season_val, "_across_sp_pred_mean.tif")),
              format = "GTiff", overwrite = TRUE)
  writeRaster(pred_sd, filename = file.path(outdir, paste0(season_val, "_across_sp_pred_sd.tif")),
              format = "GTiff", overwrite = TRUE)
  
  message("💾 Saved rasters for season ", season_val)
  
  # Optional PNGs
  png(file.path(outdir, paste0(season_val, "_across_sp_pred_mean.png")), width = 560, height = 600, res = 100)
  plot(pred_mean, main = paste("Mean Prediction\n", season_val), col = viridis::viridis(100))
  plot(mask, col = "grey80", border = "grey60", add = TRUE)
  box()
  dev.off()
  
  png(file.path(outdir, paste0(season_val, "_across_sp_pred_sd.png")), width = 560, height = 600, res = 100)
  plot(pred_sd, main = paste("Prediction SD\n", season_val), col = viridis::viridis(100))
  plot(mask, col = "grey80", border = "grey60", add = TRUE)
  box()
  dev.off()
  
  beep()
}
  
beep()
 



