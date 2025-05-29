# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_3_crop_bathy_range  Crop the rasters by species depth range
#-------------------------------------------------------------------------------
library(dplyr)
library(beepr)
library(raster)
library(lubridate)

#1. Set data repository---------------------------------------------------------

# 1.1. species depth ranges:
depthranges <- read.csv("input/depthranges.csv", sep = ";") 
head(depthranges)

data <- read.csv("temp/final/AVM_allEnviro.csv", sep = ";") 

# 1.2. subset:
# Constants and fixed values
season <- "2021"
mins <- "Mins55" #Mins55 - #Mins41 - #Mins10 - #Mins29
trawl <- "Trawl4.1" #Trawl4.1 - #Trawl3.4 - #Trawl2.9
sp_list <- unique(data$Species)
sp_list

#sp_list <- c("Rpolystigma", "Gmelastomus", "Scanicula")

# select only few species if needed:
#sp_list <- sp_list[!sp_list %in% c(
#  "Rpolystigma", "Gmelastomus", "Scanicula",
#  "Espinax", "Tmarmorata", "Cuyato", "Hgriseus", 
#  "Rclavata", "Dlicha", "Rasterias", "Ocentrina", "Dpastinaca", "Doxyrinchus")]

# select only one species if needed:
#sp_list <-  c("Doxyrinchus")

# 1.3. Create dates
date_start <- as.Date("2021-01-01") 
date_end <- as.Date("2021-01-04")
dates <- seq.Date(date_start, date_end, by="2 days")  


# 1.4. Load bathymetry raster once
bathy <- raster("input/gebco/Bathy.tif")
   
             
#2.Crop-----------------------------------------------------------------------
# Loop over species
for (sp in sp_list) {
  #sp <- sp_list[1]
  cat("\n🔁 Starting species:", sp, "\n")
  
  # Get species-specific depth range
  depth_min <- depthranges$usualMin[depthranges$species == sp]
  depth_max <- depthranges$usalMax[depthranges$species == sp]
  
  if (length(depth_min) == 0 || is.na(depth_min)) {
    cat("❌ Skipping", sp, ": no depth range found.\n")
    next
  }
  
  cat("📏 Depth range:", depth_min, "to", depth_max, "\n")
  
  # Make species-specific bathymetric mask
  bathy_filtered <- calc(bathy, function(x) {
    x[x > -depth_min | x < -depth_max] <- NA
    return(x)
  })
  bathy_mask <- calc(bathy_filtered, function(x) {
    x[!is.na(x)] <- 1
    return(x)
  })
  
  # Define directories
  indir <- file.path(output_data, "predict_mean", season, paste0(mins, "_", trawl), sp)
  outdir <- file.path(output_data, "predict_crop", season, paste0(mins, "_", trawl), sp)
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Loop over each date — independently
  for (i in seq_along(dates)) {
    #i=1
    date <- dates[i]
    YYYY <- year(date)
    MM <- sprintf("%02d", month(date))
    date_str <- format(date, "%Y%m%d")
    
    # File pattern and folder
    pat <- paste0("^mean_bathys_X", date_str, "_", sp, "_", mins, "_", trawl, "_INTER2_pred_INTER2_\\.tif$")
    #pat <- paste0("^mean_bathys_X", date_str, "_", sp, "_", mins, "_", trawl, "_pred_cir\\.tif$")

    stack_repo <- file.path(indir, YYYY, MM)
    tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
    
    if (length(tiffile) > 0) {
      cat("📂 Date:", date_str, "→ found", basename(tiffile), "\n")
      
      r <- tryCatch({
        raster(tiffile)
      }, error = function(e) {
        cat("❌ Error loading", tiffile, ":", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(r)) {
        bathy_mask_resampled <- resample(bathy_mask, r, method = "ngb")
        r_masked <- mask(r, bathy_mask_resampled)
        
        #plot(r)
        #plot(r_masked)
        
        product_folder <- file.path(outdir, MM)
        if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)
        
        clean_name <- paste0("crop_bathys_X", date_str, "_", sp, "_", mins, "_", trawl, "_pred_INTER2.tif")
        output_file <- file.path(product_folder, clean_name)
        
        tryCatch({
          writeRaster(r_masked, output_file, format = "GTiff", overwrite = TRUE)
          cat("✅ Saved:", output_file, "\n")
        }, error = function(e) {
          cat("❌ Error saving", output_file, ":", e$message, "\n")
        })
      }
    } else {
      cat("⚠️ No raster for", date_str, "— skipping.\n")
    }
  }
}

beep()
 