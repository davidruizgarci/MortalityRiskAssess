# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

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
mins <- "Mins55" #Mins55 #Mins41 #Mins10
trawl <- "Trawl4.1" #Trawl4.1 #Trawl3.4 #Trawl2.9
sp_list <- unique(data$Species)
sp_list

# 1.3. Create dates
date_start <- as.Date("2021-01-01") 
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="day")  


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
    pat <- paste0("^mean_bathys_X", date_str, "_", sp, "_", mins, "_", trawl, "_pred\\.tif$")
    pat <- paste0("^mean_bathys_X", date_str, "_", sp, "_", mins, "_", trawl, "_pred_cir\\.tif$")

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
        
        clean_name <- paste0("crop_bathys_X", date_str, "_", sp, "_", mins, "_", trawl, "_pred.tif")
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


























# Loop over all species
for (sp in sp_list) {
  #sp <- sp_list[1]
  cat("\n🔁 Starting species:", sp, "\n")
  
  # Define paths
  indir <- file.path(output_data, "predict_mean", season, paste0(mins, "_", trawl), sp)
  outdir <- file.path(output_data, "predict_crop", season, paste0(mins, "_", trawl), sp)
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Make bathy mask for this species
  depth_min <- depthranges$usualMin[depthranges$species == sp]
  depth_max <- depthranges$usalMax[depthranges$species == sp]
  
  if (length(depth_min) == 0 || is.na(depth_min)) {
    cat("❌ Skipping", sp, ": no depth range found.\n")
    next
  }
  
  cat("📏 Depth range:", depth_min, "to", depth_max, "meters\n")
  
  bathy_filtered <- calc(bathy, function(x) {
    x[x > -depth_min | x < -depth_max] <- NA
    return(x)
  })
  
  bathy_mask <- calc(bathy_filtered, function(x) {
    x[!is.na(x)] <- 1
    return(x)
  })
  
  # Pre-allocate list to hold raster stacks
  stack_list <- vector("list", length(dates))
  
  # Load raster stacks
  for (i in seq_along(dates)) {
    #i=2
    date <- dates$date[i]
    YYYY <- year(date)
    MM <- sprintf("%02d", month(date))
    
    date_str <- format(date, "%Y%m%d")
    pat <- paste0("mean_bathys_X", date_str, "_", sp, ".*_pred\\.tif$")
    stack_repo <- file.path(indir, YYYY, MM)
    cat("🔍 Trying:", pat, "in", stack_repo, "\n")
    cat("📁 Found:", length(tiffile), "file(s)\n")
    
    tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
    
    if (length(tiffile) > 0) {
      s <- tryCatch({
        raster(tiffile)  # Load single raster layer
      }, error = function(e) {
        cat("❌ Error reading", tiffile, ":", e$message, "\n")
        NULL
      })
      if (!is.null(s)) {
        stack_list[[i]] <- s
        cat("✅ Loaded", basename(tiffile), "\n")
      }
    } else {
      cat("⚠️ No file found for", format(date, "%Y-%m-%d"), "\n")
    }
  }
  
  #Debug which dates are matched
  cat("📅 Dates with rasters for", sp, ":\n")
  print(dates[which(!sapply(stack_list, is.null))])
  
  # Filter non-null stacks and get corresponding dates
  valid_indices <- which(!sapply(stack_list, is.null))
  stack_list_sp <- stack_list[valid_indices]
  dates_valid <- dates[valid_indices]
  
  if (length(stack_list_sp) == 0) {
    cat("⚠️ No valid raster stacks found for", sp, "\n")
    next
  }
  
  cat("🔄 Masking", length(stack_list_sp), "stacks...\n")
  
  # Mask each raster stack
  masked_stack_list <- list()
  
  for (i in seq_along(stack_list_sp)) {
    #i <- seq_along(stack_list_sp)[1]
    s <- stack_list_sp[[i]]
    cat("🔹 Masking stack", i, "/", length(stack_list_sp), "...\n")
    
    bathy_mask_resampled <- resample(bathy_mask, s, method = "ngb")
    s_masked <- mask(s, bathy_mask_resampled)
    masked_stack_list[[i]] <- s_masked
  }
  
  # Save masked rasters
  cat("💾 Saving masked rasters for", sp, "...\n")
  
  for (i in seq_along(masked_stack_list)) {
    #i <- seq_along(masked_stack_list)[1]
    s_masked <- masked_stack_list[[i]]
    date_i <- dates_valid[i, "date"]
    MM <- sprintf("%02d", month(date_i))
    
    product_folder <- file.path(outdir, MM)
    if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)
    
    date_str <- format(date_i, "%Y%m%d")
    clean_name <- paste0("crop_bathys_X", date_str, "_", sp, "_", mins, "_", trawl, "_pred")
    output_filename <- file.path(product_folder, paste0(clean_name, ".tif"))
    
    tryCatch({
      writeRaster(s_masked, output_filename, format = "GTiff", overwrite = TRUE)
      cat("✅ Saved:", output_filename, "\n")
    }, error = function(e) {
      cat("❌ Error saving", output_filename, ":", e$message, "\n")
    })
  }
}

# Optional: beep when done
beep()


# Load one to check:
# List all .tif files in the folder
tif_files <- list.files(
  "C:/Users/David/SML Dropbox/gitdata/chondrichthyan_mortality/output/predict_crop/2021/Mins55_Trawl4.1/Espinax/01",
  pattern = "\\.tif$",
  full.names = TRUE)
print(tif_files)

# Load the first one (or pick any)
check <- raster(tif_files[1])
print(check)
plot(check, main = basename(tif_files[1]))
