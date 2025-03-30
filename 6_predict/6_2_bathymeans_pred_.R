# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_2_calc_pred  Add AVM predictions across depth ranges (means) for each species and crop according to their depth range
#-------------------------------------------------------------------------------
library(raster)
library(dplyr)
library(sf)
library(lubridate)

#1. Set data repository---------------------------------------------------------
data <- read.csv("temp/final/AVM_allEnviro.csv", sep = ";") 

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
#print(mask)
#plot(mask)

# Create dates
date_start <- as.Date("2021-01-01") 
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="day")  
# Convert date sequences to dataframes
year_df <- data.frame(date = dates)


# Prepare your date list and other necessary variables
dates <- year_df #spring_df, winter_df, summer_df, autumn_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

# 2. Merge maps depth ranges (means) for each species---------------------------
#2.1. Bathy shallow-------------------------------------------------------------
sp_list <- unique(data$Species)
sp_list

season <- "2021"  # Or set dynamically
mins <- "Mins29" #Mins55 - #Mins41 - #Mins10 #Mins29
trawl <- "Trawl2.9" #Trawl4.1 - #Trawl3.4 - #Trawl2.9
bathy_types <- c("bathy_shallow", "bathy_med", "bathy_deep")

for (sp in sp_list) {
  #sp <- sp_list[1]
  cat("▶", sp, "\n")
  
  stack_lists <- list()
  index_lists <- list()  # To keep track of original indices
  
  for (bathy in bathy_types) {
    #bathy <- bathy_types[1]
    cat("➤ ", bathy, "for", sp, "\n")
    
    stack_list <- list()
    index_list <- c()
    
    for (i in 1:nrow(dates)) {
      #i=1
      date <- dates$date[i]
      YYYY <- year(date)
      MM <- sprintf("%02d", month(date))
      DD <- sprintf("%02d", day(date))
      
      #pat <- paste0(format(date, "%Y%m%d"), "_", sp, "_", bathy, "_", mins, "_", trawl, "_pred.tif")
      pat <- paste0(format(date, "%Y%m%d"), "_", sp, "_", bathy, "_", mins, "_", trawl, "_pred_cir.tif")
      
      stack_repo <- file.path(output_data, "predict", season, MM)
      
      tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
     
       if (length(tiffile) > 0) {
        s <- tryCatch({ raster::stack(tiffile) }, error = function(e) {
          cat("    ⚠️ Error reading", tiffile, ":", e$message, "\n")
          return(NULL)
        })
        
        if (!is.null(s)) {
          stack_list[[length(stack_list) + 1]] <- s
          index_list <- c(index_list, i)
          cat("    ✅ Loaded", basename(tiffile), "\n")
        }
      }
    }
    
    stack_lists[[bathy]] <- stack_list
    index_lists[[bathy]] <- index_list
  }
  
  # Confirm all three bathy types have matching lengths
  n_shallow <- length(stack_lists[["bathy_shallow"]])
  n_med     <- length(stack_lists[["bathy_med"]])
  n_deep    <- length(stack_lists[["bathy_deep"]])
  
  if (n_shallow == n_med && n_med == n_deep && n_shallow > 0) {
    cat("🔄 Combining stacks for", sp, "(", n_shallow, "dates )\n")
    stack_shallow <- stack_lists[["bathy_shallow"]]
    stack_med     <- stack_lists[["bathy_med"]]
    stack_deep    <- stack_lists[["bathy_deep"]]
    valid_indices <- index_lists[["bathy_shallow"]]
    
    outdir <- file.path(output_data, "predict_mean", season, paste0(mins, "_", trawl), sp)
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    
    for (j in seq_along(stack_shallow)) {
      #j <- seq_along(stack_shallow)[1]
      cat("   📅 Date", j, "of", n_shallow, "\n")
      s1 <- stack_shallow[[j]]
      s2 <- stack_med[[j]]
      s3 <- stack_deep[[j]]
      
      result_raster <- tryCatch({
        combined_stack <- stack(s1, s2, s3)
        calc(combined_stack, fun = mean, na.rm = TRUE)
      }, error = function(e) {
        cat("    ❌ Error combining rasters:", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(result_raster)) {
        i_original <- valid_indices[j]
        date <- dates$date[i_original]
        YYYY <- year(date)
        MM <- sprintf("%02d", month(date))
        
        product_folder <- file.path(outdir, YYYY, MM)
        if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)
        
        layer_name <- names(s1)
        clean_name <- gsub("_bathy_shallow", "", layer_name)
        clean_name <- sub("^mean", "crop", clean_name)
        
        #output_filename <- file.path(product_folder, paste0("mean_bathys_", clean_name, ".tif"))
        output_filename <- file.path(product_folder, paste0("mean_bathys_", clean_name, ".tif"))
        
        tryCatch({
          writeRaster(result_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
          cat("    💾 Saved:", output_filename, "\n")
        }, error = function(e) {
          cat("    ❌ Error saving", output_filename, ":", e$message, "\n")
        })
      }
    }
  } else {
    cat("⚠️ Skipping", sp, "— bathy stacks not aligned or empty (shallow:", n_shallow, 
        ", med:", n_med, ", deep:", n_deep, ")\n")
  }
}
