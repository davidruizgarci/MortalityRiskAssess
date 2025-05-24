# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_6_plot_pred  Plot the predicted AVM risk maps
#-------------------------------------------------------------------------------
library(ggplot2)

#1. Set data repository---------------------------------------------------------
# 1.1. dataset------------------------------------------------------------------
data <- read.csv("temp/final/AVM_allEnviro.csv", sep = ";") 

# Constants and fixed values
season <- "2021"
mins <- "Mins55" #Mins55 #Mins41 #Mins10 #Mins29
trawl <- "Trawl4.1" #Trawl4.1 #Trawl3.4 #Trawl2.9
sp_list <- unique(data$Species)
sp_list
season_vals <- c("2021", "Spring", "Winter", "Fall", "Summer")

# 1.1. Bathymetry---------------------------------------------------------------
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)
#Colour for bathymetry:
# Create a mask if you dont want to plot all the bathymetricla range
#bathy_bb <-  bathy_df$Bathy <= 5 #only upper break
bathy_filtered <- bathy_df %>%
  filter(Bathy >= -800 & Bathy <= 40)
# Apply the mask
print(bathy_filtered)



# 1.2. Landmask-----------------------------------------------------------------
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
mask <- st_transform(mask, crs = 4326)
e <- c(-3, 7, 35, 43)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
st_crs(bbox) <- st_crs(mask) 
mask <- st_intersection(mask, bbox)
print(mask)

# 1.3. GSAs---------------------------------------------------------------------
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)


# 1.4. Predicted habitat--------------------------------------------------------
sp <- sp_list[1]
indir <- file.path(output_data, "predict_across_sp", "2021", paste0(mins, "_", trawl))
file <- paste0(indir, "/", season, "_across_sp_pred_mean_INTER1.tif")
risk <- raster(file)
print(risk)
file_sd <- paste0(indir, "/", season, "_across_sp_pred_sd_INTER1.tif")
risk_sd <- raster(file)
print(risk_sd)

# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)



# 2. Loop to create maps--------------------------------------------------------
for (season in season_vals) {
  #season <- season_vals[1]
  message("📅 Starting maps for season: ", season)
  
  # Define file paths
  indir <- file.path(output_data, "predict_across_sp", "2021", paste0(mins, "_", trawl))
  file_mean <- file.path(indir, paste0(season, "_across_sp_pred_mean_INTER1.tif"))
  file_sd   <- file.path(indir, paste0(season, "_across_sp_pred_sd_INTER1.tif"))
  
  if (!file.exists(file_mean) || !file.exists(file_sd)) {
    message("⚠️ Missing files for ", season, ", skipping...")
    next
  }
  
  # Load rasters
  risk_mean <- raster(file_mean)
  risk_sd   <- raster(file_sd)
  
  for (metric in c("mean", "sd")) {
    risk <- if (metric == "mean") risk_mean else risk_sd
    
    # Mask by bathymetry range (-800 to -40)
    bathy_filtered <- calc(bathy, function(x) {
      x[x > -40 | x < -800] <- NA
      return(x)
    })
    bathy_mask <- calc(bathy_filtered, function(x) {
      x[!is.na(x)] <- 1
      return(x)
    })
    bathy_mask_resampled <- resample(bathy_mask, risk, method = "bilinear")
    risk_cropped <- raster::mask(risk, bathy_mask_resampled)
    
    # Convert raster to sf and crop by GSA06
    risk_df <- as.data.frame(risk_cropped, xy = TRUE)
    colnames(risk_df) <- c("x", "y", "risk")
    risk_df <- risk_df %>% filter(!is.na(risk))
    risk_sf <- st_as_sf(risk_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
    risk_sf <- st_transform(risk_sf, crs = st_crs(GSA_filtered))
    risk_clipped_sf <- st_intersection(risk_sf, GSA_filtered)
    risk_clipped_df <- risk_clipped_sf %>% as.data.frame() %>%
      dplyr::select(x, y, risk) %>% filter(!is.na(risk))
    
    # Plot
    p <- ggplot() +
      geom_tile(data = risk_clipped_df, aes(x = x, y = y, fill = risk)) +
      scale_fill_viridis_c(option = "viridis", name = ifelse(metric == "mean", "AVM risk", "SD")) +
      geom_sf(data = mask, fill = "grey80", color = "grey60") +
      geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
      coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.position = "right",
            aspect.ratio = 1)
    
    # Save
    outdir <- file.path(output_data, "fig/Map", paste0(mins, "_", trawl), season)
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    p_png <- file.path(outdir, paste0(season, "_risk_", metric, "_INTER1.png"))
    ggsave(p_png, p, width = 20, height = 20, units = "cm", dpi = 1800)
    message("✅ Saved ", metric, " map: ", p_png)
  }
}
