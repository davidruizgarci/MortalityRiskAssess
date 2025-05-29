# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Enviro_plots: Make plots for enviromental data 
#-------------------------------------------------------------------------------
library(dplyr)
library(beepr)
library(raster)


# 1. Load files-----------------------------------------------------------------
# 1.1. Landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
#print(mask)
mask <- st_transform(mask, crs = 4326)
# crop it:
e <- c(-3, 7, 35, 43)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)


# 1.2. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)


# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
#st_crs(Bathy_cont1) <- st_crs(mask)


# 2. Calculate mean temperature--------------------------------------------------
# Create date sequences that you wish:
# Year:
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="day")  # define sequence
year_df <- data.frame(date = dates, season = "Autumn")

# Prepare your date list and other necessary variables
dates <- year_df #spring_df, winter_df, summer_df, autumn_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

destination_folder <- paste0("output/ERA5/AT_Reanalysis/2021")

catalog <- read.csv2("input/Catalog_ERA5.csv", sep=";")
cat <- catalog #%>%
 # filter(variable %in% c("so"), product_type %in% c("Reanalysis")) 
var <- cat$variable[1]
var

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  stack_repo <- file.path(destination_folder, MM, DD)
  
  # Construct the path to the directory containing TIFF files
  pat <- "\\.nc$"
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern =  pat)
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
beep()

# Identify which elements in the list are NULL
null_indices <- which(sapply(stack_list, is.null))
null_indices

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = mean) 
#pred_med <- raster::calc(pred_stack, fun = sd) #sd for O2
beep()

# Convert from Kelvin to Celsius
pred_med_celsius <- pred_med - 273.15

# Crop the overlapping area to mask:
# Ensure mask is in the same CRS as the raster
mask <- st_transform(mask, crs = crs(pred_med_celsius))
# Convert sf object to Spatial if needed (raster::mask expects Spatial* for the mask)
mask_sp <- as(mask, "Spatial")
# Mask out overlapping areas (remove them)
pred_masked <- mask(pred_med_celsius, mask = mask_sp, inverse = TRUE)
plot(pred_masked)

# Define output paths
path <- paste0("input/Mean_enviro")
if (!dir.exists(path)) dir.create(path, recursive = TRUE)
#path <- paste0("input/SD_enviro")
#if (!dir.exists(path)) dir.create(path, recursive = TRUE)

tifffile <- paste0(path, "/2021", var, "_mean.tif")
pngfile <- paste0(path, "/2021", var, "_mean.png")
#tifffile <- paste0(path, "/SD_", var, ".tif")
#pngfile <- paste0(path, "/SD_", var, ".png")

# Save the median raster as TIFF
writeRaster(pred_masked, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_masked, main = var, col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()


# 3. Plot-----------------------------------------------------------------------
# Crop to bathymetry:
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
# Mask by bathymetry range (-800 to -40)
bathy_filtered <- calc(bathy, function(x) {
  x[x > -40 | x < -800] <- NA
  return(x)
})
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})
bathy_mask_resampled <- resample(bathy_mask, pred_masked, method = "bilinear")
risk_cropped <- raster::mask(pred_masked, bathy_mask_resampled)

# Convert raster to sf and crop by GSA06
risk_df <- as.data.frame(risk_cropped, xy = TRUE)
colnames(risk_df) <- c("x", "y", "risk")
risk_df <- risk_df %>% filter(!is.na(risk))
risk_sf <- st_as_sf(risk_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
risk_sf <- st_transform(risk_sf, crs = st_crs(GSA_filtered))
risk_clipped_sf <- st_intersection(risk_sf, GSA_filtered)
risk_clipped_df <- risk_clipped_sf %>% as.data.frame() %>%
  dplyr::select(x, y, risk) %>% filter(!is.na(risk))



# colour map:
color_palette_raster <- colorRampPalette(c('#6CA6CD','#B0E2FF','#FFFFE0','#FFFACD','#FFE4B5','#FFDAB9','#FF7F50','#FF6347','#FF4500','#8B3626'))(100)


# Create a ggplot object
p <- ggplot() +
  geom_tile(data = risk_clipped_df, aes(x = x, y = y, fill = risk)) +  #X2021bottomT_mean, filling_color, Bathy, slope, layer; Use the 'layer' name for fill
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  #GSA
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  scale_fill_gradientn(name = "ATEMP", colors = color_palette_raster, na.value = "transparent") +
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

p

# export plot
enviro <- "ATEMP" #slope, fishingEffort, BathyCont
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)



deep <- stack("output/ERA5/AT_Reanalysis/2021/01/01/stack_20210101_bathy_deep.gri")
med <- stack("output/ERA5/AT_Reanalysis/2021/01/01/stack_20210101_bathy_med.gri")
shallow <- stack("output/ERA5/AT_Reanalysis/2021/01/01/stack_20210101_bathy_shallow.gri")
plot(deep)
plot(med)
plot(shallow)
