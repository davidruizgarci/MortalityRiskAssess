# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_1_predict_AVM
#-------------------------------------------------------------------------------
library(raster)
library(sf)
library(lubridate)
library(base)
library(dplyr)

# 1. Set data repositories and seeting------------------------------------------
data <- read.csv("temp/final/AVM_allEnviro.csv", sep = ";") #
data <- data %>%
  mutate(
    MinsExposedtoAir =  as.numeric(gsub(",", ".", MinsExposedtoAir)),
    Trawl_duration =  as.numeric(gsub(",", ".", Trawl_duration))) %>% 
  filter(!is.na(MinsExposedtoAir), 
         !is.na(Trawl_duration))

summary(data$MinsExposedtoAir)
summary(data$Trawl_duration)


#indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")

outdir <- paste0(output_data, "/predict")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

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

# 2. Create dataframe with dates to be predicted--------------------------------
# Create dates
date_start <- as.Date("2021-01-01") 
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="30 days")  

#3. Create species list:
species_list <- unique(data$Species)


# 3. Set raster traits----------------------------------------------------------

bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)
# Filter the values between -50 and -600 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x >= -0 | x <= -800] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Load one of the stacks as example to match features:
date <- dates[1]
pat <- paste0( "stack_", format(date, "%Y%m%d"), "_bathy_shallow.grd")
# Get list of all month folders
stack_repo <- paste0("output/ERA5/AT_Reanalysis/2021/01/01")
grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
s <- raster::stack(grdfile)
s <- s+0
s <- crop(s, e)
s
#plot(s)

#remove (mask out) all areas of a raster (bathy_mask) that overlap with polygons in a vector layer (mask).
# First, make sure both layers are in the same CRS
#mask <- st_transform(mask, crs = crs(bathy_mask))
# Mask the raster — remove areas **inside** the polygons
#bathy_mask_removed <- raster::mask(bathy_mask, mask, inverse = TRUE)
#plot(bathy_mask_removed)

# make sure it is in the format of s:
bathy_mask_resampled <- resample(bathy_mask, s, method = "bilinear")
bathy_mask_resampled <- crop(bathy_mask_resampled, e)
#plot(bathy_mask_resampled)


# 2. Load model-----------------------------------------------------------------
#path <- paste0(output_data, "/model/AVM/AVM_GAMM.rds")
#path <- paste0(output_data, "/model/AVM/AVM_GAMM_interaction.rds")
#path <- paste0(output_data, "/model/AVM/AVM_GAMM_nodepth.rds")
path <- paste0(output_data, "/model/AVM/AVM_GAMM_INTER2.rds")
gamm_model <- readRDS(path)
summary(gamm_model)

# Load the mean weight of each species:
bodymass <- c(
  "Abovinus" = 9.078864009,
  "Cmonstrosa" = 5.147494477,
  "Cuyato" = 7.647786045,
  "Dlicha" = 5.549076085,
  "Doxyrinchus" = 5.252273428,
  "Dpastinaca" = 9.230241034,
  "Espinax" = 4.412798293,
  "Gmelastomus" = 5.247024072,
  "Hgriseus" = 11.1889113,
  "Maquila" = 7.92551898,
  "Ocentrina" = 8.049107721,
  "Pviolacea" = 9.239122173,
  "Rasterias" = 6.502790046,
  "Rclavata" = 6.612041035,
  "Rpolystigma" = 5.468060141,
  "Scanicula" = 5.081404365,
  "Tmarmorata" = 5.463831805
)

############### since "2021-10-06"" i=152 dates[152] =  mins_list <- 55.35 trawl_list <-  4.067 

# 3. Make spatial predict for each metier---------------------------------------
#select depth range:
bathy_range <- "_bathy_deep" #"_bathy_shallow" #"_bathy_med" #"_bathy_deep"

for (i in 1:length(dates)) {
  #i=279
  # Get time information
  date <- dates[i]
  print(date)
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  select <-  bathy_range
  
  # Locate file
  #pat <- paste0("stack_", format(date, "%Y%m%d"), ".grd")
  pat <- paste0("^stack_", format(date, "%Y%m%d"), select, "*\\.grd$")
  stack_repo <- paste0("output/ERA5/AT_Reanalysis/2021/", MM, "/", DD)
  grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  
  # Skip if no stack found
  if (length(grdfile) == 0) {
    message("⚠️ Skipping ", format(date, "%Y-%m-%d"), " — no stack found.")
    next
  }
  
  # Import environmental stack
  s <- raster::stack(grdfile)
  s <- s + 0
  s <- crop(s, e)
  s <- raster::mask(s, bathy_mask_resampled)
  #plot(s)
  
  # Fix units and variable names
  s$atm_temp <- s$atm_temp - 273.15
  names(s) <- c("at_celsius", "depth")
  
  # Filter metier combinations based on depth distribution in the raster
  depth_vals <- values(s[["depth"]])
  depth_vals <- depth_vals[!is.na(depth_vals)]
  summary(depth_vals)
  metiers_to_use <- unique(c(
    if (any(depth_vals > - 200)) 1,
    if (any(depth_vals <= -200 & depth_vals > -500)) 2,
    if (any(depth_vals <= -500 & depth_vals > -800)) 3
  ))
  
  # Filter valid Vessel × Metier combinations
  all_combs <- unique(gamm_model$model[, c("Vessel", "Metier")])
  combs <- subset(all_combs, Metier %in% metiers_to_use)
  # Skip if no relevant combinations found
  if (nrow(combs) == 0) {
    message("⚠️ Skipping ", format(date, "%Y-%m-%d"), " — no valid Metier × Vessel combinations for depth range.")
    next
  }
  
  #1st, median, 3rd quartiles for the fixed variables:
  # Define specific (mins, trawl) combinations
  mins_list <- c(55.35) #10, 28.71, 40.71, 55.35
  trawl_list <- c(4.067) #2.85, 3.417, 4.067
  
  for (sp_fixed in species_list) {
    # Safety check
    #sp_fixed <- species_list[1]
    bm_value <- bodymass[sp_fixed]
    if (is.na(bm_value)) stop(paste("Missing bodymass value for species:", sp_fixed))
    
    # Create constant rasters for this species
    ln_bodymass_r <- s[[1]]; values(ln_bodymass_r) <- bm_value
    
    for (k in seq_along(mins_list)) {
      #k <- seq_along(mins_list)[1]
      mins_val <- mins_list[k]
      trawl_val <- trawl_list[k]
     
        
        MinsExposedtoAir_r <- s[[1]]; values(MinsExposedtoAir_r) <- mins_val
        Trawl_duration_r <- s[[1]]; values(Trawl_duration_r) <- trawl_val
        
    
    # Stack everything together
    predictors <- stack(
      s[["at_celsius"]],
      s[["depth"]],
      ln_bodymass_r,
      MinsExposedtoAir_r,
      Trawl_duration_r
    )
    names(predictors) <- c("at_celsius", "depth", "ln_bodymass", "MinsExposedtoAir", "Trawl_duration")
    #predictors
    

    # Initialize list to hold predictions
    stack_list <- list()
    
    
  for (j in 1:nrow(combs)) {
      #j=1
      vessel_fixed <- combs$Vessel[j]
      metier_fixed <- combs$Metier[j]
      
      # Define prediction function with fixed random effects
      predict_fixed_randoms <- function(model, ...) {
        vals <- list(...)[[1]]  # <- fix is here
        
        input <- data.frame(
          at_celsius       = vals[1],
          depth            = vals[2],
          ln_bodymass      = vals[3],
          MinsExposedtoAir = vals[4],
          Trawl_duration   = vals[5],
          Species = factor(sp_fixed, levels = levels(model$model$Species)),
          Metier  = factor(metier_fixed, levels = levels(model$model$Metier)),
          Vessel  = factor(vessel_fixed, levels = levels(model$model$Vessel))
        )
        
        predict(model, newdata = input, type = "response")
      }
      r_pred <- raster::predict(
        predictors,
        fun = predict_fixed_randoms,
        model = gamm_model
      )
      
      
            # Store in list
      stack_list[[paste(sp_fixed, vessel_fixed, metier_fixed, sep = "_")]] <- r_pred
  }
    # Combine predictions
    pred_stack <- stack(stack_list)
    
    # Calculate summaries
    pred_med <- raster::calc(pred_stack, median)
    pred_cil <- raster::calc(pred_stack, fun = function(x) quantile(x, probs = 0.025, na.rm = TRUE))
    pred_ciu <- raster::calc(pred_stack, fun = function(x) quantile(x, probs = 0.975, na.rm = TRUE))
    pred_cir <- pred_ciu - pred_cil
    
    # Set/create output folder
    product_folder <- file.path(outdir, YYYY, MM)
    if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)
    
    # Save files
    combo_label <- paste0("Mins", round(mins_val), "_Trawl", round(trawl_val, 1))
    #mins_val <- mins_list[3]
    #trawl_val <-  trawl_list[3]
    
    base_name <- paste0(format(date, "%Y%m%d"), "_", sp_fixed, select, "_", combo_label)
    writeRaster(pred_med, file.path(product_folder, paste0(base_name, "_INTER2_pred.tif")), overwrite = TRUE)
    writeRaster(pred_cir, file.path(product_folder, paste0(base_name, "_INTER2_pred_cir.tif")), overwrite = TRUE)
    
    # Export PNGs
    #png(file.path(product_folder, paste0(base_name, "_pred.png")), width = 560, height = 600, res = 100)
    #plot(pred_med, main = paste(sp_fixed, "   Model:", "\n", date), col = viridis::viridis(100))
    #plot(mask, col = "grey80", border = "grey60", add = TRUE)
    #text(x = -3.5, y = 44, labels = date)
    #box()
    #dev.off()
    
    #png(file.path(product_folder, paste0(base_name, "_pred_cir.png")), width = 560, height = 600, res = 100)
    #plot(pred_cir, main = paste(sp_fixed, "   Model:", "\n", date), col = viridis::viridis(100))
    #plot(mask, col = "grey80", border = "grey60", add = TRUE)
    #text(x = -3.5, y = 44, labels = date)
    #box()
    #dev.off()
    }
   }
  }
 

