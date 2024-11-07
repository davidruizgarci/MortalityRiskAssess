#------------------------------------------------------------------------------------
# Extract 2D data from raster to points   
#------------------------------------------------------------------------------------
library(raster)
library(ncdf4)
library(dplyr)
library(fasterize)

#Load data
data <- read.csv("output/dataset_rm_var.csv", sep = ";") #remember having date format in your .csv (actively change it)
names(data)
head(data)

# explore temporal and spatial range
# use same temporal resolution (day) and numeric for lon and lat
data$code <- data$tripID #if your time scale has not hours
data$date <- as.Date(data$time) #if your time scale has not hours
data$time_hours <- as.POSIXct(data$time_hours, format = "%d-%m-%y %H:%M", tz = "UTC")
data$lon <- as.numeric(gsub(",", ".", data$lon))
data$lat <- as.numeric(gsub(",", ".", data$lat))
range(data$date)
range(data$time_hours)
range(data$lon)
range(data$lat)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load all the netCDF and rasters.
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 1.1) Bathymetry (depth) 
bathy <- raster("input/gebco/Bathy.tif")
bathy

data$bathy <- raster::extract(bathy, cbind(data$lon, data$lat)) 
data$bathy <- abs(data$bathy)
head(data)

# 1.2) Sea bottom tempe
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
head(catalog)
cat <- catalog %>%
  filter(var_name %in% "SBT_Reanalysis")


# The only 2D variable is SBT (med-cmcc-tem-rean-d)
# make a loop to (1) open each file ".nc" (2) configure time format and (3) extract data

# Repository to folder where netCDFs are:
repo <- paste0(input_data, "/cmems") 
#example <- brick("input/cmems/MEDSEA_MULTIYEAR_PHY_006_004/med-cmcc-tem-rean-d/SBT_Reanalysis/2020/06/18/SBT_Reanalysis_2020-06-18.nc")

# Iterate over each productid in 'cat' dataframe
for (pid in unique(cat$id_product)) {
  # Filter data corresponding to current productid
  # example for checking code:  id_product <- 1
  subset_data <- subset(cat, id_product == pid)
  
  data <- cmems2d(lon=data$lon, lat=data$lat, date=data$date, productid=pid, repo=repo, data=data)
  # Print or save any necessary output or results
  print(paste("Processed productid:", pid))
}
head(data)


# 1.3) Sea bottom oxygen
data$depth <- data$bathy #if your time scale has not hours
cat <- catalog %>%
  filter(var_name %in% "SBO_Reanalysis")

# Iterate over each productid in 'cat' dataframe
for (pid in unique(cat$id_product)) {
  # Filter data corresponding to current productid
  subset_data <- subset(cat, id_product == pid)
  
  # Apply cmems3d_bottom function to subset of data
  data_tows <- cmems3d_bottom(lon=data$lon, lat=data$lat, date=data$date, productid=pid, repo=repo, data=data)
  
  # Optionally, you can assign the modified 'data' back to your original dataframe or save it somewhere
  # For example, if you want to update 'data' in place, you can do:
  # data <- data
  
  # Print or save any necessary output or results
  print(paste("Processed productid:", pid))
}
head(data)

# Save dataframe
write.csv(data_tows, "temp/data_tows_2D_3D_dist.csv", row.names = FALSE)

