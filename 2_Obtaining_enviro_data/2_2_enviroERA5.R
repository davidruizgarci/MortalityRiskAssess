# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 2_2_enviroERA5: Download ERA5 data   
#-------------------------------------------------------------------------------
library(ecmwfr)
library(dplyr)
library(beepr)



# 1. Generate dates-------------------------------------------------------------
# Create a sequence of dates from January 1, 2021, to December 31, 2021
dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")

# Convert the dates to the desired format with the time "11:00:00"
date_times <- paste(dates, "11:00:00")

# Create a dataframe
dates <- data.frame(date = dates, date_time = date_times, stringsAsFactors = FALSE)
dates$date <- as.Date(dates$date)
dates$date_time <- as.POSIXct(dates$date_time, format = "%Y-%m-%d %H:%M:%S")

# Add a new column with the year, month and day information
dates <- dates %>%
  mutate(Year = format(date, "%Y"),
         Month = format(date, "%m"),
         Day = format(date, "%d"),
         Hour = format(date_time, "%H:%M"))
head(dates)



# 2. Define directories and open catalog----------------------------------------
# Directory to save files:
destination_folder <- paste0(output_data, "/ERA5/AT_Reanalysis")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)

# Catalog:
path <- paste0(input_data, "/Catalog_ERA5.csv")
catalogERA <- read.csv(path, sep=";")
head(catalogERA)

catalog <- catalogERA %>%
  mutate(
    xmin = as.numeric(gsub(",", ".", xmin)),
    xmax = as.numeric(gsub(",", ".", xmax)),
    ymin = as.numeric(gsub(",", ".", ymin)),
    ymax = as.numeric(gsub(",", ".", ymax)))


# 3. Proceed with download------------------------------------------------------
# login ERA5
wf_set_key(user = usernameERA, key = passwordERA)

# Define subsets
# Define the time subset you want:
df <- dates 

# Define the catalog subset you want:
#cat <- catalog
cat <- catalog #%>%
  #filter(variable %in% c("po4"), product_type %in% c("Reanalysis")) 



t <- Sys.time()
for(i in 1:nrow(cat)){ 
  
  # Calculate remaining products
  #i=1
  remaining_products <- nrow(cat) - i
  
  #If you need a folder per each date:
  for(j in 329:nrow(df)){
    # Calculate remaining dates
    #j=1
    remaining_dates <- nrow(df) - j
    
    # Print the current product and remaining products
    print(paste("Processing product", i, "of", nrow(cat), "-", remaining_products, "remaining"))
    # Print the current date and remaining dates
    print(paste("Processing date", j, "of", nrow(df), "-", remaining_dates, "remaining"))
    
  
  # Create the folder for each product if it doesn't exist already 
  dir_path <- file.path(destination_folder, df$Year[j], df$Month[j], df$Day[j])
  if (!file.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)}
  # Define the file name using the current date
  #file_name <- paste0(format(as.Date(dates$date[j], origin = "1970-01-01"), "%Y%m%d"),"_", cat$variable[i], ".nc")
  #output_path <- file.path(dir_path, file_name)
  
  # Loop through each row of the "Days" data frame
  wf_request(
    user = usernameERA,
    request = list(
    dataset_short_name = cat$service[i],
    product_type = cat$product_type[i],
    variable = cat$variable[i],
    year = df$Year[j],
    month = df$Month[j],
    day = df$Day[j], 
    time = df$Hour[j],  
    area = c(43, -2, 36, 4),
    format = "netcdf"
    ),
  transfer = TRUE,
  path = dir_path,  # Folder only
  #path = output_path,  # Explicit file name
  verbose = TRUE
  )
}
}
Sys.time() - t 
beep()

#check files:
library(ncdf4)
path <- paste0(output_data, "/ERA5/AT_Reanalysis/2021/01/01/ecmwfr_6f101d504bd7.nc")
nc <- nc_open(path)
print(nc)
# Check variable names
print(nc$var)

# Let's say you're using "t2m"
temp <- ncvar_get(nc, "t2m")
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")

# Optional: check dimensions
dim(temp)  # should be [lon, lat, time]

# Read the time values (from valid_time instead of time)
time_raw <- ncvar_get(nc, "valid_time")

# Check the units of valid_time
time_units <- ncatt_get(nc, "valid_time", "units")$value
print(time_units)

# Convert to POSIXct
time_posix <- as.POSIXct(time_raw, origin = "1970-01-01", tz = "UTC")

# View it
print(time_posix)

# Close the connection
nc_close(nc)

path <- paste0(output_data, "/ERA5/AT_Reanalysis/2021/01/01/ecmwfr_6f101d504bd7.nc")
nc <- raster(path)
print(nc)
plot(nc)

