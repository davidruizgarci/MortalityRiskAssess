# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# Download CMEMS data      
#------------------------------------------------------------------------------------
library(reticulate)
library(lubridate)
library(dplyr)
library(beepr)

# Define the name of the file and the destination
destination_folder <- paste0(input_data, "/cmems")

# Import data catalog
catalog <- read.csv("input/Catalog_CMEMS.csv", sep=";")
head(catalog)

# Import dataset
data <- read.csv2("temp/AVM_allEnviro.csv", sep = ";")

# extract the dates in which there is data:
data$date <- as.Date(data$time)
Days <- unique(data$date)
Days_df <- data.frame(Days)
Days_df$Days <- as.Date(Days_df$Days)

# Add a new column with the year information
Days_df <- Days_df %>%
  mutate(Year = format(Days, "%Y"),
         Month = format(Days, "%m"),
         Day = format(Days, "%d"))
head(Days_df)

# add mins and secs:
# Note: 11:00:00 if you use 12:00:00 CMEMS use the next day
Days_df$Days_with_time <- paste0(Days_df$Days, " 11:00:00")

min(Days_df$Days_with_time)
max(Days_df$Days_with_time)


# 2) Load CMEMS package through python (currently CMEMS data can only be accessed this way) -------------------------
# install python 
#install_python() 
#virtualenv_create(envname = "cmems")
#virtualenv_install("cmems", packages = c("copernicusmarine"))
use_virtualenv("cmems", required = TRUE)

# load package / import library (py)
cm <- import("copernicusmarine")

# log in in your CMEMS user (you should have another script with this info)
cm$login(username, password)
# for session yes == y
y





# 3) Download data -------------------------------------------------------------
# Define the time subset you want:
df <- Days_df 

# Define the catalog subset you want:
cat <- catalog
#cat <- catalog %>%
#  filter(variable %in% c("uo", "vo"), product_type %in% c("Reanalysis")) 


# Define the name of the file and the destination
destination_folder <- paste0(input_data, "/cmems")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)

t <- Sys.time()
for(i in 1:nrow(cat)){ 
  
  # Calculate remaining products
  remaining_products <- nrow(cat) - i
  
  # Create the folder for each product if it doesn't exist already 
  dir_path <- file.path(destination_folder, cat$service[i], cat$layer[i], cat$var_name[i])
  if (!file.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)}
  
  #If you need a folder per each date:
  for(j in 1:nrow(df)){
    # Calculate remaining dates
    remaining_dates <- nrow(df) - j
    
    # Print the current product and remaining products
    print(paste("Processing product", i, "of", nrow(cat), "-", remaining_products, "remaining"))
    # Print the current date and remaining dates
    print(paste("Processing date", j, "of", nrow(df), "-", remaining_dates, "remaining"))
    
    
    # Create folders for different dates inside the variable folders
    date_dir <- file.path(dir_path, df$Year[j], df$Month[j], df$Day[j])
    if (!file.exists(date_dir)) {
      dir.create(date_dir, recursive = TRUE)}
    
    # Define the file name using the current date
    file_name <- paste0(cat$var_name[i], "_", df$Days[j], ".nc")
    
    # download data
    cm$subset(
      dataset_id = cat$layer[i],
      start_datetime = df$Days_with_time[j], #format example "1994-05-16 12:00:00"
      end_datetime = df$Days_with_time[j],
      variables = list(cat$variable[i]), # attention - variable must be a list
      minimum_longitude = cat$xmin[i],
      maximum_longitude =  cat$xmax[i],
      minimum_latitude =  cat$ymin[i],
      maximum_latitude = cat$ymax[i],
      minimum_depth = cat$depth_min[i],
      maximum_depth = cat$depth_max[i],
      output_filename = file_name,
      output_directory = date_dir,
      force_download = TRUE)
  }
}
Sys.time() - t 
beep()



