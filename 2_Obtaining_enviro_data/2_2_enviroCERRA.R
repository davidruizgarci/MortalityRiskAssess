# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 2_2_enviroCERRA: Download CERRA data   
#-------------------------------------------------------------------------------

library(ecmwfr)
library(dplyr)
library(beepr)


# 1. Generate daily dates-------------------------------------------------------
dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")
date_times <- paste(dates, "12:00:00")
dates <- data.frame(
  date = dates,
  date_time = as.POSIXct(date_times, format = "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE
)
dates <- dates %>%
  mutate(
    Year = format(date, "%Y"),
    Month = format(date, "%m"),
    Day = format(date, "%d"),
    Hour = format(date_time, "%H:%M")  # This keeps "12:00"
  )

# 2. Output directories---------------------------------------------------------
destination_folder <- file.path(output_data, "CERRA/AT_Reanalysis")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)


# 3. Define minimal catalog for CERRA – 2m air temperature----------------------
catalog <- data.frame(
  service = "reanalysis-cerra-single-levels",
  variable = "2m_temperature",
  product_type = "reanalysis",
  stringsAsFactors = FALSE
)

# 4. Authenticate with CDS API--------------------------------------------------
wf_set_key(user = usernameERA, key = passwordERA)


# 5. Loop to download each day--------------------------------------------------
t <- Sys.time()
for (i in 1:nrow(catalog)) {
  for (j in 1:nrow(dates)) {
    # i=1
    # j=1
    cat(paste0(
      "Processing product ", i, "/", nrow(catalog), 
      ", date ", j, "/", nrow(dates), "\n"
    ))
    
    # Create output folder by date
    dir_path <- file.path(destination_folder, dates$Year[j], dates$Month[j], dates$Day[j])
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
    
    # Run the download
    wf_request(
      user = usernameERA,
      request = list(
        dataset_short_name = catalog$service[i],
        product_type = catalog$product_type[i],
        variable = catalog$variable[i],
        year = dates$Year[j],
        month = dates$Month[j],
        day = dates$Day[j],
        time = dates$Hour[j],  # e.g., "12:00"
        area = c(43, -2, 36, 4),  # N, W, S, E (Western Mediterranean)
        format = "netcdf"
      ),
      transfer = TRUE,
      path = dir_path,
      verbose = TRUE
    )
  }
}
Sys.time() - t
beep()




