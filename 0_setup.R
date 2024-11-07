#--------------------------------------------------------------------------------
# 0_setup.R         Setup project
#--------------------------------------------------------------------------------

# set computer
cpu <- "laptop" 

# Set main data paths
if(cpu == "laptop") main_dir <- "C:/Users/david/OneDrive/Escritorio/PRM_paper/PGLS_mortality"

# 2. Create data paths
input_data <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_data)) dir.create(input_data, recursive = TRUE)

output_data <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

#Install packages from github if needed:
#devtools::install_github('pepijn-devries/CopernicusMarine')

# Load required packages
pacman::p_load(dplyr, raster, sf, lubridate, ggplot2, rnaturalearth, rnaturalearthdata, CopernicusMarine, doParallel, 
               beepr, tidyverse, data.table, splitstackshape, tidyr, parallel,
               ncdf4, foreach, stringr, groupdata2, magrittr, grid, corrplot,
               nlme, ape, caper, car, bbmle, install = FALSE) 
