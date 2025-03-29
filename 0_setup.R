#--------------------------------------------------------------------------------
# 0_setup.R         Setup project
#--------------------------------------------------------------------------------

# set computer
cpu <- "laptop" 

# Set main data paths
if(cpu == "laptop") main_dir <- "C:/Users/david/SML Dropbox/gitdata/chondrichthyan_mortality"
setwd(main_dir)

# 2. Create data paths
input_data <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_data)) dir.create(input_data, recursive = TRUE)

temp_data <- paste(main_dir, "temp", sep="/")
if (!dir.exists(temp_data)) dir.create(temp_data, recursive = TRUE)

output_data <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


# 3. load CEMEMS username / password based on cpu
if(cpu == "laptop") f <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/user.txt"
username <- paste(readLines(f, warn = FALSE), collapse = "")
if(cpu == "laptop") f <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/psw.txt"
password <- paste(readLines(f, warn = FALSE), collapse = "")

# 3. load ERA5 username / password based on cpu
if(cpu == "laptop") f <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/userERA5.txt"
usernameERA <- paste(readLines(f, warn = FALSE), collapse = "")
if(cpu == "laptop") f <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/pswERA5.txt"
passwordERA <- paste(readLines(f, warn = FALSE), collapse = "")
