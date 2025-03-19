# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

#-------------------------------------------------------------------------------


library(dplyr)

#-------------------------------------------------------------------------------
# 1_2_TotalFigures: Calculate total biomass and individuals captured
#-------------------------------------------------------------------------------
path <- paste0(input_data, "/CPUE/ECEME/CPUE_PA_PCA.csv")
data <- read.csv(path, sep = ";")
head(data)


# 1. Calculate total biomass----------------------------------------------------
# Select columns that start with "Kg_"
filtered_data <- data %>% select(starts_with("Kg_"))

# View the first rows of the filtered dataset
head(filtered_data)

# Convert commas to dots and transform to numeric
filtered_data <- filtered_data %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", ".", ., fixed = TRUE))))

# Sum all values in the dataset
total_sum <- sum(filtered_data, na.rm = TRUE)

# Print the total sum
print(total_sum)

# Calculate the sum for each column and store it as a dataframe
column_sums <- data.frame(Total_Sum = colSums(filtered_data, na.rm = TRUE))

# Print the total sum for each column
print(column_sums)

# 2. Calculate total specimens--------------------------------------------------
# Select columns that start with "Kg_"
filtered_data <- data %>% select(starts_with("N_"))

# View the first rows of the filtered dataset
head(filtered_data)

# Convert commas to dots and transform to numeric
filtered_data <- filtered_data %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", ".", ., fixed = TRUE))))

# Sum all values in the dataset
total_sum <- sum(filtered_data, na.rm = TRUE)

# Print the total sum
print(total_sum)

# Calculate the sum for each column and store it as a dataframe
column_sums <- data.frame(Total_Sum = colSums(filtered_data, na.rm = TRUE))

# Print the total sum for each column
print(column_sums)
