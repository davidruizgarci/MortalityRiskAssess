# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------


#----------------------------------------------------------------------------------- 
# 3_2_Means: Calculate the mean, deviation, min and max for fishing operation traits for each tow, enviro parameters and biological parameters
#----------------------------------------------------------------------------------- 

# Group the data by tows and calculate mean and standard deviation for the specified columns, which are:
names(data)
# "Net_horizontal_opening", "Distance_covered_GPS", "Average_speed", "Trawl_duration", "MinsExposedtoAir", "depth"

# Merge all data from a single haul into a single row to calculate averages (add SD to check that there are no errors):
gear_df <- data %>%
  filter(!is.na(Alive_Dead)) %>%
  mutate(Net_horizontal_opening = as.numeric(gsub(",", ".", Net_horizontal_opening)),
         Distance_covered_GPS = as.numeric(gsub(",", ".", Distance_covered_GPS)),
         Average_speed = as.numeric(gsub(",", ".", Average_speed)),
         Trawl_duration = as.numeric(gsub(",", ".", Trawl_duration)),
         MinsExposedtoAir = as.numeric(gsub(",", ".", MinsExposedtoAir)),
         depth = as.numeric(gsub(",", ".", depth)),
         TotalBiomassHaul = as.numeric(gsub(",", ".", TotalBiomassHaul)),
         Sea.state = as.numeric(gsub(",", ".", Sea.state)),
         Wind.strength = as.numeric(gsub(",", ".", Wind.strength)),
         Cloud.cover = as.numeric(gsub(",", ".", Cloud.cover))) %>%
  group_by(Haul_N) %>%
  summarise(
    Mean_Net_horizontal_opening = mean(Net_horizontal_opening, na.rm = TRUE),
    SD_Net_horizontal_opening = sd(Net_horizontal_opening, na.rm = TRUE),
    Mean_Distance_covered_GPS = mean(Distance_covered_GPS, na.rm = TRUE),
    SD_Distance_covered_GPS = sd(Distance_covered_GPS, na.rm = TRUE),
    Mean_Average_speed = mean(Average_speed, na.rm = TRUE),
    SD_Average_speed = sd(Average_speed, na.rm = TRUE),
    Mean_Trawl_duration = mean(Trawl_duration, na.rm = TRUE),
    SD_Trawl_duration = sd(Trawl_duration, na.rm = TRUE),
    Mean_Trawl_duration = mean(Trawl_duration, na.rm = TRUE),
    SD_Trawl_duration = sd(Trawl_duration, na.rm = TRUE),
    Mean_depth = mean(depth, na.rm = TRUE),
    SD_Trawl_depth = sd(depth, na.rm = TRUE),
    Mean_MinsExposedtoAir = mean(MinsExposedtoAir, na.rm = TRUE),
    SD_MinsExposedtoAir = sd(MinsExposedtoAir, na.rm = TRUE),
    Mean_TotalBiomassHaul = mean(TotalBiomassHaul, na.rm = TRUE),
    SD_TotalBiomassHaul = sd(TotalBiomassHaul, na.rm = TRUE),
    Mean_Sea.state = mean(Sea.state, na.rm = TRUE),
    SD_Sea.state = sd(Sea.state, na.rm = TRUE),
    Mean_Wind.strength = mean(Wind.strength, na.rm = TRUE),
    SD_Trawl_Wind.strength = sd(Wind.strength, na.rm = TRUE),
    Mean_Cloud.cover = mean(Cloud.cover, na.rm = TRUE),
    SD_Cloud.cover = sd(Cloud.cover, na.rm = TRUE),    )

# Print the result
print(gear_df)

#Put variables as numerical:
gear_df <- gear_df %>%
  filter(Haul_N != 67) %>%
  mutate(
    Mean_Net_horizontal_opening = as.numeric(gsub(",", ".", Mean_Net_horizontal_opening)),
    Mean_Distance_covered_GPS = as.numeric(gsub(",", ".", Mean_Distance_covered_GPS)),
    Mean_Average_speed = as.numeric(gsub(",", ".", Mean_Average_speed)),
    Mean_Trawl_duration = as.numeric(gsub(",", ".", Mean_Trawl_duration)),
    Mean_MinsExposedtoAir = as.numeric(gsub(",", ".", Mean_MinsExposedtoAir)),
    Mean_depth = as.numeric(gsub(",", ".", Mean_depth)),
    Mean_TotalBiomassHaul = as.numeric(gsub(",", ".", Mean_TotalBiomassHaul)),
    Mean_Sea.state = as.numeric(gsub(",", ".", Mean_Sea.state)),
    Mean_Wind.strength = as.numeric(gsub(",", ".", Mean_Wind.strength)),
    Mean_Cloud.cover = as.numeric(gsub(",", ".", Mean_Cloud.cover)))

# Calculate overall average +-SD values:
gear_df_summary <- gear_df %>%
  summarise(
    M_Mean_Net_horizontal_opening = mean(Mean_Net_horizontal_opening, na.rm = TRUE),
    sd_Net_horizontal_opening = sd(Mean_Net_horizontal_opening, na.rm = TRUE),
    M_Mean_Distance_covered_GPS = mean(Mean_Distance_covered_GPS, na.rm = TRUE),
    sd_Distance_covered_GPS = sd(Mean_Distance_covered_GPS, na.rm = TRUE),
    M_Mean_Average_speed = mean(Mean_Average_speed, na.rm = TRUE),
    sd_Average_speed = sd(Mean_Average_speed, na.rm = TRUE),
    M_Mean_Trawl_duration = mean(Mean_Trawl_duration, na.rm = TRUE),
    sd_Trawl_duration = sd(Mean_Trawl_duration, na.rm = TRUE),
    M_Mean_MinsExposedtoAir = mean(Mean_MinsExposedtoAir, na.rm = TRUE),
    sd_MinsExposedtoAir = sd(Mean_MinsExposedtoAir, na.rm = TRUE),
    M_Mean_depth = mean(Mean_depth, na.rm = TRUE),
    sd_depth = sd(Mean_depth, na.rm = TRUE),
    M_Mean_TotalBiomassHaul = mean(Mean_TotalBiomassHaul, na.rm = TRUE),
    sd_Mean_TotalBiomassHaul = sd(Mean_TotalBiomassHaul, na.rm = TRUE),
    M_Mean_Sea.state = mean(Mean_Sea.state, na.rm = TRUE),
    sd_Mean_Sea.state = sd(Mean_Sea.state, na.rm = TRUE),
    M_Mean_Wind.strength = mean(Mean_Wind.strength, na.rm = TRUE),
    sd_Mean_Wind.strength = sd(Mean_Wind.strength, na.rm = TRUE),
    M_Mean_Cloud.cover = mean(Mean_Cloud.cover, na.rm = TRUE),
    sd_Mean_Cloud.cover = sd(Mean_Cloud.cover, na.rm = TRUE))

# Print the result
print(gear_df_summary)

#Min and Max values:
gear_df_summary_range <- gear_df %>%
  summarise(
    Min_Mean_Net_horizontal_opening = min(Mean_Net_horizontal_opening, na.rm = TRUE),
    Max_Mean_Net_horizontal_opening = max(Mean_Net_horizontal_opening, na.rm = TRUE),
    Min_Mean_Distance_covered_GPS = min(Mean_Distance_covered_GPS, na.rm = TRUE),
    Max_Mean_Distance_covered_GPS = max(Mean_Distance_covered_GPS, na.rm = TRUE),
    Min_Mean_Average_speed = min(Mean_Average_speed, na.rm = TRUE),
    Max_Mean_Average_speed = max(Mean_Average_speed, na.rm = TRUE),
    Min_Mean_Trawl_duration = min(Mean_Trawl_duration, na.rm = TRUE),
    Max_Mean_Trawl_duration = max(Mean_Trawl_duration, na.rm = TRUE),
    Min_Mean_MinsExposedtoAir = min(Mean_MinsExposedtoAir, na.rm = TRUE),
    Max_Mean_MinsExposedtoAir = max(Mean_MinsExposedtoAir, na.rm = TRUE),
    Min_Mean_depth = min(Mean_depth, na.rm = TRUE),
    Max_Mean_depth = max(Mean_depth, na.rm = TRUE),
    Min_Mean_TotalBiomassHaul = min(Mean_TotalBiomassHaul, na.rm = TRUE),
    Max_Mean_TotalBiomassHaul = max(Mean_TotalBiomassHaul, na.rm = TRUE),
    Min_Mean_Sea.state = min(Mean_Sea.state, na.rm = TRUE),
    Max_Mean_Sea.state = max(Mean_Sea.state, na.rm = TRUE),
    Min_Mean_Wind.strength = min(Mean_Wind.strength, na.rm = TRUE),
    Max_Mean_Wind.strength = max(Mean_Wind.strength, na.rm = TRUE),
    Min_Mean_Cloud.cover = min(Mean_Cloud.cover, na.rm = TRUE),
    Max_Mean_Cloud.cover = max(Mean_Cloud.cover, na.rm = TRUE))

# Print the result
print(gear_df_summary_range)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#What is the mean +- SD for size for each species? and for min an max size?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
setwd(input_data)
data <- read.csv("data_env_all.csv", sep = ";")
names(data)

# Group the data by Species and calculate mean and standard deviation for the specified columns
species_size_df <- data %>%
  mutate(TL = as.numeric(gsub(",", ".", TL)),
         DW = as.numeric(gsub(",", ".", DW)))  %>%
  group_by(Species) %>%
  summarise(
    Mean_TL = mean(TL, na.rm = TRUE),
    SD_TL = sd(TL, na.rm = TRUE),
    Mean_DW = mean(DW, na.rm = TRUE),
    SD_DW = sd(DW, na.rm = TRUE))

# Print the result
print(species_size_df)

#Min and Max size:
species_size_range <- data %>%
  mutate(TL = as.numeric(gsub(",", ".", TL)),
         DW = as.numeric(gsub(",", ".", DW)))  %>%
  group_by(Species) %>%
  summarise(
    Min_TL = min(TL, na.rm = TRUE),
    Max_TL = max(TL, na.rm = TRUE),
    Min_DW = min(DW, na.rm = TRUE),
    Max_DW = max(DW, na.rm = TRUE))

# Print the result
print(species_size_range)

# Bar chart:
# Create a new data frame with counts of specimens per size group for males (Sex = 0)
male_counts <- data_vio %>%
  filter(Sex == 0) 

Size_Group = cut(male_counts$TL, breaks = seq(5, max(male_counts$TL), by = 5), right = FALSE)
male_counts <-count(Size_Group)
male_counts <- male_counts %>%
  filter(!is.na(x))
print(male_counts)

# Create a new data frame with counts of specimens per size group for females (Sex = 1)
female_counts <- data_vio %>%
  filter(Sex == 1)

Size_Group = cut(female_counts$TL, breaks = seq(5, max(female_counts$TL), by = 5), right = FALSE)
female_counts <-count(Size_Group)
female_counts <- female_counts %>%
  filter(!is.na(x))
print(female_counts)

# Merge male and female counts into a single data frame
combined_counts <- full_join(male_counts, female_counts, by = "x") %>%
  rename(Male = freq.x, Female = freq.y) %>%
  replace_na(list(Male = 0, Female = 0))

# Reshape the data
combined_counts_long <- pivot_longer(combined_counts, cols = c(Male, Female), names_to = "Sex", values_to = "Count")

# Create a single stacked bar chart
combined_plot <- ggplot(combined_counts_long, aes(x = x, y = Count, fill = Sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Size group", y = "n") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +  # Choose appropriate colors
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(combined_plot)
p_png <- paste0(output_data, "/Gme_all_sizeDist.png")
ggsave(p_png, combined_plot, width=18, height=12, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#What is the mean +- SD for weight for each species? and for min an max size?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
names(data)

species_weight_df <- data %>%
  mutate(WeightLWR = as.numeric(gsub(",", ".", WeightLWR))) %>%  # Remove commas
  # WeightLWR = WeightLWR/1000,
  #WeightLWR = log1p(WeightLWR)) %>%
  group_by(Species) %>%
  summarise(
    Mean_WeightLWR = mean(WeightLWR, na.rm = TRUE),
    SD_WeightLWR = sd(WeightLWR, na.rm = TRUE))
print(species_weight_df)

# Group the data by Species and calculate mean and standard deviation for the specified columns
species_weight_df <- data %>%
  mutate(WeightLWR = as.numeric(gsub(",", ".", WeightLWR))/1000,  # Remove commas
         # WeightLWR = WeightLWR/1000,
         WeightLWR = log1p(WeightLWR)) %>%
  group_by(Species) %>%
  summarise(
    Mean_WeightLWR = mean(WeightLWR, na.rm = TRUE),
    SD_WeightLWR = sd(WeightLWR, na.rm = TRUE))


print(species_weight_df)

# Plot the AVM vertically:
# Reorder species:
# Define the desired order of species
desired_order <- c("Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", "Maquila", "Abovinus", "Tmarmorata")
desired_order <- rev(desired_order)
# Convert the "Species" column to a factor with the desired order
species_weight_df$Species <- factor(species_weight_df$Species, levels = desired_order)

# Create the ggplot with the reordered Species
# Modify your ggplot to include error bars for the confidence intervals
# Create the plot
p <- ggplot(species_weight_df, aes(x = Species, y = Mean_WeightLWR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_WeightLWR - SD_WeightLWR, ymax = Mean_WeightLWR + SD_WeightLWR), 
                width = 0.2, position = position_dodge(0.5)) +
  labs(x = "Species", y = "ln(Mean Weight) (kg)", title = "Mean Weight (kg)") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))
print(p)

# export plot
p_png <- paste0(output_data, "/LNweight_tree.png")
ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#What is the mean +- SD for depth for each species? and for min an max size?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
names(data)
# Group the data by Species and calculate mean and standard deviation for the specified columns
species_depth_df <- data %>%
  mutate(depth = as.numeric(gsub(",", ".", depth)))  %>%
  group_by(Species) %>%
  summarise(
    Mean_depth = mean(depth, na.rm = TRUE),
    SD_depth = sd(depth, na.rm = TRUE))

print(species_depth_df)

# Plot the depth vertically:
setwd("C:/Users/david/OneDrive/Escritorio/Survival/Figures")
depth <- read.csv("MeanDepth.csv", sep = ";")
head(depth)
depth <- depth %>%
  mutate(Mean_depth = as.numeric(gsub(",", ".", Mean_depth)),
         SD_depth = as.numeric(gsub(",", ".", SD_depth)))
# Reorder species:
# Define the desired order of species
desired_order <- c("Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", "Maquila", "Abovinus", "Tmarmorata")
desired_order <- rev(desired_order)
# Convert the "Species" column to a factor with the desired order
depth$Species <- factor(depth$Species, levels = desired_order)

# Create the ggplot with the reordered Species
# Modify your ggplot to include error bars for the confidence intervals
# Create the plot
p <- ggplot(depth, aes(x = Species, y = Mean_depth)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_depth - SD_depth, ymax = Mean_depth + SD_depth), 
                width = 0.2, position = position_dodge(0.5)) +
  labs(x = "Species", y = "Mean depth (m)", title = "Mean depth (m)") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))
print(p)

# export plot
p_png <- paste0(output_data, "/depth_tree.png")
ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#What is the mean +- SD for temp for each species? and for min an max size?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
names(data)
# Group the data by Species and calculate mean and standard deviation for the specified columns
species_sbt_df <- data %>%
  mutate(sbt_reanalysis = as.numeric(gsub(",", ".", sbt_reanalysis)))  %>%
  group_by(Species) %>%
  summarise(
    Mean_sbt_merged = mean(sbt_reanalysis, na.rm = TRUE),
    SD_sbt_merged = sd(sbt_reanalysis, na.rm = TRUE))

print(species_sbt_df)

# Create the plot
# Define the desired order of species
desired_order <- c("Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", "Maquila", "Abovinus", "Tmarmorata")
desired_order <- rev(desired_order)
# Convert the "Species" column to a factor with the desired order
species_sbt_df$Species <- factor(species_sbt_df$Species, levels = desired_order)

p <- ggplot(species_sbt_df, aes(x = Species, y = Mean_sbt_merged)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_sbt_merged - SD_sbt_merged, ymax = Mean_sbt_merged + SD_sbt_merged), 
                width = 0.2, position = position_dodge(0.5)) +
  labs(x = "Species", y = "Sea bottom temperature (ºC)", title = "Mean Weight (kg)") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))
print(p)

# export plot
p_png <- paste0(output_data, "/temp_tree.png")
ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#What is the mean +- SD for oxygen for each species? and for min an max size?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
names(data)
# Group the data by Species and calculate mean and standard deviation for the specified columns
species_sbo_df <- data %>%
  mutate(sbo_merged = as.numeric(gsub(",", ".", sbo_merged)))  %>%
  group_by(Species) %>%
  summarise(
    Mean_sbo_merged = mean(sbo_merged, na.rm = TRUE),
    SD_sbo_merged = sd(sbo_merged, na.rm = TRUE))

print(species_sbo_df)

# Create the plot
# Define the desired order of species
desired_order <- c("Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", "Maquila", "Abovinus", "Tmarmorata")
desired_order <- rev(desired_order)
# Convert the "Species" column to a factor with the desired order
species_sbo_df$Species <- factor(species_sbo_df$Species, levels = desired_order)

p <- ggplot(species_sbo_df, aes(x = Species, y = Mean_sbo_merged)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_sbo_merged - SD_sbo_merged, ymax = Mean_sbo_merged+ SD_sbo_merged), 
                width = 0.2, position = position_dodge(0.5)) +
  labs(x = "Species", y = "Sea bottom oxygen (mmol/m3)", title = "Mean Weight (kg)") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))
print(p)

# export plot
p_png <- paste0(output_data, "/oxy_tree.png")
ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)

