#-----------------------------------------------------------------------------------------------------------------
# 1. Calculate At Vessel Mortality (AVM) and Post-Release Mortality (PRM) rates; 
# 2. Calculate average sampling conditions;
# 3. Calculate discarding rate
# 4. Calculate abortion rate
# 5. Calculate general average parameters
#-----------------------------------------------------------------------------------------------------------------

#Load data
setwd(input_data)
data <- read.csv("AVM_data_Final.csv", sep = ";")
names(data)
head(data)

data <- data %>% 
  filter(!is.na(Alive_Dead)) %>% 
  mutate(Alive_Dead = factor(Alive_Dead, c(0, 1)))

#-----------------------------------------------------------------------------------------------------------------
# 1. Calculate At Vessel Mortality (AVM) and Post-Release Mortality (PRM) rates; 
#-----------------------------------------------------------------------------------------------------------------

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#How many specimens and species are there in total?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
nrow(data) #2779

#What species are there?
sp <- unique(data$Species)
sp #"Scanicula"   "Gmelastomus" "Espinax"     "Dlicha"      "Hgriseus"    "Cuyato"      "Ocentrina"   "Tmarmorata"  "RajaSp"      "RPolystigma" "Rpolystigma" "Rclavata" "Rasterias"   "Pviolacea"   "Dpastinaca"  "Doxyrinchus" "Cmonstrosa"  "Maquila"     "Abovinus"   
length(sp) #17

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#How many specimens of each?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
species_counts_df <- data %>%
  group_by(Species) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Arrange by Count in descending order
# Print the result
print(species_counts_df)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#What percentage of these perished at vessel (AVM)?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# Calculate the percentage of rows where Alive_Dead is 0 for each Species
AVM_percentage_df <- data %>%
  filter(!is.na(Alive_Dead)) %>%
  group_by(Species) %>%
  summarise(
    TotalCount = n(),  # Total count of rows for each species
    AliveDead0Count = sum(Alive_Dead == 0),  # Count of rows where Alive_Dead is 0
    PercentageAliveDead0 = (AliveDead0Count / TotalCount) * 100
  ) %>%
  arrange(desc(TotalCount))  # Arrange by TotalCount in descending order

# Print the result
print(AVM_percentage_df)

# Plot the AVM vertically:
# Reorder species:
# Define the desired order of species
desired_order <- c("Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", "Maquila", "Abovinus", "Tmarmorata")
desired_order <- rev(desired_order)
# Convert the "Species" column to a factor with the desired order
AVM_percentage_df$Species <- factor(AVM_percentage_df$Species, levels = desired_order)

# Create the ggplot with the reordered Species
p <- ggplot(AVM_percentage_df, aes(x = Species, y = PercentageAliveDead0)) +
  geom_pointrange(aes(ymin = PercentageAliveDead0, ymax = PercentageAliveDead0)) +
  labs(x = "Species", y = "AVM rate (%)", title = "Coefficient Values by Model") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)

# export plot
p_png <- paste0(output_data, "/AVM_tree.png")
ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#What is the mean +- SD for activity, wounds and bruising for each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

# Onboard it was easier to use the scale 0, 1, 2, 3 instead of 0, 0.33, 0.66, 1. Thus, you need to convert such values to fit it to 1.
# 1 must be likely to die and 0 likely to survive, I used the scale the other way around as in Braccini et al. (2012).
# However, for our research purposes it is more clear to use it the other way around.
# Thus values must be transfermed as:
# 3 -> 0 (0% likely to die; 100% likely to survive)
#' 2 -> 0.33 (33% likely to die)
#' 1 -> 0.66 (66% likely to die)
#' 0 -> 1 (100 % likely to die)
#'To do so:
# Recode values in the specified columns
data <- data %>%
  mutate(
    Activity = case_when(
      Activity == 3 ~ 1,
      Activity == 2 ~ 0.66,
      Activity == 1 ~ 0.33,
      Activity == 0 ~ 0,
      TRUE ~ Activity  # Keep other values unchanged
    ),
    Wunds = case_when(
      Wunds == 3 ~ 0,
      Wunds == 2 ~ 0.33,
      Wunds == 1 ~ 0.66,
      Wunds == 0 ~ 1,
      TRUE ~ Wunds
    ),
    Brusing = case_when(
      Brusing == 3 ~ 0,
      Brusing == 2 ~ 0.33,
      Brusing == 1 ~ 0.66,
      Brusing == 0 ~ 1,
      TRUE ~ Brusing
    )
  ) %>%
  filter(!is.na(Activity)) %>%
  filter(Alive_Dead == 1)

# Verify that the values have been recoded as expected
head(data)  # Display the first few rows of the modified data
nrow(data) #1124

#Save dataset
#output_file <- file.path(output_data, "AVM_PRM_all.csv")
#write.csv2(data, file = output_file, row.names = FALSE)

# Group the data by Species and calculate mean and standard deviation for the specified columns
species_stats_df <- data %>%
  filter(Alive_Dead == 1) %>%
  group_by(Species) %>%
  summarise(
    Mean_Activity = mean(Activity, na.rm = TRUE),
    SD_Activity = sd(Activity, na.rm = TRUE),
    Mean_Wunds = mean(Wunds, na.rm = TRUE),
    SD_Wunds = sd(Wunds, na.rm = TRUE),
    Mean_Brusing = mean(Brusing, na.rm = TRUE),
    SD_Brusing = sd(Brusing, na.rm = TRUE))
  
# Print the result
print(species_stats_df)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# What is the mean PRM rate for each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#' We first need to calculate the the PRM probability for each specimen:
data <- data %>%
  mutate(PRM_prob = (1 - (Activity * Wunds * Brusing))*100)
head(data)

#Without bruising:
data <- data %>%
  mutate(PRM_prob_actwound = (1 - (Activity * Wunds))*100)
head(data)

# Group the data by Species and calculate mean and standard deviation for the specified columns
PRM_rate_df <- data %>%
  group_by(Species) %>%
  summarise(
    Mean_PRM_prob = mean(as.numeric(PRM_prob), na.rm = TRUE),
    SD_PRM_prob = sd(as.numeric(PRM_prob), na.rm = TRUE))

# Print the result
print(PRM_rate_df)

#Without bruising:
PRM_rate_df_actwound  <- data %>%
  group_by(Species) %>%
  summarise(
    Mean_PRM_prob_actwound  = mean(as.numeric(PRM_prob_actwound ), na.rm = TRUE),
    SD_PRM_prob_actwound  = sd(as.numeric(PRM_prob_actwound ), na.rm = TRUE))

# Print the result
print(PRM_rate_df_actwound)

# Save dataset:
#Save dataset
#output_file <- file.path(output_data, "AVM_PRM_all.csv")
#write.csv2(data, file = output_file, row.names = FALSE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# What is the total PRM rate adding AVM and PRM?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# (1) You first need to integrate the AVM and PRM total into a single dataframe:
# Select the columns from AVM_percentage_df
avm_subset <- AVM_percentage_df %>%
  dplyr::select(Species, PercentageAliveDead0)

# Select the rows from PRM_rate_df for specific Species values
prm_subset <- PRM_rate_df %>%
  filter(Species %in% c("Abovinus", "Dpastinaca", "Gmelastomus", "Maquila", "Hgriseus", "Pviolacea", "Rclavata", "Rasterias", "Rpolystigma", "Scanicula", "Tmarmorata", "Cmonstrosa")) %>%
  dplyr::select(Species, Mean_PRM_prob)

# Merge the data from avm_subset and prm_subset
combined_data <- full_join(avm_subset, prm_subset, by = "Species")

# Select the rows from PRM_rate_df_actwound for specific Species values
actwound_subset <- PRM_rate_df_actwound %>%
  filter(Species %in% c("Cuyato", "Dlicha", "Espinax", "Ocentrina")) %>%
  dplyr::select(Species, Mean_PRM_prob_actwound)

# Merge the data from actwound_subset with the combined_data
combined_data <- full_join(combined_data, actwound_subset, by = "Species")

# Replace NA values in "Mean_PRM_prob" with values from "Mean_PRM_prob_actwound"
combined_data <- combined_data %>%
  mutate(Mean_PRM_prob = coalesce(Mean_PRM_prob, Mean_PRM_prob_actwound))

# Remove the "Mean_PRM_prob_actwound" column if you no longer need it
combined_data <- combined_data %>%
  dplyr::select(-Mean_PRM_prob_actwound)

print(combined_data)

# (2) Now calculate the the total mortality probability
#Calculate the mortality probability for each species by combining both at-vessel mortality and post-release mortality. The formula to calculate the total mortality probability is:
#  Total Mortality Probability = (PercentageAliveDead0 / 100) + ((100 - PercentageAliveDead0) / 100 * (Mean_PRM_prob / 100))
# Calculate the total mortality probability for each species
combined_data <- combined_data %>%
  mutate(Total_Mortality_Prob = (PercentageAliveDead0 / 100) + ((100 - PercentageAliveDead0) / 100 * (Mean_PRM_prob / 100)))%>%
  mutate(Total_Mortality_Prob = Total_Mortality_Prob * 100)

# View the updated dataframe
combined_data$Total_Mortality_Prob[is.na(combined_data$Total_Mortality_Prob)] <- 100
print(combined_data)

# Reorder species:
# Define the desired order of species
desired_order <- c("Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", "Maquila", "Abovinus", "Tmarmorata")
desired_order <- rev(desired_order)
# Convert the "Species" column to a factor with the desired order
combined_data$Species <- factor(combined_data$Species, levels = desired_order)

# Create the ggplot with the reordered Species
p <- ggplot(combined_data, aes(x = Species, y = Total_Mortality_Prob)) +
  geom_pointrange(aes(ymin = Total_Mortality_Prob, ymax = Total_Mortality_Prob)) +
  labs(x = "Species", y = "Estimated total PCM rate (%)", title = "Coefficient Values by Model") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)

# export plot
p_png <- paste0(output_data, "/PCM_tree.png")
ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)

###################################################################################
#----------------------------------------------------------------------------------- 
# 2. What is the mean +- SD for gear values for each tow? and for min an max values?
#----------------------------------------------------------------------------------- 
###################################################################################
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

###################################################################################
#----------------------------------------------------------------------------------- 
# 3. Calculate discarding rate
#----------------------------------------------------------------------------------- 
###################################################################################

#Load data
setwd(input_data)
CPUE <- read.csv("CPUE_PA_BioParameters.csv", sep = ";")
names(CPUE)
head(CPUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#How many discarded and commercialized of each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# Eliminate strange value:
CPUE <- CPUE %>%
  filter(N.commercialised != "Puesto a secar para vender a restaurante gourmet (N=151)")

CPUE <- CPUE %>%
  mutate(
    N.discarded = as.numeric(gsub(",", ".", N.discarded)),
    N.commercialised = as.numeric(gsub(",", ".", N.commercialised)))

CPUE$Species <- gsub(" ", "", CPUE$Species)

# Calculate total N discarded and N commercialised per species
totals <- aggregate(cbind(N.discarded, N.commercialised) ~ Species, data = CPUE, FUN = sum)

# Calculate the percentage discaded:
totals <- totals %>%
  mutate(PercentageDiscarded = (N.discarded / (N.discarded + N.commercialised)) * 100)

totals <- totals %>%
  mutate(PercentageCommercialised = (N.commercialised / (N.commercialised + N.discarded)) * 100)

# Print the result
print(totals)

###################################################################################
#----------------------------------------------------------------------------------- 
# 4. Calculate abortion rate
#----------------------------------------------------------------------------------- 
###################################################################################
#Load data
setwd(input_data)
CPUE <- read.csv("CPUE_PA_BioParameters.csv", sep = ";")
names(CPUE)
head(CPUE)

# Eliminate strange value:
CPUE <- CPUE %>%
  mutate(
    N.pregnant = as.numeric(gsub(",", ".", N.pregnant)),
    N.abortion = as.numeric(gsub(",", ".", N.abortion)), 
    N..mature.females = as.numeric(gsub(",", ".", N..mature.females))) %>%
  filter(!is.na(N.pregnant), N.pregnant != " ", N.pregnant != "-", N..mature.females != 0)

CPUE$Species <- gsub(" ", "", CPUE$Species)

# Calculate total N discarded and N commercialised per species
totals <- aggregate(cbind(N.pregnant, N.abortion, N..mature.females) ~ Species, data = CPUE, FUN = sum)

# Calculate the percentage discaded:
totals <- totals %>%
  mutate(PercentagePregnant = (N.pregnant / (N..mature.females)) * 100)

totals <- totals %>%
  mutate(PercentageAbort = (N.abortion / (N.pregnant)) * 100)

# Print the result
print(totals)

#-----------------------------------------------------------------------------------------------------------------
# 5. Calculate average general parameters 
#-----------------------------------------------------------------------------------------------------------------

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

#Violin plot:
data_vio <- data %>%
  filter(Species %in% c("Scanicula", "Gmelastomus")) %>%
  mutate(TL = as.numeric(gsub(",", ".", TL)))

# Create a custom variable for the combination of Species and Sex
data_vio$Species_Sex <- paste(data_vio$Species, data_vio$Sex, sep = "_")

# Define custom colors for the combinations
custom_colors <- c("Scanicula_0" = "#A9D18E",
                   "Scanicula_1" = "#A9D18E", #
                   "Gmelastomus_0" = "#F4B183",
                   "Gmelastomus_1" = "#F4B183")

# Create the violin plot
p <- ggplot(data_vio, aes(x = Species, y = TL, fill = Species_Sex)) +
  geom_violin() +
  labs(x = "Species", y = "TL (cm)") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)

# export plot
p_png <- paste0(output_data, "/Size_Sex_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

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

#Violin plot:
data <- data %>%
  mutate(WeightLWR = as.numeric(gsub(",", ".", WeightLWR))/1000,  # Remove commas
         WeightLWR = log1p(WeightLWR))
data$Species <- factor(data$Species, levels = desired_order)

p <- ggplot(data, aes(x = Species, y = WeightLWR)) +
  geom_violin(fill = "lightblue") +
  #geom_point(aes(y = WeightLWR), position = position_dodge(0.5), size = 3) +
  labs(x = "Species", y = "ln(Mean Weight) (kg)", title = "Mean Weight (kg)") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))
print(p)
# export plot
p_png <- paste0(output_data, "/LNweight_tree_VIOLIN.png")
ggsave(p_png, p, width=6, height=12, units="cm", dpi=300)


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

#Violin plot:
# Create the violin plot
data_vio$depth <- as.numeric(gsub(",", ".", data_vio$depth))
p <- ggplot(data_vio, aes(x = Species, y = depth, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "Depth (m)") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
p_png <- paste0(output_data, "/Depth_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)
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

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#Let's make plots for each fishing condition:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
names(data_vio)
#Trawl_duration
data_vio$diff_sst_sbt <- as.numeric(gsub(",", ".", data_vio$diff_sst_sbt))
p <- ggplot(data_vio, aes(x = Species, y = diff_sst_sbt, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "diff_sst_sbt") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
if (!dir.exists(p_png)) dir.create(p_png, recursive = TRUE)
p_png <- paste0(output_dir, "/difftemp_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)



#Trawl_duration
data_vio$Trawl_duration <- as.numeric(gsub(",", ".", data_vio$Trawl_duration))
p <- ggplot(data_vio, aes(x = Species, y = Trawl_duration, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "Trawl duration (h)") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
if (!dir.exists(p_png)) dir.create(p_png, recursive = TRUE)
p_png <- paste0(output_dir, "/Trawl_duration_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)
#"Sea.state"
data_vio$Sea.state <- as.numeric(gsub(",", ".", data_vio$Sea.state))
p <- ggplot(data_vio, aes(x = Species, y = Sea.state, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "Sea state") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
if (!dir.exists(p_png)) dir.create(p_png, recursive = TRUE)
p_png <- paste0(output_dir, "/Seastate_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#Cloud.cover
data_vio$Cloud.cover <- as.numeric(gsub(",", ".", data_vio$Cloud.cover))
p <- ggplot(data_vio, aes(x = Species, y = Cloud.cover, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "Cloud.cover") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/CloudCover_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#Distance_covered_GPS
data_vio$Distance_covered_GPS <- as.numeric(gsub(",", ".", data_vio$Distance_covered_GPS))
p <- ggplot(data_vio, aes(x = Species, y = Distance_covered_GPS, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "Distance_covered_GPS") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/Distance_covered_GPS_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#Average_speed
data_vio$Average_speed <- as.numeric(gsub(",", ".", data_vio$Average_speed))
p <- ggplot(data_vio, aes(x = Species, y = Average_speed, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "Average_speed") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/Average_speed_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#MinsExposedtoAir
data_vio$MinsExposedtoAir <- as.numeric(gsub(",", ".", data_vio$MinsExposedtoAir))
p <- ggplot(data_vio, aes(x = Species, y = MinsExposedtoAir, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "MinsExposedtoAir") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/MinsExposedtoAir_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#diff_sst_sbt
data_vio$diff_sst_sbt <- as.numeric(gsub(",", ".", data_vio$diff_sst_sbt))
p <- ggplot(data_vio, aes(x = Species, y = diff_sst_sbt, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "diff_sst_sbt") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/diff_sst_sbt_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#diff_sso_sbo
data_vio$diff_sso_sbo<- as.numeric(gsub(",", ".", data_vio$diff_sso_sbo))
p <- ggplot(data_vio, aes(x = Species, y = diff_sso_sbo, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "diff_sso_sbo") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/diff_sso_sbo_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#diff_SSSAL_SBSAL
data_vio$diff_SSSAL_SBSAL <- as.numeric(gsub(",", ".", data_vio$diff_SSSAL_SBSAL))
p <- ggplot(data_vio, aes(x = Species, y = diff_SSSAL_SBSAL, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "diff_SSSAL_SBSAL") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/diff_SSSAL_SBSAL_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#diff_SSph_SBph
data_vio$diff_SSph_SBph <- as.numeric(gsub(",", ".", data_vio$diff_SSph_SBph))
p <- ggplot(data_vio, aes(x = Species, y = diff_SSph_SBph, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "diff_SSph_SBph") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/diff_SSph_SBph_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)

#TotalBiomassHaul
data_vio$TotalBiomassHaul <- as.numeric(gsub(",", ".", data_vio$TotalBiomassHaul))
p <- ggplot(data_vio, aes(x = Species, y = TotalBiomassHaul, fill = Species)) +
  geom_violin() +
  labs(x = "Species", y = "TotalBiomassHaul") +
  scale_fill_manual(values = c("#F4B183", "#A9D18E")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)
# export plot
output_dir <- paste0(output_data, "/violinplots")
p_png <- paste0(output_dir, "/TotalBiomassHaul_VIOLIN.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)
