# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
# 4_1_AVM_PRM_rates:  Comparisons of means across predictors
#-----------------------------------------------------------------------------------------------------------------

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# Have deepwater species a worse health than shallow-water? and higher mortality risk?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#Split dataset for deepwater and shallowwater species
# Define your mapping of species to habitat
habitat_mapping <- c(
  "Cmonstrosa" = "deepwater", "Gmelastomus" = "deepwater",
  "Scanicula" = "shallowwater", "Cuyato" = "deepwater",
  "Ocentrina" = "deepwater", "Dlicha" = "deepwater",
  "Espinax" = "deepwater", "Hgriseus" = "deepwater",
  "Doxyrinchus" = "deepwater", "Rclavata" = "shallowwater",
  "Rasterias" = "shallowwater", "Rpolystigma" = "shallowwater",
  "Pviolacea" = "shallowwater", "Dpastinaca" = "shallowwater",
  "Maquila" = "shallowwater", "Abovinus" = "shallowwater",
  "Tmarmorata" = "shallowwater"
)

# Add the habitat column to your data frame
# All data:
data <- data %>%
  mutate(habitat = factor(habitat_mapping[numericSp], levels = c("shallowwater", "deepwater")))

# Health data:
data_health <- data_health %>%
  mutate(habitat = factor(habitat_mapping[numericSp], levels = c("shallowwater", "deepwater")))

# Split in two:
# Filter for shallowwater habitat
shallowwater_data <- data %>% filter(habitat == "shallowwater")
shallowwater_data_health <- data_health %>% filter(habitat == "shallowwater")
# Filter for deepwater habitat
deepwater_data <- data %>% filter(habitat == "deepwater")
deepwater_data_health <- data_health %>% filter(habitat == "deepwater")
# Check the results
nrow(shallowwater_data)
nrow(deepwater_data)
nrow(shallowwater_data_health)
nrow(deepwater_data_health)


# Shapiro-Wilk normality test
# Ensure that non-numeric values are handled correctly
deepwater_data$Alive_Dead <- as.numeric(as.character(deepwater_data$Alive_Dead))
shallowwater_data$Alive_Dead <- as.numeric(as.character(shallowwater_data$Alive_Dead))

# Confirm the conversion was successful
unique(deepwater_data$Alive_Dead)
unique(shallowwater_data$Alive_Dead)

shapiro_test_deep<- shapiro.test(deepwater_data$Alive_Dead)
shapiro_test_shallow <- shapiro.test(shallowwater_data$Alive_Dead)

# Print results
print(shapiro_test_deep)
print(shapiro_test_shallow)

# If both groups are normally distributed, use t-test
if (shapiro_test_deep$p.value > 0.05 && shapiro_test_shallow$p.value > 0.05) {
  t_test_result <- t.test(deepwater_data$Alive_Dead, shallowwater_data$Alive_Dead)
  print(t_test_result)
} else {
  # If not normally distributed, use Mann-Whitney U test
  wilcox_test_result <- wilcox.test(deepwater_data$Alive_Dead, shallowwater_data$Alive_Dead)
  print(wilcox_test_result)
}

# Plot proportions:
head(shallowwater_data)
head(deepwater_data)


# 1. DEEPWATER
# Calculate proportion
AVM_percentage_df <- deepwater_data %>%
  filter(!is.na(Alive_Dead)) %>%
  summarise(
    TotalCount = n(),  # Total count of rows across all species
    AliveCount = sum(Alive_Dead == 1),  # Count of rows where Alive_Dead is 1 (alive)
    DeadCount = sum(Alive_Dead == 0),  # Count of rows where Alive_Dead is 0 (dead)
    PercentageAlive = (AliveCount / TotalCount) * 100,  # Overall percentage of alive
    PercentageDead = (DeadCount / TotalCount) * 100     # Overall percentage of dead
  )
head(AVM_percentage_df)

# Convert the summary to long format
AVM_long <- AVM_percentage_df %>%
  pivot_longer(cols = c(PercentageAlive, PercentageDead), 
               names_to = "Status", 
               values_to = "Percentage") %>%
  mutate(Status = recode(Status, "PercentageAlive" = "Survived", "PercentageDead" = "Dead"))

head(AVM_long)

# Create the stacked bar plot
p <- ggplot(AVM_long, aes(x = "", y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Dead" = "#f28d7c", "Survived" = "#4cb8cf"),
                    labels = c("Dead", "Survived")) +  # Custom legend labels
  labs(x = "", y = "AVM Rate (%)", title = "DEEPWATER Overall AVM Rate and Survival") +
  coord_flip() +  # Horizontal bar
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())  # Hide x-axis labels since we have only one bar

# Display the plot
print(p)

# export plot
p_png <- paste0(output_data, "/AVMDeepwater.png")
ggsave(p_png, p, width=12, height=6, units="cm", dpi=300)

# 1. SHALLOW WATER
# Calculate proportion
AVM_percentage_df <- shallowwater_data %>%
  filter(!is.na(Alive_Dead)) %>%
  summarise(
    TotalCount = n(),  # Total count of rows across all species
    AliveCount = sum(Alive_Dead == 1),  # Count of rows where Alive_Dead is 1 (alive)
    DeadCount = sum(Alive_Dead == 0),  # Count of rows where Alive_Dead is 0 (dead)
    PercentageAlive = (AliveCount / TotalCount) * 100,  # Overall percentage of alive
    PercentageDead = (DeadCount / TotalCount) * 100     # Overall percentage of dead
  )
head(AVM_percentage_df)

# Convert the summary to long format
AVM_long <- AVM_percentage_df %>%
  pivot_longer(cols = c(PercentageAlive, PercentageDead), 
               names_to = "Status", 
               values_to = "Percentage") %>%
  mutate(Status = recode(Status, "PercentageAlive" = "Survived", "PercentageDead" = "Dead"))

head(AVM_long)

# Create the stacked bar plot
p <- ggplot(AVM_long, aes(x = "", y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Dead" = "#f28d7c", "Survived" = "#4cb8cf"),
                    labels = c("Dead", "Survived")) +  # Custom legend labels
  labs(x = "", y = "AVM Rate (%)", title = "SHALLOWWATER Overall AVM Rate and Survival") +
  coord_flip() +  # Horizontal bar
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())  # Hide x-axis labels since we have only one bar

# Display the plot
print(p)

# export plot
p_png <- paste0(output_data, "/AVM_Shallowwater.png")
ggsave(p_png, p, width=12, height=6, units="cm", dpi=300)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# Has the Dolce classification influence on AVM? 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#Split dataset for deepwater and shallowwater species
# Create the mapping of species to habitat type (benthopelagic or demersal)
habitat_mapping <- c(
  "Abovinus" = "benthopelagic",  # benthopelagic
  "Cmonstrosa" = "demersal",    # demersal
  "Cuyato" = "benthopelagic",    # benthopelagic
  "Dlicha" = "benthopelagic",    # benthopelagic
  "Doxyrinchus" = "demersal",   # demersal
  "Dpastinaca" = "demersal",    # demersal
  "Espinax" = "benthopelagic",   # benthopelagic
  "Gmelastomus" = "demersal",   # demersal
  "Hgriseus" = "benthopelagic",  # benthopelagic
  "Maquila" = "benthopelagic",   # benthopelagic
  "Ocentrina" = "demersal",     # demersal
  "Pviolacea" = "benthopelagic", # benthopelagic
  "Rasterias" = "demersal",     # demersal
  "Rclavata" = "demersal",      # demersal
  "Rpolystigma" = "demersal",   # demersal
  "Scanicula" = "demersal",     # demersal
  "Tmarmorata" = "demersal"     # demersal
)

# Add the habitat column to your data frame
# All data:
data <- data %>%
  mutate(habitat = factor(habitat_mapping[numericSp], levels = c("benthopelagic", "demersal")))

# Health data:
data_health <- data_health %>%
  mutate(habitat = factor(habitat_mapping[numericSp], levels = c("benthopelagic", "demersal")))

# Split in two:
# Filter for shallowwater habitat
benthopelagic_data <- data %>% filter(habitat == "benthopelagic")
benthopelagic_data_health <- data_health %>% filter(habitat == "benthopelagic")
# Filter for deepwater habitat
demersal_data <- data %>% filter(habitat == "demersal")
demersal_data_health <- data_health %>% filter(habitat == "demersal")
# Check the results
nrow(demersal_data)
nrow(benthopelagic_data)
nrow(demersal_data_health)
nrow(benthopelagic_data_health)

# Shapiro-Wilk normality test
# Ensure that non-numeric values are handled correctly
demersal_data$Alive_Dead <- as.numeric(as.character(demersal_data$Alive_Dead))
benthopelagic_data$Alive_Dead <- as.numeric(as.character(benthopelagic_data$Alive_Dead))

# Confirm the conversion was successful
unique(benthopelagic_data$Alive_Dead)
unique(demersal_data$Alive_Dead)

shapiro_test_demersal<- shapiro.test(demersal_data$Alive_Dead)
shapiro_test_benthopelagic <- shapiro.test(benthopelagic_data$Alive_Dead)

# Print results
print(shapiro_test_demersal)
print(shapiro_test_benthopelagic)

# If both groups are normally distributed, use t-test
if (shapiro_test_demersal$p.value > 0.05 && shapiro_test_benthopelagic$p.value > 0.05) {
  t_test_result <- t.test(demersal_data$Alive_Dead, benthopelagic_data$Alive_Dead)
  print(t_test_result)
} else {
  # If not normally distributed, use Mann-Whitney U test
  wilcox_test_result <- wilcox.test(demersal_data$Alive_Dead, benthopelagic_data$Alive_Dead)
  print(wilcox_test_result)
}

# Plot proportions:
head(benthopelagic_data)
head(demersal_data)


# 1. benthopelagic
# Calculate proportion
AVM_percentage_df <- benthopelagic_data %>%
  filter(!is.na(Alive_Dead)) %>%
  summarise(
    TotalCount = n(),  # Total count of rows across all species
    AliveCount = sum(Alive_Dead == 1),  # Count of rows where Alive_Dead is 1 (alive)
    DeadCount = sum(Alive_Dead == 0),  # Count of rows where Alive_Dead is 0 (dead)
    PercentageAlive = (AliveCount / TotalCount) * 100,  # Overall percentage of alive
    PercentageDead = (DeadCount / TotalCount) * 100     # Overall percentage of dead
  )
head(AVM_percentage_df)

# Convert the summary to long format
AVM_long <- AVM_percentage_df %>%
  pivot_longer(cols = c(PercentageAlive, PercentageDead), 
               names_to = "Status", 
               values_to = "Percentage") %>%
  mutate(Status = recode(Status, "PercentageAlive" = "Survived", "PercentageDead" = "Dead"))

head(AVM_long)

# Create the stacked bar plot
p <- ggplot(AVM_long, aes(x = "", y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Dead" = "#f28d7c", "Survived" = "#4cb8cf"),
                    labels = c("Dead", "Survived")) +  # Custom legend labels
  labs(x = "", y = "AVM Rate (%)", title = "benthopelagic Overall AVM Rate and Survival") +
  coord_flip() +  # Horizontal bar
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())  # Hide x-axis labels since we have only one bar

# Display the plot
print(p)

# export plot
p_png <- paste0(output_data, "/AVMbenthopelagic.png")
ggsave(p_png, p, width=12, height=6, units="cm", dpi=300)

# 1. demersal
# Calculate proportion
AVM_percentage_df <- demersal_data %>%
  filter(!is.na(Alive_Dead)) %>%
  summarise(
    TotalCount = n(),  # Total count of rows across all species
    AliveCount = sum(Alive_Dead == 1),  # Count of rows where Alive_Dead is 1 (alive)
    DeadCount = sum(Alive_Dead == 0),  # Count of rows where Alive_Dead is 0 (dead)
    PercentageAlive = (AliveCount / TotalCount) * 100,  # Overall percentage of alive
    PercentageDead = (DeadCount / TotalCount) * 100     # Overall percentage of dead
  )
head(AVM_percentage_df)

# Convert the summary to long format
AVM_long <- AVM_percentage_df %>%
  pivot_longer(cols = c(PercentageAlive, PercentageDead), 
               names_to = "Status", 
               values_to = "Percentage") %>%
  mutate(Status = recode(Status, "PercentageAlive" = "Survived", "PercentageDead" = "Dead"))

head(AVM_long)

# Create the stacked bar plot
p <- ggplot(AVM_long, aes(x = "", y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Dead" = "#f28d7c", "Survived" = "#4cb8cf"),
                    labels = c("Dead", "Survived")) +  # Custom legend labels
  labs(x = "", y = "AVM Rate (%)", title = "demersal Overall AVM Rate and Survival") +
  coord_flip() +  # Horizontal bar
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())  # Hide x-axis labels since we have only one bar

# Display the plot
print(p)

# export plot
p_png <- paste0(output_data, "/AVM_demersal.png")
ggsave(p_png, p, width=12, height=6, units="cm", dpi=300)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# What is the mean body mass rate for each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
data_bodymass <- read.csv("temp/AVM_allEnviro.csv", sep = ";") #data_env_all.csv
names(data_bodymass)

data_bodymass <- data_bodymass %>%
  mutate(WeightLWR = as.numeric(gsub(",", ".", WeightLWR)))  %>%
  filter(!is.na(WeightLWR))

summary(data_bodymass$WeightLWR)

## Group the data by Species and calculate mean and standard deviation for the specified columns
bodymass_df <- data_bodymass %>%
  group_by(Species) %>%
  summarise(
    Mean_bodymass_kg = round(mean(as.numeric(WeightLWR / 1000), na.rm = TRUE), 2),
    SD_bodymass_kg = round(sd(as.numeric(WeightLWR / 1000), na.rm = TRUE), 2),
    Median_bodymass = round(median(WeightLWR / 1000), 2),
    IQR_bodymass = round(IQR(WeightLWR / 1000), 2))

# Print the result
print(bodymass_df)

# Save dataset:
#Save dataset
#output_file <- file.path(output_data, "AVM_health_all.csv")
#write.csv2(data, file = output_file, row.names = FALSE)














#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# What is the mean PRM rate for each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#' We first need to calculate the the PRM probability for each specimen:
#data <- data %>%
#  mutate(PRM_prob = (1 - (Activity * Wunds * Brusing))*100)
#head(data)
#
##Without bruising:
#data <- data %>%
#  mutate(PRM_prob_actwound = (1 - (Activity * Wunds))*100)
#head(data)
#
## Group the data by Species and calculate mean and standard deviation for the specified columns
#PRM_rate_df <- data %>%
#  group_by(Species) %>%
#  summarise(
#    Mean_PRM_prob = mean(as.numeric(PRM_prob), na.rm = TRUE),
#    SD_PRM_prob = sd(as.numeric(PRM_prob), na.rm = TRUE))
#
## Print the result
#print(PRM_rate_df)
#
##Without bruising:
#PRM_rate_df_actwound  <- data %>%
#  group_by(Species) %>%
#  summarise(
#    Mean_PRM_prob_actwound  = mean(as.numeric(PRM_prob_actwound ), na.rm = TRUE),
#    SD_PRM_prob_actwound  = sd(as.numeric(PRM_prob_actwound ), na.rm = TRUE))
#
## Print the result
#print(PRM_rate_df_actwound)

# Save dataset:
#Save dataset
#output_file <- file.path(output_data, "AVM_health_all.csv")
#write.csv2(data, file = output_file, row.names = FALSE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# What is the total PRM rate adding AVM and PRM?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# (1) You first need to integrate the AVM and PRM total into a single dataframe:
# Select the columns from AVM_percentage_df
#avm_subset <- AVM_percentage_df %>%
#  dplyr::select(Species, PercentageAliveDead0)
#
## Select the rows from PRM_rate_df for specific Species values
#prm_subset <- PRM_rate_df %>%
#  filter(Species %in% c("Abovinus", "Dpastinaca", "Gmelastomus", "Maquila", "Hgriseus", "Pviolacea", "Rclavata", "Rasterias", "Rpolystigma", "Scanicula", "Tmarmorata", "Cmonstrosa")) %>%
#  dplyr::select(Species, Mean_PRM_prob)
#
## Merge the data from avm_subset and prm_subset
#combined_data <- full_join(avm_subset, prm_subset, by = "Species")
#
## Select the rows from PRM_rate_df_actwound for specific Species values
#actwound_subset <- PRM_rate_df_actwound %>%
#  filter(Species %in% c("Cuyato", "Dlicha", "Espinax", "Ocentrina")) %>%
#  dplyr::select(Species, Mean_PRM_prob_actwound)
#
## Merge the data from actwound_subset with the combined_data
#combined_data <- full_join(combined_data, actwound_subset, by = "Species")
#
## Replace NA values in "Mean_PRM_prob" with values from "Mean_PRM_prob_actwound"
#combined_data <- combined_data %>%
#  mutate(Mean_PRM_prob = coalesce(Mean_PRM_prob, Mean_PRM_prob_actwound))
#
## Remove the "Mean_PRM_prob_actwound" column if you no longer need it
#combined_data <- combined_data %>%
#  dplyr::select(-Mean_PRM_prob_actwound)
#
#print(combined_data)
#
# (2) Now calculate the the total mortality probability
#Calculate the mortality probability for each species by combining both at-vessel mortality and post-release mortality. The formula to calculate the total mortality probability is:
#  Total Mortality Probability = (PercentageAliveDead0 / 100) + ((100 - PercentageAliveDead0) / 100 * (Mean_PRM_prob / 100))
# Calculate the total mortality probability for each species
#combined_data <- combined_data %>%
#  mutate(Total_Mortality_Prob = (PercentageAliveDead0 / 100) + ((100 - PercentageAliveDead0) / 100 * (Mean_PRM_prob / 100)))%>%
#  mutate(Total_Mortality_Prob = Total_Mortality_Prob * 100)
#
## View the updated dataframe
#combined_data$Total_Mortality_Prob[is.na(combined_data$Total_Mortality_Prob)] <- 100
#print(combined_data)
#
## Reorder species:
## Define the desired order of species
#desired_order <- c("Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", "Maquila", "Abovinus", "Tmarmorata")
#desired_order <- rev(desired_order)
## Convert the "Species" column to a factor with the desired order
#combined_data$Species <- factor(combined_data$Species, levels = desired_order)
#
## Create the ggplot with the reordered Species
#p <- ggplot(combined_data, aes(x = Species, y = Total_Mortality_Prob)) +
#  geom_pointrange(aes(ymin = Total_Mortality_Prob, ymax = Total_Mortality_Prob)) +
#  labs(x = "Species", y = "Estimated total PCM rate (%)", title = "Coefficient Values by Model") +
#  coord_flip() +  # Swap X and Y axes
#  theme_minimal() +
#  theme(panel.grid = element_blank(),
#        axis.line = element_line(color = "black"),
#        axis.ticks = element_line(color = "black"))
#
#print(p)
#
## export plot
#p_png <- paste0(output_data, "/PCM_tree.png")
#ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)
