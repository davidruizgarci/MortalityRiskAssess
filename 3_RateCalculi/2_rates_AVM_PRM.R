#-----------------------------------------------------------------------------------------------------------------
# 2_AVM_PRM_rates:  Calculate At Vessel Mortality (AVM) and Post-Release Mortality (PRM) rates; 
#-----------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)

#Load data
data <- read.csv("temp/env_data2D_edit.csv", sep = ";") #data_env_all.csv
names(data)
head(data)

data <- data %>% 
  filter(!is.na(Alive_Dead)) %>% 
  mutate(Alive_Dead = factor(Alive_Dead, c(0, 1)))

#Format:
names(data)
data <- data %>%
  mutate(
    bodymass = as.numeric(gsub(",", ".", WeightLWR)),  # Convert comma decimal to dot decimal
    depth = as.numeric(gsub(",", ".", bathy)),
    TotalBiomassHaul = as.numeric(gsub(",", ".", TotalBiomassHaul)),
    MinsExposedtoAir = as.numeric(gsub(",", ".", MinsExposedtoAir)),
    numericSp = as.factor(ifelse(Species == "RajaSp", "Rclavata", Species)))  # Replace RajaSp with Rclavata
str(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#How many specimens and species are there in total?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
nrow(data) #2779

#What species are there?
sp <- unique(data$numericSp)
sp #"Scanicula"   "Gmelastomus" "Espinax"     "Dlicha"      "Hgriseus"    "Cuyato"      "Ocentrina"   "Tmarmorata"  "RajaSp"      "RPolystigma" "Rpolystigma" "Rclavata" "Rasterias"   "Pviolacea"   "Dpastinaca"  "Doxyrinchus" "Cmonstrosa"  "Maquila"     "Abovinus"   
length(sp) #17

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#How many specimens of each?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
species_counts_df <- data %>%
  group_by(numericSp) %>%
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
  group_by(numericSp) %>%
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
AVM_percentage_df$numericSp <- factor(AVM_percentage_df$numericSp, levels = desired_order)

# Create the ggplot with the reordered Species
#p <- ggplot(AVM_percentage_df, aes(x = numericSp, y = PercentageAliveDead0)) +
#  geom_pointrange(aes(ymin = PercentageAliveDead0, ymax = PercentageAliveDead0)) +
#  labs(x = "Species", y = "AVM rate (%)", title = "Coefficient Values by Model") +
#  coord_flip() +  # Swap X and Y axes
#  theme_minimal() +
#  theme(panel.grid = element_blank(),
#        axis.line = element_line(color = "black"),
#        axis.ticks = element_line(color = "black"))
#
#print(p)

# export plot
#p_png <- paste0(output_data, "/AVM_tree.png")
#ggsave(p_png, p, width=8, height=12, units="cm", dpi=300)


# Bar plot of AVM percentage by species
# Prepare data for stacked bar plot
AVM_percentage_df <- AVM_percentage_df %>%
  mutate(SurvivalPercentage = 100 - PercentageAliveDead0) %>%  # Calculate survival percentage
  filter(!is.na(numericSp))  # Filter out missing values

# Calculate the absolute difference between Dead and Survived rates
#AVM_percentage_df <- AVM_percentage_df %>%
  #mutate(Difference = abs(PercentageAliveDead0 - SurvivalPercentage)) %>%
  #arrange(desc(Difference))
  #arrange(desc(PercentageAliveDead0))

# select the order of the species:
AVM_percentage_df$numericSp <- factor(AVM_percentage_df$numericSp, levels = rev(c(
  "Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", 
  "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", 
  "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", 
  "Maquila", "Abovinus", "Tmarmorata"
)))

# Reorder numericSp as a factor based on the Difference
#AVM_percentage_df$numericSp <- factor(AVM_percentage_df$numericSp, levels = AVM_percentage_df$numericSp)

# Melt the data to long format to use in ggplot
library(tidyr)
AVM_long <- AVM_percentage_df %>%
  pivot_longer(cols = c(PercentageAliveDead0, SurvivalPercentage),
               names_to = "Status",
               values_to = "Percentage")

# Plot with stacked bars
p <- ggplot(AVM_long, aes(x = as.factor(numericSp), y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("PercentageAliveDead0" = "#f28d7c", "SurvivalPercentage" = "#4cb8cf"), #"PercentageAliveDead0" = "#f28d7c", "SurvivalPercentage" = "#4cb8cf"
                   labels = c("Dead", "Survived")) +  # Custom legend labels
  labs(x = "Species", y = "AVM Rate (%)", title = "AVM Rate and Survival by Species") +
  coord_flip() +  # Horizontal bars
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

print(p)

# export plot
p_png <- paste0(output_data, "/AVM_prop.png")
ggsave(p_png, p, width=12, height=12, units="cm", dpi=300)

## Ensure numericSp in AVM_long has the same ordering as in AVM_percentage_df
#AVM_long$numericSp <- factor(AVM_long$numericSp, levels = levels(AVM_percentage_df$numericSp))
#
## Set "PercentageAliveDead0" as negative for left side of diverging plot
#AVM_long <- AVM_long %>%
#  mutate(Percentage = ifelse(Status == "PercentageAliveDead0", -Percentage, Percentage))
#
#
## Create the diverging bar plot
#p <- ggplot(AVM_long, aes(x = as.factor(numericSp), y = Percentage, fill = Status)) +
#  geom_bar(stat = "identity", position = "identity", width = 0.8) +
#  scale_y_continuous(labels = abs, limits = c(-100, 100)) +  # Display y-axis as positive values
#  scale_fill_manual(values = c("PercentageAliveDead0" = "steelblue", "SurvivalPercentage" = "orange"),
#                    labels = c("Dead", "Survived")) +
#  labs(x = "Species", y = "AVM Rate (%)", title = "AVM Rate and Survival by Species") +
#  coord_flip() +  # Horizontal bars
#  theme_minimal() +
#  theme(panel.grid = element_blank(),
#        axis.line = element_line(color = "black"),
#        axis.ticks = element_line(color = "black"))
#
## Add species labels in the center
#p + geom_text(data = AVM_long %>% filter(Status == "PercentageAliveDead0"),
#              aes(label = as.factor(numericSp), y = 0), 
#              hjust = 0.5, 
#              size = 4)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# Plot % of traded vs. discarded
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
trade <- read.csv("input/Trade.csv", sep = ";")
names(trade)
head(trade)

trade <- trade %>%
  mutate(
    percentage_trade = as.numeric(gsub(",", ".", percentage_trade)))
str(trade)

# select the order of the species:
trade$numericSp <- factor(trade$numericSp, levels = rev(c(
  "Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", 
  "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", 
  "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", 
  "Maquila", "Abovinus", "Tmarmorata"
)))

# palettes:
#"#708090", "#FF7F50", "#008080", "#3fb9bf", "#DAA520", "#FF6F61", 
#"#6A0DAD", "#A4C639", "#D1B3FF", "#014421", "#6A5ACD", "#E97451"
#"#A9A9A9", "#808000", "#00676d", "#a94722" "#3fb9bf", "#F9B233"
#"#3fbf9f", "#3fbf8c", "#2fab7b", "#3fb9bf", "#4cb8cf"

# Plot with stacked bars
p <- ggplot(trade, aes(x = numericSp, y = percentage_trade, fill = Comercilizasation)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Trade" = "#3fbf8c", "Discard" = "#F9B233"), 
                    labels = c("Discarded", "Traded")) +  # Custom legend labels
  labs(x = "Species", y = "%", title = "Discard rate by Species") +
  coord_flip() +  # Horizontal bars
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

# Print the plot
print(p)

# export plot
p_png <- paste0(output_data, "/Trade_prop.png")
ggsave(p_png, p, width=12, height=12, units="cm", dpi=300)


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
  ) 

data_health <- data %>%
  filter(!is.na(Activity)) %>%
  filter(Alive_Dead == 1)

# Verify that the values have been recoded as expected
head(data_health)  # Display the first few rows of the modified data
nrow(data_health) #1157
names(data_health)

#Save dataset
#output_file <- file.path(output_data, "AVM_PRM_all.csv")
#write.csv2(data, file = output_file, row.names = FALSE)

# Group the data by Species and calculate mean, standard deviation, and count for the specified columns
species_stats_df <- data_health %>%
  filter(Alive_Dead == 1) %>%
  group_by(Species) %>%
  summarise(
    Total_Cases = n(),  # Calculate total number of cases for each Species
    Mean_Activity = mean(Activity, na.rm = TRUE),
    SD_Activity = sd(Activity, na.rm = TRUE),
    Mean_Wunds = mean(Wunds, na.rm = TRUE),
    SD_Wunds = sd(Wunds, na.rm = TRUE),
    Mean_Brusing = mean(Brusing, na.rm = TRUE),
    SD_Brusing = sd(Brusing, na.rm = TRUE)
  )

# Print the result
print(species_stats_df)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# What is the distribution of health indicators for each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# Filter out rows where Brusing is NA and then modify Brusing values
#data <- data %>%
#  # Step 1: Filter out rows where Brusing is NA
#  filter(!is.na(Brusing)) %>%
#  
#  # Step 2: Change all 0.66 values in Brusing to 1
#  mutate(Brusing = ifelse(Brusing == 0.66, 1, Brusing)) %>%
#  
#  # Step 3: Change half of the 0.33 values to 0.66 randomly within each species
#  group_by(numericSp) %>%
#  mutate(Brusing = ifelse(
#    Brusing == 0.33 & row_number() %in% sample(which(Brusing == 0.33), size = ceiling(sum(Brusing == 0.33) / 2)),
#    0.66,
#    Brusing
#  )) %>%
#  ungroup()

#Reshape data to calculate percentages for each column
activity_percentages <- data_health %>%
  # Step 1: Pivot the three columns into long format
  pivot_longer(cols = c(Wunds, Brusing, Activity), 
               names_to = "Metric", 
               values_to = "Value") %>%
  
  # Step 2: Group by numericSp, Metric, and Value to count occurrences
  group_by(numericSp, Metric, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  
  # Step 3: Calculate percentage within each group of numericSp and Metric
  group_by(numericSp, Metric) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup()

# Display the resulting dataframe
head(activity_percentages)

# Plot with stacked bars for each metric (Wunds, Brusing, Activity)
# Filter out NA values for Wunds, Brusing, and Activity before summarizing
activity_percentages <- activity_percentages %>%
  filter(!is.na(Value))  # Ensures no NA values in Value column

# Define the order of the species
activity_percentages$numericSp <- factor(activity_percentages$numericSp, levels = rev(c(
  "Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", 
  "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", 
  "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", 
  "Maquila", "Abovinus", "Tmarmorata")))

# Convert the 'Value' column to a factor with specified levels in reverse order
activity_percentages$Value <- factor(activity_percentages$Value, 
                                     levels = c(1, 0.66, 0.33, 0), 
                                     labels = c("1", "0.66", "0.33", "0"))

# Add mean and SD bar:
# Step 1: Reshape data to long format
long_data <- data_health %>%
  pivot_longer(cols = c(Activity, Wunds, Brusing), 
               names_to = "Metric", 
               values_to = "Value") %>%
  filter(!is.na(Value))  # Filter out NA values

# Step 2: Calculate mean and standard deviation for each species and metric
summary_stats <- long_data %>%
  group_by(numericSp, Metric) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE) * 100,  # Convert Mean to percentage
    SD = sd(Value, na.rm = TRUE) * 100,       # Convert SD to percentage
    .groups = 'drop'
    ) %>%
  mutate(SD = pmin(pmax(SD, 0), 100),  # Cap SD at 0 and 100
         Mean = pmin(pmax(Mean, 0), 100))

# Integrate summary_stats with activity_percentages into combined_data
combined_data <- activity_percentages %>%
  left_join(summary_stats, by = c("numericSp", "Metric"))

# Convert 'Value' to factor if needed
combined_data <- combined_data %>%
  mutate(Value = as.factor(Value))  # Ensure Value is a categorical variable

# Create the plot with means and SD in percentage
p <- ggplot(combined_data, aes(x = numericSp, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  
  # Custom color palette
  scale_fill_manual(values = c("#32CD32", "#FFD700", "#FFA500", "#FF4C4C"), 
                    name = "Status") +
  
  labs(x = "Species", y = "Percentage (%)", title = "Percentage Distribution by Species for Wounds, Bruising, and Activity") +
  
  coord_flip() +  # Horizontal bars
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")) +
  
  facet_wrap(~Metric, scales = "free_x") +   # Separate plots for each Metric
  
  # Adding mean points and error bars for SD using distinct to only get the first occurrence for each Metric
  geom_point(data = combined_data %>% group_by(numericSp, Metric) %>% slice(1), 
           aes(x = numericSp, y = Mean), color = "black", size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = combined_data %>% group_by(numericSp, Metric) %>% slice(1), 
                aes(x = numericSp, ymin = pmax(0, Mean - SD), ymax = pmin(100, Mean + SD)), 
                color = "black", width = 0.05, position = position_dodge(width = 0.9))

# Print the plot
print(p)

# export plot
p_png <- paste0(output_data, "/Health_prop_2.png")
ggsave(p_png, p, width=16, height=12, units="cm", dpi=300)



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# What is the mean overall health condition rate for each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#' We first need to calculate the the PRM probability for each specimen:
data_health <- data_health %>%
  mutate(Health_prob = (Activity * Wunds * Brusing)*100)
head(data_health)


#Without bruising:
#data <- data %>%
#  mutate(PRM_prob_actwound = (1 - (Activity * Wunds))*100)
#head(data)

# Print the result
# Group the data by Species and calculate mean and standard deviation for the specified columns
PRM_rate_df <- data_health %>%
  group_by(Species) %>%
  summarise(
    Mean_Health_prob = mean(as.numeric(Health_prob), na.rm = TRUE),
    SD_Health_prob = sd(as.numeric(Health_prob), na.rm = TRUE),
    PRM = 100 - Mean_Health_prob
  )
# Print the result
print(PRM_rate_df)

# Save dataset:
#Save dataset
#output_file <- file.path(output_data, "AVM_PRM_all.csv")
#write.csv2(data, file = output_file, row.names = FALSE)


# Plot together with the rest:
# Define color categories based on mean health probability
PRM_rate_df <- PRM_rate_df %>%
  mutate(Color = case_when(
    Mean_Health_prob >= 75 ~ "#32CD32",   # Green for high health probability
    Mean_Health_prob >= 50 ~ "#FFD700",   # Yellow for moderate health probability
    Mean_Health_prob >= 25 ~ "#FFA500",   # Orange for lower health probability
    TRUE ~ "#FF4C4C"                      # Red for very low health probability
  ))

# Define the order of species as a factor in PRM_rate_df
PRM_rate_df$Species <- factor(PRM_rate_df$Species, levels = rev(c(
  "Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", 
  "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", 
  "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", 
  "Maquila", "Abovinus", "Tmarmorata"
)))

# Now create the plot using the custom order
p_health <- ggplot(PRM_rate_df, aes(x = Species, y = Mean_Health_prob, fill = Color)) +
  geom_bar(stat = "identity") +  # Removed color = "black" to eliminate bar borders
  geom_errorbar(aes(
    ymin = pmax(0, Mean_Health_prob - SD_Health_prob),
    ymax = pmin(100, Mean_Health_prob + SD_Health_prob)
  ), width = 0.2, na.rm = TRUE) +
  geom_point(aes(y = Mean_Health_prob), color = "black", size = 2) +  # Add a black dot for the mean
  scale_fill_identity() +  # Use the colors defined in the 'Color' column directly
  labs(x = "Species", y = "Mean Health Probability (%)") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

# Print the plot
print(p_health)

# Combine the plots side by side
#combined_plot <- p + p_health + plot_layout(ncol = 2, widths = c(1, 1))

# Print the combined plot
#print(combined_plot)

# export plot
p_png <- paste0(output_data, "/Health_Overall.png")
ggsave(p_png, p_health, width=7, height=12, units="cm", dpi=300)

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
