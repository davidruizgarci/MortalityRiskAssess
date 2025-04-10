# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
# 5.6. Grouping Health Condition
#-------------------------------------------------------------------------------

# 1. Load AVM data--------------------------------------------------------------
data <- read.csv("temp/mod2AVM_allEnviro.csv", sep = ";")

#Format:
names(data)
data <- data %>%
  mutate(
    bodymass = as.numeric(gsub(",", ".", WeightLWR)),  # Convert comma decimal to dot decimal
    depth = as.numeric(gsub(",", ".", depth)),
    Species = as.factor(Species),
    MinsExposedtoAir =  as.numeric(gsub(",", ".", MinsExposedtoAir)),
    at_celsius =  as.numeric(gsub(",", ".", at_celsius)),
    temp_change =  as.numeric(gsub(",", ".", temp_change)),
    Trawl_duration =  as.numeric(gsub(",", ".", Trawl_duration))) 



# 2. Variability in health indicators across species ---------------------------
head(data_health)
names(data_health)
str(data_health)

# Convert Activity, Wunds, and Brusing to numeric at once
data_health <- data_health %>%
  mutate(
    Activity = as.numeric(gsub(",", ".", Activity)),
    Wunds = as.numeric(gsub(",", ".", Wunds)),
    Brusing = as.numeric(gsub(",", ".", Brusing))
  )

# Remove rows with NA, NaN, or Inf values in Activity, Wunds, and Brusing
data_filtered <- data_health %>%
  filter(!is.na(Activity), !is.na(Wunds), !is.na(Brusing)) %>%
  filter(!is.nan(Activity), !is.nan(Wunds), !is.nan(Brusing)) %>%
  filter(!is.infinite(Activity), !is.infinite(Wunds), !is.infinite(Brusing))

# Count the number of observations per species
species_counts <- data_filtered %>%
  group_by(Species) %>%
  tally()
print(species_counts)
# Filter out species with only 1 observation
data_filtered <- data_filtered %>%
  group_by(Species) %>%
  filter(n() > 10) %>%
  ungroup()

# ANOVA for Activity
anova_activity <- aov(Activity ~ Species, data = data_filtered)
summary(anova_activity)

# ANOVA for Wunds
anova_wunds <- aov(Wunds ~ Species, data = data_filtered)
summary(anova_wunds)

# ANOVA for Brusing
anova_brusing <- aov(Brusing ~ Species, data = data_filtered)
summary(anova_brusing)




# 11.1. GROUPS FOR ACTIVITY------------------------------------------------------
# Tukey post-hoc test if ANOVA is significant
tukey_activity <- TukeyHSD(anova_activity)
summary(tukey_activity)
print(tukey_activity)

# Extract significant pairs from the Tukey test
tukey_matrix <- as.data.frame(tukey_activity$Species)

# Identify non-significant pairs (p adj > 0.05)
non_significant_pairs <- subset(tukey_matrix, `p adj` > 0.05)

# Extract species names from the pairs
library(stringr)
non_sig_pairs <- str_split(rownames(non_significant_pairs), "-", simplify = TRUE)

# Create an adjacency matrix of species
species <- unique(c(non_sig_pairs))
adj_matrix <- matrix(0, nrow = length(species), ncol = length(species),
                     dimnames = list(species, species))

# Fill the adjacency matrix for non-significant pairs
for (i in 1:nrow(non_sig_pairs)) {
  species1 <- non_sig_pairs[i, 1]
  species2 <- non_sig_pairs[i, 2]
  adj_matrix[species1, species2] <- 1
  adj_matrix[species2, species1] <- 1
}

# Perform hierarchical clustering
dist_matrix <- 1 - adj_matrix  # Convert to distance matrix (1 = different, 0 = similar)
hc <- hclust(as.dist(dist_matrix), method = "complete")

# Plot dendrogram to visualize grouping
plot(hc, main = "Dendrogram of Species Groups", xlab = "Species", sub = "", cex = 0.8)

# Dynamically determine the number of clusters based on the dendrogram height (using 'h')
# You could choose a height manually or use a statistical method to determine 'h'
# Example: cutting tree at a height of 0.6 (this can be adjusted based on dendrogram structure)
height_threshold <- 0.6  # You can adjust this threshold based on the dendrogram
groups <- cutree(hc, h = height_threshold)
print(groups)

# Assign species to their respective groups
group_list <- split(names(groups), groups)
print(group_list)


# PRINT DIFFERENCES BETWEEN GROUPS:
# Calculate the mean activity for each species
species_means <- aggregate(Activity ~ Species, data = data_filtered, mean)

# Convert species column to character in species_means
species_means$Species <- as.character(species_means$Species)

# Check the names of the groups vector and ensure they match species_means$Species
print(names(groups))
print(species_means$Species)

# Assign group numbers by matching species names
species_means$Group <- groups[match(species_means$Species, names(groups))]

# Check if there are still NA values
print(species_means)

# Calculate the mean for each group
group_means <- aggregate(Activity ~ Group, data = species_means, mean)

# Print and visualize the group means
group_means <- group_means[order(-group_means$Activity), ]
print(group_means)

ggplot(group_means, aes(x = factor(Group), y = Activity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Activity by Group", x = "Group", y = "Mean Activity") +
  theme_minimal()






# 11.2. GROUPS FOR WUNDS---------------------------------------------------------
tukey_wunds <- TukeyHSD(anova_wunds)
summary(tukey_wunds)
print(tukey_wunds)

# Extract significant pairs from the Tukey test
tukey_matrix <- as.data.frame(tukey_wunds$Species)

# Identify non-significant pairs (p adj > 0.05)
non_significant_pairs <- subset(tukey_matrix, `p adj` > 0.05)

# Extract species names from the pairs
non_sig_pairs <- str_split(rownames(non_significant_pairs), "-", simplify = TRUE)

# Create an adjacency matrix of species
species <- unique(c(non_sig_pairs))
adj_matrix <- matrix(0, nrow = length(species), ncol = length(species),
                     dimnames = list(species, species))

# Fill the adjacency matrix for non-significant pairs
for (i in 1:nrow(non_sig_pairs)) {
  species1 <- non_sig_pairs[i, 1]
  species2 <- non_sig_pairs[i, 2]
  adj_matrix[species1, species2] <- 1
  adj_matrix[species2, species1] <- 1
}

# Perform hierarchical clustering
dist_matrix <- 1 - adj_matrix  # Convert to distance matrix (1 = different, 0 = similar)
hc <- hclust(as.dist(dist_matrix), method = "complete")

# Plot dendrogram to visualize grouping
plot(hc, main = "Dendrogram of Species Groups", xlab = "Species", sub = "", cex = 0.8)

# Dynamically determine the number of clusters based on the dendrogram height (using 'h')
# You could choose a height manually or use a statistical method to determine 'h'
# Example: cutting tree at a height of 0.6 (this can be adjusted based on dendrogram structure)
height_threshold <- 0.6  # You can adjust this threshold based on the dendrogram
groups <- cutree(hc, h = height_threshold)
print(groups)

# Assign species to their respective groups
group_list <- split(names(groups), groups)
print(group_list)

# PRINT DIFFERENCES BETWEEN GROUPS:
# Calculate the mean activity for each species
species_means <- aggregate(Wunds ~ Species, data = data_filtered, mean)


# Convert species column to character in species_means
species_means$Species <- as.character(species_means$Species)

# Check the names of the groups vector and ensure they match species_means$Species
print(names(groups))
print(species_means$Species)

# Assign group numbers by matching species names
species_means$Group <- groups[match(species_means$Species, names(groups))]

# Check if there are still NA values
print(species_means)

# Calculate the mean for each group
group_means <- aggregate(Wunds ~ Group, data = species_means, mean)

# Print and visualize the group means
group_means <- group_means[order(-group_means$Wunds), ]
print(group_means)

ggplot(group_means, aes(x = factor(Group), y = Wunds)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Activity by Group", x = "Group", y = "Mean Activity") +
  theme_minimal()


# 11.3. GROUPS FOR BRUISING------------------------------------------------------
tukey_brusing <- TukeyHSD(anova_brusing)
summary(tukey_brusing)
print(tukey_brusing)

# Extract significant pairs from the Tukey test
tukey_matrix <- as.data.frame(tukey_brusing$Species)

# Identify non-significant pairs (p adj > 0.05)
non_significant_pairs <- subset(tukey_matrix, `p adj` > 0.05)

# Extract species names from the pairs
non_sig_pairs <- str_split(rownames(non_significant_pairs), "-", simplify = TRUE)

# Create an adjacency matrix of species
species <- unique(c(non_sig_pairs))
adj_matrix <- matrix(0, nrow = length(species), ncol = length(species),
                     dimnames = list(species, species))

# Fill the adjacency matrix for non-significant pairs
for (i in 1:nrow(non_sig_pairs)) {
  species1 <- non_sig_pairs[i, 1]
  species2 <- non_sig_pairs[i, 2]
  adj_matrix[species1, species2] <- 1
  adj_matrix[species2, species1] <- 1
}

# Perform hierarchical clustering
dist_matrix <- 1 - adj_matrix  # Convert to distance matrix (1 = different, 0 = similar)
hc <- hclust(as.dist(dist_matrix), method = "complete")

# Plot dendrogram to visualize grouping
plot(hc, main = "Dendrogram of Species Groups", xlab = "Species", sub = "", cex = 0.8)

# Cut tree into clusters (adjust the 'k' parameter as needed)
groups <- cutree(hc, k = 4)
print(groups)

# Assign species to their respective groups
group_list <- split(names(groups), groups)
print(group_list)

# PRINT DIFFERENCES BETWEEN GROUPS:
# Calculate the mean activity for each species
species_means <- aggregate(Brusing ~ Species, data = data_filtered, mean)

# Convert species column to character in species_means
species_means$Species <- as.character(species_means$Species)

# Check the names of the groups vector and ensure they match species_means$Species
print(names(groups))
print(species_means$Species)

# Assign group numbers by matching species names
species_means$Group <- groups[match(species_means$Species, names(groups))]

# Check if there are still NA values
print(species_means)

# Calculate the mean for each group
group_means <- aggregate(Brusing ~ Group, data = species_means, mean)

# Print and visualize the group means
group_means <- group_means[order(-group_means$Brusing), ]
print(group_means)

ggplot(group_means, aes(x = factor(Group), y = Brusing)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Activity by Group", x = "Group", y = "Mean Activity") +
  theme_minimal()

# Calculate the variance for each of the health indicators (Activity, Wunds, and Bruising)
variance_activity <- var(data_filtered$Activity, na.rm = TRUE)
variance_wunds <- var(data_filtered$Wunds, na.rm = TRUE)
variance_bruising <- var(data_filtered$Brusing, na.rm = TRUE)

# Print the variance of each indicator
cat("Variance of Activity: ", variance_activity, "\n")
cat("Variance of Wunds: ", variance_wunds, "\n")
cat("Variance of Bruising: ", variance_bruising, "\n")

# Create a data frame for easy comparison
variance_summary <- data.frame(
  Indicator = c("Activity", "Wunds", "Bruising"),
  Variance = c(variance_activity, variance_wunds, variance_bruising)
)

# Print the variance summary
print(variance_summary)

# Gather the data into a long format for ggplot
library(tidyr)
data_long <- gather(data_filtered, key = "Indicator", value = "Value", Activity, Wunds, Brusing)

# Create the boxplot
ggplot(data_long, aes(x = Indicator, y = Value)) +
  geom_boxplot() +
  labs(title = "Comparison of Variances Across Health Indicators",
       x = "Health Indicator", y = "Values") +
  theme_minimal()
