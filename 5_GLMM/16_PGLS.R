#---------------------------------------------------------------------------------------------------
# PGLS: determine patterns in AVM in relation to species phylogeny (biology and ecology)
#---------------------------------------------------------------------------------------------------

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Save trees:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#Write ".tree" document based on Stein et al. (2018), and available at Vertlife.org
# Define the Newick tree string
tree <- "((((Torpedo_marmorata:222.450728121,((Pteromylaeus_bovinus:96.547369608,Myliobatis_aquila:96.547371607):35.199940655000006,(Dasyatis_pastinaca:76.06875802909998,Pteroplatytrygon_violacea:76.068757024):55.67855423):90.70341887800001):5.666544191,((Raja_polystigma:47.032964039999996,(Raja_asterias:25.075871561,Raja_clavata:25.075871551):21.957093479999997):18.834432862,Dipturus_oxyrinchus:65.8673968957):162.24987742):45.74765644,((Hexanchus_griseus:237.691764637,(((Etmopterus_spinax:150.5397219559,Dalatias_licha:150.53972195999998):7.969200557,Oxynotus_centrina:158.5089225242):9.668281922,Centrophorus_granulosus:168.177205442):69.51456121):13.41922109,(Scyliorhinus_canicula:174.055189853,Galeus_melastomus:174.05519085109998):77.05579588):22.75394301):100.5740727,Chimaera_monstrosa:374.43900248470004);"

output_dir  <- paste0(input_data, "/trees")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
tree <- read.tree(text = tree)
# Define the filename for the tree
my_tree <- paste0(output_dir, "/my_tree.tree")
# Save the tree to a file
write.tree(tree, file = my_tree)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load data:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

setwd(input_data)
data <- read.csv("GLMM_predictors2.csv", sep = ";")
names(data)
head(data)

#Format new predictors:
names(data)
data <- data %>%
  mutate(mean_bodymass = as.numeric(gsub(",", ".", mean_bodymass)),  # Convert comma decimal to dot decimal
         mean_depth = as.numeric(gsub(",", ".", mean_depth)),
         mean_temperature = as.numeric(gsub(",", ".", mean_temperature)),
         mean_oxygen = as.numeric(gsub(",", ".", mean_oxygen)),
         AVM = as.numeric(gsub(",", ".", AVM)),
         PCM = as.numeric(gsub(",", ".", PCM)))

data <- data %>% 
  mutate(RespirationMode = factor(RespirationMode, c(1,2,4)))

#Check the distribution of your variables in case any needs transformation:
#Check distribution:
hist(data$mean_bodymass)
shapiro.test(data$mean_bodymass)

data$lnmass <- log1p(data$mean_bodymass)
hist(data$lnmass)
shapiro.test(data$lnmass)

#Scale parameters:
data <- data %>% 
  mutate(mean_bodymass = scale(lnmass),
         mean_depth = scale(mean_depth),
         mean_temperature = scale(mean_temperature),
         mean_oxygen = scale(mean_oxygen))

summary(data)

# Fit pgls models
# load data
wd<- paste0(input_data, "/trees")
setwd(wd)
phy <- read.tree("my_tree.tree")


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#Plot the tree
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
pngfile <- paste0(output_data, "/mytree.png")
png(pngfile, width=2000, height=2000, res=300)
plot(phy)
dev.off()

# Plot the phylogenetic tree
library(phytools)
pngfile <- paste0(output_data, "/mytree1.png")
png(pngfile, width=2000, height=2000, res=300)
plotTree(phy)
add.scale.bar(cex = 0.8, col = "black", length = 20)
dev.off()
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#Create data frame integrated with phylogeny
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# combine phylogeny with dataset for use in pgls function
cd <- comparative.data(phy, data, names.col = "Species") 


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# check multicollinearity
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Check correlation using VIF:
vars <- c("mean_bodymass", "mean_depth", "mean_temperature", "mean_oxygen") #"RelativeGillHeight", "RelativeSpiracleLength"
# calculate correlations using Pearson
correlations <- cor(na.omit(dplyr::select(data, all_of(vars))), method="pearson")

# plot correlations
output_dir<- paste0(output_data, "/PGLS_prefitting_checks")
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}
pngfile <- paste0(output_dir, "/", "_eda_corplot2.png")
png(pngfile, width=4000, height=3700, res=300)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlations, method="color",col=col(200), tl.col = "black", order = "original", diag=FALSE, type="upper", 
         addCoef.col = "black") # Add coefficient of correlation
dev.off()

# calcualate correlations using Spearman and clustering analysis
pngfile <- paste0(output_dir, "/_eda_cluster2.png")
png(pngfile, width=2500, height=2000, res=300)
op <- par(mar=c(0.5,5,0.5,0.5))
v <- as.formula(paste("~", vars, collapse="+"))
plot(varclus(v, similarity=c("spearman"),data=data),cex=1.2) # plot cluster
abline(a=0.30,0,col="grey70",lty=1,lwd=2)
par(op)
dev.off()

# You can also check it on the full model with VIF:
modelcheck <- lm(AVM ~ mean_bodymass + mean_temperature + mean_depth + mean_oxygen + RespirationMode, data = data) 
vif(modelcheck)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Test PCA as method to reduce colinearity
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Depth and temperature are correlated (0.87). You can either:
# (1) Directly remove one of them: may depth is more informative.
# (2) make a PCA to group them.
temperature_depth_data <- data[, c("mean_temperature", "mean_depth")]
pca_result <- prcomp(temperature_depth_data, center = TRUE, scale = TRUE)
data$temperature_depth_index <- pca_result$x[, 1]
data <- data %>%
  mutate(temperature_depth_index = as.numeric(gsub(",", ".", temperature_depth_index)),
         temperature_depth_index = scale(temperature_depth_index))
## Extract the standard deviations
standard_deviations <- pca_result$sdev
## Calculate the percentage of variance explained by PC1
variance_explained_by_PC1 <- (standard_deviations[1]^2) / sum(standard_deviations^2) * 100 #82.23%
#
## You can check again the full model with VIF including the PC1:
#modelcheck <- lm(AVM ~ mean_bodymass + temperature_depth_index + mean_oxygen + RespirationMode, data = data) 
#vif(modelcheck)

#Mean body mass and respiration mode are correlated, eliminate one of them.
modelcheck <- lm(AVM ~ mean_bodymass + mean_depth + mean_oxygen, data = data) 
vif(modelcheck)

#Run all the possible combinations of models using the preditors: 
#"mean_bodymass", "mean_depth", "mean_temperature", "RespirationMode",  "mean_oxygen", "RelativeGillHeight", "RelativeSpiracleLength"
# Use all the combinations on a step forward trend:
# Define all combinations of predictors
#predictors <- c("mean_bodymass", "mean_depth", "mean_oxygen", "RespirationMode")
#predictors <- c("mean_bodymass", "mean_depth", "mean_oxygen", "mean_temperature")
#predictors <- c("mean_bodymass", "mean_temperature", "mean_oxygen", "RespirationMode")

cd <- comparative.data(phy, data, names.col = "Species") 

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Fit the models
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#You can automatise the fitting, but I will do it manually:
#predictors <- c("mean_bodymass", "temperature_depth_index", "mean_oxygen") #"RelativeGillHeight", "RelativeSpiracleLength" "RespirationMode"
# Create an empty list to store the models
#models <- list()
#
## Include the model with only the intercept (no predictors)
#intercept_model <- pgls(AVM ~ 1, data = cd, lambda = "ML")
#models[[1]] <- intercept_model
## Loop through all possible combinations of predictors
#for (i in 1:(2^length(predictors) - 1)) {
#  binary_vector <- intToBits(i)[1:length(predictors)]
#  selected_predictors <- predictors[as.logical(binary_vector)]
#  formula <- as.formula(paste("AVM ~", paste(selected_predictors, collapse = " + ")))
#  model <- pgls(formula, data = cd, lambda = "ML")
#  models[[i]] <- model
#}
# Access each model by its index in the 'models' list
# For example, to access the model with Mass + Depth + Temperature:
# zslope_mass_depth_temp <- models[[n]]

# 0 intercept only
zslope_0 <- pgls(AVM ~ 1, data = cd, lambda = "ML")

# 1. mass only: AVM varies with mass only 
zslope_wt <- pgls(AVM ~ mean_bodymass, data = cd, lambda = "ML") 

# 2. mass only: AVM varies with depth-temp index only 
zslope_depthtemp <- pgls(AVM ~ mean_depth, data = cd, lambda = "ML") 
zslope_temp <- pgls(AVM ~ mean_temperature, data = cd, lambda = "ML") 
zslope_index <- pgls(AVM ~ temperature_depth_index, data = cd, lambda = "ML")
# 3. mass only: AVM varies with oxy only 
zslope_oxy <- pgls(AVM ~ mean_oxygen, data = cd, lambda = "ML") 

# 4. mean_bodymass + Oxy 
zslope_mass_oxy <- pgls(AVM ~ mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 

# 5. mean_bodymass + depthtemp 
zslope_mass_depthtemp  <- pgls(AVM ~ mean_bodymass + mean_depth, data = cd, lambda = "ML") 
zslope_mass_temp  <- pgls(AVM ~ mean_bodymass + mean_temperature, data = cd, lambda = "ML") 
zslope_mass_index  <- pgls(AVM ~ mean_bodymass + temperature_depth_index, data = cd, lambda = "ML") 

# 6. depthtemp + Oxy
zslope_oxy_depthtemp  <- pgls(AVM ~ mean_depth + mean_oxygen, data = cd, lambda = "ML") 
zslope_oxy_temp  <- pgls(AVM ~ mean_temperature + mean_oxygen, data = cd, lambda = "ML") 
zslope_oxy_index  <- pgls(AVM ~ temperature_depth_index + mean_oxygen, data = cd, lambda = "ML") 

# 7. depthtemp * Oxy
#zslope_oxy_int_depthtemp  <- pgls(AVM ~ mean_depth * mean_oxygen, data = cd, lambda = "ML") 

# 8. depthtemp * mass
zslope_mass_int_depthtemp  <- pgls(AVM ~ mean_depth * mean_bodymass, data = cd, lambda = "ML") 
zslope_mass_int_temp  <- pgls(AVM ~ mean_temperature * mean_bodymass, data = cd, lambda = "ML") 
zslope_mass_int_index  <- pgls(AVM ~ temperature_depth_index * mean_bodymass, data = cd, lambda = "ML") 

# 9.  mass * oxy
#zslope_mass_int_oxy  <- pgls(AVM ~ mean_oxygen * mean_bodymass, data = cd, lambda = "ML") 

# 10.  mass * oxy + depthtemp
#zslope_mass_int_oxy_depthtemp  <- pgls(AVM ~ mean_oxygen * mean_bodymass + mean_depth, data = cd, lambda = "ML") 

# 11.  mass * depthtemp + oxy
zslope_mass_int_depthtem_oxy  <- pgls(AVM ~ mean_depth * mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 
zslope_mass_int_tem_oxy  <- pgls(AVM ~ mean_temperature * mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 
zslope_mass_int_index_oxy  <- pgls(AVM ~ temperature_depth_index * mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 

# 12.  mass + depthtemp * oxy
#zslope_oxy_int_depthtemp_mass  <- pgls(AVM ~ mean_depth * mean_oxygen + mean_bodymass, data = cd, lambda = "ML") 

# 13.  mass + depthtemp * oxy
zslope_oxy_depthtemp_mass  <- pgls(AVM ~ mean_depth + mean_oxygen + mean_bodymass, data = cd, lambda = "ML") 
zslope_oxy_temp_mass  <- pgls(AVM ~ mean_temperature + mean_oxygen + mean_bodymass, data = cd, lambda = "ML") 
zslope_oxy_index_mass  <- pgls(AVM ~ temperature_depth_index + mean_oxygen + mean_bodymass, data = cd, lambda = "ML") 

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Save models data
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

models <- list(
  "Intercept Only" = zslope_0,
  "mass only" = zslope_wt,
  "depthtemp only" = zslope_depthtemp,
  "temp only" = zslope_temp,
  "index only" = zslope_index,
  "oxy only" = zslope_oxy,
  "mass + Oxy" = zslope_mass_oxy,
  "mass + depthtemp" = zslope_mass_depthtemp,
  "mass + temp" = zslope_mass_temp,
  "mass + index" = zslope_mass_index,
  "depthtemp + Oxy" = zslope_oxy_depthtemp,
  "temp + Oxy" = zslope_oxy_temp,
  "index + Oxy" = zslope_oxy_index,
  #"depthtemp * Oxy" = zslope_oxy_int_depthtemp,
  "depthtemp * mass" = zslope_mass_int_depthtemp,
  "temp * mass" = zslope_mass_int_temp,
  "index * mass" = zslope_mass_int_index,
  #"mass * oxy" = zslope_mass_int_oxy,
  #"mass * oxy + depthtemp" = zslope_mass_int_oxy_depthtemp,
  "mass * depthtemp + oxy" = zslope_mass_int_depthtem_oxy,
  "mass * temp + oxy" = zslope_mass_int_tem_oxy,
  "mass * index + oxy" = zslope_mass_int_index_oxy,
  #"mass + depthtemp * oxy" = zslope_oxy_int_depthtemp_mass,
  "mass + depthtemp + oxy" = zslope_oxy_depthtemp_mass,
  "mass + temp + oxy" = zslope_oxy_temp_mass,
  "mass + index + oxy" = zslope_oxy_index_mass
)


# manually extracting information from each model object
aics <- sapply(models, function (x) bbmle::AIC(x)) 
aiccs <- sapply(models, function (x) x$aicc) 
formulas <- sapply(models,   # tidying formulas for easier reading
                   function (x) deparse(formula(x), width.cutoff = 90L)) %>%
  sub("^log\\(\\w+\\)\\s\\~\\s", "", .) %>% 
  gsub("_scaled", "", .) %>%
  gsub("\\_wt", "(M)", .) 
r2s <- sapply(models, function (x) summary(x)$r.squared) 
ar2s <- sapply(models, function (x) summary(x)$adj.r.squared)
LL <- sapply(models, function (x) x$model$log.lik) 
ks <- sapply(models, function (x) x$k) 

models_table <- data.frame(formulas, ks, LL, aics, aiccs, r2s, ar2s) %>%
  rename(Model = formulas, n = ks, AIC = aics,
         AICc = aiccs, R_sq = r2s, adj_R_sq = ar2s) %>%
  mutate(Model = as.character(formulas), 
         LL = round(LL, 1), 
         AIC = round(AIC, 1), AICc = round(AICc, 1), 
         dAIC = round(AIC - min(AIC), 2),
         dAICc = round(AICc - min(AICc), 2),
         R_sq = round(R_sq, 2), adj_R_sq = round(adj_R_sq, 2), 
         Weights = round(exp(-dAICc/2)/sum(exp(-dAICc/2)), 3)) # "weights" typically refer to the relative support or weight of each model among a set of competing models. These weights indicate the probability that each model is the best model among those being considered.

View(models_table)

### Top-ranked model diagnostic plots and analyses
topmod <- which(models_table$dAICc == 0)
bestmodel <- models[[topmod]]

#Check results:
summary(bestmodel)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Collect 95% CI for the coefficients and plot it:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# You can also do it for all of them at the same time: 
# Create a list to store the results for each model
results_list <- list()

# Loop through each model and apply the procedure
for (model_name in names(models)) {
  # Get the model
  current_model <- models[[model_name]]
  
  # Extract the coefficient estimates and standard errors
  coefficients <- coef(current_model)
  standard_errors <- summary(current_model)$coefficient[, "Std. Error"]
  
  # Define the degrees of freedom
  df <- nobs(current_model) - length(coefficients)
  
  # Calculate the 95% confidence intervals
  lower_bound <- coefficients - qt(0.975, df) * standard_errors
  upper_bound <- coefficients + qt(0.975, df) * standard_errors
  
  # Number of coefficients in the model
  num_coefficients <- length(coefficients)
  
  # Create a data frame with coefficient information
  result <- data.frame(
    "Coefficient_Name" = names(coefficients),
    "Median_Value" = coefficients,
    "CI_Lower_Bound" = lower_bound,
    "CI_Upper_Bound" = upper_bound
  )
  
  # Store the results in the list with a name corresponding to the model
  results_list[[model_name]] <- result
}

results_list

# Create an empty dataframe to store the combined results
coefficient_table <- data.frame(
  Coefficient_Name = character(),
  Median_Value = numeric(),
  CI_Lower_Bound = numeric(),
  CI_Upper_Bound = numeric(),
  Model = character(),
  stringsAsFactors = FALSE
)

# Loop through each model and apply the procedure
for (model_name in names(results_list)) {
  # Get the results for the current model
  model_results <- results_list[[model_name]]
  
  # Add a 'Model' column with the model identifier
  model_results$Model <- model_name
  
  # Append the results to the combined dataframe
  coefficient_table <- rbind(coefficient_table, model_results)
}

# Add a model identifier to the coefficient names
coefficient_table$Coefficient_Name <- paste0(
  coefficient_table$Coefficient_Name)
#  sub("^.*\\s(\\d+)$", "\\1", coefficient_table$Model)
#)

# Reset row names
rownames(coefficient_table) <- NULL

# View the combined dataframe
head(coefficient_table)

#Save it:
output_file <- file.path(output_data, "CI_coeff_models.csv")
write.csv2(coefficient_table, file = output_file, row.names = FALSE)

# Filter the data to include only specific models
desired_models <- c("mass + depthtemp", "mass * depthtemp + oxy", "mass + depthtemp + oxy", "depthtemp * mass")
filtered_data <- coefficient_table[coefficient_table$Model %in% desired_models, ]

# Create the plot with the vertical line
p <- ggplot(filtered_data, aes(x = Coefficient_Name, y = Median_Value, color = Model)) +
  geom_pointrange(aes(ymin = CI_Lower_Bound, ymax = CI_Upper_Bound), size = 0.5, position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Coefficient Name", y = "Value", title = "Coefficient Values by Model") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Add the vertical line at Y = 0

print(p)

# export plot
p_png <- paste0(output_data, "/PGLS_model_selection_together_Coefficients.png")
ggsave(p_png, p, width=20, height=12, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Check summary of each model to collect lambda data:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Acess all summary to check lambda:
model_summaries <- lapply(models, summary)
model_summaries[1]
model_summaries[2]
model_summaries[3]
model_summaries[4]
model_summaries[5]
model_summaries[6]
model_summaries[7] #good one
model_summaries[8]
model_summaries[9]
model_summaries[10]

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Calculate percentage of support of each model (is the same as weight)
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#To calculate the percentage of support for each model relative to the top-ranked model based on ΔAICc:
top_model <- models_table[which.min(models_table$AICc), ]
#Calculate the likelihood ratio (LR) for each model relative to the top-ranked model using the ΔAICc values.
models_table$LR <- exp(-0.5 * (models_table$dAICc - top_model$dAICc))
#Calculate the sum of likelihood ratios (Σ(LR)) for all models.
sum_LR <- sum(models_table$LR)
#Calculate the percentage of support for each model.
models_table$Percentage_of_Support <- (models_table$LR / sum_LR) * 100
View(models_table)
#This calculation provides a way to express the relative support for each model compared to the top-ranked model. If a model has a higher percentage of support (e.g., approximately 55%), it means that it is less well-supported by the data compared to the top-ranked model but is still a reasonable model in the context of model selection. The percentage of support gives a sense of how well the model fits the data relative to the best model considered.
output_file <- file.path(output_data, "models_table.csv")
write.csv2(models_table, file = output_file, row.names = FALSE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Plot relationships between variables:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Plot relationship between AVM and bodymass:
setwd(input_data)
data <- read.csv("GLMM_predictors2.csv", sep = ";")
names(data)
head(data)

#Format new predictors:
names(data)
data <- data %>%
  mutate(mean_bodymass = as.numeric(gsub(",", ".", mean_bodymass)),  # Convert comma decimal to dot decimal
         mean_depth = as.numeric(gsub(",", ".", mean_depth)),
         mean_temperature = as.numeric(gsub(",", ".", mean_temperature)),
         mean_oxygen = as.numeric(gsub(",", ".", mean_oxygen)),
         AVM = as.numeric(gsub(",", ".", AVM)))
#data$PCM = as.numeric(gsub(",", ".", data$PCM))
data <- data %>% 
  mutate(RespirationMode = factor(RespirationMode, c(1,2,4)))
# Create a data frame with AVM and mean_bodymass values
data2 <- data.frame(AVM = data$AVM, mean_bodymass = log1p(data$mean_bodymass), mean_depth = data$mean_depth, mean_oxygen = data$mean_oxygen, Species = data$Species)

# Create a scatter plot with a fitted line in log-log scale
# AVM VS MASS
plot <- ggplot(data2, aes(x = mean_bodymass, y = AVM, size = mean_bodymass, color = mean_depth)) +
  stat_smooth(method = "lm", se = TRUE, color = "blue") +  # Add a linear regression line with confidence intervals
  #geom_text(aes(label = Species), vjust = -1) +  # Add labels for "Species" names
  geom_point() +  # Add scatter points with size and color based on mean_bodymass and mean_depth
  labs(x = "Log(Mean Body Mass)", y = "AVM", title = "Relationship between AVM and Body Mass (log-log scale)") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +  # Set color scale
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))  # Customize axis lines and ticks
plot <- plot + coord_cartesian(ylim = c(0, 100))

print(plot)

p_png <- paste0(output_data, "/AVM_Mass.png")
ggsave(p_png, plot, width=18, height=13, units="cm", dpi=300)

# AVM VS DEPTH
plot <- ggplot(data2, aes(x = mean_depth, y = AVM, size = mean_bodymass, color = mean_depth)) +
  stat_smooth(method = "lm", se = TRUE, color = "blue") +  # Add a linear regression line with confidence intervals
  #geom_text(aes(label = Species), vjust = -1) +  # Add labels for "Species" names
  geom_point() +  # Add scatter points with size and color based on mean_bodymass and mean_depth
  labs(x = "Depth (m)", y = "AVM", title = "Relationship between AVM and Body Mass (log-log scale)") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +  # Set color scale
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))  # Customize axis lines and ticks
plot <- plot + coord_cartesian(ylim = c(0, 100))

print(plot)

p_png <- paste0(output_data, "/AVM_Depth.png")
ggsave(p_png, plot, width=18, height=13, units="cm", dpi=300)

# AVM VS oxygen
plot <- ggplot(data2, aes(x = mean_oxygen, y = AVM, size = mean_bodymass, color = mean_oxygen)) +
  stat_smooth(method = "lm", se = TRUE, color = "blue") +  # Add a linear regression line with confidence intervals
  geom_point() +  # Add scatter points with size and color based on mean_bodymass and mean_depth
  labs(x = "Oxygen", y = "AVM", title = "Relationship between AVM and Body Mass (log-log scale)") +
  scale_color_gradient(low = "darkblue", high = "lightblue") +  # Set color scale
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))  # Customize axis lines and ticks

print(plot)

p_png <- paste0(output_data, "/AVM_Oxygen.png")
ggsave(p_png, plot, width=18, height=13, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Run best model to check lambda plot and homoscedasticity and normality 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Re-running the model using nlme:gls for diagnostics
# parameter estimates are obtained
# corPagel() is used as it's equivalent to `lambda = "ML"` in pgls

bestmodel_gls <- nlme::gls(formula(bestmodel), data = cd$data, 
                           correlation = corPagel(1, cd$phy)) 

# almost identical coefficent estimates
rbind(coef(bestmodel), coef(bestmodel_gls))

# residual plots almost identical as well, homoscedastic
plot(resid(bestmodel) ~ fitted(bestmodel)) # pgls
plot(bestmodel_gls) # gls

# estimate Variance inflation factors (on gls model) to test for collinearity
vif(bestmodel_gls)

# qqplot (on gls model) looks normal
qqnorm(bestmodel_gls)

# profile plot of lambda
plot(pgls.profile(bestmodel)) # lambda closer to 1 indicates strong phylogenetic signal
#0.53

#Plot partial effect:
par(mfrow=c(2,2))
plot(bestmodel)


#---------------------------------------------------------------------------------------------------
# Repeat with PCM as reponse variable:
#---------------------------------------------------------------------------------------------------

# 0 intercept only
PCMslope_0 <- pgls(PCM ~ 1, data = cd, lambda = "ML")

# 1. mass only: AVM varies with mass only 
PCMslope_wt <- pgls(PCM ~ mean_bodymass, data = cd, lambda = "ML")

# 2. mass only: AVM varies with depth-temp index only 
PCMslope_depthtemp <- pgls(PCM ~ mean_depth, data = cd, lambda = "ML") 

# 3. mass only: AVM varies with oxy only 
PCMslope_oxy <- pgls(PCM ~ mean_oxygen, data = cd, lambda = "ML") 

# 4. mean_bodymass + Oxy 
PCMslope_mass_oxy <- pgls(PCM ~ mean_bodymass + mean_oxygen, data = cd, lambda = "ML")

# 5. mean_bodymass + depthtemp 
PCMslope_mass_depthtemp  <- pgls(PCM ~ mean_bodymass + mean_depth, data = cd, lambda = "ML") 

# 6. depthtemp + Oxy
PCMslope_oxy_depthtemp  <- pgls(PCM ~ mean_depth + mean_oxygen, data = cd, lambda = "ML") 

# 7. depthtemp * Oxy
#zslope_oxy_int_depthtemp  <- pgls(AVM ~ mean_depth * mean_oxygen, data = cd, lambda = "ML") 

# 8. depthtemp * mass
PCMslope_mass_int_depthtemp  <- pgls(PCM ~ mean_depth * mean_bodymass, data = cd, lambda = "ML") 

# 9.  mass * oxy
#zslope_mass_int_oxy  <- pgls(AVM ~ mean_oxygen * mean_bodymass, data = cd, lambda = "ML") 

# 10.  mass * oxy + depthtemp
#zslope_mass_int_oxy_depthtemp  <- pgls(AVM ~ mean_oxygen * mean_bodymass + mean_depth, data = cd, lambda = "ML") 

# 11.  mass * depthtemp + oxy
PCMslope_mass_int_depthtem_oxy  <- pgls(PCM ~ mean_depth * mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 

# 12.  mass + depthtemp * oxy
#zslope_oxy_int_depthtemp_mass  <- pgls(AVM ~ mean_depth * mean_oxygen + mean_bodymass, data = cd, lambda = "ML") 

# 13.  mass + depthtemp * oxy
PCMslope_oxy_depthtemp_mass  <- pgls(PCM ~ mean_depth + mean_oxygen + mean_bodymass, data = cd, lambda = "ML")

modelsPCM <- list(
  "Intercept Only" = PCMslope_0,
  "mass only" = PCMslope_wt,
  "depthtemp only" = PCMslope_depthtemp,
  "oxy only" = PCMslope_oxy,
  "mass + Oxy" = PCMslope_mass_oxy,
  "mass + depthtemp" = PCMslope_mass_depthtemp,
  "depthtemp + Oxy" = PCMslope_oxy_depthtemp,
  #"depthtemp * Oxy" = zslope_oxy_int_depthtemp,
  "depthtemp * mass" = PCMslope_mass_int_depthtemp,
  #"mass * oxy" = zslope_mass_int_oxy,
  #"mass * oxy + depthtemp" = zslope_mass_int_oxy_depthtemp,
  "mass * depthtemp + oxy" = PCMslope_mass_int_depthtem_oxy,
  #"mass + depthtemp * oxy" = zslope_oxy_int_depthtemp_mass,
  "mass + depthtemp + oxy" = PCMslope_oxy_depthtemp_mass
)

# manually extracting information from each model object
aics <- sapply(modelsPCM, function (x) bbmle::AIC(x)) 
aiccs <- sapply(modelsPCM, function (x) x$aicc) 
formulas <- sapply(modelsPCM,   # tidying formulas for easier reading
                   function (x) deparse(formula(x), width.cutoff = 90L)) %>%
  sub("^log\\(\\w+\\)\\s\\~\\s", "", .) %>% 
  gsub("_scaled", "", .) %>%
  gsub("\\_wt", "(M)", .) 
r2s <- sapply(modelsPCM, function (x) summary(x)$r.squared) 
ar2s <- sapply(modelsPCM, function (x) summary(x)$adj.r.squared)
LL <- sapply(modelsPCM, function (x) x$model$log.lik) 
ks <- sapply(modelsPCM, function (x) x$k) 

modelsPCM_table <- data.frame(formulas, ks, LL, aics, aiccs, r2s, ar2s) %>%
  rename(Model = formulas, n = ks, AIC = aics,
         AICc = aiccs, R_sq = r2s, adj_R_sq = ar2s) %>%
  mutate(Model = as.character(formulas), 
         LL = round(LL, 1), 
         AIC = round(AIC, 1), AICc = round(AICc, 1), 
         dAIC = round(AIC - min(AIC), 2),
         dAICc = round(AICc - min(AICc), 2),
         R_sq = round(R_sq, 2), adj_R_sq = round(adj_R_sq, 2), 
         Weights = round(exp(-dAICc/2)/sum(exp(-dAICc/2)), 3)) # "weights" typically refer to the relative support or weight of each model among a set of competing models. These weights indicate the probability that each model is the best model among those being considered.

View(modelsPCM_table)

### Top-ranked model diagnostic plots and analyses
topmodPCM <- which(modelsPCM_table$dAICc == 0)
bestmodelPCM <- modelsPCM[[topmodPCM]]

#Check results:
summary(bestmodelPCM)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 95% CI intervals for coefficients:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

results_list <- list()

# Loop through each model and apply the procedure
for (model_name in names(modelsPCM)) {
  # Get the model
  current_model <- modelsPCM[[model_name]]
  
  # Extract the coefficient estimates and standard errors
  coefficients <- coef(current_model)
  standard_errors <- summary(current_model)$coefficient[, "Std. Error"]
  
  # Define the degrees of freedom
  df <- nobs(current_model) - length(coefficients)
  
  # Calculate the 95% confidence intervals
  lower_bound <- coefficients - qt(0.975, df) * standard_errors
  upper_bound <- coefficients + qt(0.975, df) * standard_errors
  
  # Number of coefficients in the model
  num_coefficients <- length(coefficients)
  
  # Create a data frame with coefficient information
  result <- data.frame(
    "Coefficient_Name" = names(coefficients),
    "Median_Value" = coefficients,
    "CI_Lower_Bound" = lower_bound,
    "CI_Upper_Bound" = upper_bound
  )
  
  # Store the results in the list with a name corresponding to the model
  results_list[[model_name]] <- result
}

results_list

# Create an empty dataframe to store the combined results
coefficient_table <- data.frame(
  Coefficient_Name = character(),
  Median_Value = numeric(),
  CI_Lower_Bound = numeric(),
  CI_Upper_Bound = numeric(),
  Model = character(),
  stringsAsFactors = FALSE
)

# Loop through each model and apply the procedure
for (model_name in names(results_list)) {
  # Get the results for the current model
  model_results <- results_list[[model_name]]
  
  # Add a 'Model' column with the model identifier
  model_results$Model <- model_name
  
  # Append the results to the combined dataframe
  coefficient_table <- rbind(coefficient_table, model_results)
}

# Add a model identifier to the coefficient names
coefficient_table$Coefficient_Name <- paste0(
  coefficient_table$Coefficient_Name)
#  sub("^.*\\s(\\d+)$", "\\1", coefficient_table$Model)
#)

# Reset row names
rownames(coefficient_table) <- NULL

# View the combined dataframe
head(coefficient_table)

output_file <- file.path(output_data, "PCM_CI_table.csv")
write.csv2(coefficient_table, file = output_file, row.names = FALSE)

# Filter the data to include only specific models
desired_models <- c("mass + depthtemp", "mass * depthtemp + oxy", "mass + depthtemp + oxy", 
                    "depthtemp * mass", "mass + Oxy", "mass only", "oxy only")
filtered_data <- coefficient_table[coefficient_table$Model %in% desired_models, ]

# Create the plot with the vertical line
p <- ggplot(filtered_data, aes(x = Coefficient_Name, y = Median_Value, color = Model)) +
  geom_pointrange(aes(ymin = CI_Lower_Bound, ymax = CI_Upper_Bound), size = 0.5, position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Coefficient Name", y = "Value", title = "Coefficient Values by Model") +
  coord_flip() +  # Swap X and Y axes
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Add the vertical line at Y = 0

print(p)

# export plot
p_png <- paste0(output_data, "/PCM_PGLS_model_selection_together_Coefficients.png")
ggsave(p_png, p, width=20, height=12, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Summaries to collect lambda data:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Acess all summary to check lambda:
model_summaries <- lapply(modelsPCM, summary)
model_summaries[1]
model_summaries[2]
model_summaries[3]
model_summaries[4]
model_summaries[5]
model_summaries[6] #good one
model_summaries[7] 
model_summaries[8]
model_summaries[9]
model_summaries[10]

#To calculate the percentage of support for each model relative to the top-ranked model based on ΔAICc:
top_model <- modelsPCM_table[which.min(modelsPCM_table$AICc), ]
#Calculate the likelihood ratio (LR) for each model relative to the top-ranked model using the ΔAICc values.
modelsPCM_table$LR <- exp(-0.5 * (modelsPCM_table$dAICc - top_model$dAICc))
#Calculate the sum of likelihood ratios (Σ(LR)) for all models.
sum_LR <- sum(modelsPCM_table$LR)
#Calculate the percentage of support for each model.
modelsPCM_table$Percentage_of_Support <- (modelsPCM_table$LR / sum_LR) * 100
View(modelsPCM_table)
#This calculation provides a way to express the relative support for each model compared to the top-ranked model. If a model has a higher percentage of support (e.g., approximately 55%), it means that it is less well-supported by the data compared to the top-ranked model but is still a reasonable model in the context of model selection. The percentage of support gives a sense of how well the model fits the data relative to the best model considered.
output_file <- file.path(output_data, "PCM_models_table.csv")
write.csv2(models_table, file = output_file, row.names = FALSE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Plot relationships between variables:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Plot relationship between AVM and bodymass:
setwd(input_data)
data <- read.csv("GLMM_predictors2.csv", sep = ";")
names(data)
head(data)

#Format new predictors:
names(data)
data <- data %>%
  mutate(mean_bodymass = as.numeric(gsub(",", ".", mean_bodymass)),  # Convert comma decimal to dot decimal
         mean_depth = as.numeric(gsub(",", ".", mean_depth)),
         mean_temperature = as.numeric(gsub(",", ".", mean_temperature)),
         mean_oxygen = as.numeric(gsub(",", ".", mean_oxygen)),
         AVM = as.numeric(gsub(",", ".", AVM)),
         PCM = as.numeric(gsub(",", ".", PCM)))
#data$PCM = as.numeric(gsub(",", ".", data$PCM))
data <- data %>% 
  mutate(RespirationMode = factor(RespirationMode, c(1,2,4)))
# Create a data frame with AVM and mean_bodymass values
data2 <- data.frame(PCM = data$PCM, mean_bodymass = log1p(data$mean_bodymass), mean_depth = data$mean_depth, mean_oxygen = data$mean_oxygen, Species = data$Species)

# Create a scatter plot with a fitted line in log-log scale
# AVM VS MASS
plot <- ggplot(data2, aes(x = mean_bodymass, y = PCM, size = mean_bodymass, color = mean_depth)) +
  stat_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point() +
  #geom_text(aes(label = Species), vjust = -1) +  # Add labels for "Species" names
  labs(x = "ln(mass)", y = "Estimated post-capture mortality rate (%)", title = "Relationship between PCM and Body Mass (log-log scale)") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))

# Limit the Y-axis to the range 0-100
plot <- plot + coord_cartesian(ylim = c(0, 100))

print(plot)

p_png <- paste0(output_data, "/PCM_Mass.png")
ggsave(p_png, plot, width=18, height=13, units="cm", dpi=300)

# AVM VS DEPTH
plot <- ggplot(data2, aes(x = mean_depth, y = PCM, size = mean_bodymass, color = mean_depth)) +
  stat_smooth(method = "lm", se = TRUE, color = "blue") +  # Add a linear regression line with confidence intervals
  #geom_text(aes(label = Species), vjust = -1) +  # Add labels for "Species" names
  geom_point() +  # Add scatter points with size and color based on mean_bodymass and mean_depth
  labs(x = "Depth (m)", y = "PCM", title = "Relationship between AVM and Body Mass (log-log scale)") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +  # Set color scale
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))  # Customize axis lines and ticks
plot <- plot + coord_cartesian(ylim = c(0, 100))

print(plot)


p_png <- paste0(output_data, "/PCM_Depth.png")
ggsave(p_png, plot, width=18, height=13, units="cm", dpi=300)

# AVM VS oxygen
plot <- ggplot(data2, aes(x = mean_oxygen, y = PCM, size = mean_bodymass, color = mean_oxygen)) +
  stat_smooth(method = "lm", se = TRUE, color = "blue") +  # Add a linear regression line with confidence intervals
  #geom_text(aes(label = Species), vjust = -1) +  # Add labels for "Species" names
  geom_point() +  # Add scatter points with size and color based on mean_bodymass and mean_depth
  labs(x = "Oxygen", y = "PCM", title = "Relationship between AVM and Body Mass (log-log scale)") +
  scale_color_gradient(low = "darkblue", high = "lightblue") +  # Set color scale
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"))  # Customize axis lines and ticks
plot <- plot + coord_cartesian(ylim = c(0, 100))
print(plot)

p_png <- paste0(output_data, "/PCM_Oxygen.png")
ggsave(p_png, plot, width=18, height=13, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Run best model to check lambda plot and homoscedasticity and normality 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Re-running the model using nlme:gls for diagnostics
# parameter estimates are obtained
# corPagel() is used as it's equivalent to `lambda = "ML"` in pgls

bestmodel_gls <- nlme::gls(formula(bestmodelPCM), data = cd$data, 
                           correlation = corPagel(1, cd$phy)) 

# almost identical coefficent estimates
rbind(coef(bestmodelPCM), coef(bestmodel_gls))

# residual plots almost identical as well, homoscedastic
plot(resid(bestmodelPCM) ~ fitted(bestmodelPCM)) # pgls
plot(bestmodel_gls) # gls

# estimate Variance inflation factors (on gls model) to test for collinearity
vif(bestmodel_gls)

# qqplot (on gls model) looks normal
qqnorm(bestmodel_gls)

# profile plot of lambda
plot(pgls.profile(bestmodelPCM)) # lambda closer to 1 indicates strong phylogenetic signal

#Plot partial effect:
par(mfrow=c(2,2))
plot(bestmodelPCM)
