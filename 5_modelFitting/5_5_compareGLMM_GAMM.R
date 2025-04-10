# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
# 5.5. Compare GLMM and GAMM
#-------------------------------------------------------------------------------

# 1. Load AVM data--------------------------------------------------------------
data <- read.csv("temp/mod5AVM_allEnviro - copia.csv", sep = ";")

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

# Add habitat:
habitat_mapping <- c(
  "Abovinus" = "benthopelagic",  # pelagic
  "Cmonstrosa" = "benthopelagic",    # demersal
  "Cuyato" = "benthopelagic",    # demersal
  "Dlicha" = "benthopelagic",    # demersal
  "Doxyrinchus" = "demersal",   # demersal
  "Dpastinaca" = "demersal",    # demersal
  "Espinax" = "benthopelagic",   # demersal
  "Gmelastomus" = "demersal",   # demersal
  "Hgriseus" = "benthopelagic",  # demersal
  "Maquila" = "benthopelagic",   # pelagic
  "Ocentrina" = "benthopelagic",     #
  "Pviolacea" = "benthopelagic", # 
  "Rasterias" = "demersal",     # demersal
  "Rclavata" = "demersal",      # demersal
  "Rpolystigma" = "demersal",   # demersal
  "Scanicula" = "demersal",     # demersal
  "Tmarmorata" = "demersal"     # demersal
)

#Add the habitat column to your data frame
data <- data %>%
  mutate(habitat = factor(habitat_mapping[Species], levels = c("benthopelagic", "demersal")))
str(data)

# Add reproductive mode: 
# matrotrophic viviparity (viviparous)
# lecithotrophic viviparity (ovoviviparous: yolk-sac viviparous)
# lecithotrophic oviparity (oviparous)
repro_mapping <- c(
  "Abovinus" = "viviparous",  
  "Cmonstrosa" = "oviparous", 
  "Cuyato" = "ovoviviparous", 
  "Dlicha" = "ovoviviparous", 
  "Doxyrinchus" = "oviparous", 
  "Dpastinaca" = "viviparous", 
  "Espinax" = "ovoviviparous", 
  "Gmelastomus" = "oviparous", 
  "Hgriseus" = "ovoviviparous", 
  "Maquila" = "viviparous", 
  "Ocentrina" = "ovoviviparous", 
  "Pviolacea" = "viviparous", 
  "Rasterias" = "oviparous", 
  "Rclavata" = "oviparous", 
  "Rpolystigma" = "oviparous", 
  "Scanicula" = "oviparous", 
  "Tmarmorata" = "ovoviviparous" 
)

#Add the habitat column to your data frame
data <- data %>%
  mutate(repro = factor(repro_mapping[Species], levels = c("oviparous", "ovoviviparous", "viviparous")))
str(data)


# Add  Active hypoxia tolerances (Aeco): 
# Data source: Penn and Curtis (2022) https://doi.org/10.1098/rstb.2022.0487
# Open the data downloaded from their supplementary material in https://github.com/jlpenn/MI_traits_obis
#dataAeco <- read.csv("input/Aeco/species_Aeco.csv", sep = ",")

Aeco_mapping <- c(
  "Abovinus" = 19.152030,  
  "Cmonstrosa" = 5.868567, 
  "Cuyato" = 18.48787, 
  "Dlicha" = 9.797474, 
  "Doxyrinchus" = 6.140164, 
  "Dpastinaca" = 5.84354, 
  "Espinax" = 7.641649, 
  "Gmelastomus" = 6.023641, 
  "Hgriseus" = 7.494590, 
  "Maquila" = 9.793311, 
  "Ocentrina" = 19.15203, 
  "Pviolacea" = 8.297255, 
  "Rasterias" = 6.2329480, 
  "Rclavata" = 6.522564,  
  "Rpolystigma" = 5.4093170, 
  "Scanicula" = 6.745532, 
  "Tmarmorata" = 9.298298)

#Add the habitat column to your data frame
data <- data %>%
  mutate(Aeco = as.numeric(Aeco_mapping[Species]))
str(data)


# Add ventilation mode:
Ventilation_mapping <- c(
  "Abovinus" = "stationary",  
  "Cmonstrosa" = "stationary", 
  "Cuyato" = "stationary", 
  "Dlicha" = "stationary", 
  "Doxyrinchus" = "stationary", 
  "Dpastinaca" = "stationary", 
  "Espinax" = "stationary", 
  "Gmelastomus" = "stationary", 
  "Hgriseus" = "ram", 
  "Maquila" = "stationary", 
  "Ocentrina" = "stationary", 
  "Pviolacea" = "stationary", 
  "Rasterias" = "stationary", 
  "Rclavata" = "stationary",  
  "Rpolystigma" = "stationary", 
  "Scanicula" = "stationary", 
  "Tmarmorata" = "stationary" 
)

#Add the habitat column to your data frame
data <- data %>%
  mutate(ventilation = factor(Ventilation_mapping[Species], levels = c("stationary", "ram")))
str(data)


#2. Scale variables for modelling-----------------------------------------------
#vars to be used <- bodymass, depth, habitat, temperature exposed, time exposed

#Weight
hist(data$bodymass)
shapiro.test(data$bodymass)
#data[data$code == "21_10_06_CRT_VG_L2" & data$Species == "Hgriseus", "bodymass"] <- 73990.5636
data$ln_bodymass <- log1p(data$bodymass)
hist(data$ln_bodymass)
shapiro.test(data$ln_bodymass)
#scale
data$sln_bodymass<-scale(data$ln_bodymass)

#Depth
hist(data$depth)
shapiro.test(data$depth)
#data$ln_depth <- log1p(data$depth)
#hist(data$ln_depth)
#shapiro.test(data$ln_depth)
#scale
data$sdepth<-scale(data$depth)

#MinsExposed
hist(data$MinsExposedtoAir)
shapiro.test(data$MinsExposedtoAir)
#data$ln_MinsExposedtoAir <- log1p(data$MinsExposedtoAir)
#hist(data$ln_MinsExposedtoAir)
#shapiro.test(data$ln_MinsExposedtoAir)
#scale
data$sMinsExposedtoAir<-scale(data$MinsExposedtoAir)

#AT
hist(data$at_celsius)
shapiro.test(data$at_celsius)
#data$ln_at_celsius <- log1p(data$at_celsius)
#hist(data$ln_at_celsius)
#shapiro.test(data$ln_at_celsius)
#scale
data$sat_celsius<-scale(data$at_celsius)

#T_change
hist(data$temp_change)
shapiro.test(data$temp_change)
#data$ln_temp_change <- log1p(data$temp_change)
#hist(data$ln_temp_change)
#shapiro.test(data$ln_temp_change)
#scale
data$stemp_change<-scale(data$temp_change)

#Duration
#hist(data$Trawl_duration)
#shapiro.test(data$Trawl_duration)
#scale
#data$sduration<-scale(data$Trawl_duration)

# Reproductive mode
barplot(table(data$repro), main="Repro Categories", col="skyblue")


# Active hypoxia tolerance
hist(data$Aeco)
shapiro.test(data$Aeco)
data$ln_Aeco <- log1p(data$Aeco)
hist(data$ln_Aeco)
shapiro.test(data$ln_Aeco)
#scale
data$sln_Aeco<-scale(data$ln_Aeco)


# Respiration mode
barplot(table(data$ventilation), main="Ventilation Categories", col="skyblue")


# For fitting the health model, remove health NAs:
data_health <- data %>%
  filter(!is.na(Activity)) %>%
  filter(Alive_Dead == 1)  %>%
  mutate(Health_prob = as.numeric(gsub(",", ".", Health_prob)))


# 3. Compare GLMM and GAMM for AVM and select-------------------------------------------
# Cross-validation for GLMM
library(lme4)
library(caret)

# Define folds for cross-validation
folds <- createFolds(data$Alive_Dead, k = 5)

# Initialize a list to store RMSE for each fold
rmse_values <- c()

# Perform k-fold cross-validation
for (i in 1:5) {
  # Split the data into training and testing sets
  train_data <- data[-folds[[i]], ]
  test_data <- data[folds[[i]], ]
  
  # Fit the model on the training set
  model <- lmer(Alive_Dead ~ sln_bodymass + sat_celsius + sMinsExposedtoAir + sdepth + (1 | Species), 
                data = train_data, REML = FALSE)

  # Predict on the testing set
  predictions <- predict(model, newdata = test_data, allow.new.levels = TRUE)
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$Alive_Dead - predictions)^2))
  rmse_values <- c(rmse_values, rmse)
}

# Average RMSE across folds
mean_rmse_glmm <- mean(rmse_values)
mean_rmse_glmm

# Cross-validation for GAMM
# Define folds for cross-validation
set.seed(123)
folds <- createFolds(data$Alive_Dead, k = 5)


# Initialize a list to store RMSE for each fold
rmse_values_gamm <- c()

# Perform k-fold cross-validation
for (i in 1:5) {
  # Split the data into training and testing sets
  train_data <- data[-folds[[i]], ]
  test_data <- data[folds[[i]], ]
  
  # Fit the GAMM on the training set
  gamm_model <- gam(Alive_Dead ~ 
                        s(sMinsExposedtoAir, k = 5) + 
                        s(sdepth, k = 15) + 
                        s(sln_bodymass, k = 15) + 
                        s(sat_celsius, k = 15) +
                        #habitat + 
                        #s(sln_Aeco, k = 5) +
                        s(Species, bs = "re"),
                    family = betar(link = "logit"), 
                    data = train_data#)
                      ,  gamma=1.4)

  # Predict on the testing set
  predictions <- predict(gamm_model, newdata = test_data, type = "response")
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$Alive_Dead - predictions)^2))
  rmse_values_gamm <- c(rmse_values_gamm, rmse)
}

# Average RMSE across folds
mean_rmse_gamm <- mean(rmse_values_gamm)
mean_rmse_gamm

#The model with the lower RMSE is the better choice for predictive performance.
cat("Mean RMSE for GLMM:", mean_rmse_glmm, "\n")
cat("Mean RMSE for GAMM:", mean_rmse_gamm, "\n")








# 4. Compare GLMM and GAMM for Health and select-------------------------------------------
data_health$Health_prob_scaled <- data_health$Health_prob / 100

# Cross-validation for GLMM
# Define folds for cross-validation
folds <- createFolds(data_health$Health_prob_scaled, k = 5)

# Initialize a list to store RMSE for each fold
rmse_values <- c()

# Perform k-fold cross-validation
for (i in 1:5) {
  # Split the data into training and testing sets
  train_data <- data_health[-folds[[i]], ]
  test_data <- data_health[folds[[i]], ]

  # Fit the model on the training set
  model <- lmer(Health_prob_scaled ~ sdepth + sMinsExposedtoAir + sat_celsius + (1 | Species), 
                data = train_data, REML = FALSE)

  # Predict on the testing set
  predictions <- predict(model, newdata = test_data, allow.new.levels = TRUE)
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$Health_prob_scaled - predictions)^2))
  rmse_values <- c(rmse_values, rmse)
}

# Average RMSE across folds
mean_rmse_glmm <- mean(rmse_values)
mean_rmse_glmm

# Cross-validation for GAMM
# re-scale response variable:
n <- nrow(data_health)
epsilon <- 1 / (2 * n) # Small adjustment factor
data_health$Health_prob_scaled <- (data_health$Health_prob / 100) * (1 - 2 * epsilon) + epsilon

hist(data_health$Health_prob_scaled)
shapiro.test(data_health$Health_prob_scaled)
summary(data_health$Health_prob_scaled)

# Define folds for cross-validation
set.seed(123)
folds <- createFolds(data_health$Health_prob_scaled, k = 5)


# Initialize a list to store RMSE for each fold
rmse_values_gamm <- c()

# Perform k-fold cross-validation
for (i in 1:5) {
  # Split the data into training and testing sets
  train_data <- data_health[-folds[[i]], ]
  test_data <- data_health[folds[[i]], ]
  
  # Fit the GAMM on the training set
  gamm_model <- gam(Health_prob_scaled ~ 
                      s(MinsExposedtoAir, k = 5) + 
                      s(depth, k = 15) + 
                      s(ln_bodymass, k = 5) + 
                      s(sat_celsius, k = 15) +
                      #habitat + 
                      #s(sln_Aeco, k = 5) +
                      s(Species, bs = "re"),
                    family = betar(link = "logit"), 
                    data = train_data)
                    #,  gamma=1.4)
  
  
  # Predict on the testing set
  predictions <- predict(gamm_model, newdata = test_data, type = "response")
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$Health_prob_scaled - predictions)^2))
  rmse_values_gamm <- c(rmse_values_gamm, rmse)
}

# Average RMSE across folds
mean_rmse_gamm <- mean(rmse_values_gamm)
mean_rmse_gamm

#The model with the lower RMSE is the better choice for predictive performance.
cat("Mean RMSE for GLMM:", mean_rmse_glmm, "\n")
cat("Mean RMSE for GAMM:", mean_rmse_gamm, "\n")
