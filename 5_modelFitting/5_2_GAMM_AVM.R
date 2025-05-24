# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
# 5.2. GAMM for AVM rates
#-------------------------------------------------------------------------------
library(dplyr)
library(lme4)
library(mgcv)
library(arm)
library(car)
library(beepr)
library(gratia)
library(visreg)
library(ggplot2)
library(gamm4)
library(PresenceAbsence)  # For AUC calculation
library(caret)  # For cross-validation
library(mgcViz) # partial effects

# 1. Load AVM data--------------------------------------------------------------
data <- read.csv("temp/final/AVM_allEnviro.csv", sep = ";") #

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

# Check with a subset for Gmelastomus
#subset_gmelastomus <- data %>% filter(Species == "Gmelastomus")

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

# Check with a subset for Gmelastomus
#subset_gmelastomus <- data %>% filter(Species == "Gmelastomus")

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

# Check with a subset for Gmelastomus
#subset_gmelastomus <- data %>% filter(Species == "Gmelastomus")


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

summary_stats <- data %>%
  dplyr::group_by(Species) %>%
  summarise(
    mean_bodymass = mean(bodymass, na.rm = TRUE),
    sd_bodymass = sd(bodymass, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mean_sd = paste0(round(mean_bodymass, 2), " ± ", round(sd_bodymass, 2)))

print(summary_stats)


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

# Trawl duration
hist(data$Trawl_duration)
shapiro.test(data$Trawl_duration)
#data$ln_Trawl_duration <- log1p(data$Trawl_duration)
#hist(data$ln_Trawl_duration)
#shapiro.test(data$ln_Trawl_duration)
#scale
data$sTrawl_duration<-scale(data$Trawl_duration)


# Respiration mode
barplot(table(data$ventilation), main="Ventilation Categories", col="skyblue")

# habitat
barplot(table(data$habitat), main="Habitat", col="skyblue")


# Filter NAs:
data <- data %>% 
  filter(!is.na(at_celsius), 
         !is.na(sln_bodymass)) %>% 
  mutate(Alive_Dead = ifelse(Alive_Dead == 0, 1, 0),
         Metier = factor(Metier),
         Vessel = factor(Vessel),
         habitat = factor(habitat))


# 3. Build a GAMM for AVM ------------------------------------------------------
# Fit the GAMM with a binomial family for a binary response


# 3.1. Build in an automatised way----------------------------------------------
# You can use a loop to fit it, but it is better to check every parameter closely:
## Add all the possible combinations within a loop:
## Define your predictors (to test all combinations)
#predictors <- c("s(sMinsExposedtoAir)", "s(sdepth)", "s(sln_bodymass)", "habitat", "s(sat_celsius)", "s(sln_Aeco)", "s(sTrawl_duration)") #repro

## Generate all subsets of the predictors
#all_combinations <- lapply(1:length(predictors), function(x) combn(predictors, x, simplify = FALSE)) %>%
#  unlist(recursive = FALSE)

## Initialize a data frame to store results
#model_results <- data.frame(
#  formula = character(),
#  AIC = numeric(),
#  deviance_explained = numeric(),
#  adj_r_squared = numeric(),
#  errors = character())

## Loop through each combination and fit models
#for (combo in all_combinations) {
#  #combo <- all_combinations[1]
#  base_formula <- "s(Species, bs = 're')"

#  # Create model formula by combining the base and current combination of predictors
#  formula <- as.formula(paste("Alive_Dead ~", paste(c(combo, base_formula), collapse = " + ")))
#  
#  # Try fitting the model
#  tryCatch({
#    model <- gam(formula, family = binomial(link = "logit"), data = data)

#    # Extract metrics if model fitting succeeds
#    model_summary <- summary(model)
#    aic <- AIC(model)
#    dev_expl <- if (!is.null(model_summary$dev.expl)) model_summary$dev.expl else NA
#    summary_string <- capture.output(model_summary)
#    adj_r2 <- grep("R-sq\\.\\(adj\\)", summary_string, value = TRUE)
#    if (length(adj_r2) > 0) {
#      r_sq_adj <- as.numeric(sub(".*R-sq\\.\\(adj\\) =\\s*([0-9\\.]+).*", "\\1", adj_r2))
#    } else {
#      adj_r2 <- NA  # Handle case where the line is not found
#    }

#    # Append results to the data frame
#    model_results <- rbind(
#      model_results,
#      data.frame(
#        formula = deparse(formula),
#        AIC = aic,
#        deviance_explained = dev_expl,
#        adj_r_squared = adj_r2,
#        errors = NA
#      )
#    )
#  }, error = function(e) {
#    # If an error occurs, log the formula and the error message
#    model_results <- rbind(
#      model_results,
#      data.frame(
#        formula = deparse(formula),
#        AIC = NA,
#        deviance_explained = NA,
#        adj_r_squared = NA,
#        errors = as.character(e$message)
#      )
#    )
#  })
#}

## Sort models by AIC (or other metrics)
#model_results <- model_results %>% arrange(AIC)

## Display the results
#model_results
#beep()

# Fit selected model:
#gamm_model <- gam(
#  Alive_Dead ~ 
#    s(MinsExposedtoAir, k = 7) + 
#    s(depth, k = 5) + 
#    s(ln_bodymass, k = 7) + 
#    s(at_celsius, k = 5) +
#    #habitat + 
#    #s(Trawl_duration, k = 3) +
#    s(ln_Aeco, k = 7) +
#    s(Species, bs = "re"),
#  family = binomial(link = "logit"),
#  data = data
#  #,  gamma=1.4
#)

#summary(gamm_model)         # Summary of the model
#AIC(gamm_model)             # 1819.563 #1819.827
#plot(gamm_model, pages = 1) # Visualize smooth terms
#gam.check(gamm_model)
#appraise(gamm_model, method = 'simulate')      # Check model diagnostics
##R-sq.(adj) =  0.619   Deviance explained = 56.1%
#
##Check the type of effect:
#draw(gamm_model, scales = 'fixed')
#plot(gamm_model, pages = 1, scheme = 2, shade = TRUE)
#draw(gamm_model, scales = 'free')
#plot(gamm_model, pages = 1, scheme = 2, shade = TRUE)




# 3.2. Build the model manually, pay attention to details-----------------------
# 3.2.1. Check correlation------------------------------------------------------
# Fit a GLM without random effects for VIF computation
glm_model <- glm(
  Alive_Dead ~ MinsExposedtoAir + depth + ln_bodymass + at_celsius + habitat + Trawl_duration + ln_Aeco, # + repro has to be removed
  family = binomial(link = "logit"),
  data = data
)

# Compute VIF
vif_values <- car::vif(glm_model)
print(vif_values) # VIF > 5 indicates collinearity

# explore relationships:
library(GGally)
ggpairs(data %>% dplyr::select(habitat, repro, depth, at_celsius, ln_bodymass))


# Fit the full model:
gamm_model <- gamm4::gamm4(
  Alive_Dead ~ 
    s(MinsExposedtoAir) + s(depth) +  s(ln_bodymass) + s(at_celsius) +  habitat + s(Trawl_duration) +  s(ln_Aeco), #+ repro
  random = ~ (1|Metier/Vessel) + (1|Species), 
  family = "binomial",  data, REML=TRUE)
beep()


# Compute
#For GAMM models, collinearity is called concurvity (nonlinear dependence between smooth terms). Use:
concurvity(gamm_model$gam, full = TRUE) # High concurvity (>0.8) suggests predictors share too much information.



# 3.2.2. Fit the model----------------------------------------------------------
# potential interactions should be included as: s(sdepth, sTrawl_duration) if same scale or te(depth, Trawl_duration) if different scales

#Fit the model using backwards and forwards:

#Notes on model selection:
# Aeco + habitat + depth (0.215, 1798.118), trawl (0.0873, 1805.607)
# Aeco + trawl (0.134, 1805.808), depth (0.243,1799.107), ->##habitat (0.11, 1796.275)##<-
# trawl, depth, at, mins, bodymass, habitat (0.138, 1799.369), #Aeco (0.145,1797.095)#
# All (0.215,1798.39)

#Fit the model using forwards:
t <- Sys.time() 
gamm_model <- gamm4::gamm4(
  Alive_Dead ~ 
    s(sln_bodymass) +
    s(sMinsExposedtoAir) +
    s(sat_celsius) +
    s(sdepth) +
    s(sTrawl_duration), #+
    #habitat +
    #s(sln_Aeco),
    random = ~ (1|Metier/Vessel) + (1|Species),  # Nested random effects 
    #random = ~ (1 | Species) + (1 | Vessel),  #metier and species as random effect
    family = binomial(link = "logit"),  # Corrected family specification
    data = data,  # Explicitly define dataset
    REML = TRUE)
beep()
Sys.time() - t 


summary(gamm_model$gam) 
summary(gamm_model$mer) 
AIC(gamm_model$mer) 
plot(gamm_model$gam, pages = 1) 


# Fit the full model using gam():
gamm_model <- gam(    #1782.687
  Alive_Dead ~ 
    s(ln_bodymass, k=3) + 
    s(at_celsius, k=3) +
    s(MinsExposedtoAir, k=3) + 
    s(Trawl_duration, k=3) +
    s(depth, k=3) + 
    #factor(habitat) +  
    #s(ln_Aeco, k=3) +
    s(Species, bs = "re") +  # Random effect for species
    s(Vessel, bs = "re", by = Metier),
  family = binomial(link = "logit"),  
  data = data,
  method = "REML")

gamm_model <- gam(   
  Alive_Dead ~ 
    s(ln_bodymass, k=3) + 
    s(at_celsius, k=3) +
    s(MinsExposedtoAir, k=3) + 
    s(Trawl_duration, k=3) +
    #s(depth, k=3) + 
    #te(MinsExposedtoAir, Trawl_duration, k = c(3, 3)) +  # interaction term
    te(ln_bodymass, at_celsius, k = c(3, 3)) +  # interaction term
    s(Species, bs = "re") +  # Random effect for species
    #s(Vessel, bs = "re"),
    s(Vessel, bs = "re", by = Metier),
  family = binomial(link = "logit"),  
  data = data,
  method = "REML")

summary(gamm_model)         
AIC(gamm_model)       #1783.253 #1764.692
draw(gamm_model, scales = 'free')
#plot(gamm_model, pages = 1) 
#gam.check(gamm_model)
appraise(gamm_model, method = 'simulate')      

# Variance explained
summary(gamm_model)$dev.expl #55.32%

# Save the model:
path <- paste0(output_data, "/model/AVM")
if (!dir.exists(path)) dir.create(path, recursive = TRUE)
file_name <- paste0(path, "/AVM_GAMM_INTER1.rds")
#file_name <- paste0(path, "/AVM_GAMM_interaction.rds")
saveRDS(gamm_model, file = file_name)


# check results:
# Get predicted probabilities
#data$predicted_prob <- predict(gamm_model$gam, type = "response")
#data$predicted_prob <- predict(gamm_model, type = "response")
# Plot predicted vs. actual
#ggplot(data, aes(x = predicted_prob, y = Alive_Dead)) +
#  geom_jitter(alpha = 0.2) +  # Adds jitter for visibility
#  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
#  labs(x = "Predicted Probability", y = "Observed Alive_Dead", title = "Predicted vs. Observed")

# Check the variance explained by the random factors:
gamm_fixed <- gam(   # 18.3 %
  Alive_Dead ~ 
    s(ln_bodymass, k=3) + 
    s(at_celsius, k=3) +
    s(MinsExposedtoAir, k=3) + 
    s(Trawl_duration, k=3) +
    #s(depth, k=3) + 
    #te(MinsExposedtoAir, Trawl_duration, k = c(3, 3)) +  # interaction term
    te(ln_bodymass, at_celsius, k = c(3, 3)),
  family = binomial(link = "logit"),  
  data = data,
  method = "REML")

fix <- summary(gamm_fixed)$dev.expl     # model without random effects
all <- summary(gamm_model)$dev.expl     # model with random effects
random <- all-fix
print(paste("Percentage of variance explained by random factors:", round(random * 100/all, 1), "%"))
# Percentage of variance explained by random factors: 21.5% 


# 4. Plot the factor effect-----------------------------------------------------
summary(gamm_model)

#Export
outdir <- paste0('C:/Users/david/OneDrive/Escritorio/PRM_paper/Figures/GAMM4/AVM')
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
setwd(outdir)
jpeg(file = "AVM_GAMM_final_interaction.jpeg", 
     width = 20, height = 20, units = "cm", res = 600)

#par(mfrow = c(2, 3), pty = "s")  # Ensures a 2x2 grid & square aspect ratio
#plot(gamm_model,select=1,shade=TRUE,ylab='s(ln_bodymass)',xlab='MASS', col = "steelblue", lwd = 2, ylim =  NULL)
#plot(gamm_model,select=2,shade=TRUE,ylab='s(at_celsius)',xlab='AT', col = "steelblue", lwd = 2, ylim = NULL)
#plot(gamm_model,select=3,shade=TRUE,ylab='s(MinsExposedtoAir)',xlab='DECKTIME', col = "steelblue", lwd = 2, ylim = NULL)
#plot(gamm_model,select=4,shade=TRUE,ylab='s(Trawl_duration)',xlab='Trawl duration', col = "steelblue", lwd = 2, ylim = NULL)
#plot(gamm_model,select=5,shade=TRUE,ylab='s(depth)',xlab='DEPTH', col = "steelblue", lwd = 2, ylim = NULL)
#plot(gamm_model,select=6,shade=TRUE,ylab='s(ln_Aeco)',xlab='Aeco', col = "steelblue", lwd = 2)
#plot(gamm_model,select=7,shade=FALSE,all.terms=TRUE,ylab='',xlab='habitat')

b <- getViz(gamm_model)
# print() only needed because we want to plot on a single page
print(plot(b), pages = 1)

dev.off()

# Check interactions:
#par(mfrow = c(1, 1))
#vis.gam(gamm_model$gam, view=c('Trawl_duration','depth'), theta=150, color = 'heat')
#vis.gam(gamm_model, view=c('Trawl_duration','depth'), theta=150, color = 'heat')

#library(sjPlot)
#plot_model(gamm_model,type='pred',terms=c('ln_bodymass','habitat'),
#           axis.title=c('depth',''),
#           title='',show.data = TRUE)
#library(modEvA)
#Dsquared(gamm_model)





# 5. Coefficient random effect for each species --------------------------------
# Extract species-specific random effects from the GAMM


#gam():
# Extract unique Species levels
species_levels <- unique(data$Species)
#vessel_levels <- unique(data$Vessel)
#metier_levels <- unique(data$Metier)

# Create a new dataset with only unique species for prediction
new_data <- data.frame(
  Species = species_levels,   # Unique species
  Vessel = data$Vessel[2],
  Metier = data$Metier[3],
  MinsExposedtoAir = median(data$MinsExposedtoAir, na.rm = TRUE), # Use median
  depth = median(data$depth, na.rm = TRUE),
  ln_bodymass = median(data$ln_bodymass, na.rm = TRUE),
  at_celsius = median(data$at_celsius, na.rm = TRUE),
  habitat = factor(levels(data$habitat)[1], levels = levels(data$habitat)), # First level of habitat
  Trawl_duration = median(data$Trawl_duration, na.rm = TRUE),
  ln_Aeco = median(data$ln_Aeco, na.rm = TRUE)
)
# Predict random effects for species (only Species smooth term)
ranef_species <- predict(gamm_model, newdata = new_data, type = "terms", terms = "s(Species)", se.fit = TRUE)

ranef_df <- data.frame(
  Species = species_levels,      # Extracted species names
  Intercept = ranef_species$fit, # Extracted random effect values
  SE = ranef_species$se.fit      # Standard error
  )


#gamm4():
# Extract random effects for species
#ranef_species <- ranef(gamm_model$mer, condVar = TRUE)$Species  # Extract species-level effects

# Convert to a dataframe
#ranef_df <- data.frame(
#  Species = rownames(ranef_species),  # Extract species names
#  Intercept = ranef_species[,1]  # Extract random intercept values
  #, SE = sqrt(attr(ranef(gamm_model$mer, condVar = TRUE)$Species, "postVar")[1, , ])  # Extract standard errors
#)
head(ranef_df)

# Add depth and body mass information
depthrange <- c(
  "Cmonstrosa" = 502, 
  "Gmelastomus" = 535,
  "Scanicula" = 241, 
  "Cuyato" = 446,
  "Ocentrina" = 150, 
  "Dlicha" = 591,
  "Espinax" = 557, 
  "Hgriseus" = 535,
  "Doxyrinchus" = 329, 
  "Rclavata" = 179,
  "Rasterias" = 66, 
  "Rpolystigma" = 117,
  "Pviolacea" = 68, 
  "Dpastinaca" = 133,
  "Maquila" = 73, 
  "Abovinus" = 73,
  "Tmarmorata" = 104
)

bodymass <- c(
  "Abovinus" = 9.078864009,
  "Cmonstrosa" = 5.147494477,
  "Cuyato" = 7.647786045,
  "Dlicha" = 5.549076085,
  "Doxyrinchus" = 5.252273428,
  "Dpastinaca" = 9.230241034,
  "Espinax" = 4.412798293,
  "Gmelastomus" = 5.247024072,
  "Hgriseus" = 11.1889113,
  "Maquila" = 7.92551898,
  "Ocentrina" = 8.049107721,
  "Pviolacea" = 9.239122173,
  "Rasterias" = 6.502790046,
  "Rclavata" = 6.612041035,
  "Rpolystigma" = 5.468060141,
  "Scanicula" = 5.081404365,
  "Tmarmorata" = 5.463831805
)

ranef_df$Depth <- depthrange[as.character(ranef_df$Species)]
ranef_df$bodymass <- bodymass[as.character(ranef_df$Species)]
print(ranef_df)

# Set order
desired_order <- c(
  "Cmonstrosa", 
  "Gmelastomus", 
  "Scanicula", 
  "Cuyato", 
  "Ocentrina", 
  "Dlicha", 
  "Espinax", 
  "Hgriseus", 
  "Doxyrinchus", 
  "Rclavata", 
  "Rasterias", 
  "Rpolystigma", 
  "Pviolacea", 
  "Dpastinaca", 
  "Maquila", 
  "Abovinus", 
  "Tmarmorata"
)

# Reorder the factor levels in reverse order
ranef_df$Species <- factor(ranef_df$Species, levels = rev(desired_order))

# Ensure the order is correct
ranef_df <- ranef_df[order(match(ranef_df$Species, rev(desired_order))), ]
print(ranef_df)

# Ensure Species is a factor in the desired order
#ranef_df$Species <- factor(ranef_df$Species, levels = rev(names(depthrange)))

# Normalize bodymass values for point size scaling
#ranef_df$PointSize <- scales::rescale(ranef_df$bodymass, to = c(4, 11))

# Define color palette
color_palette_bathy <- colorRampPalette(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46'))(100)

# Plot with body mass as point size
colnames(ranef_df)
colnames(ranef_df) <- c("Species", "Intercept", "SE",  "Depth", "bodymass") #
head(ranef_df)

p_sp <- ggplot(ranef_df, aes(x = Species, y = Intercept, fill = Depth, size =  bodymass)) + 
  geom_hline(yintercept = 0, color = "steelblue", size = 1.2) +
  geom_point(shape = 21, color = "black", stroke = 0.5) +  # Dark contour, colored fill
  #geom_text(aes(label = Species), hjust = -0.2, size = 3.5) +  # Adjust label position and size
    labs(
    title = "Random Effects for Species in GAMM",
    x = "Species",
    y = "Deviation from Overall Health Condition",
    fill = "Depth Range",
    size = "Bodymass"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    aspect.ratio = 1 
  ) +
  scale_fill_gradientn(colors = color_palette_bathy) +
  scale_size_continuous(range = c(2, 11)) +  # Increase the minimum and maximum point sizes
  coord_flip()

# Print plot
print(p_sp)

#  Save the plot
path <- paste0(output_data, "/Figures/GAMM/AVM")
if (!dir.exists(path)) dir.create(path, recursive = TRUE)
p_png <- paste0(path, "/AVM_spIntercept_final.png")
ggsave(p_png, p_sp, width = 20, height = 20, units = "cm", dpi = 600)


# 5.1.  Coefficient random effect for each Metier --------------------------------
# Extract species-specific random effects from the GAMM

# Extract estimated smooth terms from the GAM model
smooths_all <- smooth_estimates(gamm_model)
unique(smooths_all$.smooth)

# Subset the smooth estimates for the Vessel random effect in Metier1 only
vessel_metier1_smooth <- smooths_all %>%
  filter(.smooth == "s(Vessel):Metier1")

mean(vessel_metier1_smooth$.estimate)

# Plot the random effects for vessels (Metier1 only)
ggplot(vessel_metier1_smooth, aes(x = Vessel, y = .estimate)) +
  # Add a horizontal line at 0 for reference (neutral effect)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Plot the vessel random effect as points (log-odds scale)
  geom_point(shape = 21, size = 4, fill = "darkorange", color = "black") +
  # Add error bars showing ±1 SE around the estimate
  geom_errorbar(aes(ymin = .estimate - .se, ymax = .estimate + .se), width = 0.2) +
  # Add vessel names as labels next to the points
  geom_text(aes(label = Vessel), hjust = -0.2, size = 3.5) +
  # Flip the axes to make it horizontal (vessels on y-axis)
  coord_flip() +
  # Add title and axis labels
  labs(
    title = "Random Effects for Vessels in Metier1",
    y = "Effect (log-odds scale)",
    x = "Vessel"
  ) +
  theme_minimal() +
  # Optional: Improve axis text readability
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )




vessel_smooth <- smooths_all %>%
  filter(.smooth == "s(Vessel)")
# Plot the random effects for vessels (Metier1 only)
ggplot(vessel_smooth, aes(x = Vessel, y = .estimate)) +
  # Add a horizontal line at 0 for reference (neutral effect)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Plot the vessel random effect as points (log-odds scale)
  geom_point(shape = 21, size = 4, fill = "darkorange", color = "black") +
  # Add error bars showing ±1 SE around the estimate
  geom_errorbar(aes(ymin = .estimate - .se, ymax = .estimate + .se), width = 0.2) +
  # Add vessel names as labels next to the points
  geom_text(aes(label = Vessel), hjust = -0.2, size = 3.5) +
  # Flip the axes to make it horizontal (vessels on y-axis)
  coord_flip() +
  # Add title and axis labels
  labs(
    title = "Random Effects for Vessels in Metier1",
    y = "Effect (log-odds scale)",
    x = "Vessel"
  ) +
  theme_minimal() +
  # Optional: Improve axis text readability
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )






# 6. Calculate model AUC in cross-validation------------------------------------

set.seed(123)  # Ensure reproducibility

#6.1. Create folds--------------------------------------------------------------
# Function to assign folds while ensuring all species are in all folds
#assign_folds <- function(species_data) {
#  num_obs <- nrow(species_data)
#  
#  if (num_obs < 5) {
#    # Replicate rows with replacement until the species has at least 5 rows
#    species_data <- species_data %>%
#      dplyr::slice(sample(1:num_obs, size = 5, replace = TRUE))
#    
#    # Assign folds sequentially to ensure at least one instance in each fold
#    species_data$fold <- rep(1:5, length.out = nrow(species_data))
#    
#  } else {
#    # Assign folds proportionally using stratified 5-fold cross-validation
#    species_data$fold <- sample(rep(1:5, length.out = num_obs))  # Ensure balanced assignment
#  }
#  
#  return(species_data)
#}
#
## Apply the function to each species group
#data <- data %>%
#  group_by(Species) %>%
#  group_modify(~ assign_folds(.x)) %>%
#  ungroup()
#
## Check fold distribution
#species_fold_distribution <- data %>%
#  group_by(Species, fold) %>%
#  summarise(count = n(), .groups = "drop")
#
#print(species_fold_distribution)  # Verify fold assignment




# 6.2. Fit the model in a cross-validation process------------------------------
# Create an empty list to store AUC results per species
#auc_results <- list()
#test_predictions <- list()  # Store actual test data for ROC
#
#  # Perform 5-fold cross-validation
#  for (f in 1:5) {
#    #f=2
#    # Split data into training and testing sets
#    train_data <- data %>% filter(fold != f)
#    test_data <- data %>% filter(fold == f)
#    
#    # Fit GAMM model on training data
#    # gamm4:
#    # gamm_model <- gamm4::gamm4(
#    #   Alive_Dead ~ 
#    #     s(sMinsExposedtoAir) + 
#    #     s(sdepth) +  
#    #     s(sln_bodymass) + 
#    #     s(sat_celsius) +  
#    #     habitat +
#    #    s(sTrawl_duration), # +  
#    #   #s(sln_Aeco),
#    #   random = ~ (1 | Species),  # Corrected random effect formula
#    #   family = binomial(link = "logit"),  # Corrected family specification
#    #   data = train_data,  # Explicitly define dataset
#    #   REML = TRUE
#    # )
#    # beep()
#    
#    #gam()
#    gamm_model <- gam(
#      Alive_Dead ~ 
#        s(ln_bodymass, k=3) + 
#        s(at_celsius, k=3) +
#        s(MinsExposedtoAir, k=3) + 
#        s(Trawl_duration, k=3) +
#        s(depth, k=3) + 
#        #factor(habitat) +  
#        #s(ln_Aeco, k=3) +
#        s(Species, bs = "re") +  # Random effect for species
#        s(Vessel, bs = "re", by = Metier),
#      family = binomial(link = "logit"),  
#      data = train_data,
#      method = "REML")
#    
#    
#    #gamm4():
#    #summary(gamm_model$gam)  
#    #AIC(gamm_model$mer) 
#    #par(mfrow = c(2, 3))  
#    #plot(gamm_model$gam, residuals = TRUE, shade = TRUE, seWithMean = TRUE)
#    #par(mfrow = c(1,1))  # Reset plot layout
#    
#    #gam():
#    #summary(gamm_model)
#    #AIC(gamm_model)             # 1819.563 #1819.827
#    #plot(gamm_model, pages = 1) 
#    
#    
#    # Make predictions on the test data
#    #gam()
#    test_data$predicted_prob <- predict(gamm_model, newdata = test_data, type = "response")
#    #gamm4()
#    #test_data$predicted_prob <- predict(gamm_model$gam, type = "response", test_data)
#    
#    # Prepare data for AUC calculation
#    observed <- test_data %>% dplyr::select(Alive_Dead)
#    predicted <- test_data %>%
#      dplyr::mutate(predicted_prob = as.numeric(predicted_prob)) %>%  # Convert to numeric
#      dplyr::select(predicted_prob)
#    # Rename prediction column explicitly (required by presence.absence.accuracy)
#    colnames(predicted) <- "Model_1"
#    
#    #make dataframe:
#    auc_data <- bind_cols(observed, predicted)
#    # Ensure 'Alive_Dead' is first and prediction column is second
#    auc_data <- auc_data %>%
#      dplyr::select(Alive_Dead, Model_1)  # Reorder columns
#    
#    # Convert to a standard dataframe (ensures compatibility with base R functions)
#    auc_data <- as.data.frame(auc_data)
#    
#    # Add an ID column (if `presence.absence.accuracy()` expects an identifier)
#    auc_data$ID <- seq_len(nrow(auc_data))
#    
#    # Ensure 'Alive_Dead' is first, 'Model_1' is second, and an identifier is included
#    auc_data <- auc_data %>%
#      dplyr::select(ID, Alive_Dead, Model_1)
#    summary(auc_data)
#    
#    # Compute AUC, ensuring correct column selection
#    auc_result <- presence.absence.accuracy(auc_data)
#    
#    # Convert AUC results to a data frame
#    auc_result <- as.data.frame(auc_result) %>%
#      dplyr::select(AUC, AUC.sd) %>%
#      dplyr::mutate(test_iteration = f)
#    print(auc_result)
#    
#    # Store AUC result for this fold
#    auc_results[[f]] <- auc_result
#  }
#  
#  # Combine AUC results across folds
#  auc_binded <- do.call(bind_rows, auc_results)
#  
#  # Compute mean and SD of AUC per species
#  auc_summary <- auc_binded %>%
#    dplyr::summarize(
#      mean_AUC = round(mean(AUC), 5),
#      sd_AUC = round(sd(AUC), 5)
#    )
#
## Print AUC summary
#print(auc_summary)
#beep()

