# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
# 5.3. GLMM for Health Condition
#-------------------------------------------------------------------------------

# 1. Load AVM data--------------------------------------------------------------
data <- read.csv("temp/mod2AVM_allEnviro - copia.csv", sep = ";") #mod2

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
#add quadriatic term:
data$qdepth<-(data$depth)^2
data$sqdepth<-scale(data$qdepth)

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




#3. GLMM to Fit model (Health) ---------------------------------------------------------

# Decide model distribution: in my case binomial
str(data_health)
hist(data_health$Health_prob)
shapiro.test(data_health$Health_prob)
summary(data_health$Health_prob)
#scale
data_health$Health_prob_scaled <- data_health$Health_prob / 100


# HEALTH CONDITION:
glmm <- lmer(Health_prob_scaled ~  sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius + sln_Aeco + (1 | Species), #sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius
             data = data_health)
vif(glmm)

#sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius + sln_Aeco + repro 
glmm <- lmer(Health_prob_scaled ~ sln_bodymass + sdepth + sMinsExposedtoAir + sat_celsius + (1 | Species), 
             data = data_health)
display(glmm) #AIC -10565.6 (+ habitat + sln_Aeco + habitat)



#sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius + sln_Aeco + repro 
glmm <- lmer(Health_prob_scaled ~ sdepth + sMinsExposedtoAir + sat_celsius +  (1 | Species), 
             data = data_health)
display(glmm) #AIC -10571 (sln_Aeco + habitat + sln_bodymass + )


confint(glmm)
anova(glmm)
ranef(glmm)
summary(glmm)

# Check random effect:
glmm_no_random <- lm(Health_prob_scaled ~ sln_bodymass + sdepth + sMinsExposedtoAir + sat_celsius, data = data_health) #sln_bodymass + sdepth + habitat
lrtest(glmm_no_random, glmm) # Pr(>Chisq) = < 2.2e-16 *** -> important influence of the random effect Species

# Residual vs. Fitted plot
plot(fitted(glmm), resid(glmm), 
     main="Residuals vs Fitted", 
     xlab="Fitted values", ylab="Residuals")
abline(h = 0, col = "red")

#If the spread of the residuals increases or decreases systematically with fitted values, it suggests heteroscedasticity (non-constant variance).
#If the residuals are randomly scattered around zero, this suggests that the assumption of homoscedasticity is likely met.

# Histogram of residuals
hist(resid(glmm), 
     main="Histogram of Residuals", 
     xlab="Residuals", 
     breaks=30)

#In a good model, the histogram of residuals should resemble a normal distribution, with a bell-shaped curve centered around zero.

# Q-Q plot for residuals
qqnorm(resid(glmm))
qqline(resid(glmm), col = "red")
#If the residuals are normally distributed, the points should lie close to the straight line.
#Large deviations from the line indicate departures from normality (e.g., heavy tails, skewness).

# shapiro on residuals
shapiro.test(resid(glmm))
#If the p-value is low (usually below 0.05), it suggests that the residuals are not normally distributed. However, in large datasets, normality tests can be too sensitive, so graphical methods (like the histogram and Q-Q plot) are generally more informative.

# Plot residuals against predictors to check linearity
plot(data_health$sln_bodymass, resid(glmm), 
     main="Residuals vs sln_bodymass", 
     xlab="sln_bodymass", ylab="Residuals")
abline(h = 0, col = "red")
#If any data points have Cook's distance larger than the threshold, it suggests that they are influential and may need further investigation.

# Calculate Cook's distance
cooksd <- cooks.distance(glmm)
plot(cooksd, main="Cook's Distance")
abline(h = 4 / length(cooksd), col = "red")  # Threshold for influential points

# ACF plot
acf(resid(glmm))
#In a well-behaved model, you want the residuals to not show significant autocorrelation (i.e., no pattern in the ACF plot).

# R-squared
#method1
r.squaredGLMM(glmm)
#method2
predictions <- predict(glmm)
cor(predictions,data_health$Health_prob_scaled)
# Extract variance components from the model
var_fixed <- sum((fixef(glmm))^2)  # Variance explained by fixed effects
var_random <- as.numeric(VarCorr(glmm)$Species[1])  # Variance explained by random effects
# Total variance is the sum of fixed and random variances
total_variance <- var_fixed + var_random
# Proportion of variance explained by fixed and random effects
proportion_fixed <- var_fixed / total_variance
proportion_random <- var_random / total_variance
print(proportion_fixed)
print(proportion_random)



#5. Plot partial effects (Health) ----------------------------------------------

# Plot partial effects:
# 5.1. TIME ON DECK-------------------------------------------------------------
lm_sp <- lmer(Health_prob_scaled ~  depth + MinsExposedtoAir + at_celsius + (1 | Species), #ln_bodymass + 
              data = data_health)

partial_effect <- ggpredict(lm_sp, terms = "MinsExposedtoAir [all]")

#unscale variable x:
hist(data$MinsExposedtoAir)
# Retrieve the mean and standard deviation
#mean_MinsExposedtoAir <- mean(data$MinsExposedtoAir, na.rm = TRUE)
#sd_MinsExposedtoAir <- sd(data$MinsExposedtoAir, na.rm = TRUE)

# Create the plot
# Modify the plot to display the unscaled x values
p_deck <- ggplot(partial_effect, aes(x = x, y = predicted))  +
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect of Minutes Exposed to Air on Health Condition",
    x = "Minutes Exposed to Air",  # Update the label to reflect unscaled units
    y = "Predicted Health Condition"
  ) +
  #ylim(0, 1) +  # Set the Y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p_deck)

# export plot
p_png <- paste0(output_data, "/Health_DeckTime.png")
ggsave(p_png, p_deck, width=12, height=12, units="cm", dpi=300)



# 5.2. DEPTH--------------------------------------------------------------------
partial_effect <- ggpredict(lm_sp, terms = "depth [all]")

#unscale variable x:
hist(data$depth)

# Create the plot
# Modify the plot to display the unscaled x values
p_depth <- ggplot(partial_effect, aes(x = x, y = predicted)) +  # Unscale x
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect of Minutes Exposed to Air on Health Condition",
    x = "Depth",  # Update the label to reflect unscaled units
    y = "Predicted Health Condition"
  ) +
  #ylim(0, 1) +  # Set the Y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p_depth)
#This plot shows the partial effect of depth on the predicted health conditio. The main goal of the plot is to isolate and visualize how depth alone influences the overall health across species, while keeping the time exposed to air (MinsExposedtoAir) constant at its median value.
#Line (geom_line()): shows how the predicted health condition changes as depth changes, holding the other variables in the model constant. A rising line would indicate that health improves with depth, while a declining line would suggest that health decreases as depth increases.
#Confidence Interval (geom_ribbon()): The shaded area around the line represents the confidence interval for the predictions. The ribbon spans from the lower bound (conf.low) to the upper bound (conf.high) of the 95% confidence interval for the predicted health condition at each depth. This indicates the uncertainty around the model's predictions. A wider ribbon indicates greater uncertainty.
#it represents an averaged or overall effect of depth on health, assuming an average species intercept (random effect).

# export plot
p_png <- paste0(output_data, "/Health_Depth.png")
ggsave(p_png, p_depth, width=12, height=12, units="cm", dpi=300)


# 5.3. BodyMass--------------------------------------------------------------------
partial_effect <- ggpredict(lm_sp, terms = "ln_bodymass [all]")

#unscale variable x:
hist(data$ln_bodymass)

# Create the plot
# Modify the plot to display the unscaled x values
p_bodymass <- ggplot(partial_effect, aes(x = x, y = predicted)) +  # Unscale x
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect ",
    x = "Body mass",  # Update the label to reflect unscaled units
    y = "Predicted Health Condition"
  ) +
  #ylim(0, 1) +  # Set the Y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p_bodymass)
#This plot shows the partial effect of depth on the predicted health conditio. The main goal of the plot is to isolate and visualize how depth alone influences the overall health across species, while keeping the time exposed to air (MinsExposedtoAir) constant at its median value.
#Line (geom_line()): shows how the predicted health condition changes as depth changes, holding the other variables in the model constant. A rising line would indicate that health improves with depth, while a declining line would suggest that health decreases as depth increases.
#Confidence Interval (geom_ribbon()): The shaded area around the line represents the confidence interval for the predictions. The ribbon spans from the lower bound (conf.low) to the upper bound (conf.high) of the 95% confidence interval for the predicted health condition at each depth. This indicates the uncertainty around the model's predictions. A wider ribbon indicates greater uncertainty.
#it represents an averaged or overall effect of depth on health, assuming an average species intercept (random effect).

# export plot
p_png <- paste0(output_data, "/Health_BodyMass.png")
ggsave(p_png, p_depth, width=12, height=12, units="cm", dpi=300)

# 5.4. BodyMass--------------------------------------------------------------------
partial_effect <- ggpredict(lm_sp, terms = "at_celsius [all]")

#unscale variable x:
hist(data$at_celsius)

# Create the plot
# Modify the plot to display the unscaled x values
p_at_celsius <- ggplot(partial_effect, aes(x = x, y = predicted)) +  # Unscale x
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect ",
    x = "Atmospheric temperature",  # Update the label to reflect unscaled units
    y = "Predicted Health Condition"
  ) +
  #ylim(0, 1) +  # Set the Y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p_at_celsius)
#This plot shows the partial effect of depth on the predicted health conditio. The main goal of the plot is to isolate and visualize how depth alone influences the overall health across species, while keeping the time exposed to air (MinsExposedtoAir) constant at its median value.
#Line (geom_line()): shows how the predicted health condition changes as depth changes, holding the other variables in the model constant. A rising line would indicate that health improves with depth, while a declining line would suggest that health decreases as depth increases.
#Confidence Interval (geom_ribbon()): The shaded area around the line represents the confidence interval for the predictions. The ribbon spans from the lower bound (conf.low) to the upper bound (conf.high) of the 95% confidence interval for the predicted health condition at each depth. This indicates the uncertainty around the model's predictions. A wider ribbon indicates greater uncertainty.
#it represents an averaged or overall effect of depth on health, assuming an average species intercept (random effect).

# export plot
p_png <- paste0(output_data, "/Health_BodyMass.png")
ggsave(p_png, p_depth, width=12, height=12, units="cm", dpi=300)


# 5.3. Partial effects for each species (Deck time)------------------------------

# Generate predicted probabilities for a range of values of sdepth and sMinsExposedtoAir considering Species influence
# Fit the GLMM 
lm_sp <- lmer(Health_prob_scaled ~  MinsExposedtoAir  + depth + (1 | Species), #sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius
              data = data_health)
# 5.3.1. Depth effect----------------------------------------------------------

# Create a new prediction grid for depth with the median ln_bodymass and all levels of numericSp
pred_data_depth <- expand.grid(
  depth = seq(min(data$depth), max(data$depth), length.out = 100),
  MinsExposedtoAir = median(data$MinsExposedtoAir, na.rm = TRUE),
  Species = unique(data$Species[!data$Species %in% c("Cmonstrosa", "Doxyrinchus")])
)

# Unscale the sdepth variable in pred_data_depth
#mean_depth <- mean(data$depth, na.rm = TRUE)
#sd_depth <- sd(data$depth, na.rm = TRUE)

# Generate predictions
#pred_data_depth$sdepth_unscaled <- pred_data_depth$sdepth * sd_depth + mean_depth

pred_data_depth$predicted_prob <- predict(lm_sp, newdata = pred_data_depth, type = "response")

# Plot the results
library(viridis)

p <- ggplot(pred_data_depth, aes(x = depth, y = predicted_prob, color = as.factor(Species))) +
  geom_line() +
  labs(title = "Predicted Probability of Alive by Depth for Each Species",
       x = "Depth (unscaled)",
       y = "Predicted Probability of Alive",
       color = "Species (numericSp)") +
  scale_color_viridis(discrete = TRUE) +  # Use viridis color scale
  #scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "yellow", "grey")) +  # Define your own colors
  theme_minimal() +
  theme(legend.position = "right")

print(p)
#This plot shows the probability of having certain health condition based on depth for or each species, while keeping the time exposed to air (MinsExposedtoAir) constant at its median value.


# export plot
p_png <- paste0(output_data, "/Health_Depth_Sp.png")
ggsave(p_png, p, width=12, height=12, units="cm", dpi=300)





# 5.3.2. Combine general and sp-specific (Depth effect)-------------------------
# First plot with partial effects
p1 <- ggplot(partial_effect, aes(x = (x * sd_depth) + mean_depth, y = predicted)) +  # Unscale x
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect of Minutes Exposed to Air on Health Condition",
    x = "Depth",  # Update the label to reflect unscaled units
    y = "Predicted Health Condition"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

# Add second plot with predicted probabilities for each species (with thinner dashed lines)
p2 <- p1 + 
  geom_line(data = pred_data_depth, aes(x = sdepth_unscaled, y = predicted_prob, color = as.factor(Species)), 
            size = 0.4,       # Reduce line width (default is 1)
            linetype = "dashed") +  # Make the line dashed
  scale_color_viridis(discrete = TRUE) +  # Use viridis color scale for species
  labs(color = "Species (numericSp)")  # Adjust legend title if needed

# Print combined plot
print(p2)

# export plot
p_png <- paste0(output_data, "/Health_Depth_Sp_combined.png")
ggsave(p_png, p2, width=16, height=12, units="cm", dpi=300)


## if you want to represent the range for the minimum and maximum values of sMinsExposedtoAir:
## Create a new prediction grid for depth with min, median, and max sMinsExposedtoAir
#pred_data_depth <- expand.grid(
#  sdepth = seq(min(data$sdepth), max(data$sdepth), length.out = 100),
#  sMinsExposedtoAir = c(min(data$sMinsExposedtoAir, na.rm = TRUE), 
#                        median(data$sMinsExposedtoAir, na.rm = TRUE), 
#                        max(data$sMinsExposedtoAir, na.rm = TRUE)),
#  Species = unique(data$Species[!data$Species %in% c("Cmonstrosa", "Doxyrinchus")])
#)
#
## Generate predictions
#pred_data_depth$predicted_prob <- predict(lm_sp, newdata = pred_data_depth, type = "response")
#
## Separate data for min, median, and max sMinsExposedtoAir
#pred_min <- pred_data_depth[pred_data_depth$sMinsExposedtoAir == min(data$sMinsExposedtoAir, na.rm = TRUE), ]
#pred_median <- pred_data_depth[pred_data_depth$sMinsExposedtoAir == median(data$sMinsExposedtoAir, na.rm = TRUE), ]
#pred_max <- pred_data_depth[pred_data_depth$sMinsExposedtoAir == max(data$sMinsExposedtoAir, na.rm = TRUE), ]
#
## Now combine the min, median, and max predictions into one data frame
#pred_data_combined <- merge(pred_min, pred_max, by = c("sdepth", "Species"), suffixes = c("_min", "_max"))
#pred_data_combined <- merge(pred_data_combined, pred_median, by = c("sdepth", "Species"))
#
## Plot the results
#ggplot(pred_data_combined, aes(x = sdepth)) +
#  # Add ribbon for min and max values of sMinsExposedtoAir
#  geom_ribbon(aes(ymin = predicted_prob_min, ymax = predicted_prob_max, fill = as.factor(Species)), alpha = 0.1) +
#  # Add line for the median value of sMinsExposedtoAir
#  geom_line(aes(y = predicted_prob, color = as.factor(Species)), size = 1) +
#  labs(title = "Predicted Probability of Alive by Depth for Each Species",
#       x = "Standardized Depth (sdepth)",
#       y = "Predicted Probability of Alive",
#       color = "Species (numericSp)",
#       fill = "Species (numericSp)") +
#  theme_minimal() +
#  theme(legend.position = "right")







# 5.3.3. Mins exposed effect-------------------------------------------------------
# Create a new prediction grid for depth with the median ln_bodymass and all levels of numericSp
pred_data_MinsExposedtoAir <- expand.grid(
  MinsExposedtoAir = seq(min(data$MinsExposedtoAir), max(data$MinsExposedtoAir), length.out = 100),
  depth = median(data$depth, na.rm = TRUE),
  Species = unique(data$Species[!data$Species %in% c("Cmonstrosa", "Doxyrinchus")])
)

## Unscale the sMinsExposedtoAir variable in pred_data_MinsExposedtoAir
#mean_MinsExposedtoAir <- mean(data$MinsExposedtoAir, na.rm = TRUE)
#sd_MinsExposedtoAir <- sd(data$MinsExposedtoAir, na.rm = TRUE)

# Generate predictions
#pred_data_sMinsExposedtoAir$sMinsExposedtoAir_unscaled <- pred_data_sMinsExposedtoAir$sMinsExposedtoAir * sd_MinsExposedtoAir + mean_MinsExposedtoAir
#pred_data_sMinsExposedtoAir$sMinsExposedtoAir_unscaled <- pmax(pred_data_sMinsExposedtoAir$sMinsExposedtoAir_unscaled, 0)

pred_data_MinsExposedtoAir$predicted_prob <- predict(lm_sp, newdata = pred_data_MinsExposedtoAir, type = "response")

# Plot the results
library(viridis)

p <- ggplot(pred_data_MinsExposedtoAir, aes(x = MinsExposedtoAir, y = predicted_prob, color = as.factor(Species))) +
  geom_line() +
  labs(title = "Predicted Probability of Alive by MinsExposedtoAir for Each Species",
       x = "MinsExposedtoAir (unscaled)",
       y = "Predicted Probability of Alive",
       color = "Species (numericSp)") +
  scale_color_viridis(discrete = TRUE) +  # Use viridis color scale
  #scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "yellow", "grey")) +  # Define your own colors
  theme_minimal() +
  theme(legend.position = "right")

print(p)

# export plot
p_png <- paste0(output_data, "/Health_MinsExposedtoAir_Sp.png")
ggsave(p_png, p, width=12, height=12, units="cm", dpi=300)

# 5.3.4. Combine general and sp-specific (MinsExposedtoAir effect)-------------------------
# First plot with partial effects
p1 <- ggplot(partial_effect, aes(x , y = predicted)) +  # Unscale x
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect of Minutes Exposed to Air on Health Condition",
    x = "MinsExposedtoAir",  # Update the label to reflect unscaled units
    y = "Predicted Health Condition"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

# Add second plot with predicted probabilities for each species (with thinner dashed lines)
p2 <- p1 + 
  geom_line(data = pred_data_MinsExposedtoAir, aes(x = MinsExposedtoAir, y = predicted_prob, color = as.factor(Species)), 
            size = 0.4,       # Reduce line width (default is 1)
            linetype = "dashed") +  # Make the line dashed
  scale_color_viridis(discrete = TRUE) +  # Use viridis color scale for species
  labs(color = "Species (numericSp)")  # Adjust legend title if needed

# Print combined plot
print(p2)

# export plot
p_png <- paste0(output_data, "/Health_MinsExposedtoAir_Sp_combined.png")
ggsave(p_png, p2, width=16, height=12, units="cm", dpi=300)



# 5.4. Coefficient random effect for each species -------------------------------
# Extract and plot random effects for Species
ranef_species <- ranef(glmm)$Species
ranef_df <- as.data.frame(ranef_species)
colnames(ranef_df) <- c("Intercept")
ranef_df$Species <- rownames(ranef_df)
head(ranef_df)

# Habitat mapping from species to depth range
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

# Add the depth values to ranef_df based on Species
ranef_df$Depth <- depthrange[ranef_df$Species]

# Ensure that the 'Species' variable is ordered as a factor in the desired order in ranef_df
ranef_df$Species <- factor(ranef_df$Species, levels = rev(c(
  "Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", 
  "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", 
  "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", 
  "Maquila", "Abovinus", "Tmarmorata"
)))

# Plotting the random effects for each species, colored by Depth
color_palette_bathy <- colorRampPalette(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46'))(100)

p_sp <- ggplot(ranef_df, aes(x = Species, y = Intercept, fill = Depth)) + 
  geom_hline(yintercept = 0, color = "steelblue", size = 1.2) +
  geom_point(size = 3, shape = 21, color = "black", stroke = 0.5) +  # Dark contour, colored fill
  labs(
    title = "Random Effects for Species by Depth Range",
    x = "Species",
    y = "Deviation from Overall Health Condition",
    fill = "Depth Range"  # Label for the fill color scale
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    aspect.ratio = 1 
  ) +
  scale_fill_gradientn(colors = color_palette_bathy) +  # Apply the custom color palette based on Depth
  coord_flip()   # Inverts the axes

#This random effects plot shows:
#Species Deviations: Points above or below zero indicate whether each species has a higher or lower health condition than the overall mean.
#Zero Line: Species at or near zero have health conditions close to the overall average.

print(p_sp)

# export plot
p_png <- paste0(output_data, "/Health_spIntercept.png")
ggsave(p_png, p_sp, width=16, height=12, units="cm", dpi=300)

