# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------
# 5.1. GLMM for AVM
#-------------------------------------------------------------------------------


# 1. Load AVM data--------------------------------------------------------------
data <- read.csv("temp/mod2AVM_allEnviro - copia.csv", sep = ";")

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
data$sAeco<-scale(data$Aeco)


# Respiration mode
barplot(table(data$ventilation), main="Ventilation Categories", col="skyblue")


# For fitting the health model, remove health NAs:
data_health <- data %>%
  filter(!is.na(Activity)) %>%
  filter(Alive_Dead == 1)  %>%
  mutate(Health_prob = as.numeric(gsub(",", ".", Health_prob)))
# Filter NAs:
data <- data %>% 
  filter(!is.na(at_celsius), 
         !is.na(sln_bodymass)) %>% 
  mutate(Alive_Dead = ifelse(Alive_Dead == 0, 1, 0))

# 3. Fit GLMM model (AVM) ------------------------------------------------------------
# Fit the GLMM with a binomial family for a binary response
lm <- glmer(Alive_Dead ~ sln_bodymass + sat_celsius + sMinsExposedtoAir + sdepth + habitat + sAeco + (1 | Species), #sln_bodymass + sdepth + sMinsExposedtoAir + sat_celsius + habitat + sAeco + repro
            data = data, 
            family = binomial(link = "logit"))
display(lm) #AIC 2189.1
vif(lm)
# repro removed for colinearity
#sln_bodymass + sdepth + sMinsExposedtoAir + sat_celsius + habitat + sAeco 
lm <- glmer(Alive_Dead ~ sln_bodymass + sat_celsius + sMinsExposedtoAir + sdepth + (1 | Species), 
            data = data, 
            family = binomial(link = "logit"))
display(lm) #AIC  2186.9


#plot(resid(lm))
#confint(lm)

# Check multicollinearity and significancy of the fixed factor:
# check model:
display(lm) 
anova(lm)
ranef(lm)
summary(lm)

# R-squared
library(MuMIn)
#method1
r.squaredGLMM(lm)
#method2
predictions <- predict(lm)
cor(predictions,data$Alive_Dead)
# Extract variance components from the model
var_fixed <- sum((fixef(lm))^2)  # Variance explained by fixed effects
var_random <- as.numeric(VarCorr(lm)$Species[1])  # Variance explained by random effects
# Total variance is the sum of fixed and random variances
total_variance <- var_fixed + var_random
# Proportion of variance explained by fixed and random effects
proportion_fixed <- var_fixed / total_variance
proportion_random <- var_random / total_variance
print(proportion_fixed)
print(proportion_random)

# Check random factor:
lm_no_random <- glm(Alive_Dead ~ sln_bodymass + sat_celsius + sMinsExposedtoAir + sdepth, 
                    data = data, 
                    family = binomial(link = "logit")) 
lrtest(lm_no_random, lm)
# Pr(>Chisq) = < 2.2e-16 *** -> important influence of the random effect Species

# Residual vs. Fitted plot
plot(fitted(lm), resid(lm), 
     main="Residuals vs Fitted", 
     xlab="Fitted values", ylab="Residuals")
abline(h = 0, col = "red")

#If the spread of the residuals increases or decreases systematically with fitted values, it suggests heteroscedasticity (non-constant variance).
#If the residuals are randomly scattered around zero, this suggests that the assumption of homoscedasticity is likely met.

# Histogram of residuals
hist(resid(lm), 
     main="Histogram of Residuals", 
     xlab="Residuals", 
     breaks=30)

#In a good model, the histogram of residuals should resemble a normal distribution, with a bell-shaped curve centered around zero.

# Q-Q plot for residuals
qqnorm(resid(lm))
qqline(resid(lm), col = "red")
#If the residuals are normally distributed, the points should lie close to the straight line.
#Large deviations from the line indicate departures from normality (e.g., heavy tails, skewness).

# shapiro on residuals
shapiro.test(resid(lm))
#If the p-value is low (usually below 0.05), it suggests that the residuals are not normally distributed. However, in large datasets, normality tests can be too sensitive, so graphical methods (like the histogram and Q-Q plot) are generally more informative.

# Plot residuals against predictors to check linearity
# Ensure you're extracting the residuals correctly
residuals <- resid(lm, type = "response")  # or use "pearson" depending on your preference
# Extract residuals for this simplified model
residuals_no_random <- resid(lm_no_random, type = "response")
fitted_values <- fitted(lm)
# Check the lengths of residuals and predictors
length(residuals)  # Should match the number of observations in your data
length(residuals_no_random)
length(data_health$sln_bodymass)  # Should match the number of observations in your data
# Plot residuals against fitted values
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
#If you see a non-linear pattern in the plot, this suggests that a non-linear relationship may be needed for that variable.

# Calculate Cook's distance
cooksd <- cooks.distance(lm)
plot(cooksd, main="Cook's Distance")
abline(h = 4 / length(cooksd), col = "red")  # Threshold for influential points
#If any data points have Cook's distance larger than the threshold, it suggests that they are influential and may need further investigation.

# ACF plot
acf(resid(lm))
#In a well-behaved model, you want the residuals to not show significant autocorrelation (i.e., no pattern in the ACF plot).


# 4. Plot partial effects (AVM) ------------------------------------------------------------
lm_AVM <- glmer(Alive_Dead ~ ln_bodymass + at_celsius + MinsExposedtoAir + depth + (1 | Species), #sln_bodymass + sdepth + sMinsExposedtoAir + sat_celsius + habitat 
                data = data, 
                family = binomial(link = "logit"))


# 9.1. TIME ON DECK-------------------------------------------------------------
partial_effect <- ggpredict(lm, terms="sMinsExposedtoAir [all]")
#The argument terms = "MinsExposedtoAir [all]" tells ggpredict() to generate predictions for all observed values of the MinsExposedtoAir variable within the dataset, essentially creating a smooth curve of predictions across the range of this variable.

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
    y = "Predicted AVM"
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
p_png <- paste0(output_data, "/AVM_DeckTime.png")
ggsave(p_png, p_deck, width=12, height=12, units="cm", dpi=300)



# 9.2. BIOMASS -----------------------------------------------------------------
partial_effect <- ggpredict(lm_AVM, terms="ln_bodymass [all]")
#The argument terms = "MinsExposedtoAir [all]" tells ggpredict() to generate predictions for all observed values of the MinsExposedtoAir variable within the dataset, essentially creating a smooth curve of predictions across the range of this variable.

#unscale variable x:
hist(data$ln_bodymass)

# Create the plot
# Modify the plot to display the unscaled x values
p_Biomass <- ggplot(partial_effect, aes(x = x, y = predicted))  +
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect of ln_bodymass on Health Condition",
    x = "ln_bodymass",  # Update the label to reflect unscaled units
    y = "Predicted AVM"
  ) +
  #ylim(0, 1) +  # Set the Y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p_Biomass)

# export plot
p_png <- paste0(output_data, "/AVM_Biomass.png")
ggsave(p_png, p_Biomass, width=12, height=12, units="cm", dpi=300)

# 9.3. ATMOSPHERIC TEMPERATURE--------------------------------------------------
partial_effect <- ggpredict(lm_AVM, terms="at_celsius [all]")
#The argument terms = "MinsExposedtoAir [all]" tells ggpredict() to generate predictions for all observed values of the MinsExposedtoAir variable within the dataset, essentially creating a smooth curve of predictions across the range of this variable.

#unscale variable x:
hist(data$at_celsius)

# Create the plot
# Modify the plot to display the unscaled x values
p_AT <- ggplot(partial_effect, aes(x = x, y = predicted))  +
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect of at_celsius on Health Condition",
    x = "at_celsius",  # Update the label to reflect unscaled units
    y = "Predicted AVM"
  ) +
  #ylim(0, 1) +  # Set the Y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p_AT)

# export plot
p_png <- paste0(output_data, "/AVM_AT.png")
ggsave(p_png, p_AT, width=12, height=12, units="cm", dpi=300)



# 9.4. DEPTH -------------------------------------------------------------------
partial_effect <- ggpredict(lm_AVM, terms=" depth [all]")
#The argument terms = "MinsExposedtoAir [all]" tells ggpredict() to generate predictions for all observed values of the MinsExposedtoAir variable within the dataset, essentially creating a smooth curve of predictions across the range of this variable.
hist(data$depth)

# Create the plot
# Modify the plot to display the unscaled x values
p_depth <- ggplot(partial_effect, aes(x = x, y = predicted))  +
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "darkgrey") +
  labs(
    title = "Partial Effect of depth on Health Condition",
    x = "depth",  # Update the label to reflect unscaled units
    y = "Predicted AVM"
  ) +
  #ylim(0, 1) +  # Set the Y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p_depth)

# export plot
p_png <- paste0(output_data, "/AVM_depth.png")
ggsave(p_png, p_depth, width=12, height=12, units="cm", dpi=300)



# 9.5. Coefficient random effect for each species -------------------------------
# Extract and plot random effects for Species
ranef_species <- ranef(lm)$Species
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


# Add the depth values to ranef_df based on Species
ranef_df$Depth <- depthrange[ranef_df$Species]
ranef_df$bodymass <- bodymass[ranef_df$Species]

# Ensure that the 'Species' variable is ordered as a factor in the desired order in ranef_df
ranef_df$Species <- factor(ranef_df$Species, levels = rev(c(
  "Cmonstrosa", "Gmelastomus", "Scanicula", "Cuyato", "Ocentrina", 
  "Dlicha", "Espinax", "Hgriseus", "Doxyrinchus", "Rclavata", 
  "Rasterias", "Rpolystigma", "Pviolacea", "Dpastinaca", 
  "Maquila", "Abovinus", "Tmarmorata"
)))

# Plotting the random effects for each species, colored by Depth
color_palette_bathy <- colorRampPalette(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46'))(100)

# Normalize bodymass values for point size scaling
ranef_df$PointSize <- scales::rescale(ranef_df$bodymass, to = c(4, 11))

# Plotting with bodymass as point size
p_sp <- ggplot(ranef_df, aes(x = Species, y = Intercept, fill = Depth, size = PointSize)) + 
  geom_hline(yintercept = 0, color = "steelblue", size = 1.2) +
  geom_point(shape = 21, color = "black", stroke = 0.5) +  # Dark contour, colored fill
  labs(
    title = "Random Effects for Species by Depth Range",
    x = "Species",
    y = "Deviation from Overall Health Condition",
    fill = "Depth Range",  # Label for the fill color scale
    size = "Bodymass"  # Label for the point size scale
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    aspect.ratio = 1 
  ) +
  scale_fill_gradientn(colors = color_palette_bathy)+
  coord_flip()

#This random effects plot shows:
#Species Deviations: Points above or below zero indicate whether each species has a higher or lower health condition than the overall mean.
#Zero Line: Species at or near zero have health conditions close to the overall average.

print(p_sp)

# export plot
p_png <- paste0(output_data, "/AVM_spIntercept.png")
ggsave(p_png, p_sp, width=16, height=12, units="cm", dpi=300)





# 10. Plot coefficients for each model-------------------------------------------
library(broom.mixed)  # To extract model coefficients and confidence intervals

# 10.1. AVM model ---------------------------------------------------------------
# Fit the model (already done in your case)
lm <- glmer(Alive_Dead ~ sln_bodymass + sat_celsius + sMinsExposedtoAir + sdepth + (1 | Species),
            data = data, 
            family = binomial(link = "logit"))
summary(lm)
# Get the model's fixed effects and calculate the confidence intervals
fixed_effects <- fixef(lm)  # Extract the fixed effects estimates
confint_values <- confint(lm, method = "Wald")  # Get 95% CI for fixed effects using Wald method
# Remove the .sig01 row (random effects) from the confint_values data frame
confint_values <- confint_values[-1, ]

# Create a data frame with coefficients and CIs
coef_df <- data.frame(
  Coefficient_Name = names(fixed_effects),
  Estimate = fixed_effects,
  CI_Lower_Bound = confint_values[, 1],
  CI_Upper_Bound = confint_values[, 2]
)

# Plotting the coefficients with error bars (95% CI)
p_coef_AVM <- ggplot(coef_df, aes(x = Coefficient_Name, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_Lower_Bound, ymax = CI_Upper_Bound), size = 0.5, color = "steelblue") +  # Change dot color to blue
  labs(x = "Coefficient Name", y = "Estimate Value", title = "Coefficient Estimates with 95% Confidence Intervals") +
  coord_flip() +  # Swap axes for a horizontal plot
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        aspect.ratio = 1) +
  # Change the color of the dashed line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Change the line color to red

# Print the plot
print(p_coef_AVM)

# export plot
p_png <- paste0(output_data, "/AVM_coef.png")
ggsave(p_png, p_coef_AVM, width=12, height=12, units="cm", dpi=300)


# 10.2. Health model ---------------------------------------------------------------
# Fit the model (already done in your case)
lm <- lmer(Health_prob_scaled ~  sMinsExposedtoAir  + sdepth + (1 | Species), #sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius
           data = data_health)
summary(lm)
# Get the model's fixed effects and calculate the confidence intervals
fixed_effects <- fixef(lm)  # Extract the fixed effects estimates
confint_values <- confint(lm, method = "Wald")  # Get 95% CI for fixed effects using Wald method
# Remove the .sig01 row (random effects) from the confint_values data frame
confint_values <- confint_values[-1, ]

# Create a data frame with coefficients and CIs
coef_df <- data.frame(
  Coefficient_Name = names(fixed_effects),
  Estimate = fixed_effects,
  CI_Lower_Bound = confint_values[, 1],
  CI_Upper_Bound = confint_values[, 2]
)

# Plotting the coefficients with error bars (95% CI)
p_coef_Health <- ggplot(coef_df, aes(x = Coefficient_Name, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_Lower_Bound, ymax = CI_Upper_Bound), size = 0.5, color = "steelblue") +  # Change dot color to blue
  labs(x = "Coefficient Name", y = "Estimate Value", title = "Coefficient Estimates with 95% Confidence Intervals") +
  coord_flip() +  # Swap axes for a horizontal plot
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        aspect.ratio = 1) +
  # Change the color of the dashed line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Change the line color to red

# Print the plot
print(p_coef_Health)

# export plot
p_png <- paste0(output_data, "/Health_coef.png")
ggsave(p_png, p_coef_Health, width=12, height=12, units="cm", dpi=300)






