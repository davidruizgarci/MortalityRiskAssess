#---------------------------------------------------------------------------------------------------
# GLMM: determine patterns in AVM in relation to species biology and ecology
#---------------------------------------------------------------------------------------------------

#1. Load libraries -------------------------------------------------------------
# Load required packages
library(dplyr)
library(lme4)
library(lmerTest)
library(lmtest)
library(arm)
library(car)
library(effects)
library(ggplot2)






#2. Load data ------------------------------------------------------------------
# Import enviro variables from previous extraction (only once)
#data <- read.csv("output/AVM_health_all.csv", sep = ";")
#data2 <- read.csv("C:/Users/david/OneDrive/Escritorio/Survival/chondrichthyan_mortality/output/data_env_all.csv", sep = ";")
#
##Import enviro variables:
#names(data2)
#names(data)
## Select the first 'at_celsius' for each matching Species and tripID
#data2_first_at_celsius <- data2 %>%
#  group_by(Species, tripID) %>%
#  slice(1) %>%  # Keep only the first row in each group
#  ungroup() %>%
#  dplyr::select(Species, tripID, at_celsius)  # Use dplyr::select to avoid function conflict
#
## Merge with the original data
#data <- merge(data, data2_first_at_celsius, 
#                     by.x = c("Species", "code"), 
#                     by.y = c("Species", "tripID"), 
#                     all.x = TRUE)
#
## remove NAs:
#data <- data %>% 
#  filter(!is.na(Alive_Dead)) %>% 
#  mutate(Alive_Dead = factor(Alive_Dead, c(0, 1)))
#
#

#Save dataframes:
#output_file <- file.path(output_data, "AVM_allEnviro.csv")
#write.csv2(data, file = output_file, row.names = FALSE)

# Load data (including enviro vars):
data <- read.csv("temp/AVM_allEnviro.csv", sep = ";")
#Format:
names(data)
data <- data %>%
  mutate(
    bodymass = as.numeric(gsub(",", ".", WeightLWR)),  # Convert comma decimal to dot decimal
    depth = as.numeric(gsub(",", ".", depth)),
    Species = as.factor(Species),
    MinsExposedtoAir =  as.numeric(gsub(",", ".", MinsExposedtoAir)),
    at_celsius =  as.numeric(gsub(",", ".", at_celsius)),) 

# Add habitat:
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

#Add the habitat column to your data frame
data <- data %>%
  mutate(habitat = factor(habitat_mapping[Species], levels = c("benthopelagic", "demersal")))
str(data)






#3. Scale variables for modelling-----------------------------------------------
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
#scale
data$sdepth<-scale(data$depth)

#MinsExposed
hist(data$MinsExposedtoAir)
shapiro.test(data$MinsExposedtoAir)
#scale
data$sMinsExposedtoAir<-scale(data$MinsExposedtoAir)

#AT
hist(data$at_celsius)
shapiro.test(data$at_celsius)
#scale
data$sat_celsius<-scale(data$at_celsius)

# For fitting the health model, remove health NAs:
data_health <- data %>%
  filter(!is.na(Activity)) %>%
  filter(Alive_Dead == 1) %>%
  mutate(Health_prob = as.numeric(gsub(",", ".", Health_prob)))






#4. Fit model (Health) ---------------------------------------------------------

# Decide model distribution: in my case binomial
str(data_health)
hist(data_health$Health_prob)
shapiro.test(data_health$Health_prob)
summary(data_health$Health_prob)
#scale
data_health$Health_prob_scaled <- data_health$Health_prob / 100


# HEALTH CONDITION:
# remove NAs:
summary(data_health$sln_bodymass)
data_health <- data_health %>% 
  filter(!is.na(at_celsius)) 

glmm <- lmer(Health_prob_scaled ~  sMinsExposedtoAir  + sdepth + (1 | Species), #sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius
             data = data_health)
#plot(resid(glmm))
#confint(glmm)
display(glmm) #AIC 452.2
anova(glmm)
ranef(glmm)
summary(glmm)

# Check random effect:
glmm_no_random <- lm(Health_prob_scaled ~ sMinsExposedtoAir, data = data_health) #sln_bodymass + sdepth + habitat
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




#5. Plot partial effects (Health) ----------------------------------------------

# Plot partial effects:
library(ggeffects)

# 5.1. TIME ON DECK-------------------------------------------------------------
lm_sp <- lmer(Health_prob_scaled ~  MinsExposedtoAir  + depth + (1 | Species), #sln_bodymass + sdepth + habitat + sMinsExposedtoAir + sat_celsius
              data = data_health)

partial_effect <- ggpredict(lm_sp, terms = "MinsExposedtoAir")

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
partial_effect <- ggpredict(lm_sp, terms = "depth")

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





# 5.3.1. Combine general and sp-specific (Depth effect)-------------------------
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

# 5.3.1. Combine general and sp-specific (MinsExposedtoAir effect)-------------------------
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



#5.4. Coefficient random effect for each species -------------------------------
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
  geom_point(size = 3, shape = 21, color = "black", stroke = 0.5) +  # Dark contour, colored fill
  geom_hline(yintercept = 0, linetype = "dashed", color = "steelblue") +
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


#































#6. Fit model (AVM) ------------------------------------------------------------
# AT-VESSEL MORTALITY:
# Filter NAs:
summary(data$sMinsExposedtoAir)
data <- data %>% 
  filter(!is.na(at_celsius),
         !is.na(sln_bodymass)) 

# Fit the GLMM with a binomial family for a binary response
lm <- glmer(Alive_Dead ~ sln_bodymass + sdepth + sMinsExposedtoAir + at_celsius + (1 | Species), #habitat + sMinsExposedtoAir + at_celsius
               data = data, 
               family = binomial(link = "logit"))

#plot(resid(lm))
#confint(lm)

# Check multicollinearity and significancy of the fixed factor:
vif(lm)

# check model:
display(lm) #AIC 2142.4    #1775.5
anova(lm)
ranef(lm)
summary(lm)

# Check random factor:
lm_no_random <- glm(Alive_Dead ~ sln_bodymass + sdepth + sMinsExposedtoAir + at_celsius, 
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


#7. Plot partial effects (AVM) ------------------------------------------------------------











# Extract random effects for Species
library(tibble)
species_re <- ranef(lm_sp)$numericSp
species_re_df <- as.data.frame(species_re) %>% 
  rownames_to_column(var = "numericSp") %>% 
  rename(Intercept = `(Intercept)`)

# Plot the random intercepts for each species
ggplot(species_re_df, aes(x = reorder(numericSp, Intercept), y = Intercept)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts for Species",
       x = "Species",
       y = "Random Intercept (Effect on Alive/Dead)") +
  coord_flip() +
  theme_minimal()

# Generate predicted probabilities for a range of values of depth and ln_bodymass considering Species influence
# Fit the GLMM with a binomial family for a binary response
lm_sp <- glmer(Alive_Dead ~ depth + ln_bodymass + at_celsius + TotalBiomassHaul + MinsExposedtoAir + (1 | numericSp), 
               data = data, 
               family = binomial(link = "logit"))

# CONSIDERING RANDOM EFFECTS:
# Depth effect:

# Create a new prediction grid for depth with the median ln_bodymass and all levels of numericSp
pred_data_depth <- expand.grid(
  depth = seq(min(data$depth), max(data$depth), length.out = 100),
  ln_bodymass = median(data$ln_bodymass, na.rm = TRUE),
  at_celsius = median(data$at_celsius, na.rm = TRUE), # Control for temperature
  TotalBiomassHaul = median(data$TotalBiomassHaul, na.rm = TRUE), # Control for biomass
  MinsExposedtoAir = median(data$MinsExposedtoAir, na.rm = TRUE), # Control for exposure time
  numericSp = unique(data$numericSp)
)

# Generate predictions
pred_data_depth$predicted_prob <- predict(lm_sp, newdata = pred_data_depth, type = "response")

# Plot the results
ggplot(pred_data, aes(x = depth, y = predicted_prob, color = as.factor(numericSp))) +
  geom_line() +
  labs(title = "Predicted Probability of Alive by Depth for Each Species",
       x = "Standardized Depth (sdepth)",
       y = "Predicted Probability of Alive",
       color = "Species (numericSp)") +
  theme_minimal() +
  theme(legend.position = "right")


# Body mass effect:
# Create a new prediction grid for sdepth with the median sln_bodymass and all levels of numericSp
median_depth <- median(data$depth, na.rm = TRUE)

pred_data <- expand.grid(
  depth = median_depth,
  ln_bodymass = seq(min(data$ln_bodymass), max(data$ln_bodymass), length.out = 100),
  numericSp = unique(data$numericSp))

# Generate predictions
pred_data$predicted_prob <- predict(lm_sp, newdata = pred_data, type = "response")

ggplot(pred_data, aes(x = ln_bodymass, y = predicted_prob, color = as.factor(numericSp))) +
  geom_line() +
  labs(title = "Predicted Probability of Alive by Depth for Each Species",
       x = "Body mass",
       y = "Predicted Probability of Alive",
       color = "Species (numericSp)") +
  theme_minimal() +
  theme(legend.position = "right")

# AVERAGING ALL SPECIES:
# Depth effect:
# Calculate the median of ln_bodymass to create a simpler prediction grid
median_ln_bodymass <- median(data$ln_bodymass, na.rm = TRUE)

# Create a new prediction grid for depth with the median ln_bodymass
pred_data <- data.frame(
  depth = seq(min(data$depth), max(data$depth), length.out = 100),
  ln_bodymass = median_ln_bodymass
)

# Generate predictions and standard errors
predictions <- predict(lm_sp, newdata = pred_data, type = "link", se.fit = TRUE, re.form = NA)

# Convert link predictions to probabilities
pred_data$predicted_prob <- plogis(predictions$fit)  # Logistic transformation
pred_data$lower_CI <- plogis(predictions$fit - 1.96 * predictions$se.fit)  # Lower CI
pred_data$upper_CI <- plogis(predictions$fit + 1.96 * predictions$se.fit)  # Upper CI

# Plot the general trend for depth effect with confidence intervals
ggplot(pred_data, aes(x = depth, y = predicted_prob)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(title = "Predicted Probability of Alive by Depth",
       x = "Depth (sdepth)",
       y = "Predicted Probability of Alive") +
  theme_minimal()

# Body mass effect:
# Create a new prediction grid for ln_bodymass with the median depth
median_depth <- median(data$depth, na.rm = TRUE)

pred_data_bodymass <- data.frame(
  depth = median_depth,
  ln_bodymass = seq(min(data$ln_bodymass), max(data$ln_bodymass), length.out = 100)
)

# Generate predictions and standard errors for body mass
predictions_bodymass <- predict(lm_sp, newdata = pred_data_bodymass, type = "link", se.fit = TRUE, re.form = NA)

# Convert link predictions to probabilities and calculate CI
pred_data_bodymass$predicted_prob <- plogis(predictions_bodymass$fit)
pred_data_bodymass$lower_CI <- plogis(predictions_bodymass$fit - 1.96 * predictions_bodymass$se.fit)  # Lower CI
pred_data_bodymass$upper_CI <- plogis(predictions_bodymass$fit + 1.96 * predictions_bodymass$se.fit)  # Upper CI

# Plot the general trend for body mass effect with confidence intervals
ggplot(pred_data_bodymass, aes(x = ln_bodymass, y = predicted_prob)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(title = "Predicted Probability of Alive by Body Mass",
       x = "Body Mass (ln_bodymass)",
       y = "Predicted Probability of Alive") +
  theme_minimal()











# Create a data frame with combinations of depth and ln_bodymass
pred_data <- expand.grid(
  depth = seq(min(data$depth), max(data$depth), length.out = 100),
  ln_bodymass = median(data$ln_bodymass, na.rm = TRUE)
)

# Predict using the model
#Predict Without Random Effects
pred_data$predicted_prob <- predict(lm_sp, newdata = pred_data, type = "response", re.form = NA)
pred_data$predicted_prob <- predict(lm_sp, newdata = pred_data, type = "response")

# Get predictions along with standard errors
predictions <- predict(lm_sp, newdata = pred_data, type = "link", se.fit = TRUE)

# Convert link predictions to probabilities and calculate confidence intervals
pred_data$predicted_prob <- plogis(predictions$fit)  # Convert to probability scale
pred_data$lower_CI <- plogis(predictions$fit - 1.96 * predictions$se.fit)  # Lower bound of 95% CI
pred_data$upper_CI <- plogis(predictions$fit + 1.96 * predictions$se.fit)  # Upper bound of 95% CI

# Plot with confidence interval ribbon
ggplot(pred_data, aes(x = depth, y = predicted_prob)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(title = "Effect of Depth on Alive/Dead Probability",
       x = "Standardized Depth (sdepth)",
       y = "Predicted Probability of Alive") +
  theme_minimal()

#























# Fit a logistic regression model without random effects
lm_sp_fixed <- glm(Alive_Dead ~ depth + ln_bodymass, data = data, family = binomial)

# Create a new data frame for predictions
pred_data <- data.frame(
  depth = seq(min(data$depth), max(data$depth), length.out = 100),
  ln_bodymass = median(data$ln_bodymass, na.rm = TRUE)  # Use the median for sln_bodymass
)

# Get predictions with standard errors
predictions <- predict(lm_sp_fixed, newdata = pred_data, type = "link", se.fit = TRUE)

# Convert link predictions to probabilities and calculate confidence intervals
pred_data$predicted_prob <- plogis(predictions$fit)  # Convert to probability scale
pred_data$lower_CI <- plogis(predictions$fit - 1.96 * predictions$se.fit)  # Lower bound of 95% CI
pred_data$upper_CI <- plogis(predictions$fit + 1.96 * predictions$se.fit)  # Upper bound of 95% CI

# Plot with confidence interval ribbon
ggplot(pred_data, aes(x = depth, y = predicted_prob)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(title = "Effect of Depth on Alive/Dead Probability",
       x = "Standardized Depth (sdepth)",
       y = "Predicted Probability of Alive") +
  theme_minimal()










#Once you have your model plot the partial effect of your results:
setwd('C:/Users/Addres...')
jpeg(file = "Name.jpeg", 
     width = 23.8, height = 21.65, units = "cm", res = 400)
colors <- rainbow(3)
e1.mm1<-plot(predictorEffects(lm_sp, ~ sdepth),lines=list(multiline=TRUE, col=colors), confint=list(style="auto"),
             axes=list(y=list(transform=exp, lab="AVM")),
             lattice=list(key.args=list(x=.75, y=.80, corner=c(0, 0),fontfamily="serif",cex=1,
                                        padding.text=1.25)),main="")
dev.off()

setwd('C:/Users/Addres...')
jpeg(file = "Name.jpeg", 
     width = 23.8, height = 21.65, units = "cm", res = 400)
colors <- rainbow(3)
e1.mm1<-plot(predictorEffects(lm_sp, ~ sln_bodymass),lines=list(multiline=TRUE, col=colors), confint=list(style="auto"),
             axes=list(y=list(transform=exp, lab="AVM")),
             lattice=list(key.args=list(x=.75, y=.80, corner=c(0, 0),fontfamily="serif",cex=1,
                                        padding.text=1.25)),main="")
dev.off()
