# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

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
# Filter NAs:
data <- data %>% 
  filter(!is.na(at_celsius), 
         !is.na(sln_bodymass)) %>% 
  mutate(Alive_Dead = ifelse(Alive_Dead == 0, 1, 0))


# 3. Build a GAMM for AVM ---------------------------------------------------
# Fit the GLMM with a binomial family for a binary response
lm <- glmer(Alive_Dead ~ sln_bodymass + sat_celsius + sMinsExposedtoAir + sdepth + habitat + sln_Aeco + (1 | Species), #sln_bodymass + sdepth + sMinsExposedtoAir + sat_celsius + habitat 
            data = data, 
            family = binomial(link = "logit"))
display(lm) #AIC 2135 (2185.9)
vif(lm) #remove repro for colinearity: repro 

# Add all the possible combinations within a loop:
# Define your predictors (to test all combinations)
predictors <- c("s(sMinsExposedtoAir)", "s(sdepth)", "s(sln_bodymass)", "habitat", "s(sat_celsius)", "s(sln_Aeco)") #, "s(sduration)")

# Generate all subsets of the predictors
all_combinations <- lapply(1:length(predictors), function(x) combn(predictors, x, simplify = FALSE)) %>%
  unlist(recursive = FALSE)

# Initialize a data frame to store results
model_results <- data.frame(
  formula = character(),
  AIC = numeric(),
  deviance_explained = numeric(),
  adj_r_squared = numeric(),
  errors = character()
)

# Loop through each combination and fit models
for (combo in all_combinations) {
  #combo <- all_combinations[1]
  base_formula <- "s(Species, bs = 're')"
  
  # Create model formula by combining the base and current combination of predictors
  formula <- as.formula(paste("Alive_Dead ~", paste(c(combo, base_formula), collapse = " + ")))
  
  # Try fitting the model
  tryCatch({
    model <- gam(formula, family = binomial(link = "logit"), data = data)
    
    # Extract metrics if model fitting succeeds
    model_summary <- summary(model)
    aic <- AIC(model)
    dev_expl <- if (!is.null(model_summary$dev.expl)) model_summary$dev.expl else NA
    summary_string <- capture.output(model_summary)
    adj_r2 <- grep("R-sq\\.\\(adj\\)", summary_string, value = TRUE)
    if (length(adj_r2) > 0) {
      r_sq_adj <- as.numeric(sub(".*R-sq\\.\\(adj\\) =\\s*([0-9\\.]+).*", "\\1", adj_r2))
    } else {
      adj_r2 <- NA  # Handle case where the line is not found
    }
    
    # Append results to the data frame
    model_results <- rbind(
      model_results,
      data.frame(
        formula = deparse(formula),
        AIC = aic,
        deviance_explained = dev_expl,
        adj_r_squared = adj_r2,
        errors = NA
      )
    )
  }, error = function(e) {
    # If an error occurs, log the formula and the error message
    model_results <- rbind(
      model_results,
      data.frame(
        formula = deparse(formula),
        AIC = NA,
        deviance_explained = NA,
        adj_r_squared = NA,
        errors = as.character(e$message)
      )
    )
  })
}

# Sort models by AIC (or other metrics)
model_results <- model_results %>% arrange(AIC)

# Display the results
model_results
beep()

# Fit selected model:
gamm_model <- gam(
  Alive_Dead ~ 
    s(MinsExposedtoAir, k = 5) + 
    s(depth, k = 6) + 
    s(ln_bodymass, k = 3) + 
    s(at_celsius, k = 3) +
    #habitat + 
    s(ln_Aeco, k = 5) +
    s(Species, bs = "re"),
  family = binomial(link = "logit"),
  data = data
  #,  gamma=1.4
)

summary(gamm_model)         # Summary of the model
AIC(gamm_model)             #1873.463
plot(gamm_model, pages = 1) # Visualize smooth terms
gam.check(gamm_model)
appraise(gamm_model, method = 'simulate')      # Check model diagnostics
#R-sq.(adj) =  0.619   Deviance explained = 56.1%

#Check the type of effect:
draw(gamm_model, scales = 'fixed')
plot(gamm_model, pages = 1, scheme = 2, shade = TRUE)
draw(gamm_model, scales = 'free')
plot(gamm_model, pages = 1, scheme = 2, shade = TRUE)


# Plot the factor effect
#visreg(gamm_model, "habitat", type = "conditional", gg = TRUE) +
#  labs(
#    title = "Effect of Habitat AVM probability",
#    x = "Habitat",
#    y = "Predicted AVM probability")

##export
outdir <- paste0('C:/Users/david/OneDrive/Escritorio/PRM_paper/Figures/GAMM/AVM')
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
setwd(outdir)
jpeg(file = "AVM_GAMM.jpeg", 
     width = 20, height = 20, units = "cm", res = 600)

par(mfrow = c(2, 3), pty = "s")  # Ensures a 2x2 grid & square aspect ratio
plot(gamm_model,select=1,shade=TRUE,ylab='s(MinsExposedtoAir)',xlab='DECKTIME', col = "steelblue", lwd = 2)
plot(gamm_model,select=2,shade=TRUE,ylab='s(depth)',xlab='DEPTH', col = "steelblue", lwd = 2)
plot(gamm_model,select=3,shade=TRUE,ylab='s(ln_bodymass)',xlab='MASS', col = "steelblue", lwd = 2)
plot(gamm_model,select=4,shade=TRUE,ylab='s(at_celsius)',xlab='AT', col = "steelblue", lwd = 2)
plot(gamm_model,select=5,shade=TRUE,ylab='s(ln_Aeco)',xlab='Aeco', col = "steelblue", lwd = 2)
#plot(gamm_model,select=6,shade=TRUE,all.terms=TRUE,ylab='Partial for habitat',xlab='Habitat')
#mtext("Health Condition",line = - 2,
#      outer = TRUE)
dev.off()

#vis.gam(gamm_model,view=c('ln_bodymass','depth'),theta=130,color = 'heat')
#vis.gam(gamm_model,view=c('habitat','ln_bodymass'),theta=130,color = 'heat')

#library(sjPlot)
#plot_model(gamm_model,type='pred',terms=c('ln_bodymass','habitat'),
#           axis.title=c('depth',''),
#           title='',show.data = TRUE)
#library(modEvA)
#Dsquared(gamm_model)

# 4. Coefficient random effect for each species --------------------------------
# Extract species-specific random effects from the GAMM
# Extract unique Species levels
species_levels <- unique(data$Species)

# Create a new dataset with only unique species for prediction
new_data <- data.frame(
  Species = species_levels,   # Unique species
  MinsExposedtoAir = median(data$MinsExposedtoAir, na.rm = TRUE), # Use median
  depth = median(data$depth, na.rm = TRUE),
  ln_bodymass = median(data$ln_bodymass, na.rm = TRUE),
  at_celsius = median(data$at_celsius, na.rm = TRUE),
  habitat = factor(levels(data$habitat)[1], levels = levels(data$habitat)), # First level of habitat
  ln_Aeco = median(data$ln_Aeco, na.rm = TRUE)
)

# Predict random effects for species (only Species smooth term)
ranef_species <- predict(gamm_model, newdata = new_data, type = "terms", terms = "s(Species)", se.fit = TRUE)

# Create a dataframe
ranef_df <- data.frame(
  Species = species_levels,      # Extracted species names
  Intercept = ranef_species$fit, # Extracted random effect values
  SE = ranef_species$se.fit      # Standard error
)

# View first rows
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
colnames(ranef_df) <- c("Species", "Intercept", "SE", "Depth", "bodymass")
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
p_png <- paste0("AVM_spIntercept_GAMM.png")
ggsave(p_png, p_sp, width = 20, height = 20, units = "cm", dpi = 600)



plot_model(gamm_model, type = "pred", terms = "Species")
