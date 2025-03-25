# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

#-------------------------------------------------------------------------------
library(corrplot)
library(Hmisc)
library(dplyr)


#-------------------------------------------------------------------------------
# 4_2_check_correlation    
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


#2. Transform skewed variables for modelling------------------------------------

#Weight
data$ln_bodymass <- log1p(data$bodymass)

# Active hypoxia tolerance
data$ln_Aeco <- log1p(data$Aeco)

# Filter NAs:
data <- data %>% 
  filter(!is.na(at_celsius), 
         !is.na(ln_bodymass)) %>% 
  mutate(Alive_Dead = ifelse(Alive_Dead == 0, 1, 0))

# For fitting the health model, remove health NAs:
data_health <- data %>%
  filter(!is.na(Activity)) %>%
  filter(Alive_Dead == 1)  %>%
  mutate(Health_prob = as.numeric(gsub(",", ".", Health_prob)))


# 2. Check Spearman correlation-------------------------------------------------
names(data)
names(data_health)

# Select specific columns from the data dataset in which you want to assess correlation
vars  <- c("MinsExposedtoAir", "depth", "ln_bodymass", "at_celsius", 
           "Trawl_duration", "ln_Aeco") 

#"fishingEffort", "bottom_nppv", "bottom_ph", "bottom_nppv", "distMounts", 
#"distCanyons", "distFans", "bottom_nh4""SD_bottomT", "SD_o2", "bottom_oxygen",

# calculate correlations using Pearson
#correlations <- cor(na.omit(dplyr::select(data, all_of(vars))), method="pearson")
correlations <- cor(na.omit(dplyr::select(data_health, all_of(vars))), method="pearson")


# plot correlations
model <- "health" #"AVM"
output_dir <- paste0(output_data, "/prefitting_checks/", model)
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}
pngfile <- paste0(output_dir, "/", model, "_eda_corplot.png")
png(pngfile, width=3000, height=3000, res=300)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlations, method="color",col=col(200), tl.col = "black", order = "original", diag=FALSE, type="upper", 
         addCoef.col = "black") # Add coefficient of correlation
dev.off()

# Calculate correlations using Spearman and clustering analysis
# plot: #A9D18E for S canicula and #F4B183 for G melastomus
pngfile <- paste0(output_dir, "/", model, "_eda_cluster.png")
png(pngfile, width=2500, height=2000, res=300)
op <- par(mar=c(0.5,5,0.5,0.5))
v <- as.formula(paste("~", vars, collapse="+"))
plot(varclus(v, similarity=c("spearman"),data=data),cex=1.2) # plot cluster
abline(a=0.30,0,col="steelblue",lty=1,lwd=2)
par(op)
dev.off()

# If two variables are very correlated, there will be convergence. You must eliminate the correlated ones.
# Then you select based on your interest (more ecologic sense, something that you can explain better)
# Spearman is usually used for this. When the values is larger than 0.7 between two variables, you must select only one of this.
# All predictors are uncorrelated (Spearman correlations <0.7), therefore none of them is discarded.
# You may use VIF as well, but this is used on the model, rather than upon the variables itself. Here we want to look at variables it self, because we are using machine learning.
# VIF may be also only for linear relationships and Spearman for non-linear too, but we are not sure, should look for more info. But basically jut use Spearman.
