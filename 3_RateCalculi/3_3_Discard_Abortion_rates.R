# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

#-------------------------------------------------------------------------------


#----------------------------------------------------------------------------------- 
# 4_OtherRates: Calculate other rates: discarding and abortion rate
#----------------------------------------------------------------------------------- 

#Load data
setwd(input_data)
CPUE <- read.csv("input/CPUE/CPUE_PA_BioParameters.csv", sep = ";")
names(CPUE)
head(CPUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
#How many discarded and commercialized of each species?
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# Eliminate strange value:
CPUE <- CPUE %>%
  filter(N.commercialised != "Puesto a secar para vender a restaurante gourmet (N=151)")

CPUE <- CPUE %>%
  mutate(
    N.discarded = as.numeric(gsub(",", ".", N.discarded)),
    N.commercialised = as.numeric(gsub(",", ".", N.commercialised)),
    X..Kg.commercialised = as.numeric(gsub(",", ".", X..Kg.commercialised)),
    X..Kg.discarded = as.numeric(gsub(",", ".", X..Kg.discarded)))

CPUE$Species <- gsub(" ", "", CPUE$Species)

# Calculate total N discarded and N commercialised per species
totals <- aggregate(cbind(N.discarded, N.commercialised) ~ Species, data = CPUE, FUN = sum)
totals_kg <- aggregate(cbind(X..Kg.discarded, X..Kg.commercialised) ~ Species, data = CPUE, FUN = sum)

# Calculate the percentage discaded:
totals <- totals %>%
  mutate(PercentageDiscarded = (N.discarded / (N.discarded + N.commercialised)) * 100)

totals_kg <- totals_kg %>%
  mutate(PercentageCommercialised = (X..Kg.discarded / (X..Kg.discarded + X..Kg.commercialised)) * 100)

# Print the result
print(totals)
print(totals_kg)

# Compute mean and standard deviation
mean_biomass <- mean(totals_kg$PercentageCommercialised)
sd_biomass <- sd(totals_kg$PercentageCommercialised)

# Print result as mean ± SD
cat("Mean ± SD biomass per species:", round(mean_biomass, 2), "±", round(sd_biomass, 2), "kg\n")


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# % of biomass and individuals discarded overall
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

# Calculate total biomass per tow
CPUE$Total_Biomass <- CPUE$X..Kg.discarded + CPUE$X..Kg.commercialised
CPUE$Total_N <- CPUE$N.discarded + CPUE$N.commercialised

# Calculate percentage discarded per tow
CPUE$Perc_Discarded <- (CPUE$X..Kg.discarded / CPUE$Total_Biomass) * 100
CPUE$PercN_Discarded <- (CPUE$N.discarded / CPUE$Total_N) * 100

# Compute mean ± SD
mean_perc <- mean(CPUE$Perc_Discarded, na.rm = TRUE)
sd_perc <- sd(CPUE$Perc_Discarded, na.rm = TRUE)

meanN_perc <- mean(CPUE$PercN_Discarded, na.rm = TRUE)
sdN_perc <- sd(CPUE$PercN_Discarded, na.rm = TRUE)

# Print result
cat("Mean ± SD percentage of biomass discarded per tow:", round(mean_perc, 2), "±", round(sd_perc, 2), "%\n")
cat("Mean ± SD percentage of N discarded per tow:", round(meanN_perc, 2), "±", round(sdN_perc, 2), "%\n")

#----------------------------------------------------------------------------------- 
# 4. Calculate abortion rate
#----------------------------------------------------------------------------------- 

#Load data
setwd(input_data)
CPUE <- read.csv("CPUE_PA_BioParameters.csv", sep = ";")
names(CPUE)
head(CPUE)

# Eliminate strange value:
CPUE <- CPUE %>%
  mutate(
    N.pregnant = as.numeric(gsub(",", ".", N.pregnant)),
    N.abortion = as.numeric(gsub(",", ".", N.abortion)), 
    N..mature.females = as.numeric(gsub(",", ".", N..mature.females))) %>%
  filter(!is.na(N.pregnant), N.pregnant != " ", N.pregnant != "-", N..mature.females != 0)

CPUE$Species <- gsub(" ", "", CPUE$Species)

# Calculate total N discarded and N commercialised per species
totals <- aggregate(cbind(N.pregnant, N.abortion, N..mature.females) ~ Species, data = CPUE, FUN = sum)

# Calculate the percentage discaded:
totals <- totals %>%
  mutate(PercentagePregnant = (N.pregnant / (N..mature.females)) * 100)

totals <- totals %>%
  mutate(PercentageAbort = (N.abortion / (N.pregnant)) * 100)

# Print the result
print(totals)
