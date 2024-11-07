#----------------------------------------------------------------------------------- 
# 4_OtherRates: Calculate other rates: discarding and abortion rate
#----------------------------------------------------------------------------------- 

#Load data
setwd(input_data)
CPUE <- read.csv("CPUE_PA_BioParameters.csv", sep = ";")
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
    N.commercialised = as.numeric(gsub(",", ".", N.commercialised)))

CPUE$Species <- gsub(" ", "", CPUE$Species)

# Calculate total N discarded and N commercialised per species
totals <- aggregate(cbind(N.discarded, N.commercialised) ~ Species, data = CPUE, FUN = sum)

# Calculate the percentage discaded:
totals <- totals %>%
  mutate(PercentageDiscarded = (N.discarded / (N.discarded + N.commercialised)) * 100)

totals <- totals %>%
  mutate(PercentageCommercialised = (N.commercialised / (N.commercialised + N.discarded)) * 100)

# Print the result
print(totals)

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
