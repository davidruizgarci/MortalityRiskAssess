# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_7_diff_season  Assess differences among seasons
#-------------------------------------------------------------------------------

# Load seasonal predicted mean rasters across species
library(tidyverse)
library(raster)
library(ggplot2)
library(smplot2)

# Define seasons and input path
season_vals <- c("Spring", "Winter", "Fall", "Summer")

mins <- "Mins10" #Mins55 #Mins41 #Mins10
trawl <- "Trawl2.9" #Trawl4.1 #Trawl3.4 #Trawl2.9
indir <- file.path(output_data, "predict_across_sp", "2021", paste0(mins, "_", trawl))

# Loop to read each raster mean, extract values, and store in a list
data_list <- list()

for (season in season_vals) {
  #season <- season_vals[1]
  file_mean <- file.path(indir, paste0(season, "_across_sp_pred_mean.tif"))
  
  if (file.exists(file_mean)) {
    r <- raster(file_mean)
    df <- as.data.frame(r, xy = TRUE)
    colnames(df) <- c("x", "y", "risk")
    df <- df %>% filter(!is.na(risk))
    df$season <- season
    data_list[[season]] <- df
  } else {
    message("⚠️ Missing file for season: ", season)
  }
}

# Combine into a single dataframe
seasonal_data <- bind_rows(data_list)
head(seasonal_data)

# Shapiro-Wilk normality tests per season
shapiro_tests <- lapply(split(seasonal_data$risk, seasonal_data$season), shapiro.test)
print(shapiro_tests)

# Kruskal-Wallis test
kruskal_test <- kruskal.test(risk ~ season, data = seasonal_data)
print(kruskal_test)

# Results:
#Mins55_Trawl4.1: Kruskal-Wallis chi-squared = 464.58, df = 3, p-value < 2.2e-16
#Mins41_Trawl3.4: Kruskal-Wallis chi-squared = 455.72, df = 3, p-value < 2.2e-16
#Mins10_Trawl2.9: Kruskal-Wallis chi-squared = 445.72, df = 3, p-value < 2.2e-16


# Pairwise Wilcoxon with Bonferroni correction
pairwise_test <- pairwise.wilcox.test(seasonal_data$risk, seasonal_data$season, p.adjust.method = "bonferroni")
print(pairwise_test)


#Pairwise comparisons using Wilcoxon rank sum test with continuity correction
#Mins55_Trawl4.1:
#Fall   Spring Summer
#Spring 1.0000 -      -     
#Summer <2e-16 <2e-16 -     
#Winter 0.0227 0.0092 <2e-16

#Mins41_Trawl3.4:
#Fall   Spring Summer
#Spring 1.0000 -      -     
#Summer <2e-16 <2e-16 -     
#Winter 0.0092 0.0056 <2e-16

#Mins10_Trawl2.9:
#Fall   Spring Summer
#Spring 1.0000 -      -     
#Summer <2e-16 <2e-16 -     
#Winter 0.0015 0.0022 <2e-16


# Violin and boxplot
p <- ggplot(data = seasonal_data, aes(x = season, y = risk)) +
  geom_violin(aes(fill = season), trim = FALSE, color = "transparent", alpha = 0.5) + 
  geom_boxplot(aes(fill = season), width = 0.1, outlier.shape = NA) + 
  scale_fill_manual(values = c(
    "Spring" = "#B2DF8A", 
    "Summer" = "#FDBF6F", 
    "Fall"   = "#FFFF99", 
    "Winter" = "#A6CEE3"
  )) +
  ylab("Mean AVM risk (%)") +
  theme(
    axis.title.x = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

print(p)

# Export
outdir <- file.path(output_data, "fig", "raincloud_seasons", paste0(mins, "_", trawl))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
file <- paste0(mins, "_", trawl, "_AVM_across_sp_violin.png")
ggsave(file.path(outdir, file), p, width = 17, height = 10, units = "cm", dpi = 300)

