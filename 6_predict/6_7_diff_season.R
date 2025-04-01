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

mins <- "Mins29" #Mins55 #Mins41 #Mins10
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
p <- ggplot(data = seasonal_data, aes(x = season, y = risk, fill = season)) +
  geom_violin(trim = FALSE, width = 1, scale = "width", alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.9, color = "black") + 
  scale_fill_manual(values = c(
    "Spring" = "#B2DF8A", 
    "Summer" = "#FDBF6F", 
    "Fall"   = "#FFFF99", 
    "Winter" = "#A6CEE3"
  )) +
  ylab("Mean AVM risk (%)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  )

print(p)

# Export
outdir <- file.path(output_data, "fig", "raincloud_seasons", paste0(mins, "_", trawl))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
file <- paste0(mins, "_", trawl, "_AVM_across_sp_violin.png")
ggsave(file.path(outdir, file), p, width = 17, height = 10, units = "cm", dpi = 300)


# Get summary for each season:
seasonal_data %>%
  group_by(season) %>%
  summarize(
    count = n(),
    min = min(risk, na.rm = TRUE),
    Q1 = quantile(risk, 0.25, na.rm = TRUE),
    median = median(risk, na.rm = TRUE),
    Q3 = quantile(risk, 0.75, na.rm = TRUE),
    max = max(risk, na.rm = TRUE),
    mean = mean(risk, na.rm = TRUE),
    sd = sd(risk, na.rm = TRUE)
  ) %>%
  arrange(season)

#season count      min       Q1  median      Q3     max    mean      sd
#1 Fall     211 0.000280 0.00110  0.00343 0.00400 0.00727 0.00291 0.00174
#2 Spring   211 0.000261 0.00132  0.00334 0.00419 0.0110  0.00313 0.00203
#3 Summer   211 0.00132  0.0200   0.0469  0.0640  0.188   0.0482  0.0345 
#4 Winter   211 0.000244 0.000996 0.00295 0.00349 0.00596 0.00251 0.00152