# ------------------------------------------------------------------------------

# Title: Mortality risk assessment reveals bycatch mitigation priorities for chondrichthyans in a trawl fishery

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

mins <- "Mins10" #Mins55 #Mins41 #Mins10 #Mins29
trawl <- "Trawl2.9" #Trawl4.1 #Trawl3.4 #Trawl2.9
indir <- file.path(output_data, "predict_across_sp", "2021", paste0(mins, "_", trawl))

# Loop to read each raster mean, extract values, and store in a list
data_list <- list()

for (season in season_vals) {
  #season <- season_vals[1]
  file_mean <- file.path(indir, paste0(season, "_across_sp_pred_mean_INTER2.tif"))
  
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
#Mins55_Trawl4.1: Kruskal-Wallis chi-squared = 2298.5, df = 3, p-value < 2.2e-16
#Mins41_Trawl3.4: Kruskal-Wallis chi-squared = 3360.4, df = 3, p-value < 2.2e-16
#Mins29_Trawl2.9: Kruskal-Wallis chi-squared = 4145.4, df = 3, p-value < 2.2e-16
#Mins10_Trawl2.9: Kruskal-Wallis chi-squared = 4738.1, df = 3, p-value < 2.2e-16


# Pairwise Wilcoxon with Bonferroni correction
pairwise_test <- pairwise.wilcox.test(seasonal_data$risk, seasonal_data$season, p.adjust.method = "bonferroni")
print(pairwise_test)


#Pairwise comparisons using Wilcoxon rank sum test with continuity correction
#Mins55_Trawl4.1:
#       Fall   Spring Summer
#Spring 1      -      -     
#Summer <2e-16 <2e-16 -     
#Winter <2e-16 <2e-16 <2e-16

#Mins41_Trawl3.4:
#         Fall   Spring Summer
#Spring 0.017  -      -     
#Summer <2e-16 <2e-16 -     
#Winter <2e-16 <2e-16 <2e-16

#Mins29_Trawl2.9:
#         Fall   Spring Summer
#Spring 0.017  -      -     
#Summer <2e-16 <2e-16 -     
#Winter <2e-16 <2e-16 <2e-16

#Mins10_Trawl2.9:
#         Fall   Spring Summer
#Spring 0.009  -      -     
#Summer <2e-16 <2e-16 -     
#Winter <2e-16 <2e-16 <2e-16


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
file <- paste0(mins, "_", trawl, "_AVM_across_sp_violin_INTER2.png")
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

#Mins55_Trawl4.1:
#season count   min    Q1 median    Q3   max  mean    sd
#1 Fall    2857 0.185 0.210  0.307 0.510 0.880 0.383 0.194
#2 Spring  2857 0.176 0.219  0.323 0.508 0.920 0.387 0.191
#3 Summer  2857 0.318 0.459  0.568 0.667 0.996 0.586 0.167
#4 Winter  2857 0.157 0.178  0.280 0.480 0.857 0.354 0.194

#Mins41_Trawl3.4:
#season count    min     Q1 median    Q3   max  mean    sd
#1 Fall    2857 0.0653 0.0772  0.166 0.352 0.695 0.226 0.166
#2 Spring  2857 0.0626 0.0842  0.180 0.350 0.797 0.232 0.166
#3 Summer  2857 0.154  0.336   0.461 0.593 0.987 0.491 0.201
#4 Winter  2857 0.0542 0.0649  0.149 0.323 0.657 0.207 0.157

#Mins29_Trawl2.9:
#season count    min     Q1 median    Q3   max  mean     sd
#1 Fall    2857 0.0218 0.0262 0.0796 0.195 0.448 0.117 0.101 
#2 Spring  2857 0.0207 0.0286 0.0876 0.194 0.576 0.120 0.103 
#3 Summer  2857 0.0623 0.213  0.330  0.521 0.960 0.388 0.220 
#4 Winter  2857 0.0178 0.0217 0.0688 0.178 0.401 0.103 0.0908

#Mins10_Trawl2.9:
#season count     min      Q1 median     Q3    max   mean     sd
#1 Fall    2857 0.00322 0.00390 0.0158 0.0435 0.119  0.0245 0.0232
#2 Spring  2857 0.00305 0.00425 0.0175 0.0433 0.189  0.0260 0.0258
#3 Summer  2857 0.0103  0.0573  0.137  0.346  0.808  0.211  0.184 
#4 Winter  2857 0.00261 0.00320 0.0131 0.0382 0.0971 0.0209 0.0199