# ------------------------------------------------------------------------------

# Title: Small-sized and deepwater chondrichthyans face increased mortality risk in bottom trawling

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 6_8_diff_scenario  Assess differences among scenarios 
#-------------------------------------------------------------------------------

library(tidyverse)
library(raster)
library(ggplot2)
library(smplot2)

# 1. Set constants --------------------------------------------------------------
season_vals <- c("Spring", "Winter", "Fall", "Summer", "2021")

scenarios <- list(
  "Favorable" = list(mins = "Mins10", trawl = "Trawl2.9"),
  "Baseline"  = list(mins = "Mins41", trawl = "Trawl3.4"),
  "Adverse"   = list(mins = "Mins55", trawl = "Trawl4.1")
)

# 2. Loop through each season ---------------------------------------------------
for (season in season_vals) {
  #season <- season_vals[1]
  message("📅 Processing season: ", season)
  data_list <- list()
  
  for (scenario_name in names(scenarios)) {
   #scenario_name <- names(scenarios)[1]
    mins <- scenarios[[scenario_name]]$mins
    trawl <- scenarios[[scenario_name]]$trawl
    
    indir <- file.path(output_data, "predict_across_sp", "2021", paste0(mins, "_", trawl))
    file_mean <- file.path(indir, paste0(season, "_across_sp_pred_mean.tif"))
    
    if (!file.exists(file_mean)) {
      message("⚠️ Missing file for ", scenario_name, " in ", season)
      next
    }
    
    r <- raster(file_mean)
    df <- as.data.frame(r, xy = TRUE)
    colnames(df) <- c("x", "y", "risk")
    df <- df %>% filter(!is.na(risk))
    df$scenario <- scenario_name
    data_list[[scenario_name]] <- df
  }
  
  # Combine data
  season_data <- bind_rows(data_list)
  
  # Statistical tests -----------------------------------------------------------
  message("🔍 Running Kruskal-Wallis test for ", season)
  print(kruskal.test(risk ~ scenario, data = season_data))
  
  message("🔍 Running pairwise Wilcoxon tests for ", season)
  print(pairwise.wilcox.test(season_data$risk, season_data$scenario, p.adjust.method = "bonferroni"))
  
  # Plotting --------------------------------------------------------------------
  p <- ggplot(data = season_data, aes(x = scenario, y = risk)) +
    geom_violin(aes(fill = scenario), trim = FALSE, alpha = 0.5, color = "transparent") +
    geom_boxplot(aes(fill = scenario), width = 0.1, outlier.shape = NA) +
    scale_fill_manual(values = c(
      "Favorable" = "#ADD8E6",  # pastel blue
      "Baseline"  = "#D3D3D3",  # pastel grey
      "Adverse"   = "#FA8072"   # pastel red
    )) +
    ylab("Mean AVM risk (%)") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    ) +
    ggtitle(paste0("Scenario Comparison – ", season))
  
  print(p)
  
  # Save output
  outdir <- file.path(output_data, "fig", "raincloud_scenarios")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  ggsave(file.path(outdir, paste0("AVM_scenarios_", season, ".png")), p, width = 17, height = 10, units = "cm", dpi = 300)
}


# Results:
#📅 Processing season: Spring
#🔍 Running Kruskal-Wallis test for Spring
#Kruskal-Wallis chi-squared = 479.98, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Spring
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  


#📅 Processing season: Winter
#🔍 Running Kruskal-Wallis test for Winter
#Kruskal-Wallis chi-squared = 478.43, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Winter
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  


#📅 Processing season: Fall
#🔍 Running Kruskal-Wallis test for Fall
#Kruskal-Wallis chi-squared = 479.29, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Fall
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  

#📅 Processing season: Summer
#🔍 Running Kruskal-Wallis test for Summer
#Kruskal-Wallis chi-squared = 447.05, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Summer
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  

#📅 Processing season: 2021
#🔍 Running Kruskal-Wallis test for 2021
#Kruskal-Wallis chi-squared = 481.73, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for 2021
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  