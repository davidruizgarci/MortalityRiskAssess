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
#season_vals <- c("2021")

scenarios <- list(
  "Optimal" = list(mins = "Mins10", trawl = "Trawl2.9", color = "#B2E2B2"),   # pastel green
  "Favorable" = list(mins = "Mins29", trawl = "Trawl2.9", color = "#B3DDF2"), # softened blue (more pastel)
  "Baseline"  = list(mins = "Mins41", trawl = "Trawl3.4", color = "#E0E0E0"), # slightly lighter grey
  "Adverse"   = list(mins = "Mins55", trawl = "Trawl4.1", color = "#F4A6A6")  # softened pastel red
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
  
  # Convert scenario to factor for order
  season_data$scenario <- factor(season_data$scenario, levels = c("Adverse", "Baseline", "Favorable"))
  
  # Summary stats by scenario
  summary_stats <- season_data %>%
    group_by(scenario) %>%
    summarize(
      count = n(),
      min = min(risk, na.rm = TRUE),
      Q1 = quantile(risk, 0.25, na.rm = TRUE),
      median = median(risk, na.rm = TRUE),
      Q3 = quantile(risk, 0.75, na.rm = TRUE),
      max = max(risk, na.rm = TRUE),
      mean = mean(risk, na.rm = TRUE),
      sd = sd(risk, na.rm = TRUE)
    )
  
  message("📊 Summary for season: ", season)
  print(summary_stats)
  
  # Statistical tests -----------------------------------------------------------
  message("🔍 Running Kruskal-Wallis test for ", season)
  print(kruskal.test(risk ~ scenario, data = season_data))
  
  message("🔍 Running pairwise Wilcoxon tests for ", season)
  print(pairwise.wilcox.test(season_data$risk, season_data$scenario, p.adjust.method = "bonferroni"))
  
  # Plotting --------------------------------------------------------------------
  p <- ggplot(season_data, aes(x = scenario, y = risk, fill = scenario)) +
    geom_violin(trim = FALSE, alpha = 0.4, color = NA, scale = "width") +  # width-scaled violins
    geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 1, color = "black", linewidth = 0.3) +
    scale_fill_manual(values = c(
      "Favorable" = scenarios[["Favorable"]]$color,
      "Baseline"  = scenarios[["Baseline"]]$color,
      "Adverse"   = scenarios[["Adverse"]]$color
    )) +
    ylab("Mean AVM risk (%)") +
    xlab("") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12),
      #panel.grid.major = element_line(color = "gray90"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(),
      legend.position = "none"
    )
  print(p)
  
  # Save output
  outdir <- file.path(output_data, "fig", "raincloud_scenarios")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  ggsave(file.path(outdir, paste0("AVM_scenarios_", season, ".png")), p, width = 17, height = 10, units = "cm", dpi = 300)

  # ➕ Row 3: Add dot + SD range (mean ± sd) plot
  p_summary <- ggplot(summary_stats, aes(x = scenario, y = mean, color = scenario)) +
    geom_pointrange(
      aes(ymin = mean - sd, ymax = mean + sd),
      linewidth = 0.6, size = 1.8
    ) +
    scale_color_manual(values = c(
      "Favorable" = scenarios[["Favorable"]]$color,
      "Baseline"  = scenarios[["Baseline"]]$color,
      "Adverse"   = scenarios[["Adverse"]]$color
    )) +
    ylab("Mean AVM risk (%)") +
    xlab("") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.position = "none"
    )
  
  # Preview
  print(p_summary)
  
  # Optional: export summary dot plot
  ggsave(file.path(outdir, paste0("AVM_scenarios_", season, "_mean_sd_dot.png")),
    p_summary, width = 17, height = 10, units = "cm", dpi = 300)
  }




# Results:
#📅 Processing season: Spring
#🔍 Running Kruskal-Wallis test for Spring
#Kruskal-Wallis chi-squared = 479.98, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Spring
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  

#📊 Summary for season: Spring
#scenario  count      min      Q1  median      Q3    max    mean      sd
#1 Adverse     211 0.0167   0.0666  0.101   0.123   0.180  0.0948  0.0430 
#2 Baseline    211 0.00507  0.0238  0.0465  0.0571  0.0934 0.0429  0.0230 
#3 Favorable   211 0.000261 0.00132 0.00334 0.00419 0.0110 0.00313 0.00203



#📅 Processing season: Winter
#🔍 Running Kruskal-Wallis test for Winter
#Kruskal-Wallis chi-squared = 478.43, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Winter
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  

#📊 Summary for season: Winter
#scenario  count      min       Q1  median      Q3     max    mean      sd
#1 Adverse     211 0.0156   0.0548   0.0942  0.107   0.169   0.0835  0.0406 
#2 Baseline    211 0.00474  0.0185   0.0425  0.0485  0.0806  0.0366  0.0203 
#3 Favorable   211 0.000244 0.000996 0.00295 0.00349 0.00596 0.00251 0.00152



#📅 Processing season: Fall
#🔍 Running Kruskal-Wallis test for Fall
#Kruskal-Wallis chi-squared = 479.29, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Fall
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16

#📊 Summary for season: Fall
#scenario  count      min      Q1  median      Q3     max    mean      sd
#1 Adverse     211 0.0179   0.0601  0.102   0.117   0.187   0.0914  0.0426 
#2 Baseline    211 0.00544  0.0203  0.0475  0.0556  0.0927  0.0410  0.0221 
#3 Favorable   211 0.000280 0.00110 0.00343 0.00400 0.00727 0.00291 0.00174



#📅 Processing season: Summer
#🔍 Running Kruskal-Wallis test for Summer
#Kruskal-Wallis chi-squared = 447.05, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for Summer
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  

#📊 Summary for season: Summer
#scenario  count     min     Q1 median     Q3   max   mean     sd
#1 Adverse     211 0.0634  0.281  0.345  0.426  0.688 0.351  0.113 
#2 Baseline    211 0.0232  0.170  0.229  0.297  0.501 0.231  0.0959
#3 Favorable   211 0.00132 0.0200 0.0469 0.0640 0.188 0.0482 0.0345



#📅 Processing season: 2021
#🔍 Running Kruskal-Wallis test for 2021
#Kruskal-Wallis chi-squared = 481.73, df = 2, p-value < 2.2e-16

#🔍 Running pairwise Wilcoxon tests for 2021
#Adverse Baseline
#Baseline  <2e-16  -       
#Favorable <2e-16  <2e-16  

#📊 Summary for season: 2021
#scenario  count      min      Q1  median      Q3     max    mean      sd
#1 Adverse     211 0.0190   0.0711  0.108   0.131   0.191   0.100   0.0445 
#2 Baseline    211 0.00580  0.0255  0.0509  0.0617  0.0956  0.0461  0.0241 
#3 Favorable   211 0.000299 0.00141 0.00378 0.00466 0.00993 0.00342 0.00210