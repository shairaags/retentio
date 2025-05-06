# qualityChecks.R

library(dplyr)
library(stringr)

# ---- Fonction principale : flag_anomalies ----
flag_anomalies <- function(data, rt_tolerance = 0.2, min_area_threshold = 1000) {
  
  data_checked <- data %>%
    # Ne tente de convertir RT que si elle existe
    { if ("RT" %in% names(.)) mutate(., RT = as.numeric(RT)) else . } %>%
    mutate(
      Area = as.numeric(Area)  # toujours convertir Area
    ) %>%
    group_by(Compound) %>%
    mutate(
      # Moyenne RT si colonne RT existe
      MeanRT = if ("RT" %in% names(.)) mean(RT, na.rm = TRUE) else NA_real_,
      RT_diff = if ("RT" %in% names(.)) abs(RT - MeanRT) else NA_real_,
      
      Flag_RT = if ("RT" %in% names(.)) RT_diff > rt_tolerance else FALSE,
      Flag_Area = Area < min_area_threshold,
      
      # Doublons si Ion existe, sinon on ignore
      Flag_Duplicate = if (all(c("Ion", "RT") %in% names(.))) {
        duplicated(paste(Compound, RT, Ion, Sequence, sep = "_"))
      } else {
        FALSE
      },
      
      Flagged = Flag_RT | Flag_Area | Flag_Duplicate
    ) %>%
    ungroup()
  
  return(data_checked)
}

# ---- Fonction secondaire : tag_high_cv ----
tag_high_cv <- function(data, cv_threshold = 30) {
  
  cv_table <- data %>%
    filter(!is.na(Area)) %>%
    group_by(Compound, Sequence) %>%
    summarise(
      mean_area = mean(Area, na.rm = TRUE),
      sd_area = sd(Area, na.rm = TRUE),
      cv = 100 * sd_area / mean_area,
      .groups = "drop"
    ) %>%
    mutate(
      CV_flag = cv > cv_threshold
    )
  
  return(cv_table)
}

#noni