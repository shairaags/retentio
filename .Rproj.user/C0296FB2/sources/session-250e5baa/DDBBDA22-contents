library(tidyverse)
library(lubridate)
library(readxl)

# ----------- FICHIER BRUT CLASSIQUE -----------

preprocess_single_file <- function(filepath) {
  df <- tryCatch(read_csv2(filepath, show_col_types = FALSE), error = function(e) return(tibble()))
  if (nrow(df) == 0) return(tibble())
  
  cols <- names(df)
  rename_list <- list(Sample = "Sample", Compound = "Name", Area = "Area")
  if ("R.T. (min)" %in% cols) rename_list$RT <- "R.T. (min)"
  if ("Quant Masses" %in% cols) rename_list$Ion <- "Quant Masses"
  
  df <- df %>%
    rename(!!!rename_list) %>%
    mutate(
      Area = as.numeric(str_replace(as.character(Area), ",", ".")),
      Sequence = tools::file_path_sans_ext(basename(filepath)),
      source_file = basename(filepath),
      Date = suppressWarnings(ymd(str_sub(Sample, 1, 10))),
      CV = NA_real_,
      Flagged = FALSE,
      Type = "Analyte"
    ) %>%
    relocate(Compound, Sample, Date, Area, CV, Type, Sequence, source_file, Flagged)
  
  return(df)
}

# ----------- FICHIER QC SPÉCIAL -----------

preprocess_QC_file <- function(file_path, filename) {
  df <- read_csv2(file_path, show_col_types = FALSE)
  if (nrow(df) == 0) return(tibble())
  
  df <- df %>%
    rename(Compound = Name) %>%
    mutate(
      Area = as.numeric(str_replace(as.character(Area), ",", ".")),
      Sequence = tools::file_path_sans_ext(basename(filename)),  # ← ⚠️ clé ici
      Date = suppressWarnings(ymd(str_extract(filename, "\\d{8}"))),  # ← vraie date
      CV = NA_real_,
      Flagged = FALSE,
      Type = "Analyte",
      source_file = filename
    )
  
  if ("1st Dimension" %in% names(df)) {
    df <- df %>% mutate(RT = `1st Dimension`)
  } else {
    df <- df %>% mutate(RT = NA_character_)
  }
  
  df <- df %>%
    group_by(Compound, Sequence) %>%
    mutate(CV = ifelse(n() > 1,
                       100 * sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE),
                       NA_real_)) %>%
    ungroup() %>%
    relocate(Compound, Sample, Date, Area, CV, Type, Sequence, source_file, Flagged)
  
  return(df)
}


# ----------- FICHIER FORMATÉ (TABLEAU FINAL) -----------

preprocess_smart <- function(file_path, filename, sheet_name = NULL) {
  if (grepl("^QC_", basename(filename))) {
    message("\U0001F4C2 Fichier QC détecté")
    return(preprocess_QC_file(file_path, filename))
  }
  
  df <- tryCatch(read_csv2(file_path, show_col_types = FALSE), error = function(e) return(tibble()))
  if (nrow(df) == 0) return(tibble())
  
  cols_lower <- tolower(names(df))
  
  if (all(c("sample", "name", "r.t. (min)", "quant masses", "area") %in% cols_lower)) {
    message("\U0001F9EA Fichier Tenax brut détecté")
    return(preprocess_single_file(file_path))
  }
  
  if (any(str_detect(names(df), "^Area_\\d{4}-\\d{2}-\\d{2}[a-z]?$"))) {
    message("📄 Détecté : fichier CSV formaté (Area/CV par date)")
    
    area_cols <- names(df)[str_detect(names(df), "^Area_\\d{4}-\\d{2}-\\d{2}[a-z]?$")]
    cv_cols   <- names(df)[str_detect(names(df), "^CV_\\d{4}-\\d{2}-\\d{2}[a-z]?$")]
    
    area_long <- df %>%
      select(Compound, all_of(area_cols)) %>%
      pivot_longer(-Compound, names_to = "Measure", values_to = "Area") %>%
      mutate(Date = str_remove(Measure, "^Area_"))
    
    cv_long <- df %>%
      select(Compound, all_of(cv_cols)) %>%
      pivot_longer(-Compound, names_to = "Measure", values_to = "CV") %>%
      mutate(Date = str_remove(Measure, "^CV_"))
    
    df_long <- left_join(area_long %>% select(-Measure),
                         cv_long %>% select(-Measure),
                         by = c("Compound", "Date")) %>%
      mutate(
        Date = ymd(Date),
        Area = as.numeric(as.character(Area)),
        CV   = as.numeric(as.character(CV)),
        Type = case_when(
          str_detect(tolower(Compound), "fame") ~ "FAME",
          str_detect(tolower(Compound), "-d[0-9]+") ~ "Étalon Interne",
          TRUE ~ "Analyte"
        ),
        Sequence = paste0("Formaté_", Date),
        source_file = filename,
        Flagged = FALSE
      ) %>%
      relocate(Compound, Date, Area, CV, Type, Sequence, source_file, Flagged)
    
    return(df_long)
  }
  
  if (all(c("compound", "area") %in% cols_lower)) {
    message("⚡ Fichier allégé détecté")
    
    df <- df %>%
      mutate(
        Compound = as.character(Compound),
        Area = as.numeric(str_replace(as.character(Area), ",", ".")),
        RT = NA_real_,
        Ion = NA_character_,
        Date = Sys.Date(),
        CV = NA_real_,
        Type = "Analyte",
        Sequence = "Allégé",
        source_file = filename,
        Flagged = FALSE
      ) %>%
      relocate(Compound, Date, Area, CV, Type, Sequence, source_file, Flagged)
    
    return(df)
  }
  
  stop(paste0("❌ Structure inconnue pour le fichier : ", filename))
}

# ----------- TRAITEMENT DOSSIER TENAX -----------
# ----------- TRAITEMENT DOSSIER TENAX -----------
preprocess_folder_tenax <- function(dir_path) {
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble())
  
  all_tables <- map(files, function(filepath) {
    df <- tryCatch(read_csv2(filepath, show_col_types = FALSE), error = function(e) return(tibble()))
    if (nrow(df) == 0) return(tibble())
    
    # Renommage
    if ("Name" %in% names(df)) df <- df %>% rename(Compound = Name)
    if (!all(c("Compound", "Sample", "Area") %in% names(df))) return(tibble())
    
    df <- df %>%
      mutate(
        Area = as.numeric(str_replace(as.character(Area), ",", ".")),
        Sample = as.character(Sample)
      )
    
    file_label <- basename(filepath) %>%
      tools::file_path_sans_ext() %>%
      str_replace_all("[^A-Za-z0-9]", "_")
    
    area_col <- paste0("Area_", file_label)
    
    df %>%
      group_by(Compound) %>%
      summarise(
        !!area_col := mean(Area, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  all_tables <- all_tables[map_lgl(all_tables, ~ nrow(.) > 0)]
  if (length(all_tables) == 0) return(tibble())
  
  # Fusion horizontale
  wide_data <- reduce(all_tables, full_join, by = "Compound") %>%
    select(Compound, everything())
  
  # Calculs finaux : Moyenne globale + CV global
  area_values <- wide_data %>%
    select(starts_with("Area_")) %>%
    mutate_all(as.numeric)
  
  wide_data <- wide_data %>%
    mutate(
      Mean_Area = rowMeans(area_values, na.rm = TRUE),
      CV_Global = apply(area_values, 1, function(x) {
        if (sum(!is.na(x)) <= 1) return(NA_real_)
        100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
      })
    )
  
  return(wide_data)
}
