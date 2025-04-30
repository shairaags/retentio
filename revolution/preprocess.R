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
  message("\U0001F4C2 Lecture d'un fichier QC spécial :", filename)
  
  df <- read_csv2(file_path, show_col_types = FALSE)
  if (nrow(df) == 0) return(tibble())
  
  cols <- names(df)
  has_long_structure <- all(c("Name", "Sample", "Area") %in% cols)
  if (!has_long_structure) {
    warning(paste0("❌ Fichier QC non reconnu comme format long : ", filename))
    return(tibble())
  }
  
  df <- df %>%
    rename(Compound = Name) %>%
    mutate(
      Sample = Sample,
      Date = suppressWarnings(ymd(str_sub(Sample, 1, 10))),
      Area = as.numeric(str_replace(as.character(Area), ",", ".")),
      CV = NA_real_,
      Flagged = FALSE,
      Type = "Analyte",
      source_file = filename
    )
  
  if ("1st Dimension" %in% cols) {
    df <- df %>% mutate(RT = `1st Dimension`)
  } else {
    df <- df %>% mutate(RT = NA_character_)
  }
  
  sequence_name <- str_extract(tools::file_path_sans_ext(filename), "\\d{8}[a-z]?")
  if (is.na(sequence_name)) sequence_name <- tools::file_path_sans_ext(filename)
  
  df <- df %>%
    mutate(Sequence = sequence_name) %>%
    group_by(Compound) %>%
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

preprocess_folder_tenax <- function(dir_path) {
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble())
  
  all_data <- map_dfr(files, function(fp) {
    filename <- basename(fp)
    if (str_starts(filename, "QC_")) {
      preprocess_QC_file(fp, filename)
    } else {
      preprocess_single_file(fp)
    }
  })
  
  if (nrow(all_data) == 0) return(tibble())
  
  summary_table <- all_data %>%
    group_by(Compound, Sequence) %>%
    summarise(
      Area_Mean = mean(Area, na.rm = TRUE),
      Area_SD = sd(Area, na.rm = TRUE),
      CV = ifelse(length(Area[!is.na(Area)]) > 1, 100 * Area_SD / Area_Mean, NA_real_),
      .groups = "drop"
    )
  
  # Ordre correct des colonnes : Area_DATE / CV_DATE (intercalé par date)
  area_wide <- summary_table %>%
    select(Compound, Sequence, Area_Mean) %>%
    pivot_wider(names_from = Sequence, values_from = Area_Mean, names_prefix = "Area_")
  
  cv_wide <- summary_table %>%
    select(Compound, Sequence, CV) %>%
    pivot_wider(names_from = Sequence, values_from = CV, names_prefix = "CV_")
  
  # Fusion et tri des colonnes dans l’ordre souhaité
  all_cols <- union(
    str_remove(names(area_wide)[-1], "^Area_"),
    str_remove(names(cv_wide)[-1], "^CV_")
  ) %>% sort()
  
  ordered_names <- c("Compound", as.vector(rbind(
    paste0("Area_", all_cols),
    paste0("CV_", all_cols)
  )))
  
  final_table <- left_join(area_wide, cv_wide, by = "Compound") %>%
    select(any_of(ordered_names))
  
  return(final_table)
}


#noni