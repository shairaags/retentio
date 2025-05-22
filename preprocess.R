library(tidyverse)
library(lubridate)
library(readxl)

# 🔧 Lecture CSV robuste (gère aussi les colonnes collées entre guillemets)
read_csv_tenax_robust <- function(filepath) {
  first_line <- readLines(filepath, n = 1, warn = FALSE)
  
  # Cas collé avec guillemets
  if (str_detect(first_line, '^"[^"]+","[^"]+')) {
    df <- tryCatch(read.csv(filepath, quote = '"', stringsAsFactors = FALSE), error = function(e) return(tibble()))
    
    # Cas brut classique avec virgules
  } else if (str_count(first_line, ",") > str_count(first_line, ";")) {
    df <- tryCatch(read.csv(filepath, stringsAsFactors = FALSE), error = function(e) return(tibble()))
    
    # Cas séparateur point-virgule classique
  } else {
    df <- tryCatch(read.csv2(filepath, stringsAsFactors = FALSE), error = function(e) return(tibble()))
  }
  
  names(df) <- str_replace_all(names(df), '"', "")
  return(df)
}



# ---- FONCTION POUR DÉCOLLER UN FICHIER BRUT COLLÉ EN UNE SEULE COLONNE ----

# # Nouvelle fonction plus robuste
# try_fix_csv_with_quotes <- function(file_path) {
#   # Lecture ligne brute
#   raw_lines <- readLines(file_path, warn = FALSE)
#   if (length(raw_lines) < 2) return(NULL)
#   
#   # Teste présence de colonnes collées mais avec guillemets
#   if (!any(str_detect(raw_lines[1], '","'))) return(NULL)  # Pas le bon format
#   
#   # Lecture avec gestion des guillemets et virgules
#   df <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE, quote = '"'), error = function(e) return(NULL))
#   if (is.null(df) || ncol(df) < 3) return(NULL)
#   
#   # Vérifie les colonnes attendues
#   colnames(df)[1:3] <- c("Sample", "Name", "Area")
#   df$Name <- str_replace_all(df$Name, " acid, methyl ester", " acid methyl ester")
#   
#   # Sauvegarde vers fichier temporaire utilisable downstream
#   tmp_path <- tempfile(fileext = ".csv")
#   write_csv2(df, tmp_path)
#   return(tmp_path)
# }

try_fix_csv_with_quotes <- function(file_path) {
  raw_lines <- readLines(file_path, warn = FALSE)
  if (length(raw_lines) < 2) return(NULL)
  if (!any(str_detect(raw_lines[1], '","'))) return(NULL)
  df <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE, quote = '"'), error = function(e) return(NULL))
  if (is.null(df) || ncol(df) < 3) return(NULL)
  colnames(df)[1:3] <- c("Sample", "Name", "Area")
  df$Name <- str_replace_all(df$Name, " acid, methyl ester", " acid methyl ester")
  tmp_path <- tempfile(fileext = ".csv")
  write_csv2(df, tmp_path)
  return(tmp_path)
}






# ----------- FICHIER BRUT CLASSIQUE -----------

# ----------- FICHIER BRUT CLASSIQUE -----------

preprocess_single_file <- function(filepath) {
  
  # 🧪 Essai de réparation avec CSV à guillemets + virgules
  fixed_path <- try_fix_csv_with_quotes(filepath)
  if (!is.null(fixed_path)) {
    message("🧬 Fichier brut collé avec guillemets détecté et corrigé automatiquement.")
    filepath <- fixed_path
  }
  
  # Puis fallback classique si encore échoué
  if (!file.exists(filepath)) return(tibble())
  
  df <- tryCatch(read.csv2(filepath, stringsAsFactors = FALSE), error = function(e) return(tibble()))
  if (nrow(df) == 0) return(tibble())
  
  # ✅ ENLÈVE les guillemets des noms de colonnes
  names(df) <- str_replace_all(names(df), '"', "")
  
  if (nrow(df) == 0) return(tibble())
  
  # ✅ CORRECTION OBLIGATOIRE : enlever les guillemets des noms de colonnes
  names(df) <- str_replace_all(names(df), '"', "")  # ← C'est ici le fix essentiel
  
  # ✅ Nettoyage des colonnes
  if ("Name" %in% names(df)) df <- df %>% rename(Compound = Name)
  if (!("Sample" %in% names(df))) stop("❌ Fichier sans colonne 'Sample' — structure non valide.")
  if (!("Area" %in% names(df))) stop("❌ Fichier sans colonne 'Area' — structure non valide.")
  
  df <- df %>%
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
  fixed_path <- try_split_single_column_csv(file_path)
  if (!is.null(fixed_path)) {
    message("🧬 Fichier brut collé détecté et corrigé automatiquement.")
    return(preprocess_single_file(fixed_path))
  }
  
  df <- tryCatch(read.csv2(file_path, stringsAsFactors = FALSE), error = function(e) return(tibble()))
  if (nrow(df) == 0) return(tibble())
  
  # 🔧 Nettoyage des noms
  names(df) <- str_replace_all(names(df), '"', "")
  
  df <- df %>%
    rename(Compound = Name) %>%
    mutate(
      Area = as.numeric(str_replace(as.character(Area), ",", ".")),
      Sequence = tools::file_path_sans_ext(basename(filename)),
      Date = suppressWarnings(ymd(str_extract(filename, "\\d{8}"))),
      CV = NA_real_,
      Flagged = FALSE,
      Type = "Analyte",
      source_file = filename
    )
  
  df <- if ("1st Dimension" %in% names(df)) {
    df %>% mutate(RT = `1st Dimension`)
  } else {
    df %>% mutate(RT = NA_character_)
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

# ----------- FICHIER FORMATÉ (TABLEAU FINAL) -----------

preprocess_smart <- function(file_path, filename, sheet_name = NULL) {
  if (grepl("^QC_", basename(filename))) {
    message("\U0001F4C2 Fichier QC détecté")
    return(preprocess_QC_file(file_path, filename))
  }
  
  # 🔍 Tentative de réparation fichier collé
  fixed_path <- try_split_single_column_csv(file_path)
  if (!is.null(fixed_path)) {
    message("🧬 Fichier brut collé détecté et corrigé automatiquement.")
    return(preprocess_single_file(fixed_path))  # injection dans le flux normal
  }
  
  # ✅ Lecture robuste + nettoyage noms
  df <- tryCatch(read.csv2(file_path, stringsAsFactors = FALSE), error = function(e) return(tibble()))
  if (nrow(df) == 0) return(tibble())
  
  # ✅ Nettoyage des guillemets dans les noms de colonnes
  names(df) <- str_replace_all(names(df), '"', "")
  
  cols_lower <- tolower(names(df))
  
  if (all(c("sample", "name", "area") %in% cols_lower)) {
    message("\U0001F9EA Fichier Tenax brut simplifié détecté")
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
  
  all_tables <- list()
  
  for (filepath in files) {
    # Lecture robuste
    raw_lines <- readLines(filepath, warn = FALSE)
    if (str_detect(raw_lines[1], '^"[^"]+","[^"]+')) {
      df <- tryCatch(read.csv(filepath, quote = '"', stringsAsFactors = FALSE), error = function(e) return(tibble()))
    } else if (str_count(raw_lines[1], ",") > str_count(raw_lines[1], ";")) {
      df <- tryCatch(read.csv(filepath, stringsAsFactors = FALSE), error = function(e) return(tibble()))
    } else {
      df <- tryCatch(read.csv2(filepath, stringsAsFactors = FALSE), error = function(e) return(tibble()))
    }
    
    if (nrow(df) == 0) next
    names(df) <- str_replace_all(names(df), '"', "")
    if (!all(c("Sample", "Name", "Area") %in% names(df))) next
    
    df <- df %>%
      rename(Compound = Name) %>%
      mutate(
        Area = as.numeric(str_replace(as.character(Area), ",", ".")),
        Sample = as.character(Sample)
      )
    
    # Nom de la colonne : Area_<nom_du_fichier_sans_ext>
    file_label <- basename(filepath) %>%
      tools::file_path_sans_ext() %>%
      str_replace_all("[^A-Za-z0-9]", "_")
    
    area_col <- paste0("Area_", file_label)
    
    summarised <- df %>%
      group_by(Compound) %>%
      summarise(
        !!area_col := mean(Area, na.rm = TRUE),
        .groups = "drop"
      )
    
    all_tables[[length(all_tables) + 1]] <- summarised
  }
  
  all_tables <- all_tables[map_lgl(all_tables, ~ nrow(.) > 0)]
  if (length(all_tables) == 0) return(tibble())
  
  # Fusion horizontale
  wide_data <- reduce(all_tables, full_join, by = "Compound") %>%
    select(Compound, everything())
  
  # Calculs finaux : Moyenne globale + CV global
  area_values <- wide_data %>% select(starts_with("Area_")) %>% mutate_all(as.numeric)
  
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
