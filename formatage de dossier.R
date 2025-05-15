#pr transformer en fichier csv

# 📦 Charge les packages nécessaires
library(readxl)

# 📁 Dossier contenant tes faux CSV
input_dir <- "C:/Users/Masspeclab/Desktop/15052025 10h46/Suivi_Els_Tenax_GCxGC"

# 📂 Dossier de sortie
output_dir <- file.path(input_dir, "csv_export_excel_fixes")
dir.create(output_dir, showWarnings = FALSE)

# 🔍 Liste tous les fichiers avec extension .csv, .xls, .xlsx
files <- list.files(input_dir, pattern = "\\.(csv|xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)

# 🔁 Conversion de chaque fichier
for (file in files) {
  message("📄 Traitement de : ", basename(file))
  
  # Essaye Excel en priorité (au cas où fichier Excel renommé)
  success <- FALSE
  try({
    sheets <- excel_sheets(file)
    df <- read_excel(file, sheet = sheets[1])
    success <- TRUE
  }, silent = TRUE)
  
  # Si Excel échoue, tente CSV standard
  if (!success) {
    try({
      df <- read.csv2(file, stringsAsFactors = FALSE)
      success <- TRUE
    }, silent = TRUE)
  }
  
  # Si toujours pas lisible, on skippe
  if (!success) {
    message("❌ Échec de lecture : ", basename(file))
    next
  }
  
  # Sauvegarde propre en CSV
  output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), "_clean.csv"))
  write.csv2(df, file = output_file, row.names = FALSE)
  message("✅ Exporté vers : ", output_file)
}

message("🎉 Tous les fichiers valides ont été convertis dans : ", output_dir)



# 📁 Dossier contenant les fichiers Excel convertis
input_dir <- "C:/Users/Masspeclab/Desktop/15052025 10h46/Suivi_Els_Tenax_GCxGC/csv_export_excel_fixes"

# 📂 Dossier de sortie final
output_dir <- file.path(input_dir, "csv_export")
dir.create(output_dir, showWarnings = FALSE)

# 📄 Liste tous les fichiers CSV
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

# 🔁 Boucle sur chaque fichier
for (file in files) {
  message("📄 Traitement de : ", basename(file))
  
  data <- read.csv2(file, stringsAsFactors = FALSE)
  index <- grep("Mol|Name", colnames(data))
  if (length(index) == 0) next  # aucun tableau détecté
  
  data_list <- list()
  for (j in seq_along(index)) {
    end_col <- if (j < length(index)) index[j + 1] - 1 else ncol(data)
    data_list[[j]] <- data[, index[j]:end_col]
  }
  
  for (k in seq_along(data_list)) {
    data_sub <- data_list[[k]]
    data_sub <- data_sub[data_sub[, 1] != "", ]
    if (ncol(data_sub) < 4 || nrow(data_sub) == 0) next
    
    d <- data.frame(
      Sample = data_sub[, 2],
      Name   = data_sub[, 1],
      Area   = data_sub[, 4],
      stringsAsFactors = FALSE
    )
    
    # 📆 Extraire la date du nom du fichier
    file_basename <- basename(file)
    date_raw <- stringr::str_extract(file_basename, "\\d{8}")  # format 25042025
    if (is.na(date_raw)) next
    
    output_file <- file.path(output_dir, paste0("clean_", date_raw, ".csv"))
    write.csv2(d, file = output_file, row.names = FALSE)
    message("✅ Exporté : ", output_file)
  }
}

message("🎉 Tous les fichiers ont été exportés dans : ", output_dir)