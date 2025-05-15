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
