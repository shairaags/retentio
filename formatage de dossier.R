
library(readxl)


input_dir <- "C:/Users/Masspeclab/Desktop/Suivi_EIs_Tenax_GCxGC"


output_dir <- file.path(input_dir, "csv_export_excel_fixes")
dir.create(output_dir, showWarnings = FALSE)


files <- list.files(input_dir, pattern = "\\.(csv|xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)


for (file in files) {
  message("📄 Traitement de : ", basename(file))
  
  
  success <- FALSE
  try({
    sheets <- excel_sheets(file)
    df <- read_excel(file, sheet = sheets[1])
    success <- TRUE
  }, silent = TRUE)
  
  
  if (!success) {
    try({
      df <- read.csv2(file, stringsAsFactors = FALSE)
      success <- TRUE
    }, silent = TRUE)
  }
  
  
  if (!success) {
    message("❌ Échec de lecture : ", basename(file))
    next
  }
  
  
  output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), "_clean.csv"))
  write.csv2(df, file = output_file, row.names = FALSE)
  message("✅ Exporté vers : ", output_file)
}






input_dir <- "C:/Users/Masspeclab/Desktop/15052025 10h46 et après/Suivi_Els_Tenax_GCxGC"



output_dir <- file.path(input_dir, "csv_export")
dir.create(output_dir, showWarnings = FALSE)


files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)


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
    
    
    file_basename <- basename(file)
    date_raw <- stringr::str_extract(file_basename, "\\d{8}")  # format 25042025
    if (is.na(date_raw)) next
    
    output_file <- file.path(output_dir, paste0("clean_", date_raw, ".csv"))
    write.csv2(d, file = output_file, row.names = FALSE)
    message("✅ Exporté : ", output_file)
  }
}



path<-"C:/Users/Masspeclab/Desktop/Suivi_EIs_Tenax_GCxGC"
files <- list.files(path, pattern = "^QC_.*\\.csv$", full.names = TRUE)


df_all <- purrr::map_dfr(files, function(f) {
  df <- read_csv2(f, show_col_types = FALSE)
  df <- df %>%
    filter(!is.na(Area)) %>%
    mutate(
      Compound = as.character(Name),
      Area = as.numeric(Area),
      Sample = as.character(Sample),
      File = basename(f),
      TubeID = str_extract(basename(f), "QC_\\d+"),
      Date = as.Date(str_extract(basename(f), "\\d{8}"), "%d%m%Y")
    )
})


f<-files[1]
df <- read_csv2(f, show_col_types = FALSE)

as.data.frame(Reduce(rbind,apply(df,1,function(x) strsplit(x = x,split = ",",fixed = TRUE)[[1]])))
