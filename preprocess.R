# preprocess.R

library(tidyverse)
library(lubridate)
library(readr)

# Fonction principale : prétraitement des fichiers CSV d’une séquence GCxGC-MS
preprocess_data <- function(path = "data") {
  
  # 1. Liste tous les fichiers CSV dans le dossier spécifié
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    warning("Aucun fichier CSV trouvé dans le dossier spécifié.")
    return(tibble())
  }
  
  # 2. Lecture et concaténation des fichiers avec enrichissement
  data_raw <- map_dfr(files, function(file) {
    df <- tryCatch(
      read_csv(file, show_col_types = FALSE),
      error = function(e) {
        warning(glue::glue("Erreur lors de la lecture du fichier : {file}"))
        return(NULL)
      }
    )
    
    if (is.null(df)) return(tibble())
    
    # Ajout de métadonnées
    sequence_name <- tools::file_path_sans_ext(basename(file))
    date_extracted <- str_extract(sequence_name, "\\d{4}-\\d{2}-\\d{2}")
    date_formatted <- suppressWarnings(ymd(date_extracted))
    
    df %>%
      mutate(
        source_file = basename(file),
        Sequence = sequence_name,
        Date = date_formatted
      )
  })
  
  # 3. Vérification des colonnes obligatoires
  required_cols <- c("Compound", "Area", "RT", "Ion")
  missing_cols <- setdiff(required_cols, colnames(data_raw))
  if (length(missing_cols) > 0) {
    stop(glue::glue("Colonnes manquantes dans les fichiers CSV : {paste(missing_cols, collapse = ', ')}"))
  }
  
  # 4. Nettoyage : suppression des lignes avec valeurs manquantes critiques
  data_clean <- data_raw %>%
    filter(!is.na(Compound), !is.na(Area), !is.na(RT), !is.na(Date))
  
  # 5. Création colonne 'Flagged' pour annoter plus tard les anomalies
  data_clean <- data_clean %>%
    mutate(Flagged = ifelse(is.na(Flagged), FALSE, Flagged))
  
  # 6. Normalisation : trim des noms de composés et mise en forme propre
  data_clean <- data_clean %>%
    mutate(Compound = str_trim(Compound),
           Ion = as.character(Ion))
  
  # 7. Retourne les données prêtes à être utilisées
  return(data_clean)
}



















# # preprocess.R
# 
# # Chargement des bibliothèques nécessaires
# library(tidyverse)   # Ensemble complet pour la manipulation de données (dplyr, ggplot2, etc.)
# library(lubridate)   # Pour gérer les dates (par ex. extraire et formater une date)
# library(readr)       # Pour lire efficacement des fichiers CSV
# 
# # Fonction principale : prétraitement des fichiers CSV issus d'une séquence GCxGC-MS
# # Argument :
# #   - path : chemin du dossier contenant les fichiers CSV à traiter (par défaut : "data")
# preprocess_data <- function(path = "data") {
#   
#   # Étape 1 : recherche de tous les fichiers .csv dans le dossier spécifié
#   files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)  # Retourne les chemins complets
#   if (length(files) == 0) {
#     # Si aucun fichier trouvé, afficher un avertissement et retourner un tibble vide
#     warning("Aucun fichier CSV trouvé dans le dossier spécifié.")
#     return(tibble())
#   }
#   
#   # Étape 2 : lecture de chaque fichier CSV et fusion dans un seul tableau
#   data_raw <- map_dfr(files, function(file) {
#     # Lecture sécurisée du fichier CSV avec gestion des erreurs
#     df <- tryCatch(
#       read_csv(file, show_col_types = FALSE),  # Lecture du fichier sans afficher les types de colonnes
#       error = function(e) {
#         # Si erreur, afficher un warning et retourner NULL
#         warning(glue::glue("Erreur lors de la lecture du fichier : {file}"))
#         return(NULL)
#       }
#     )
#     
#     # Si la lecture a échoué, retourner un tibble vide pour ce fichier
#     if (is.null(df)) return(tibble())
#     
#     # Extraction du nom de la séquence depuis le nom du fichier (sans extension)
#     sequence_name <- tools::file_path_sans_ext(basename(file))
#     
#     # Tentative d'extraction d'une date dans le nom du fichier (format AAAA-MM-JJ)
#     date_extracted <- str_extract(sequence_name, "\\d{4}-\\d{2}-\\d{2}")
#     date_formatted <- suppressWarnings(ymd(date_extracted))  # Transformation en format Date, sans afficher d’avertissement
#     
#     # Ajout des métadonnées (nom du fichier, nom de la séquence, date)
#     df %>%
#       mutate(
#         source_file = basename(file),     # Nom du fichier source
#         Sequence = sequence_name,         # Nom de la séquence (souvent utile pour suivi analytique)
#         Date = date_formatted             # Date extraite du nom de la séquence
#       )
#   })
#   
#   # Étape 3 : vérifie que certaines colonnes critiques sont bien présentes
#   required_cols <- c("Compound", "Area", "RT", "Ion")  # Colonnes minimales attendues
#   missing_cols <- setdiff(required_cols, colnames(data_raw))  # Liste des colonnes manquantes
#   if (length(missing_cols) > 0) {
#     # Si des colonnes critiques manquent, arrêter l’exécution avec un message d’erreur
#     stop(glue::glue("Colonnes manquantes dans les fichiers CSV : {paste(missing_cols, collapse = ', ')}"))
#   }
#   
#   # Étape 4 : nettoyage des lignes contenant des valeurs NA critiques
#   data_clean <- data_raw %>%
#     filter(!is.na(Compound), !is.na(Area), !is.na(RT), !is.na(Date))  # On garde uniquement les lignes complètes
#   
#   # Étape 5 : ajout d’une colonne 'Flagged' (initialement FALSE) pour annoter plus tard les anomalies
#   data_clean <- data_clean %>%
#     mutate(Flagged = ifelse(is.na(Flagged), FALSE, Flagged))  # S’assure que la colonne existe et ne contient pas de NA
#   
#   # Étape 6 : nettoyage des noms de composés et mise au format texte des ions
#   data_clean <- data_clean %>%
#     mutate(
#       Compound = str_trim(Compound),      # Supprime les espaces en début/fin de nom de composé
#       Ion = as.character(Ion)             # S’assure que les ions sont bien en format texte
#     )
#   
#   # Étape 7 : retourne les données prêtes à être utilisées dans l’analyse ou Shiny
#   return(data_clean)
# }










# preprocess.R

library(tidyverse)
library(lubridate)
library(readr)

preprocess_data <- function(path = "data") {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble())
  
  data_raw <- map_dfr(files, function(file) {
    df <- tryCatch(read_csv(file, show_col_types = FALSE), error = function(e) return(NULL))
    if (is.null(df)) return(tibble())
    
    sequence_name <- tools::file_path_sans_ext(basename(file))
    date_extracted <- str_extract(sequence_name, "\\d{4}-\\d{2}-\\d{2}")
    date_formatted <- suppressWarnings(ymd(date_extracted))
    
    df %>%
      mutate(
        source_file = basename(file),
        Sequence = sequence_name,
        Date = date_formatted
      )
  })
  
  required_cols <- c("Compound", "Area", "RT", "Ion")
  missing_cols <- setdiff(required_cols, colnames(data_raw))
  if (length(missing_cols) > 0) {
    stop(glue::glue("Colonnes manquantes dans les fichiers CSV : {paste(missing_cols, collapse = ', ')}"))
  }
  
  data_clean <- data_raw %>%
    filter(!is.na(Compound), !is.na(Area), !is.na(RT), !is.na(Date)) %>%
    mutate(
      Compound = str_trim(Compound),
      Ion = as.character(Ion),
      Flagged = ifelse(is.na(Flagged), FALSE, Flagged)
    )
  
  return(data_clean)
}
