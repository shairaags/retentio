library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(readxl)
library(shinyFiles)
library(shinyjs)


source("preprocess.R")

source("qualityChecks.R")


# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Retentio - Suivi Métabolomique Plasma"),
  dashboardSidebar(
    fileInput(
      "file_upload", 
      "Ajouter vos fichiers (.xlsx ou .csv)", 
      accept = c(".xlsx", ".csv"), 
      multiple = TRUE
    ),
    # fileInput("file_upload", "Ajouter un ou plusieurs fichiers Excel (.xlsx)", accept = ".xlsx", multiple = TRUE),
    # fileInput("csv_upload", "Ajouter des fichiers CSV (.csv)", multiple = TRUE, accept = ".csv"),
    # Section pour Tenax
    shinyDirButton("tenax_dir", "📁 Choisir dossier Tenax", "Sélectionner dossier Tenax"),
    downloadButton("tenax_export", "📉 Fichier formaté Tenax"),
    
    tags$br(), tags$br(),
    
    # Section pour Plasma
    shinyDirButton("dir_plasma", "📁 Choisir dossier Plasma", "Sélectionner dossier Plasma"),
    downloadButton("download_plasma", "📊 Fichier formaté Plasma"),
    tags$hr(),
    uiOutput("sheet_selector"),
    actionButton("refresh", "Rafraîchir les données"),
    actionButton("reset", "Réinitialiser les filtres"),
    tags$hr(),
    h4("Filtres"),
    pickerInput("analyte", "Choisir un composé :", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE)),
    pickerInput("multi_analytes", "Selection multiple ^^ :", 
                choices = NULL, multiple = TRUE, selected = NULL,
                options = list(`actions-box` = TRUE, `live-search` = TRUE)), #a modifier deuxieme bouton dois etre plus performant
    pickerInput("sequence", "Séquences :", choices = NULL, multiple = TRUE),
    pickerInput("type", "Type de composé :", choices = c("Tous", "FAME", "Étalon Interne", "Analyte")),
    checkboxInput("flag_only", "Voir uniquement les anomalies", value = FALSE),
    tags$hr(),
    h4("Thème"),
    switchInput(
      inputId = "dark_mode",
      label = NULL,
      onLabel = "🌙",  # Icône lune
      offLabel = "☀️", # Icône soleil
      value = FALSE
    )
    
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
      /* Couleur barre de gauche */
      .main-sidebar {
        background-color: #5C4033 !important; /* gris clair */
      }

      /* Couleur barre du haut */
      .main-header .navbar {
        background-color: #003366 !important; /* bleu foncé */
      }

      /* Titre dans la barre du haut */
      .main-header .logo {
        background-color: #003366 !important;
        color: white !important;
      }

      /* Texte dans la barre du haut */
      .main-header .navbar .nav > li > a {
        color: white !important;
      }
      .dark-mode {
  background-color: #2c2c2c !important;
  color: #f5f5f5 !important;
}

.dark-mode .main-sidebar {
  background-color: #1f1f1f !important;
}

.dark-mode .main-header .navbar {
  background-color: #1f1f1f !important;
}

.dark-mode .main-header .logo {
  background-color: #1f1f1f !important;
}

.dark-mode .box {
  background-color: #3c3c3c !important;
  color: white !important;
}

.dark-mode .tab-pane, .dark-mode .content-wrapper {
  background-color: #2c2c2c !important;
}

      
      
    "))
    ),
    
    # fluidRow(
    #   valueBoxOutput("cvBox"),
    #   valueBoxOutput("meanBox"),
    #   valueBoxOutput("nSeqBox"),
    #   valueBoxOutput("cvBoxFAME"),
    #   valueBoxOutput("cvBoxInterne")
    # ),
    fluidRow(
      valueBoxOutput("cvBox"),
      valueBoxOutput("meanBox"),
      conditionalPanel(
        condition = "output.showSequenceBox",
        valueBoxOutput("nSeqBox")
      ),
      conditionalPanel(
        condition = "output.showCVFAME",
        valueBoxOutput("cvBoxFAME")
      ),
      valueBoxOutput("cvBoxInterne")
      # conditionalPanel(
      #   condition = "output.showCVInterne",
      #   valueBoxOutput("cvBoxInterne")
      # )
    ),
    fluidRow(
      box(title = "Fichiers chargés", width = 12,
          verbatimTextOutput("loadedFiles"),
          verbatimTextOutput("data_summary")
      )
    ),
    
    fluidRow(
      tabBox(title = "Visualisation", width = 12,
             tabPanel("CV% par séquence", plotlyOutput("CVPlot_time")),
             tabPanel("Aires par Date", plotlyOutput("trendPlot")),
             tabPanel("Aires par date désordonné", plotlyOutput("areaPlot")),
             tabPanel("Cinétiques multi-composés",
                      downloadButton("download_cv_plot", "Télécharger CV (%) PNG"),
                      downloadButton("download_area_plot", "Télécharger Aire (log10) PNG"),
                      plotlyOutput("multiCVPlot"),
                      plotlyOutput("multiAreaPlot")),
             
             tabPanel("Résumé Tenax",
                      fileInput("tenax_summary_file", "📂 Charger un fichier formatté Tenax (.csv)", accept = ".csv"),
                      plotlyOutput("tenax_summary_plot"),
                      plotlyOutput("tenax_cv_plot"),
                      DTOutput("tenax_summary_table"))
             
      )
    ),
    fluidRow(
      box(title = "Données filtrées", width = 12, DTOutput("dataTable"))
    ),
    fluidRow(
      downloadButton("downloadCSV", "Télécharger CSV nettoyé"),
      actionButton("zoom_issues", "Zoom sur problèmes"),
      actionButton("manual_correct", "Corriger manuellement")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  Sys.setlocale("LC_NUMERIC", "C")
  
  data_reactive <- reactiveVal()
  
  source("qualityChecks.R")
  source("preprocess.R")
  
  output$sheet_selector <- renderUI({
    req(input$file_upload)
    
    # Vérifie si au moins un fichier est un Excel
    excel_files <- input$file_upload$datapath[grepl("\\.xlsx$", input$file_upload$name, ignore.case = TRUE)]
    
    if (length(excel_files) == 0) {
      return(NULL)  # Pas d'Excel, pas besoin d'afficher le sélecteur de feuille
    }
    
    # S'il y a un fichier Excel : proposer la sélection de feuille
    sheets <- excel_sheets(excel_files[1])
    
    selectInput("sheet", "Choisir une feuille Excel :", choices = sheets)
  })
  
  
  
  # output$sheet_selector <- renderUI({
  #   req(input$file_upload)
  #   sheets <- excel_sheets(input$file_upload$datapath)
  #   selectInput("sheet", "Choisir une feuille Excel :", choices = sheets)
  # })
  
  preprocess_excel <- function(file_path, sheet_name) {
    df_raw <- read_excel(file_path, sheet = sheet_name, skip = 0, col_names = TRUE)
    
    # ⚠️ Forcer les colonnes à être des caractères (évite les erreurs de conversion Excel avec virgule)
    df_raw <- df_raw %>%
      mutate(across(starts_with("Area_"), as.character),
             across(starts_with("CV_"), as.character))
    
    # Détection des colonnes Area et CV
    area_cols <- names(df_raw)[str_detect(names(df_raw), "^Area_\\d{4}-\\d{2}-\\d{2}")]
    cv_cols <- names(df_raw)[str_detect(names(df_raw), "^CV_\\d{4}-\\d{2}-\\d{2}")]
    
    if (length(area_cols) == 0 || length(cv_cols) == 0) {
      warning("Aucune colonne Area_ ou CV_ trouvée.")
      return(tibble())
    }
    
    # Pivot vers format long
    area_long <- df_raw %>%
      select(Compound, all_of(area_cols)) %>%
      pivot_longer(-Compound, names_to = "Date", values_to = "Area") %>%
      mutate(Date = str_remove(Date, "Area_"),
             Date = ymd(Date))
    
    cv_long <- df_raw %>%
      select(Compound, all_of(cv_cols)) %>%
      pivot_longer(-Compound, names_to = "Date", values_to = "CV") %>%
      mutate(Date = str_remove(Date, "CV_"),
             Date = ymd(Date))
    
    # Fusion
    df <- left_join(area_long, cv_long, by = c("Compound", "Date"))
    
    # ✅ Conversion des valeurs avec virgule → point, puis transformation en nombre
    df <- df %>%
      mutate(
        Area = as.numeric(str_replace(as.character(Area), ",", ".")),
        CV   = as.numeric(str_replace(as.character(CV), ",", "."))
      )
    # Calcul automatique des CV si toutes les valeurs sont NA
    if (all(is.na(df$CV))) {
      message("🔁 Toutes les colonnes CV sont NA, calcul du CV global par composé...")
      
      df <- df %>%
        group_by(Compound) %>%
        mutate(
          CV = ifelse(length(Area[!is.na(Area)]) > 1,
                      100 * sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE),
                      NA_real_)
        ) %>%
        ungroup()
    }
    #   
    #   # Activation de la sélection de dossier
    #   shinyDirChoose(input, "dir", roots = c(home = "~"), session = session)
    #   
    #   # Observe le choix du dossier
    #   observeEvent(input$dir, {
    #     dir_path <- parseDirPath(roots = c(home = "~"), input$dir)
    #     req(dir_path)
    #     
    #     # Traitement Retentio+ standard
    #     data <- preprocess_data(path = dir_path)
    #     
    #     # Étape get_best_hits
    #     data_best <- data %>%
    #       group_by(Sample = Sequence, Compound) %>%
    #       slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
    #       ungroup()
    #     
    #     # Pivot vers format large
    #     data_wide <- data_best %>%
    #       pivot_wider(names_from = Sample, values_from = Area)
    #     
    #     # Préparation du téléchargement
    #     output$download_formatted <- downloadHandler(
    #       filename = function() paste0("tableau_formate_", Sys.Date(), ".csv"),
    #       content = function(file) {
    #         write_csv(data_wide, file)
    #       }
    #     )
    #   })
    #   
    # }
    
    
    # Ajout d'infos analytiques
    df <- df %>%
      mutate(
        Type = case_when(
          str_detect(tolower(Compound), "fame") ~ "FAME",
          str_detect(tolower(Compound), "-d[0-9]+") ~ "Étalon Interne",
          TRUE ~ "Analyte"
        ),
        Sequence = sheet_name,
        source_file = basename(file_path),
        Flagged = FALSE
      ) %>%
      relocate(Compound, Date, Area, CV, Type, Sequence, source_file, Flagged)
    
    # Vérification : présence obligatoire des colonnes clés
    required_cols <- c("Compound", "Area", "CV", "Date")
    missing_cols <- setdiff(required_cols, names(df))
    
    validate(
      need(length(missing_cols) == 0, paste0("❌ Colonnes manquantes dans ", filename, " : ", paste(missing_cols, collapse = ", ")))
    )
    
    
    return(df)
  }
  
  # Fonction intelligente : lit Excel ou CSV selon extension
  preprocess_smart <- function(file_path, filename, sheet_name = NULL) {
    
    if (grepl("^QC_", basename(filename))) {  # Détecter fichier QC spécial
      return(preprocess_QC_file(file_path, filename))
    }
    
    
    if (grepl("\\.csv$", filename, ignore.case = TRUE)) {
      df <- read_csv2(file_path, show_col_types = FALSE)
      
      cols_lower <- tolower(names(df))
      
      if (all(c("sample", "name", "r.t. (min)", "quant masses", "area") %in% cols_lower)) {
        message("🧪 Détecté fichier CSV brut. Traitement preprocess_single_file.")
        return(preprocess_single_file(file_path))
        
      } else if (any(str_detect(cols_lower, "^area_\\d{4}-\\d{2}-\\d{2}"))) {
        message("📄 Détecté fichier CSV formaté. Traitement classique.")
        
        df <- df %>%
          pivot_longer(
            cols = -Compound,
            names_to = "Measure",
            values_to = "Value"
          ) %>%
          separate(Measure, into = c("TypeMeasure", "Date"), sep = "_", extra = "merge") %>%
          pivot_wider(names_from = TypeMeasure, values_from = Value) %>%
          mutate(
            Date = ymd(Date),
            Area = as.numeric(str_replace(as.character(Area), ",", ".")),
            CV = as.numeric(str_replace(as.character(CV), ",", "."))
          ) %>%
          mutate(
            Type = case_when(
              str_detect(tolower(Compound), "fame") ~ "FAME",
              str_detect(tolower(Compound), "-d[0-9]+") ~ "Étalon Interne",
              TRUE ~ "Analyte"
            ),
            Sequence = "Formaté CSV",
            source_file = filename,
            Flagged = FALSE
          ) %>%
          relocate(Compound, Date, Area, CV, Type, Sequence, source_file, Flagged)
        
        return(df)
        
      } else {
        stop(paste0("❌ Structure de fichier CSV inconnue pour : ", filename))
      }
    }
    
    
    else if (grepl("\\.xlsx$", filename, ignore.case = TRUE)) {
      # Traitement Excel brut
      message("📚 Lecture d'un fichier Excel brut.")
      
      req(sheet_name)  # nécessaire pour Excel
      df_raw <- read_excel(file_path, sheet = sheet_name)
      
      area_cols <- names(df_raw)[str_detect(names(df_raw), "^Area_\\d{4}-\\d{2}-\\d{2}")]
      cv_cols <- names(df_raw)[str_detect(names(df_raw), "^CV_\\d{4}-\\d{2}-\\d{2}")]
      
      if (length(area_cols) == 0 || length(cv_cols) == 0) {
        warning("Aucune colonne Area_ ou CV_ trouvée.")
        return(tibble())
      }
      
      area_long <- df_raw %>%
        select(Compound, all_of(area_cols)) %>%
        pivot_longer(-Compound, names_to = "Date", values_to = "Area") %>%
        mutate(Date = str_remove(Date, "Area_"), Date = ymd(Date))
      
      cv_long <- df_raw %>%
        select(Compound, all_of(cv_cols)) %>%
        pivot_longer(-Compound, names_to = "Date", values_to = "CV") %>%
        mutate(Date = str_remove(Date, "CV_"), Date = ymd(Date))
      
      df <- left_join(area_long, cv_long, by = c("Compound", "Date")) %>%
        mutate(
          Area = as.numeric(str_replace(as.character(Area), ",", ".")),
          CV = as.numeric(str_replace(as.character(CV), ",", "."))
        ) %>%
        mutate(
          Type = case_when(
            str_detect(tolower(Compound), "fame") ~ "FAME",
            str_detect(tolower(Compound), "-d[0-9]+") ~ "Étalon Interne",
            TRUE ~ "Analyte"
          ),
          Sequence = sheet_name,
          source_file = filename,
          Flagged = FALSE
        ) %>%
        relocate(Compound, Date, Area, CV, Type, Sequence, source_file, Flagged)
      
      return(df)
      
    } else {
      stop("❌ Format de fichier non supporté. (attendu .csv ou .xlsx)")
    }
  }
  
  # 1. observeEvent Excel
  
  observeEvent(input$refresh, {
    req(input$file_upload)
    
    files <- input$file_upload$datapath
    filenames <- input$file_upload$name
    
    full_data <- tibble()
    
    withProgress(message = "⏳ Chargement des fichiers...", value = 0, {
      total_steps <- length(files)
      
      for (i in seq_along(files)) {
        file_path <- files[i]
        file_name <- filenames[i]
        
        if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
          # ✅ Lecture intelligente même pour fichiers déjà formatés (Plasma ou Tenax)
          df <- preprocess_smart(file_path, file_name)
          full_data <- bind_rows(full_data, df)
        }
        
        
        # # 🧠 Intelligent : si c'est .csv
        # if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        #   # Lire le CSV
        #   df_try <- read_csv2(file_path, show_col_types = FALSE)
        #   
        #   # Détecte s'il a les colonnes brutes ou formatées
        #   cols_lower <- tolower(names(df_try))
        #   
        #   if (all(c("compound", "area", "rt", "ion") %in% cols_lower)) {
        #     # C'est un fichier brut ➔ preprocess_data()
        #     temp_dir <- tempdir()
        #     file.copy(file_path, file.path(temp_dir, basename(file_path)))
        #     df_csv <- preprocess_data(path = temp_dir)
        #     full_data <- bind_rows(full_data, df_csv)
        #   } else if (any(str_detect(cols_lower, "^area_\\d{4}-\\d{2}-\\d{2}"))) {
        #     # C'est un fichier déjà formaté ➔ preprocess_smart()
        #     df <- preprocess_smart(file_path, file_name)
        #     full_data <- bind_rows(full_data, df)
        #   } else {
        #     showNotification(paste0("❌ Fichier ", file_name, " ignoré : structure inconnue."), type = "error")
        #   }
        # }
        
        # Cas Excel
        if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
          sheets <- excel_sheets(file_path)
          for (sheet in sheets) {
            df <- preprocess_smart(file_path, file_name, sheet)
            full_data <- bind_rows(full_data, df)
          }
        }
        
        incProgress(1 / total_steps)
      }
    })
    
    if (nrow(full_data) == 0) {
      showModal(
        modalDialog(
          title = "❌ Erreur",
          "Aucun fichier exploitable trouvé.",
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      return()
    }
    
    
    #df_flagged <- flag_anomalies(full_data)
    if ("RT" %in% names(full_data)) {
      df_flagged <- flag_anomalies(full_data)
    } else {
      df_flagged <- full_data
    }
    
    data_reactive(df_flagged)
    
    updatePickerInput(session, "analyte", choices = sort(unique(df_flagged$Compound)))
    updatePickerInput(session, "sequence", choices = sort(unique(df_flagged$Sequence)))
    updatePickerInput(session, "multi_analytes", choices = unique(df_flagged$Compound))
    
    showNotification("✅ Fichiers chargés avec succès.", type = "message")
  })
  
  observe({
    if (input$dark_mode) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  
  
  
  # # 2. observeEvent CSV
  # observeEvent(input$csv_upload, {
  #   req(input$csv_upload)
  #   
  #   temp_dir <- tempdir()
  #   file_paths <- input$csv_upload$datapath
  #   names(file_paths) <- input$csv_upload$name
  #   file.copy(file_paths, file.path(temp_dir, basename(file_paths)))
  #   
  #   df_csv <- preprocess_data(path = temp_dir)
  #   if (nrow(df_csv) == 0) {
  #     showNotification("Aucun fichier CSV valide", type = "error")
  #     return()
  #   }
  #   
  #   df_flagged <- flag_anomalies(df_csv)
  #   data_reactive(df_flagged)
  #   
  #   updatePickerInput(session, "analyte", choices = sort(unique(df_flagged$Compound)))
  #   updatePickerInput(session, "sequence", choices = sort(unique(df_flagged$Sequence)))
  #   updatePickerInput(session, "multi_analytes", choices = unique(df_flagged$Compound))
  # })
  
  
  observeEvent(input$reset, {
    updatePickerInput(session, "analyte", selected = character(0))
    updatePickerInput(session, "sequence", selected = character(0))
    updatePickerInput(session, "type", selected = "Tous")
    updateCheckboxInput(session, "flag_only", value = FALSE)
  })
  
  filtered_data <- reactive({
    req(data_reactive(), input$analyte) #req(data_reactive(), input$analytes) code de base pour la première slide bar
    data <- data_reactive()
    df <- data %>% filter(str_to_lower(Compound) == str_to_lower(input$analyte))
    if (input$type != "Tous") df <- df %>% filter(Type == input$type)
    if (!is.null(input$sequence)) df <- df %>% filter(Sequence %in% input$sequence)
    if (input$flag_only) df <- df %>% filter(Flagged)
    validate(need(nrow(df) > 0, "Aucune donnée disponible avec les filtres actuels."))
    df
  })
  output$cvBox <- renderValueBox({
    df <- filtered_data()
    if (all(is.na(df$CV))) return(valueBox("NA", subtitle = "CV%", color = "aqua"))
    
    cv_moyen <- round(mean(df$CV, na.rm = TRUE), 1)  # ✅ On utilise directement la colonne CV
    box_color <- if (cv_moyen < 30) "green" else "red"
    
    valueBox(cv_moyen, subtitle = "CV%", color = box_color)
  })
  
  
  # output$cvBox <- renderValueBox({
  #   df <- filtered_data()
  #   if (all(is.na(df$Area))) return(valueBox("NA", subtitle = "CV%", color = "aqua"))
  #   cv <- round(sd(df$Area, na.rm = TRUE) / mean(df$Area, na.rm = TRUE) * 100, 1) # changer par valeur dans colonne CV
  #   box_color <- if (is.na(cv)) "aqua" else if (cv < 30) "green" else "red"
  #   valueBox(ifelse(is.na(cv), "NA", cv), subtitle = "CV%", color = box_color)
  # })
  # 
  output$meanBox <- renderValueBox({
    df <- filtered_data()
    mean_val <- round(mean(df$Area, na.rm = TRUE), 2)
    formatted <- format(mean_val, decimal.mark = ".", big.mark = "", scientific = FALSE)
    valueBox(ifelse(is.na(mean_val), "NA", formatted), subtitle = "Aire Moyenne", color = "blue")
  })
  
  
  output$nSeqBox <- renderValueBox({
    df <- filtered_data()
    valueBox(length(unique(df$Sequence)), subtitle = "Séquences", color = "purple")
  })
  
  # --- LOGIQUE CONDITIONNELLE POUR CACHER LES BOX INUTILES ---
  
  output$showCVFAME <- reactive({
    df <- filtered_data()
    any(df$Type == "FAME") && any(!is.na(df$Area))
  })
  outputOptions(output, "showCVFAME", suspendWhenHidden = FALSE)
  
  output$showCVInterne <- reactive({
    df <- filtered_data()
    any(df$Type == "Étalon Interne") && any(!is.na(df$Area))
  })
  outputOptions(output, "showCVInterne", suspendWhenHidden = FALSE)
  
  output$showSequenceBox <- reactive({
    df <- filtered_data()
    length(unique(df$Sequence)) > 1
  })
  outputOptions(output, "showSequenceBox", suspendWhenHidden = FALSE)
  
  
  output$cvBoxFAME <- renderValueBox({
    df <- filtered_data() %>% filter(Type == "FAME")
    if (nrow(df) == 0 || all(is.na(df$Area))) return(valueBox("NA", subtitle = "CV FAME", color = "aqua"))
    cv <- round(sd(df$Area, na.rm = TRUE) / mean(df$Area, na.rm = TRUE) * 100, 1)
    box_color <- if (cv < 10) "green" else "orange"
    valueBox(cv, subtitle = "CV injection (FAME)", color = box_color)
  })
  output$cvBoxInterne <- renderValueBox({
    df <- filtered_data()
    df_etalon <- df %>% filter(Type == "Étalon Interne")
    
    if (nrow(df_etalon) == 0 || all(is.na(df_etalon$CV))) {
      return(valueBox("NA", subtitle = "CV extraction (Étalon)", color = "aqua"))
    }
    
    cv_moyen <- round(mean(df_etalon$CV, na.rm = TRUE), 1)
    box_color <- if (cv_moyen < 30) "green" else "orange"
    
    valueBox(cv_moyen, subtitle = "CV extraction (Étalon)", color = box_color)
  })
  
  # output$cvBoxInterne <- renderValueBox({
  #   df <- filtered_data()
  #   df_etalon <- df %>% filter(Type == "Étalon Interne")
  #   
  #   if (nrow(df_etalon) == 0 || all(is.na(df_etalon$Area))) {
  #     return(valueBox(
  #       value = "N/A",
  #       subtitle = "CV extraction (Étalon)",
  #       color = "light-blue"  # ou aqua si tu veux
  #     ))
  #   }
  #   
  #   cv <- round(sd(df_etalon$Area, na.rm = TRUE) / mean(df_etalon$Area, na.rm = TRUE) * 100, 1)
  #   box_color <- if (cv < 30) "green" else "orange"
  #   
  #   valueBox(
  #     value = cv,
  #     subtitle = "CV extraction (Étalon)",
  #     color = box_color
  #   )
  # })
  
  
  # output$cvBoxInterne <- renderValueBox({
  #   df <- filtered_data() %>% filter(Type == "Étalon Interne")
  #   
  #   if (nrow(df) == 0 || all(is.na(df$Area))) {
  #     return(valueBox("Aucune donnée", subtitle = "CV extraction (Étalon)", color = "aqua"))
  #   }
  #   
  #   cv <- round(sd(df$Area, na.rm = TRUE) / mean(df$Area, na.rm = TRUE) * 100, 1)
  #   box_color <- if (is.na(cv)) "aqua" else if (cv < 30) "green" else "orange"
  #   
  #   valueBox(cv, subtitle = "CV extraction (Étalon)", color = box_color)
  # })
  # 
  # # output$cvBoxInterne <- renderValueBox({
  # #   df <- filtered_data() %>% filter(Type == "Étalon Interne")
  # #   if (nrow(df) == 0 || all(is.na(df$Area))) {
  # #     return(valueBox("NA", subtitle = "CV extraction (Étalon)", color = "aqua"))
  # #   }
  #   
  #   cv <- round(sd(df$Area, na.rm = TRUE) / mean(df$Area, na.rm = TRUE) * 100, 1)
  #   box_color <- if (cv < 30) "green" else "orange"
  #   
  #   valueBox(
  #     value = cv,
  #     subtitle = "CV extraction (Étalon)",
  #     color = box_color
  #   )
  # })
  
  
  # output$cvBoxInterne <- renderValueBox({
  #   df <- filtered_data() %>% filter(Type == "Étalon Interne")
  #   if (nrow(df) == 0 || all(is.na(df$Area))) return(valueBox("NA", subtitle = "CV Ét. Interne", color = "aqua"))
  #   cv <- round(sd(df$Area, na.rm = TRUE) / mean(df$Area, na.rm = TRUE) * 100, 1)
  #   box_color <- if (cv < 30) "green" else "red"
  #   valueBox(cv, subtitle = "CV extraction (Étalon)", color = box_color)
  # })
  # 
  
  output$cvPlot <- renderPlotly({
    df <- filtered_data() %>% group_by(Sequence) %>%
      summarise(CV = sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100, .groups = "drop")
    plot_ly(df, x = ~Sequence, y = ~CV, type = 'bar') %>%
      layout(yaxis = list(title = "CV%"),
             shapes = list(list(type = "line", x0 = 0, x1 = 1, y0 = 30, y1 = 30, xref = "paper", yref = "y",
                                line = list(dash = "dash", color = "red"))))
  })
  
  output$areaPlot <- renderPlotly({
    df <- filtered_data()
    plot_ly(df, x = ~Date, y = ~Area, type = 'scatter', mode = 'lines+markers') %>%
      layout(yaxis = list(title = "Aire"))
  })
  
  output$CVPlot_time<- renderPlotly({
    df <- filtered_data()
    df$Date  = as.Date(df$Date)
    df = df[order(df$Date), ]
    #df$CV <- df$CV * 100 ca a ruiner ma vi ca
    
    plot_ly(df, x = ~Date, y = ~CV, type = 'scatter', mode = 'lines+markers') %>% #rajt  color= Compound avant type avec virgule
      layout(yaxis = list(title = "CV (%)"))
  })
  
  output$trendPlot <- renderPlotly({
    df <- filtered_data()
    summary <- df %>%
      group_by(Date) %>%
      summarise(
        Min = min(Area, na.rm = TRUE),
        Max = max(Area, na.rm = TRUE),
        Mean = mean(Area, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(summary, x = ~Date) %>%
      add_lines(y = ~Min, name = "Min") %>%
      add_lines(y = ~Mean, name = "Moyenne") %>%
      add_lines(y = ~Max, name = "Max") %>%
      add_trace(y = ~Max - Min, type = "scatter", mode = "lines+markers", name = "Amplitude") %>%
      layout(yaxis = list(title = "Tendance des Aires"))
  })
  
  
  output$dataTable <- renderDT({
    datatable(filtered_data(), editable = "cell", options = list(pageLength = 10))
  })
  
  observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    str(info)  # utile pour debug
    df <- data_reactive()
    
    # mise à jour uniquement si colonne Flagged éditée
    if (info$col == which(colnames(df) == "Flagged")) {
      df[info$row, "Flagged"] <- as.logical(info$value)
      data_reactive(df)
    }
  })
  
  
  output$downloadCSV <- downloadHandler(
    filename = function() paste0("donnees_filtrees_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered_data(), file)
  )
  
  # output$loadedFiles <- renderText({
  #   req(input$file_upload)
  #   paste("Fichiers sélectionnés :", paste(input$file_upload$name, collapse = ", "))
  # })
  output$loadedFiles <- renderText({
    req(input$file_upload)
    
    uploaded_files <- input$file_upload$name
    
    paste0("Fichiers chargés :\n", paste(uploaded_files, collapse = "\n"))
  })
  
  output$data_summary <- renderText({
    req(data_reactive())  # attendre que les données soient prêtes
    df <- data_reactive()
    
    n_compounds <- df %>% pull(Compound) %>% unique() %>% length()
    n_sequences <- df %>% pull(Sequence) %>% unique() %>% length()
    n_dates <- df %>% pull(Date) %>% unique() %>% length()
    n_points <- nrow(df)
    
    paste0("✅ ", n_compounds, " composés | ", 
           n_sequences, " séquences | ",
           n_dates, " dates | ",
           n_points, " points")
  })
  
  
  
  # output$loadedFiles <- renderText({
  #   paste(
  #     if (!is.null(input$file_upload)) paste("Excel :", paste(input$file_upload$name, collapse = ", ")) else "",
  #     if (!is.null(input$csv_upload)) paste("CSV :", paste(input$csv_upload$name, collapse = ", ")) else "",
  #     sep = "\n"
  #   )
  # })
  # 
  
  output$multiCVPlot <- renderPlotly({
    df <- data_reactive() %>%
      filter(Compound %in% input$multi_analytes,
             !is.na(Date))
    
    # df <- data_reactive() %>%
    #   filter(Type == "Étalon Interne",
    #          Compound %in% input$multi_analytes,
    #          !is.na(Date))
    # df <- data_reactive() %>%
    #   filter(Type == "Étalon Interne",
    #          Compound %in% input$multi_analytes,
    #          !is.na(Date), !is.na(Area), Area > 0)
    # Message d'avertissement si certaines molécules n'ont pas de données
    missing <- setdiff(input$multi_analytes, unique(df$Compound))
    if (length(missing) > 0) {
      showNotification(paste("⚠️ Pas de données pour :", paste(missing, collapse = ", ")), type = "warning", duration = 8)
    }
    
    
    if (nrow(df) == 0) return(NULL)
    
    cv_data <- df %>%
      # group_by(Compound, Date) %>%
      # summarise(
      #   CV = sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100,
      #   .groups = "drop"
      # ) %>%
      filter(!is.na(CV)) %>%
      arrange(Date) #%>%
    
    # 
    # p <- ggplot(cv_data, aes(x = Date, y = CV, color = Compound)) +
    #   geom_line() + geom_point() +
    #   geom_hline(yintercept = 30, linetype = "dashed", color = "red") +  # ← ligne seuil
    #   theme_minimal() +
    #   labs(title = "Cinétique des CV (%) des étalons internes",
    #        y = "CV (%)", x = "Date") +
    #   theme(legend.position = "bottom")
    
    #mutate(CV = CV * 100) ruinay
    p <- ggplot(cv_data, aes(x = Date, y = CV, color = Compound)) +
      geom_line() + geom_point() +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Cinétique des CV (%) des étalons internes",
           y = "CV (%)", x = "Date") +
      theme(legend.position = "bottom")
    
    # p <- ggplot(cv_data, aes(x = Date, y = CV, color = Compound)) +
    #   geom_line() + geom_point() +
    #   geom_hline(yintercept = 30, linetype = "dashed", color = "red") +  # 30 × 10
    #   theme_minimal() +
    #   labs(title = "Cinétique des CV (%) des étalons internes (×10)",
    #        y = "CV (%)", x = "Date") +
    #   theme(legend.position = "bottom")
    
    
    ggplotly(p)
  })
  
  output$multiAreaPlot <- renderPlotly({
    df <- data_reactive() %>%
      filter(Compound %in% input$multi_analytes,
             !is.na(Date))
    # 
    # df <- data_reactive() %>%
    #   filter(Type == "Étalon Interne",
    #          Compound %in% input$multi_analytes,
    #          !is.na(Date))
    # df <- data_reactive() %>%
    #   filter(Type == "Étalon Interne",
    #          Compound %in% input$multi_analytes,
    #          !is.na(Date), !is.na(Area), Area > 0)
    # Message d'avertissement si certaines molécules n'ont pas de données
    missing <- setdiff(input$multi_analytes, unique(df$Compound))
    if (length(missing) > 0) {
      showNotification(paste("⚠️ Pas de données pour :", paste(missing, collapse = ", ")), type = "warning", duration = 8)
    }
    
    
    print("Composés disponibles après filtre multiAreaPlot :")
    print(unique(df$Compound))
    
    print("Résumé des types de composés :")
    print(table(df$Type))
    
    
    
    if (nrow(df) == 0) return(NULL)
    
    df_log <- df %>%
      # mutate(logArea = log10(Area)) %>%
      filter(!is.na(Area)) %>%
      arrange(Date)  # ← tri par date
    
    
    p <- ggplot(df_log, aes(x = Date, y = Area, color = Compound)) +
      geom_line() + geom_point() +
      theme_minimal() +
      labs(title = "Cinétique des Aires des étalons internes",
           y = "Area", x = "Date") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Fonction pour enregistrer le plot CV en PNG
  output$download_cv_plot <- downloadHandler(
    filename = function() {
      paste0("CV_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- data_reactive() %>%
        filter(Compound %in% input$multi_analytes,
               !is.na(Date), !is.na(Area), Area > 0)
      # 
      # df <- data_reactive() %>%
      #   filter(Type == "Étalon Interne",
      #          Compound %in% input$multi_analytes,
      #          !is.na(Date), !is.na(Area), Area > 0)
      if (nrow(df) == 0) return(NULL)
      
      # cv_data <- df %>%
      #   # group_by(Compound, Date) %>%
      #   # summarise(
      #   #   CV = sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100,
      #   #   .groups = "drop"
      #   # ) %>%
      #   filter(!is.na(CV)) %>%
      #   arrange(Date)
      cv_data <- df %>%
        filter(!is.na(CV)) %>%
        arrange(Date) #%>%
      #mutate(CV = CV * 100) ruiner
      
      p <- ggplot(cv_data, aes(x = Date, y = CV)) +
        geom_point(aes(color = CV > 30)) +  # points rouges si CV > 30
        geom_line(aes(group = Compound), color = "black") +  # lignes noires
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(title = "Cinétique des CV (%) des étalons internes",
             y = "CV (%)", x = "Date") +
        theme(legend.position = "none")
      
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Fonction pour enregistrer le plot Aire log10 en PNG
  output$download_area_plot <- downloadHandler(
    filename = function() {
      paste0("Area_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- data_reactive() %>%
        filter(Compound %in% input$multi_analytes,
               !is.na(Date), !is.na(Area), Area > 0)
      
      # df <- data_reactive() %>%
      #   filter(Type == "Étalon Interne",
      #          Compound %in% input$multi_analytes,
      #          !is.na(Date), !is.na(Area), Area > 0)
      if (nrow(df) == 0) return(NULL)
      
      df_log <- df %>%
        mutate(logArea = log10(Area)) %>%
        filter(!is.na(logArea)) %>%
        arrange(Date)  # ← tri par date
      
      
      p <- ggplot(df_log, aes(x = Date, y = logArea, color = Compound)) +
        geom_line() + geom_point() +
        theme_minimal() +
        labs(title = "Cinétique des Aires (log10) des étalons internes",
             y = "log10(Area)", x = "Date") +
        theme(legend.position = "bottom")
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  #shinyDirChoose(input, "dir", roots = c(home = "~"), session = session)
  volumes <- c(
    "Home" = "C:/Users/Masspeclab",
    "Bureau" = "C:/Users/Masspeclab/Desktop",
    "Documents" = "C:/Users/Masspeclab/Documents"
  )
  shinyDirChoose(input, "dir", roots = volumes, session = session)
  
  observeEvent(input$dir, {
    dir_path <- parseDirPath(volumes, input$dir)
    req(dir_path)
    
    data_wide <- preprocess_folder_tenax(dir_path)  # 🛠 utiliser le bon traitement global
    
    if (nrow(data_wide) == 0) {
      showModal(
        modalDialog(
          title = "Erreur de chargement",
          div(style = "font-size:20px; color:red; text-align:center;",
              "❌ Aucun fichier CSV exploitable trouvé."),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      return(NULL)
    }
    
    output$download_formatted <- downloadHandler(
      filename = function() paste0("tableau_formate_", Sys.Date(), ".csv"),
      content = function(file) {
        write_csv2(data_wide, file)
        showNotification("✅ Données formatées prêtes au téléchargement.", type = "message")
      }
    )
  })
  
  
  
  # 
  # observeEvent(input$dir, {
  #   dir_path <- parseDirPath(volumes, input$dir)
  #   req(dir_path)
  #   
  #   files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  #   if (length(files) == 0) {
  #     showModal(
  #       modalDialog(
  #         title = "Erreur de chargement",
  #         div(style = "font-size:20px; color:red; text-align:center;",
  #             "❌ Aucun fichier CSV trouvé. Veuillez sélectionner un fichier correct."),
  #         easyClose = TRUE,
  #         footer = modalButton("OK")
  #       )
  #     )
  #     return(NULL)
  #   }
  #   
  #   all_tables <- map(files, function(filepath) {
  #     df <- preprocess_smart(filepath, basename(filepath))
  #     if (nrow(df) == 0) return(tibble())
  #     
  #     # Seulement pour CSV bruts (Tenax)
  #     is_raw <- all(c("Sample", "Compound", "Area") %in% names(df)) && any(!is.na(df$Sample))
  #     if (!is_raw) return(tibble())
  #     
  #     best_hits <- df %>%
  #       group_by(Sample, Compound) %>%
  #       slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
  #       ungroup()
  #     
  #     # Extrait la date + suffixe si présent
  #     date_str <- str_extract(basename(filepath), "\\d{8}[a-z]?")
  #     if (is.na(date_str)) date_str <- tools::file_path_sans_ext(basename(filepath))
  #     
  #     date_label <- tryCatch({
  #       base_date <- as.Date(substr(date_str, 1, 8), format = "%d%m%Y")
  #       suffix <- substr(date_str, 9, 9)
  #       paste0(format(base_date, "%Y-%m-%d"), suffix)
  #     }, error = function(e) date_str)
  #     
  #     best_hits %>%
  #       group_by(Compound) %>%
  #       summarise(
  #         !!paste0("Area_", date_label) := mean(Area, na.rm = TRUE),
  #         !!paste0("CV_", date_label) := ifelse(length(Area[!is.na(Area)]) > 1,
  #                                               100 * sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE),
  #                                               NA_real_),
  #         .groups = "drop"
  #       )
  #   })
  #   
  #   all_tables <- all_tables[map_lgl(all_tables, ~ nrow(.) > 0)]
  #   if (length(all_tables) == 0) {
  #     showNotification("Aucune donnée exploitable.", type = "error")
  #     return(NULL)
  #   }
  #   
  #   data_wide <- reduce(all_tables, full_join, by = "Compound")
  #   
  #   output$download_formatted <- downloadHandler(
  #     filename = function() paste0("tableau_formate_", Sys.Date(), ".csv"),
  #     content = function(file) {
  #       write_csv2(data_wide, file)
  #       showNotification("✅ Données formatées prêtes au téléchargement.", type = "message")
  #     }
  #   )
  # })
  # 
  # 
  # 
  # 
  
  #   observeEvent(input$dir, {
  #     dir_path <- parseDirPath(volumes, input$dir)
  #     req(dir_path)
  #     
  #     files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  #     if (length(files) == 0) {
  #       showModal(
  #         modalDialog(
  #           title = "Erreur de chargement",
  #           div(style = "font-size:20px; color:red; text-align:center;",
  #               "❌ Aucun fichier CSV trouvé. Veuillez sélectionner un fichier correct."),
  #           easyClose = TRUE,
  #           footer = modalButton("Okiiiiiii👍")
  #         )
  #       )
  #       
  #       #showNotification("Aucun fichier CSV trouvé.", type = "error")
  #       return(NULL)
  #     }
  #     
  #     data_wide <- preprocess_folder_tenax(dir_path)
  #     
  #     
  #     # all_tables <- map(files, function(filepath) {
  #     #   df <- preprocess_smart(filepath, basename(filepath))
  #     #   #df <- preprocess_single_file(filepath)
  #     #   if (nrow(df) == 0) return(tibble())
  #     #   
  #     #   # Correction best hit
  #     #   best_hits <- df %>%
  #     #     group_by(Sample, Compound) %>%
  #     #     slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
  #     #     ungroup()
  #     #   
  #     #   # Résumé par composé pour ce fichier
  #     #   date_extracted <- str_extract(basename(filepath), "\\d{8}")
  #     #   date_formatted <- format(as.Date(date_extracted, "%d%m%Y"), "%Y-%m-%d")
  #     #   
  #     #   best_hits %>%
  #     #     group_by(Compound) %>%
  #     #     summarise(
  #     #       Area = mean(Area, na.rm = TRUE),
  #     #       CV = ifelse(length(Area[!is.na(Area)]) > 1, 
  #     #                   100 * sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE),
  #     #                   NA_real_),
  #     #       .groups = "drop"
  #     #     ) %>%
  #     #     rename(
  #     #       !!paste0("Area_", date_formatted) := Area,
  #     #       !!paste0("CV_", date_formatted) := CV
  #     #     )
  #     #   
  #     #   
  #     #   # best_hits %>%
  #     #   #   group_by(Compound) %>%
  #     #   #   summarise(
  #     #   #     !!paste0("Area_", date_formatted) := mean(Area, na.rm = TRUE),
  #     #   #     !!paste0("CV_", date_formatted) := (sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE)) * 100,
  #     #   #     .groups = "drop"
  #     #   #   )
  #     # })
  #     # 
  #     # all_tables <- all_tables[map_lgl(all_tables, ~ nrow(.) > 0)]
  #     # 
  #     # if (length(all_tables) == 0) {
  #     #   showNotification("Aucune donnée exploitable.", type = "error")
  #     #   return(NULL)
  #     # }
  #     # 
  #     # data_wide <- reduce(all_tables, full_join, by = "Compound")
  #     
  #     output$download_formatted <- downloadHandler(
  #       filename = function() paste0("tableau_formate_", Sys.Date(), ".csv"),
  #       content = function(file) {
  #         write_csv2(data_wide, file)
  #         showNotification("✅ Données formatées prêtes au téléchargement.", type = "message")
  #       }
  #     )
  #   })
  #   
  #   
  # #   observeEvent(input$dir, {
  # #   dir_path <- parseDirPath(roots = volumes, input$dir)
  # #   req(dir_path)
  # #   
  # #   files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  # #   if (length(files) == 0) {
  # #     showNotification("Aucun fichier CSV trouvé.", type = "error")
  # #     return(NULL)
  # #   }
  # #   
  # #   # 🔥 Nouveau traitement fichier par fichier
  # #   all_tables <- map(files, function(filepath) {
  # #     df <- preprocess_data(path = dir_path) %>% 
  # #       filter(source_file == basename(filepath))
  # #     
  # #     if (nrow(df) == 0) return(tibble())
  # #     
  # #     date_extracted <- str_extract(basename(filepath), "\\d{4}-\\d{2}-\\d{2}")
  # #     
  # #     best_hits <- df %>%
  # #       group_by(Sample = Sequence, Compound) %>%
  # #       slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
  # #       ungroup()
  # #     
  # #     best_hits %>%
  # #       group_by(Compound) %>%
  # #       summarise(
  # #         !!paste0("Area_", date_extracted) := mean(Area, na.rm = TRUE),
  # #         !!paste0("CV_", date_extracted) := (sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE)) * 100,
  # #         .groups = "drop"
  # #       )
  # #   })
  # #   
  # #   # 🔥 Fusion de tous les résultats
  # #   data_wide <- reduce(all_tables, full_join, by = "Compound")
  # #   
  # #   output$download_formatted <- downloadHandler(
  # #     filename = function() paste0("tableau_formate_", Sys.Date(), ".csv"),
  # #     content = function(file) {
  # #       write_csv(data_wide, file)
  # #       showNotification("✅ Données chargées et formatées. Téléchargement prêt.", type = "message")
  # #     }
  # #   )
  # # })
  
  # -- Choix dossier Tenax --
  shinyDirChoose(input, "dir_tenax", roots = volumes, session = session)
  
  observeEvent(input$dir_tenax, {
    dir_path <- parseDirPath(volumes, input$dir_tenax)
    req(dir_path)
    
    files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(files) == 0) {
      showNotification("❌ Aucun fichier CSV trouvé.", type = "error")
      return()
    }
    
    all_tables <- map(files, function(filepath) {
      df <- read_csv2(filepath, show_col_types = FALSE)
      
      # --- Détection fichier brut ou déjà résumé ---
      if ("Sample" %in% names(df) && "Area" %in% names(df)) {
        # ⚡ Fichier brut : get_best_hits
        best_hits <- df %>%
          group_by(Sample, Compound) %>%
          slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
          ungroup()
        
        date_extracted <- str_extract(basename(filepath), "\\d{8}")
        date_formatted <- format(as.Date(date_extracted, "%d%m%Y"), "%Y-%m-%d")
        
        best_hits %>%
          group_by(Compound) %>%
          summarise(
            !!paste0("Area_", date_formatted) := mean(Area, na.rm = TRUE),
            !!paste0("CV_", date_formatted) := sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100,
            .groups = "drop"
          )
        
      } else if (all(c("Compound", "Area", "CV") %in% names(df))) {
        # ⚡ Fichier déjà résumé
        df <- df %>%
          mutate(Compound = as.character(Compound)) %>%
          select(Compound, Area, CV)
        
        date_extracted <- str_extract(basename(filepath), "\\d{8}")
        date_formatted <- format(as.Date(date_extracted, "%d%m%Y"), "%Y-%m-%d")
        
        df %>%
          rename(
            !!paste0("Area_", date_formatted) := Area,
            !!paste0("CV_", date_formatted) := CV
          )
      } else {
        # 🛑 Fichier non reconnu
        return(tibble())
      }
    })
    
    all_tables <- all_tables[map_lgl(all_tables, ~ nrow(.) > 0)]
    if (length(all_tables) == 0) {
      showNotification("❌ Aucun fichier exploitable Tenax.", type = "error")
      return()
    }
    
    final_table <- reduce(all_tables, full_join, by = "Compound")
    
    output$download_tenax <- downloadHandler(
      filename = function() paste0("tenax_formate_", Sys.Date(), ".csv"),
      content = function(file) {
        write_csv2(final_table, file)
        showNotification("✅ Données Tenax prêtes au téléchargement.", type = "message")
      }
    )
  })
  
  # observeEvent(input$dir_tenax, {
  #   dir_path <- parseDirPath(volumes, input$dir_tenax)
  #   req(dir_path)
  #   
  #   final_table <- preprocess_folder_tenax(dir_path)
  #   if (nrow(final_table) == 0) {
  #     showNotification("Aucune donnée Tenax exploitable.", type = "error")
  #     return()
  #   }
  #   
  #   output$download_tenax <- downloadHandler(
  #     filename = function() paste0("tenax_formate_", Sys.Date(), ".csv"),
  #     content = function(file) {
  #       write_csv2(final_table, file)
  #       showNotification("✅ Données Tenax prêtes au téléchargement.", type = "message")
  #     }
  #   )
  # })
  
  # -- Choix dossier Plasma --
  shinyDirChoose(input, "dir_plasma", roots = volumes, session = session)
  observeEvent(input$dir_plasma, {
    dir_path <- parseDirPath(volumes, input$dir_plasma)
    req(dir_path)
    
    files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(files) == 0) {
      showModal(modalDialog(title = "Erreur", "Aucun fichier CSV trouvé."))
      return()
    }
    
    all_tables <- map(files, function(filepath) {
      df <- preprocess_single_file(filepath)
      if (nrow(df) == 0) return(tibble())
      
      best_hits <- df %>%
        group_by(Sample, Compound) %>%
        slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      date_extracted <- str_extract(basename(filepath), "\\d{8}")
      date_formatted <- format(as.Date(date_extracted, "%d%m%Y"), "%Y-%m-%d")
      
      best_hits %>%
        group_by(Compound) %>%
        summarise(
          !!paste0("Area_", date_formatted) := mean(Area, na.rm = TRUE),
          !!paste0("CV_", date_formatted) := sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100,
          .groups = "drop"
        )
    })
    
    all_tables <- all_tables[map_lgl(all_tables, ~ nrow(.) > 0)]
    if (length(all_tables) == 0) {
      showNotification("Aucune donnée Plasma exploitable.", type = "error")
      return()
    }
    
    final_table <- reduce(all_tables, full_join, by = "Compound")
    
    output$download_plasma <- downloadHandler(
      filename = function() paste0("plasma_formate_", Sys.Date(), ".csv"),
      content = function(file) {
        write_csv2(final_table, file)
        showNotification("✅ Données Plasma prêtes au téléchargement.", type = "message")
      }
    )
  })
  
  
  observeEvent(input$tenax_dir, {
    volumes <- c(Home = fs::path_home(), "R" = getwd())
    shinyDirChoose(input, "tenax_dir", roots = volumes, session = session)
    
    tenax_path <- parseDirPath(volumes, input$tenax_dir)
    
    if (length(tenax_path) == 0 || tenax_path == "") return(NULL)
    
    # Lister tous les fichiers CSV dans le dossier
    files <- list.files(tenax_path, pattern = "\\.csv$", full.names = TRUE)
    
    # Lecture + extraction des données
    data_all <- purrr::map_dfr(files, function(file) {
      df <- readr::read_csv2(file, show_col_types = FALSE)
      if (!"Compound" %in% names(df)) df <- df %>% rename(Compound = Name)
      df$Area <- as.numeric(gsub(",", ".", gsub("\\.", "", df$Area)))
      df$Date <- stringr::str_extract(basename(file), "\\d{8}") |> lubridate::dmy()
      df
    })
    
    # Nettoyage suffixe (a, b, c...) dans les dates
    data_all$Date_clean <- data_all$Date
    
    # Aire moyenne par molécule et par jour (1 point par jour même si a/b/c)
    aire_par_date <- data_all %>%
      group_by(Compound, Date_clean) %>%
      summarise(Area = mean(Area, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Date_clean, values_from = Area,
                         names_prefix = "Area_")
    
    # CV global par molécule
    cv_global <- data_all %>%
      group_by(Compound) %>%
      summarise(mean_area = mean(Area, na.rm = TRUE),
                sd_area = sd(Area, na.rm = TRUE),
                CV_Global = round(100 * sd_area / mean_area, 2),
                .groups = "drop")
    
    # Fusion finale
    tenax_final <- left_join(aire_par_date, cv_global, by = "Compound")
    
    # Stocker pour export
    output$tenax_export <- downloadHandler(
      filename = function() {
        paste0("tenax_formate_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv2(tenax_final, file)
      }
    )
    
    
  })
  
  
  observeEvent(input$tenax_summary_file, {
    req(input$tenax_summary_file)
    
    df <- read_csv2(input$tenax_summary_file$datapath)
    
    df_long <- df %>%
      pivot_longer(cols = starts_with("Area_"), names_to = "Date", values_to = "Area") %>%
      mutate(Date = str_remove(Date, "Area_"), Date = ymd(Date))
    
    output$tenax_summary_table <- renderDT({
      datatable(df, options = list(scrollX = TRUE, pageLength = 10))
    })
    
    output$tenax_summary_plot <- renderPlotly({
      ggplotly(
        ggplot(df_long, aes(x = Date, y = Area, color = Compound)) +
          geom_line() + geom_point() +
          theme_minimal() +
          labs(title = "Évolution des aires moyennes (Tenax)", x = "Date", y = "Aire") +
          theme(legend.position = "bottom")
      )
    })
    
    
    # output$tenax_summary_plot <- renderPlotly({
    #   ggplot(df_long, aes(x = Date, y = Area, color = Compound)) +
    #     geom_line() + geom_point() +
    #     theme_minimal() +
    #     labs(title = "Évolution des aires moyennes (Tenax)", x = "Date", y = "Aire") +
    #     theme(legend.position = "bottom") %>%
    #     ggplotly()
    # })
    
    output$tenax_cv_plot <- renderPlotly({
      req(input$tenax_summary_file)
      df <- read_csv2(input$tenax_summary_file$datapath, show_col_types = FALSE)
      
      # Marquer les composés extrêmes
      df <- df %>%
        mutate(is_extreme = Compound %in% c("Acetone", "Acetonitrile"))
      
      # Séparer les deux jeux de données
      df_normaux <- df %>% filter(!is_extreme)
      df_extremes <- df %>% filter(is_extreme)
      
      # Barres pour composés normaux (axe Y1)
      trace_normaux <- plot_ly(df_normaux, x = ~Compound, y = ~CV_Global, type = "bar",
                               name = "CV standards", marker = list(color = 'steelblue'),
                               yaxis = "y")
      
      # Barres pour composés extrêmes (axe Y2)
      trace_extremes <- plot_ly(df_extremes, x = ~Compound, y = ~CV_Global, type = "bar",
                                name = "CV extrêmes", marker = list(color = 'firebrick'),
                                yaxis = "y2")
      
      # Combine les deux avec une ligne rouge à 30% sur Y1
      subplot(trace_normaux, trace_extremes) %>%
        layout(
          title = "CV global (%) par composé (double axe Y)",
          xaxis = list(title = "Composés"),
          yaxis = list(
            title = "CV standards",
            side = "left",
            range = c(0, max(df_normaux$CV_Global, na.rm = TRUE) * 1.2),
            overlaying = NULL
          ),
          yaxis2 = list(
            title = "CV extrêmes",
            side = "right",
            overlaying = "y",
            showgrid = FALSE,
            range = c(0, max(df_extremes$CV_Global, na.rm = TRUE) * 1.2)
          ),
          shapes = list(list(
            type = "line",
            x0 = -0.5,
            x1 = length(df$Compound) - 0.5,
            y0 = 30,
            y1 = 30,
            yref = "y",
            line = list(color = "red", dash = "dash")
          )),
          legend = list(orientation = "h", x = 0.1, y = 1.1)
        )
    })
    
    
    
    
    
    
    
    # output$tenax_cv_plot <- renderPlotly({
    #   plot_ly(df, x = ~Compound, y = ~CV_Global, type = "bar", name = "CV") %>%
    #     layout(title = "CV global (%) par composé", yaxis = list(title = "CV (%)"))
    # })
  })
  
  
}


# ---- APP ----
shinyApp(ui, server)




#noni
