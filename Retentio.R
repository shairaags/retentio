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
  dashboardHeader(title = "Retentio"),
  dashboardSidebar(
    uiOutput("dynamic_sidebar")
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
    .shiny-notification {
      position: fixed;
      top: 50% !important;
      left: 50% !important;
      transform: translate(-50%, -50%) !important;
      width: 350px;
      font-size: 18px;
      z-index: 9999;
    }
  "))
    ),
    tags$head(
      tags$style(HTML("
      /* ici tu peux conserver TON style existant */
    "))
    ),
    
    # 🧠 Ajout d’un onglet principal à deux panneaux
    tabBox(
      id = "main_tabs",
      width = 12,
      height = "auto",
      
      # ONGLET 1 : Retentio Original
      tabPanel("Tenax",
               # 👇 tout ton contenu actuel ici
               fluidRow(
                 valueBoxOutput("cvBox"),
                 valueBoxOutput("meanBox"),
                 conditionalPanel(condition = "output.showSequenceBox", valueBoxOutput("nSeqBox")),
                 conditionalPanel(condition = "output.showCVFAME", valueBoxOutput("cvBoxFAME")),
                 valueBoxOutput("cvBoxInterne")
               ),
               
               fluidRow(
                 box(title = "Fichiers chargés", width = 12,
                     verbatimTextOutput("loadedFiles"),
                     verbatimTextOutput("data_summary"))
               ),
               
               
               fluidRow(
                 tabBox(
                   title = "Visualisation",
                   width = 12,
                   id = "main_tab",
                   
                   # Onglet résumé standard
                   tabPanel("📊",
                            plotlyOutput("tenax_summary_plot"),
                            plotlyOutput("tenax_cv_plot"),
                            DTOutput("tenax_summary_table")),
                   
                   # Onglet par tube déplacé ici
                   tabPanel("Suivi par tube",
                            shinyDirButton("dir_tubes", "📁 Choisir dossier tubes individuels", "Sélectionner dossier de fichiers QC_xxx.csv"),
                            uiOutput("tube_selector_ui"),
                            verbatimTextOutput("tube_file_info"),
                            tags$hr(),
                            fluidRow(
                              valueBoxOutput("tube_area_mean"),
                              valueBoxOutput("tube_cv_mean"),
                              valueBoxOutput("tube_cv_above_30"),
                              valueBoxOutput("tube_file_count")
                            ),
                            plotlyOutput("plot_area_by_date"),
                            plotlyOutput("plot_cv_by_date"),
                            plotlyOutput("plot_heatmap_tube"),
                            DTOutput("table_tube_data"),
                            downloadButton("download_tube_csv", "Télécharger données tube")
                   )
                 )
               ),
               
               
               
               
               fluidRow(box(title = "Données filtrées", width = 12, DTOutput("dataTable"))),
               fluidRow(
                 downloadButton("downloadCSV", "Télécharger CSV nettoyé"),
                 actionButton("zoom_issues", "Zoom sur problèmes"),
                 actionButton("manual_correct", "Corriger manuellement")
               )
      ),
      
      # ONGLET 2 : Clone
      tabPanel("Plasma", uiOutput("retentio2_ui"))
    )
  )
  
)

# ---- SERVER ----
server <- function(input, output, session) {
  Sys.setlocale("LC_NUMERIC", "C")
  
  data_reactive_1 <- reactiveVal()  # pour onglet Retentio 1
  data_reactive <- reactiveVal()  # ✅ Ajout indispensable
  data_reactive_2 <- reactiveVal()  # pour onglet Retentio 2
  
  
  data_plasma_wide <- reactiveVal()
  
  tube_raw_data <- reactiveVal()
  tube_available_ids <- reactiveVal()
  
  
  
  
  source("qualityChecks.R")
  source("preprocess.R")
  
  output$dynamic_sidebar <- renderUI({
    if (input$main_tabs == "Tenax") {
      # 🧠 Sidebar pour le premier onglet
      tagList(
        fileInput("file_upload_combined", "🧭 Charger vos fichiers Tenax (.xlsx ou .csv formaté)", 
                  accept = c(".xlsx", ".csv"), multiple = TRUE),
        shinyDirButton("tenax_dir", "📁 Choisir dossier Tenax", "Sélectionner dossier Tenax"),
        downloadButton("tenax_export", "Fichier formaté Tenax 💾"),
        tags$br(), tags$br(),
        tags$hr(),
        uiOutput("sheet_selector"),
        actionButton("refresh", "✨Rafraîchir les données"),
        actionButton("reset", "✨Réinitialiser les filtres"),
        tags$hr(),
        h4("Filtres"),
        pickerInput("analyte", "Choisir un composé :", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE)),
        conditionalPanel(
          condition = "input.main_tabs != 'Tenax'",  # ↩ ne l'affiche pas dans l'onglet Tenax
          pickerInput("multi_analytes", "Selection multiple ^^ :", 
                      choices = NULL, multiple = TRUE, selected = NULL,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE))
        ),
        checkboxInput("flag_only", "Voir uniquement les anomalies", value = FALSE),
        
        
        tags$hr(),
        h4("Suivi GCxGC-MS"),
        switchInput("dark_mode", label = NULL, onLabel = "🌙", offLabel = "☀️", value = FALSE)
      )
      
    } else if (input$main_tabs == "Plasma") {
      # 🧠 Sidebar du second onglet → tu peux dupliquer OU créer des filtres différents ici
      tagList(
        fileInput("file_upload2", "🧭 Ajouter vos fichiers (.xlsx ou .csv)", accept = c(".xlsx", ".csv"), multiple = TRUE),
        shinyDirButton("dir_plasma", "📁 Choisir dossier Plasma", "Sélectionner dossier Plasma"),
        downloadButton("download_plasma", "Fichier formaté Plasma 💾"),
        tags$hr(style = "border-top: 1px solid white;"),  # 👈 ligne blanche immédiatement après
        actionButton("refresh2", "✨Rafraîchir les données"),
        actionButton("reset2", "✨Réinitialiser les filtres"),
        tags$hr(),
        h4("Filtres"),
        pickerInput("analyte2", "Choisir un composé :", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE)),
        pickerInput("multi_analytes2", "Sélection multiple pour Cinétiques multi-composés :", choices = NULL, multiple = TRUE, selected = NULL, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        checkboxInput("flag_only2", "Voir uniquement les anomalies", value = FALSE),
        tags$hr(),
        h4("Suivi GCxGC-MS"),
        switchInput("dark_mode", label = NULL, onLabel = "🌙", offLabel = "☀️", value = FALSE)
      )
      
    }
  })
  
  filtered_data_1 <- reactive({
    req(data_reactive_1(), input$analyte)
    df <- data_reactive_1()
    df <- df %>% filter(str_to_lower(Compound) == str_to_lower(input$analyte))
    if (input$flag_only) df <- df %>% filter(Flagged)
    validate(need(nrow(df) > 0, "Aucune donnée disponible avec les filtres actuels."))
    df
  })
  
  filtered_data_2 <- reactive({
    req(data_reactive_2(), input$analyte2)
    df <- data_reactive_2()
    df <- df %>% filter(str_to_lower(Compound) == str_to_lower(input$analyte2))
    if (input$flag_only2) df <- df %>% filter(Flagged)
    validate(need(nrow(df) > 0, "Aucune donnée disponible avec les filtres actuels."))
    df
  })
  
  
  
  
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
        
      } else if (any(str_detect(cols_lower, "^area_"))) {
        message("📄 Détecté fichier CSV formaté. Traitement avec DateLabel...")
        
        df <- df %>%
          pivot_longer(
            cols = -Compound,
            names_to = "Measure",
            values_to = "Value"
          ) %>%
          separate(Measure, into = c("TypeMeasure", "DateRaw"), sep = "_", extra = "merge") %>%
          mutate(
            Date = suppressWarnings(ymd(DateRaw)),  # ← ✅ Ajouté ici proprement
            DateLabel = str_remove(DateRaw, "^QC_|^bio_|^Area_|^CV_"),
            DateLabel = str_replace_all(DateLabel, "\\.", "_"),
            DateLabel = factor(DateLabel, levels = unique(DateRaw)),  # ordre de lecture
            Value = as.numeric(str_replace(as.character(Value), ",", "."))
          ) %>%
          pivot_wider(names_from = TypeMeasure, values_from = Value) %>%
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
          relocate(Compound, Date, DateLabel, Area, CV, Type, Sequence, source_file, Flagged)
        
        
        # df <- df %>%
        #   pivot_longer(
        #     cols = -Compound,
        #     names_to = "Measure",
        #     values_to = "Value"
        #   ) %>%
        #   separate(Measure, into = c("TypeMeasure", "DateRaw"), sep = "_", extra = "merge") %>%
        #   mutate(
        #     DateLabel = str_remove(DateRaw, "^QC_|^bio_|^Area_|^CV_"),
        #     DateLabel = str_replace_all(DateLabel, "\\.", "_"),
        #     DateLabel = factor(DateLabel, levels = unique(DateRaw)),  # ordre de lecture
        #     Value = as.numeric(str_replace(as.character(Value), ",", "."))
        #   ) %>%
        #   pivot_wider(names_from = TypeMeasure, values_from = Value) %>%
        #   mutate(
        #     Type = case_when(
        #       str_detect(tolower(Compound), "fame") ~ "FAME",
        #       str_detect(tolower(Compound), "-d[0-9]+") ~ "Étalon Interne",
        #       TRUE ~ "Analyte"
        #     ),
        #     Sequence = "Formaté CSV",
        #     source_file = filename,
        #     Flagged = FALSE
        #   ) %>%
        #   relocate(Compound, DateLabel, Area, CV, Type, Sequence, source_file, Flagged)
        
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
  
  # observeEvent(input$refresh, {
  #   req(input$file_upload)
  #   
  #   files <- input$file_upload$datapath
  #   filenames <- input$file_upload$name
  #   
  #   full_data <- tibble()
  #   
  #   withProgress(message = "⏳ Chargement des fichiers...", value = 0, {
  #     total_steps <- length(files)
  #     
  #     for (i in seq_along(files)) {
  #       file_path <- files[i]
  #       file_name <- filenames[i]
  #       
  #       if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
  #         # ✅ Lecture intelligente même pour fichiers déjà formatés (Plasma ou Tenax)
  #         df <- preprocess_smart(file_path, file_name)
  #         full_data <- bind_rows(full_data, df)
  #       }
  #       
  #       
  #       
  #       # Cas Excel
  #       if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
  #         sheets <- excel_sheets(file_path)
  #         for (sheet in sheets) {
  #           df <- preprocess_smart(file_path, file_name, sheet)
  #           full_data <- bind_rows(full_data, df)
  #         }
  #       }
  #       
  #       incProgress(1 / total_steps)
  #     }
  #   })
  #   
  #   if (nrow(full_data) == 0) {
  #     showModal(
  #       modalDialog(
  #         title = "❌ Erreur",
  #         "Aucun fichier exploitable trouvé.",
  #         easyClose = TRUE,
  #         footer = modalButton("OK")
  #       )
  #     )
  #     return()
  #   }
  #   
  #   
  #   #df_flagged <- flag_anomalies(full_data)
  #   if ("RT" %in% names(full_data)) {
  #     df_flagged <- flag_anomalies(full_data)
  #   } else {
  #     df_flagged <- full_data
  #   }
  #   
  #   data_reactive(df_flagged)
  #   
  #   updatePickerInput(session, "analyte", choices = sort(unique(df_flagged$Compound)))
  #   updatePickerInput(session, "multi_analytes", choices = unique(df_flagged$Compound))
  #   
  #   showNotification("✅ Fichiers chargés avec succès.", type = "message")
  # })
  
  observe({
    req(input$dark_mode)
    
    if (input$dark_mode) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  observeEvent(input$refresh2, {
    req(input$file_upload2)
    
    files <- input$file_upload2$datapath
    filenames <- input$file_upload2$name
    
    
    
    full_data <- tibble()
    
    for (i in seq_along(files)) {
      file_path <- files[i]
      file_name <- filenames[i]
      
      if (grepl("\\.csv$", file_name)) {
        df <- preprocess_smart(file_path, file_name)
      } else {
        df <- preprocess_smart(file_path, file_name, sheet_name = NULL)
      }
      full_data <- bind_rows(full_data, df)
    }
    
    data_reactive_2(full_data)
    
    if (any(str_detect(names(full_data), "^Area_"))) {
      data_plasma_wide(full_data)
    }
    
    
    # CLASSIFICATION intelligente des composés
    compounds <- sort(unique(full_data$Compound))
    choices_grouped <- split(compounds, case_when(
      str_detect(compounds, "-d\\d+") ~ "Étalon Interne",
      str_detect(compounds, "^C\\d+") ~ "FAME",
      TRUE ~ "FAME"
    ))
    
    
    #updatePickerInput(session, "analyte2", choices = sort(unique(full_data$Compound)))
    updatePickerInput(session, "analyte2", choices = choices_grouped, selected = NULL)
    
    #updatePickerInput(session, "multi_analytes2", choices = sort(unique(full_data$Compound)))
    updatePickerInput(session, "multi_analytes2", choices = choices_grouped, selected = NULL)
    
    updatePickerInput(session, "sequence2", choices = sort(unique(full_data$Sequence)))
    updatePickerInput(session, "type2", selected = "Tous")
    updateCheckboxInput(session, "flag_only2", value = FALSE)
    
  })
  
  
  
  observeEvent(input$reset, {
    data_reactive(NULL)  # on vide les données
    data_reactive_1(NULL)
    
    output$tenax_summary_plot <- renderPlotly({ NULL })
    output$tenax_cv_plot <- renderPlotly({ NULL })
    output$tenax_summary_table <- renderDT({ NULL })
    
    
    updatePickerInput(session, "analyte", selected = character(0), choices = NULL)
    updatePickerInput(session, "multi_analytes", selected = character(0), choices = NULL)
    updateCheckboxInput(session, "flag_only", value = FALSE)
    updatePickerInput(session, "sequence", selected = character(0), choices = NULL)
    updatePickerInput(session, "type", selected = "Tous", choices = c("Tous", "FAME", "Analyte", "Étalon Interne"))
    updatePickerInput(session, "sequence", selected = character(0), choices = NULL)
    updatePickerInput(session, "type", selected = "Tous", choices = c("Tous", "FAME", "Analyte", "Étalon Interne"))
    
    
    showNotification("🔁 Onglet Tenax réinitialisé", type = "message")
    
    shinyjs::reset("file_upload_combined")
    
    tube_raw_data(NULL)
    tube_available_ids(NULL)
    updateSelectInput(session, "selected_tube", selected = character(0), choices = character(0))
    
    
    
  })
  
  
  observeEvent(input$reset2, {
    data_reactive_2(NULL)  # vide les données de l'onglet 2
    
    updatePickerInput(session, "analyte2", selected = character(0), choices = NULL)
    updatePickerInput(session, "multi_analytes2", selected = character(0), choices = NULL)
    updateCheckboxInput(session, "flag_only2", value = FALSE)
    updatePickerInput(session, "sequence2", selected = character(0), choices = NULL)
    updatePickerInput(session, "type2", selected = "Tous", choices = c("Tous", "FAME", "Analyte", "Étalon Interne"))
    
    showNotification("🔁 Onglet Plasma réinitialisé", type = "message")
    
    shinyjs::reset("file_upload2")
    
    
  })
  
  
  
  filtered_data <- reactive({
    req(data_reactive(), input$analyte)
    df <- data_reactive()
    
    df <- df %>% filter(str_to_lower(Compound) == str_to_lower(input$analyte))
    
    if (input$flag_only) df <- df %>% filter(Flagged)
    
    # 🎯 Filtrage par séquence
    if (!is.null(input$sequence) && length(input$sequence) > 0) {
      df <- df %>% filter(Sequence %in% input$sequence)
    }
    
    # 🎯 Filtrage par type
    if (!is.null(input$type) && input$type != "Tous") {
      df <- df %>% filter(Type == input$type)
    }
    
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
  
  output$meanBox <- renderValueBox({
    df <- filtered_data()
    mean_val <- mean(df$Area, na.rm = TRUE)
    formatted <- format(mean_val, scientific = TRUE, digits = 3)
    valueBox(ifelse(is.na(mean_val), "NA", formatted), subtitle = "Aire Moyenne", color = "blue")
  })
  
  
  
  
  # output$meanBox <- renderValueBox({
  #   df <- filtered_data()
  #   mean_val <- round(mean(df$Area, na.rm = TRUE), 2)
  #   formatted <- format(mean_val, decimal.mark = ".", big.mark = "", scientific = FALSE)
  #   valueBox(ifelse(is.na(mean_val), "NA", formatted), subtitle = "Aire Moyenne", color = "blue")
  # })
  
  
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
  
  
  
  output$multiCVPlot <- renderPlotly({
    df <- data_reactive() %>%
      filter(Compound %in% input$multi_analytes,
             !is.na(Date))
    
    
    # Message d'avertissement si certaines molécules n'ont pas de données
    missing <- setdiff(input$multi_analytes, unique(df$Compound))
    if (length(missing) > 0) {
      showNotification(paste("⚠️ Pas de données pour :", paste(missing, collapse = ", ")), type = "warning", duration = 8)
    }
    
    
    if (nrow(df) == 0) return(NULL)
    
    cv_data <- df %>%
      
      filter(!is.na(CV)) %>%
      arrange(Date) #%>%
    
    
    #mutate(CV = CV * 100) ruinay
    p <- ggplot(cv_data, aes(x = Date, y = CV, color = Compound)) +
      geom_line() + geom_point() +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Cinétique des CV (%) des étalons internes",
           y = "CV (%)", x = "Date") +
      theme(legend.position = "bottom")
    
    
    
    
    ggplotly(p)
  })
  
  output$multiAreaPlot <- renderPlotly({
    df <- data_reactive() %>%
      filter(Compound %in% input$multi_analytes,
             !is.na(Date))
    
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
  
  output$multiCVPlot2 <- renderPlotly({
    req(input$multi_analytes2)
    df <- data_reactive() %>%
      filter(Compound %in% input$multi_analytes2, !is.na(Date), !is.na(CV)) %>%
      arrange(Date)
    
    if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "Aucune donnée à afficher"))
    
    p <- ggplot(df, aes(x = Date, y = CV, color = Compound)) +
      geom_line() + geom_point() +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Cinétique des CV (%)", y = "CV (%)", x = "Date") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$multiAreaPlot2 <- renderPlotly({
    req(input$multi_analytes2)
    df <- data_reactive() %>%
      filter(Compound %in% input$multi_analytes2, !is.na(Date), !is.na(Area)) %>%
      arrange(Date)
    
    if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "Aucune donnée à afficher"))
    
    p <- ggplot(df, aes(x = Date, y = Area, color = Compound)) +
      geom_line() + geom_point() +
      theme_minimal() +
      labs(title = "Cinétique des Aires", y = "Area", x = "Date") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  
  output$cvBoxFAME2 <- renderValueBox({
    df <- filtered_data_2() %>% filter(Type == "FAME")
    if (nrow(df) == 0 || all(is.na(df$Area))) return(valueBox("NA", subtitle = "CV FAME", color = "aqua"))
    cv <- round(sd(df$Area, na.rm = TRUE) / mean(df$Area, na.rm = TRUE) * 100, 1)
    box_color <- if (cv < 10) "green" else "orange"
    valueBox(cv, subtitle = "CV injection (FAME)", color = box_color)
  })
  
  output$cvBoxInterne2 <- renderValueBox({
    df <- filtered_data_2() %>% filter(Type == "Étalon Interne")
    if (nrow(df) == 0 || all(is.na(df$CV))) return(valueBox("NA", subtitle = "CV extraction (Étalon)", color = "aqua"))
    cv_moyen <- round(mean(df$CV, na.rm = TRUE), 1)
    box_color <- if (cv_moyen < 30) "green" else "orange"
    valueBox(cv_moyen, subtitle = "CV extraction (Étalon)", color = box_color)
  })
  
  output$showCVFAME2 <- reactive({
    df <- filtered_data_2()
    any(df$Type == "FAME") && any(!is.na(df$Area))
  })
  outputOptions(output, "showCVFAME2", suspendWhenHidden = FALSE)
  
  output$showCVInterne2 <- reactive({
    df <- filtered_data_2()
    any(df$Type == "Étalon Interne") && any(!is.na(df$Area))
  })
  outputOptions(output, "showCVInterne2", suspendWhenHidden = FALSE)
  
  output$showSequenceBox2 <- reactive({
    df <- filtered_data_2()
    length(unique(df$Sequence)) > 1
  })
  outputOptions(output, "showSequenceBox2", suspendWhenHidden = FALSE)
  
  output$multiCVPlot2 <- renderPlotly({
    req(data_reactive_2(), nrow(data_reactive_2()) > 0)
    req(input$multi_analytes2)
    
    df <- data_reactive_2() %>%
      filter(Compound %in% input$multi_analytes2, !is.na(Date), !is.na(CV)) %>%
      arrange(Date)
    
    if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = Date, y = CV, color = Compound)) +
      geom_line() + geom_point() +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Cinétique des CV (%)", y = "CV (%)", x = "Date") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  
  output$multiAreaPlot2 <- renderPlotly({
    req(data_reactive_2(), nrow(data_reactive_2()) > 0)
    req(input$multi_analytes2)
    
    df <- data_reactive_2() %>%
      filter(Compound %in% input$multi_analytes2, !is.na(Date), !is.na(Area)) %>%
      arrange(Date)
    
    if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = Date, y = Area, color = Compound)) +
      geom_line() + geom_point() +
      theme_minimal() +
      labs(title = "Cinétique des Aires", y = "Aire", x = "Date") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  
  output$downloadCSV2 <- downloadHandler(
    filename = function() paste0("donnees_filtrees_retentio2_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered_data_2(), file)
  )
  
  output$loadedFiles2 <- renderText({
    req(input$file_upload2)
    paste0("Fichiers chargés :\n", paste(input$file_upload2$name, collapse = "\n"))
  })
  
  output$data_summary2 <- renderText({
    req(data_reactive_2())
    df <- data_reactive_2()
    paste0("✅ ", length(unique(df$Compound)), " composés | ",
           length(unique(df$Sequence)), " séquences | ",
           length(unique(df$Date)), " dates | ",
           nrow(df), " points")
  })
  
  # 🔍 Validation automatique sous les graphiques CV / Multi-Analytes (Plasma)
  output$sequence_validation_cv <- renderText({
    df <- filtered_data_2()
    req(nrow(df) > 0)
    
    seuil <- 30
    dépassements <- df %>% filter(!is.na(CV) & CV > seuil) %>%
      group_by(Compound) %>%
      summarise(CV_max = round(max(CV, na.rm = TRUE), 1), .groups = "drop")
    
    if (nrow(dépassements) == 0) {
      return("✅ Séquence validée : tous les composés ont un CV ≤ 30%.")
    } else {
      msg <- paste0("❌ Séquence non validée : ", nrow(dépassements), " composé(s) dépassent 30% de CV.\n\n")
      msg <- paste0(msg, "Composés concernés :\n")
      details <- paste0("- ", dépassements$Compound, " : ", dépassements$CV_max, " %")
      return(paste(c(msg, details), collapse = "\n"))
    }
  })
  
  output$sequence_validation_multi <- renderText({
    df <- data_reactive_2()
    req(nrow(df) > 0, input$multi_analytes2)
    
    df <- df %>% filter(Compound %in% input$multi_analytes2)
    
    seuil <- 30
    dépassements <- df %>% filter(!is.na(CV) & CV > seuil) %>%
      group_by(Compound) %>%
      summarise(CV_max = round(max(CV, na.rm = TRUE), 1), .groups = "drop")
    
    if (nrow(dépassements) == 0) {
      return("✅ Séquence validée : tous les composés sélectionnés ont un CV ≤ 30%.")
    } else {
      msg <- paste0("❌ Séquence non validée : ", nrow(dépassements), " composé(s) sélectionnés dépassent 30% de CV.\n\n")
      msg <- paste0(msg, "Composés concernés :\n")
      details <- paste0("- ", dépassements$Compound, " : ", dépassements$CV_max, " %")
      return(paste(c(msg, details), collapse = "\n"))
    }
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
      
      if (nrow(df) == 0) return(NULL)
      
      
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
  
  output$cvBox2 <- renderValueBox({
    df <- filtered_data_2()
    if (all(is.na(df$CV))) return(valueBox("NA", subtitle = "CV%", color = "aqua"))
    cv_moyen <- round(mean(df$CV, na.rm = TRUE), 1)
    box_color <- if (cv_moyen < 30) "green" else "red"
    valueBox(cv_moyen, subtitle = "CV%", color = box_color)
  })
  
  output$meanBox2 <- renderValueBox({
    df <- filtered_data_2()
    mean_val <- mean(df$Area, na.rm = TRUE)
    formatted <- format(mean_val, scientific = TRUE, digits = 3)
    valueBox(ifelse(is.na(mean_val), "NA", formatted), subtitle = "Aire Moyenne", color = "blue")
  })
  
  
  # output$meanBox2 <- renderValueBox({
  #   df <- filtered_data_2()
  #   mean_val <- round(mean(df$Area, na.rm = TRUE), 2)
  #   formatted <- format(mean_val, decimal.mark = ".", big.mark = "", scientific = FALSE)
  #   valueBox(ifelse(is.na(mean_val), "NA", formatted), subtitle = "Aire Moyenne", color = "blue")
  # })
  
  output$nSeqBox2 <- renderValueBox({
    df <- filtered_data_2()
    valueBox(length(unique(df$Sequence)), subtitle = "Séquences", color = "purple")
  })
  
  output$dataTable2 <- renderDT({
    datatable(filtered_data_2(), editable = "cell", options = list(pageLength = 10))
  })
  
  observeEvent(input$dataTable2_cell_edit, {
    info <- input$dataTable2_cell_edit
    df <- data_reactive_2()
    if (info$col == which(colnames(df) == "Flagged")) {
      df[info$row, "Flagged"] <- as.logical(info$value)
      data_reactive_2(df)
    }
  })
  
  output$CVPlot_time2 <- renderPlotly({
    req(data_reactive_2(), nrow(data_reactive_2()) > 0)
    
    df <- filtered_data_2()
    df$Date <- as.Date(df$Date)
    df <- df[order(df$Date), ]
    
    plot_ly(df, x = ~Date, y = ~CV, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        yaxis = list(title = "CV (%)"),
        shapes = list(
          list(
            type = "line",
            x0 = min(df$Date, na.rm = TRUE),
            x1 = max(df$Date, na.rm = TRUE),
            y0 = 30,
            y1 = 30,
            line = list(dash = "dash", color = "red"),
            xref = "x",
            yref = "y"
          )
        )
      )
  })
  
  
  
  output$areaPlot2 <- renderPlotly({
    req(data_reactive_2(), nrow(data_reactive_2()) > 0)
    
    df <- filtered_data_2()
    plot_ly(df, x = ~Date, y = ~Area, type = 'scatter', mode = 'lines+markers') %>%
      layout(yaxis = list(title = "Aire"))
  })
  
  
  output$trendPlot2 <- renderPlotly({
    req(input$file_upload2)
    
    # 🔍 Lecture brute du fichier chargé (comme le user l’a fourni)
    filepath <- input$file_upload2$datapath[1]
    df_raw <- read_csv2(filepath, show_col_types = FALSE)
    
    # Vérifie la présence de colonnes Max_
    max_cols <- grep("^Max_", names(df_raw), value = TRUE)
    if (length(max_cols) == 0) {
      return(plotly_empty() %>% layout(title = "❌ Les colonnes Max_ n'ont pas été détectées, vérifie l'encodage ou le séparateur CSV."))
    }
    
    # ⚙️ Long format Min / Max / Moyenne
    df_long <- df_raw %>%
      filter(Compound == input$analyte2) %>%  # ⬅️ FILTRAGE ajouté ici
      pivot_longer(cols = -Compound, names_to = "Measure", values_to = "Value") %>%
      mutate(
        Stat = case_when(
          str_starts(Measure, "Min_") ~ "Min",
          str_starts(Measure, "Max_") ~ "Max",
          str_starts(Measure, "Area_") ~ "Moyenne",
          TRUE ~ NA_character_
        ),
        Date = str_extract(Measure, "\\d{4}-\\d{2}-\\d{2}")
      ) %>%
      filter(!is.na(Stat)) %>%
      mutate(
        Date = as.Date(Date),
        Date = factor(Date, levels = sort(unique(Date))),
        Value = as.numeric(str_replace(as.character(Value), ",", "."))
      ) %>%      # ✅ ici le pipe est bien relié à la suite
      arrange(Compound, Stat, Date)  # 🔁 tri chronologique réel
    
    df_long$Date <- as.Date(df_long$Date)
    
    
    
    
    
    # mutate(
    #   Date = as.Date(Date),
    #   Value = as.numeric(str_replace(as.character(Value), ",", "."))
    # )
    
    # 📈 Plotly
    plot_ly(df_long, x = ~Date, y = ~Value, color = ~Stat, type = "scatter", mode = "lines+markers") %>%
      
      layout(
        title = "Tendances Min / Max / Moyenne des aires",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Aire"),
        legend = list(orientation = "h", x = 0.1, y = 1.1),
        shapes = list(
          list(
            type = "line",
            x0 = min(df_long$Date, na.rm = TRUE),
            x1 = max(df_long$Date, na.rm = TRUE),
            y0 = mean(df_long$Value[df_long$Stat == "Moyenne"], na.rm = TRUE),
            y1 = mean(df_long$Value[df_long$Stat == "Moyenne"], na.rm = TRUE),
            line = list(dash = "dash", color = "red"),
            xref = "x",
            yref = "y"
          )
        )
      )
    
    
    # layout(
    #   title = "Tendances Min / Max / Moyenne des aires",
    #   xaxis = list(title = "Date"),
    #   yaxis = list(title = "Aire"),
    #   legend = list(orientation = "h", x = 0.1, y = 1.1)
    # )
  })
  
  
  
  
  
  
  
  
  # output$trendPlot2 <- renderPlotly({
  #   req(data_reactive_2(), nrow(data_reactive_2()) > 0)
  #   
  #   df <- filtered_data_2()
  #   summary <- df %>%
  #     group_by(Date) %>%
  #     summarise(Min = min(Area, na.rm = TRUE),
  #               Max = max(Area, na.rm = TRUE),
  #               Mean = mean(Area, na.rm = TRUE),
  #               .groups = "drop")
  #   
  #   plot_ly(summary, x = ~Date) %>%
  #     add_lines(y = ~Min, name = "Min", line = list(color = "blue")) %>%
  #     add_lines(y = ~Mean, name = "Moyenne", line = list(color = "orange")) %>%
  #     add_lines(y = ~Max, name = "Max", line = list(color = "green")) %>%
  #     layout(yaxis = list(title = "Tendance des Aires"))
  # })
  
  
  
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
  
  output$download_cv_plot2 <- downloadHandler(
    filename = function() {
      paste0("CV_plot_ret2_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- data_reactive_2() %>%
        filter(Compound %in% input$multi_analytes2,
               !is.na(Date), !is.na(Area), Area > 0)
      
      if (nrow(df) == 0) return(NULL)
      
      cv_data <- df %>%
        filter(!is.na(CV)) %>%
        arrange(Date)
      
      p <- ggplot(cv_data, aes(x = Date, y = CV)) +
        geom_point(aes(color = CV > 30)) +
        geom_line(aes(group = Compound), color = "black") +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(title = "Cinétique des CV (%) des étalons internes",
             y = "CV (%)", x = "Date") +
        theme(legend.position = "none")
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_area_plot2 <- downloadHandler(
    filename = function() {
      paste0("Area_plot_ret2_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- data_reactive_2() %>%
        filter(Compound %in% input$multi_analytes2,
               !is.na(Date), !is.na(Area), Area > 0)
      
      if (nrow(df) == 0) return(NULL)
      
      df_log <- df %>%
        mutate(logArea = log10(Area)) %>%
        filter(!is.na(logArea)) %>%
        arrange(Date)
      
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
    
    withProgress(message = "⏳ Traitement des fichiers Tenax...", value = 0, {
      total_steps <- length(files)
      all_tables <- list()
      
      for (i in seq_along(files)) {
        filepath <- files[i]
        df <- read_csv2(filepath, show_col_types = FALSE)
        
        if ("Sample" %in% names(df) && "Area" %in% names(df)) {
          best_hits <- df %>%
            group_by(Sample, Compound) %>%
            slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
            ungroup()
          
          date_extracted <- str_extract(basename(filepath), "\\d{8}")
          date_formatted <- format(as.Date(date_extracted, "%d%m%Y"), "%Y-%m-%d")
          
          summarised <- best_hits %>%
            group_by(Compound) %>%
            summarise(
              !!paste0("Area_", date_formatted) := mean(Area, na.rm = TRUE),
              !!paste0("CV_", date_formatted) := sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100,
              .groups = "drop"
            )
          all_tables[[i]] <- summarised
        } else if (all(c("Compound", "Area", "CV") %in% names(df))) {
          date_extracted <- str_extract(basename(filepath), "\\d{8}")
          date_formatted <- format(as.Date(date_extracted, "%d%m%Y"), "%Y-%m-%d")
          summarised <- df %>%
            rename(
              !!paste0("Area_", date_formatted) := Area,
              !!paste0("CV_", date_formatted) := CV
            )
          all_tables[[i]] <- summarised
        } else {
          all_tables[[i]] <- tibble()
        }
        
        incProgress(1 / total_steps)
      }
      
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
  })
  
  shinyDirChoose(input, "dir_tubes", roots = volumes, session = session)
  
  observeEvent(input$dir_tubes, {
    path <- parseDirPath(volumes, input$dir_tubes)
    req(path)
    
    files <- list.files(path, pattern = "^QC_.*\\.csv$", full.names = TRUE)
    if (length(files) == 0) {
      showNotification("❌ Aucun fichier QC_ trouvé dans ce dossier", type = "error")
      return()
    }
    
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
    
    tube_raw_data(df_all)
    tube_available_ids(unique(df_all$TubeID))
  })
  
  output$tube_selector_ui <- renderUI({
    req(tube_available_ids())
    selectizeInput("selected_tube", "🔍 Choisir un tube :", choices = tube_available_ids(), selected = NULL)
  })
  
  
  output$tube_file_info <- renderText({
    req(tube_raw_data(), input$selected_tube)
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    dates <- sort(unique(df$Date))
    paste0("✅ ", nrow(df), " points | ", length(unique(df$File)), " fichiers\n📅 ", format(min(dates)), " → ", format(max(dates)))
  })
  
  output$tube_area_mean <- renderValueBox({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    valueBox(format(mean(df$Area, na.rm = TRUE), scientific = TRUE, digits = 3), "Aire moyenne", color = "blue")
  })
  
  output$tube_cv_mean <- renderValueBox({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    df_cv <- df %>% group_by(Compound, Date) %>% summarise(cv = sd(Area)/mean(Area)*100, .groups = "drop")
    valueBox(round(mean(df_cv$cv, na.rm = TRUE), 1), "CV moyen (%)", color = "aqua")
  })
  
  output$tube_cv_above_30 <- renderValueBox({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    df_cv <- df %>% group_by(Compound, Date) %>% summarise(cv = sd(Area)/mean(Area)*100, .groups = "drop")
    valueBox(sum(df_cv$cv > 30, na.rm = TRUE), "Composés > 30%", color = "orange")
  })
  
  output$tube_file_count <- renderValueBox({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    valueBox(length(unique(df$File)), "Fichiers détectés", color = "purple")
  })
  
  output$plot_area_by_date <- renderPlotly({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    plot_ly(df, x = ~Date, y = ~Area, color = ~Compound, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Aires par date", yaxis = list(title = "Area"), xaxis = list(title = "Date"))
  })
  
  output$plot_cv_by_date <- renderPlotly({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    df_cv <- df %>% group_by(Compound, Date) %>% summarise(cv = sd(Area)/mean(Area)*100, .groups = "drop")
    plot_ly(df_cv, x = ~Date, y = ~cv, color = ~Compound, type = "scatter", mode = "lines+markers") %>%
      layout(title = "CV (%) par date", yaxis = list(title = "CV (%)"), xaxis = list(title = "Date"))
  })
  
  output$plot_heatmap_tube <- renderPlotly({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    df_avg <- df %>% group_by(Compound, Date) %>% summarise(Aire = mean(Area), .groups = "drop")
    df_wide <- pivot_wider(df_avg, names_from = Date, values_from = Aire)
    mat <- as.matrix(df_wide[,-1])
    rownames(mat) <- df_wide$Compound
    
    plot_ly(z = mat, type = "heatmap", x = colnames(mat), y = rownames(mat), colorscale = "Viridis") %>%
      layout(title = "Heatmap Aire", xaxis = list(title = "Date"), yaxis = list(title = "Composé"))
  })
  
  output$table_tube_data <- renderDT({
    df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
    datatable(df %>% select(Compound, Area, Date, File), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$download_tube_csv <- downloadHandler(
    filename = function() paste0("tube_", input$selected_tube, "_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- tube_raw_data() %>% filter(TubeID == input$selected_tube)
      write_csv2(df, file)
    }
  )
  
  
  
  
  
  
  # -- Choix dossier Plasma --
  shinyDirChoose(input, "dir_plasma", roots = volumes, session = session)
  observeEvent(input$dir_plasma, {
    dir_path <- parseDirPath(volumes, input$dir_plasma)
    req(dir_path)
    
    if (!str_detect(tolower(basename(dir_path)), "plasma")) {
      showModal(modalDialog(
        title = "Erreur",
        div(style = "font-size:18px; color:red; text-align:center;",
            "❌ Ceci n'est pas un dossier Plasma."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    
    files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(files) == 0) {
      showModal(modalDialog(title = "Erreur", "Aucun fichier CSV trouvé."))
      return()
    }
    
    withProgress(message = "⏳ Traitement des fichiers Plasma...", value = 0, {
      total_steps <- length(files)
      all_tables <- list()
      
      for (i in seq_along(files)) {
        filepath <- files[i]
        df <- preprocess_single_file(filepath)
        if (nrow(df) == 0) next
        
        best_hits <- df %>%
          group_by(Sample, Compound) %>%
          slice_max(order_by = Area, n = 1, with_ties = FALSE) %>%
          ungroup()
        
        date_extracted <- str_extract(basename(filepath), "\\d{8}")
        date_formatted <- format(as.Date(date_extracted, "%d%m%Y"), "%Y-%m-%d")
        
        summarised <- best_hits %>%
          group_by(Compound) %>%
          summarise(
            !!paste0("Min_", date_formatted) := min(Area, na.rm = TRUE),
            !!paste0("Max_", date_formatted) := max(Area, na.rm = TRUE),
            !!paste0("Area_", date_formatted) := mean(Area, na.rm = TRUE),
            !!paste0("CV_", date_formatted) := sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100,
            .groups = "drop"
          )
        
        
        
        
        # summarized <- best_hits %>%
        #   group_by(Compound) %>%
        #   summarise(
        #     !!paste0("Area_", date_formatted) := mean(Area, na.rm = TRUE),
        #     !!paste0("CV_", date_formatted) := sd(Area, na.rm = TRUE) / mean(Area, na.rm = TRUE) * 100,
        #     .groups = "drop"
        #   )
        all_tables[[i]] <- summarised
        
        incProgress(1 / total_steps)
      }
      
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
  })
  
  
  
  observeEvent(input$tenax_dir, {
    volumes <- c(Home = fs::path_home(), "R" = getwd())
    shinyDirChoose(input, "tenax_dir", roots = volumes, session = session)
    
    tenax_path <- parseDirPath(volumes, input$tenax_dir)
    req(tenax_path)
    
    if (!str_detect(tolower(basename(tenax_path)), "tenax")) {
      showModal(modalDialog(
        title = "Erreur",
        div(style = "font-size:18px; color:red; text-align:center;",
            "❌ Ceci n'est pas un dossier Tenax."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    
    tenax_final <- preprocess_folder_tenax(tenax_path)
    
    if (nrow(tenax_final) == 0) {
      showNotification("❌ Aucun fichier Tenax exploitable.", type = "error")
      return()
    }
    
    output$tenax_export <- downloadHandler(
      filename = function() {
        paste0("tenax_formate_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv2(tenax_final, file)
        showNotification("✅ Fichier Tenax formaté prêt !", type = "message")
      }
    )
  })
  
  
  
  
  
  observeEvent(input$tenax_summary_file, {
    req(input$tenax_summary_file)
    
    df <- read_csv2(input$tenax_summary_file$datapath)
    
    
    df_long <- df %>%
      pivot_longer(cols = starts_with("Area_"), names_to = "Measure", values_to = "Area") %>%
      mutate(
        DateRaw = str_remove(Measure, "Area_"),
        DateLabel = factor(DateRaw, levels = unique(DateRaw)),  # ➜ garde l’ordre d’apparition
        Date = as.Date(str_extract(DateRaw, "\\d{4}-\\d{2}-\\d{2}")),  # ➜ utile pour d'autres usages
        Compound = as.character(Compound)
      )
    
    
    output$tenax_summary_table <- renderDT({
      req(data_reactive())
      req(nrow(data_reactive()) > 0)
      datatable(df, options = list(scrollX = TRUE, pageLength = 10))
    })
    
    
    output$tenax_summary_plot <- renderPlotly({
      req(data_reactive())  # ✅ ne fait rien si reset
      req(nrow(data_reactive()) > 0)
      
      df_long_sorted <- df_long %>%
        arrange(Compound, Date)
      
      p <- ggplot(df_long_sorted, aes(x = DateLabel, y = Area, group = Compound, color = Compound)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(title = "Évolution des aires moyennes (Tenax)",
             x = "Date/nom du fichier", y = "Aire") +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 8)  # ← rotation + lisibilité
        )
      
      ggplotly(p)
    })
    
    
    
    output$tenax_cv_plot <- renderPlotly({
      req(data_reactive())
      req(nrow(data_reactive()) > 0)
      req(input$tenax_summary_file)
      df <- read_csv2(input$tenax_summary_file$datapath, show_col_types = FALSE)
      
      # ✅ Vérifie que CV_Global existe
      if (!"CV_Global" %in% colnames(df)) {
        return(plotly_empty(type = "scatter", mode = "markers") %>% 
                 layout(title = "Aucune colonne 'CV_Global' trouvée"))
      }
      
      df <- df %>%
        mutate(
          is_extreme = Compound %in% c("Acetone", "Acetonitrile"),
          CV_Global = as.numeric(str_replace(as.character(CV_Global), ",", "."))
        )
      
      df_normaux <- df %>% filter(!is_extreme)
      df_extremes <- df %>% filter(is_extreme)
      
      trace_normaux <- plot_ly(df_normaux, x = ~Compound, y = ~CV_Global, type = "bar",
                               name = "CV standards", marker = list(color = 'steelblue'),
                               yaxis = "y")
      
      trace_extremes <- plot_ly(df_extremes, x = ~Compound, y = ~CV_Global, type = "bar",
                                name = "CV extrêmes", marker = list(color = 'firebrick'),
                                yaxis = "y2")
      
      subplot(trace_normaux, trace_extremes) %>%
        layout(
          title = "CV global (%) par composé (double axe Y)",
          xaxis = list(title = "Composés"),
          yaxis = list(
            title = "CV standards",
            side = "left",
            overlaying = NULL
          ),
          yaxis2 = list(
            title = "CV extrêmes",
            side = "right",
            overlaying = "y",
            showgrid = FALSE
          ),
          shapes = list(
            list(
              type = "line",
              x0 = -0.5,
              x1 = nrow(df) - 0.5,  # ✅ plus sûr que length(df$Compound)
              y0 = 30,
              y1 = 30,
              yref = "y",
              line = list(color = "red", dash = "dash")
            )
          ),
          legend = list(orientation = "h", x = 0.1, y = 1.1)
        )
    })
    
  })
  
  
  
  output$retentio2_ui <- renderUI({
    tagList(
      fluidRow(
        valueBoxOutput("cvBox2"),
        valueBoxOutput("meanBox2"),
        conditionalPanel(condition = "output.showSequenceBox2", valueBoxOutput("nSeqBox2")),
        conditionalPanel(condition = "output.showCVFAME2", valueBoxOutput("cvBoxFAME2")),
        valueBoxOutput("cvBoxInterne2")
      ),
      
      fluidRow(
        box(title = "Fichiers chargés", width = 12,
            verbatimTextOutput("loadedFiles2"),
            verbatimTextOutput("data_summary2"))
      ),
      
      
      
      fluidRow(
        tabBox(title = "Visualisation", width = 12,
               tabPanel("CV% par séquence📉",
                        plotlyOutput("CVPlot_time2"),
                        tags$br(),
                        verbatimTextOutput("sequence_validation_cv")
               ),  # 👈 fermeture de ce premier tabPanel ici !
               
               tabPanel("Aires par Date📉", plotlyOutput("trendPlot2")),
               
               tabPanel("Cinétiques multi-composés📊",
                        downloadButton("download_cv_plot2", "Télécharger CV (%) PNG"),
                        downloadButton("download_area_plot2", "Télécharger Aire (log10) PNG"),
                        plotlyOutput("multiCVPlot2"),
                        plotlyOutput("multiAreaPlot2"),
                        tags$br(),
                        verbatimTextOutput("sequence_validation_multi")
               )
        )
      ),
      
      
      
      fluidRow(box(title = "Données filtrées", width = 12, DTOutput("dataTable2"))),
      
      fluidRow(
        downloadButton("downloadCSV2", "Télécharger CSV nettoyé"),
        actionButton("zoom_issues2", "Zoom sur problèmes"),
        actionButton("manual_correct2", "Corriger manuellement")
      )
    )
  })
  
  observeEvent(input$file_upload_combined, {
    req(input$file_upload_combined)
    
    files <- input$file_upload_combined$datapath
    filenames <- input$file_upload_combined$name
    
    full_data <- tibble()
    
    withProgress(message = "⏳ Chargement des fichiers...", value = 0, {
      total_steps <- length(files)
      
      for (i in seq_along(files)) {
        file_path <- files[i]
        file_name <- filenames[i]
        
        if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
          df <- preprocess_smart(file_path, file_name)
        } else if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
          sheets <- excel_sheets(file_path)
          for (sheet in sheets) {
            df <- preprocess_smart(file_path, file_name, sheet)
            full_data <- bind_rows(full_data, df)
          }
        }
        
        full_data <- bind_rows(full_data, df)
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
    
    
    # --- ⬇️ Si un seul fichier chargé et contient Area_XXX : traiter comme fichier résumé Tenax
    withProgress(message = "⏳ Chargement des fichiers...", value = 0, {
      total_steps <- length(files)
      
      for (i in seq_along(files)) {
        file_path <- files[i]
        file_name <- filenames[i]
        
        # 👇⚠️ Cas spécial fichier résumé Tenax détecté tout de suite
        if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
          df_test <- read_csv2(file_path, show_col_types = FALSE)
          if (any(grepl("^Area_", names(df_test))) && "Compound" %in% names(df_test)) {
            message("📄 Fichier résumé Tenax détecté !")
            
            output$tenax_summary_table <- renderDT({
              req(data_reactive())
              req(nrow(data_reactive()) > 0)
              
              datatable(df_test, options = list(scrollX = TRUE, pageLength = 10))
            })
            
            df_long <- df_test %>%
              pivot_longer(cols = starts_with("Area_"), names_to = "Measure", values_to = "Area") %>%
              mutate(
                DateRaw = str_remove(Measure, "Area_"),
                DateLabel = factor(DateRaw, levels = unique(DateRaw)),
                Date = as.Date(str_extract(DateRaw, "\\d{4}-\\d{2}-\\d{2}")),
                Compound = as.character(Compound)
              ) %>%
              arrange(Compound, Date)
            
            output$tenax_summary_plot <- renderPlotly({
              req(data_reactive())
              req(nrow(data_reactive()) > 0)
              p <- ggplot(df_long, aes(x = DateLabel, y = Area, group = Compound, color = Compound)) +
                geom_line() +
                geom_point() +
                theme_minimal() +
                labs(title = "Évolution des aires moyennes (Tenax)",
                     x = "Date/nom du fichier", y = "Aire") +
                theme(
                  legend.position = "bottom",
                  axis.text.x = element_text(angle = 60, hjust = 1, size = 8)
                )
              ggplotly(p)
            })
            
            output$tenax_cv_plot <- renderPlotly({
              req(data_reactive())
              req(nrow(data_reactive()) > 0)
              
              if (!"CV_Global" %in% colnames(df_test)) {
                return(plotly_empty(type = "scatter", mode = "markers") %>% 
                         layout(title = "Aucune colonne 'CV_Global' trouvée"))
              }
              
              df_test <- df_test %>%
                mutate(
                  is_extreme = Compound %in% c("Acetone", "Acetonitrile"),
                  CV_Global = as.numeric(str_replace(as.character(CV_Global), ",", "."))
                )
              
              df_normaux <- df_test %>% filter(!is_extreme)
              df_extremes <- df_test %>% filter(is_extreme)
              
              trace_normaux <- plot_ly(df_normaux, x = ~Compound, y = ~CV_Global, type = "bar",
                                       name = "CV standards", marker = list(color = 'steelblue'),
                                       yaxis = "y")
              
              trace_extremes <- plot_ly(df_extremes, x = ~Compound, y = ~CV_Global, type = "bar",
                                        name = "CV extrêmes", marker = list(color = 'firebrick'),
                                        yaxis = "y2")
              
              subplot(trace_normaux, trace_extremes) %>%
                layout(
                  title = "CV global (%) par composé (double axe Y)",
                  xaxis = list(title = "Composés"),
                  yaxis = list(title = "CV standards", side = "left"),
                  yaxis2 = list(title = "CV extrêmes", side = "right", overlaying = "y", showgrid = FALSE),
                  shapes = list(
                    list(type = "line", x0 = -0.5, x1 = nrow(df_test) - 0.5, y0 = 30, y1 = 30, yref = "y",
                         line = list(color = "red", dash = "dash"))
                  ),
                  legend = list(orientation = "h", x = 0.1, y = 1.1)
                )
            })
            
            showNotification("📈 Fichier résumé Tenax détecté et graphiques affichés !", type = "message")
            next  # ⚠️ on saute ce fichier résumé → ne pas le traiter en plus
          }
        }
        
        # 👇 Traitement classique sinon
        if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
          df <- preprocess_smart(file_path, file_name)
        } else if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
          sheets <- excel_sheets(file_path)
          for (sheet in sheets) {
            df <- preprocess_smart(file_path, file_name, sheet)
            full_data <- bind_rows(full_data, df)
          }
        }
        
        full_data <- bind_rows(full_data, df)
        incProgress(1 / total_steps)
      }
    })
    
    
    
    # Si données valides → on les stocke dans le reactive
    df_flagged <- if ("RT" %in% names(full_data)) flag_anomalies(full_data) else full_data
    data_reactive(df_flagged)
    
    updatePickerInput(session, "sequence", choices = sort(unique(df_flagged$Sequence)))
    updatePickerInput(session, "type", selected = "Tous")  # valeurs fixes déjà dans choices
    
    
    updatePickerInput(session, "analyte", choices = sort(unique(df_flagged$Compound)))
    
    
    updatePickerInput(session, "multi_analytes", choices = unique(df_flagged$Compound))
    
    showNotification("✅ Fichiers Tenax chargés avec succès !", type = "message")
  })
  
  
  
  
  
  
}


# ---- APP ----
shinyApp(ui, server)

#15h50 -> 15/05/2025
#dopinoooooooooooooooooooooooooooooooooooooooooooooooooos