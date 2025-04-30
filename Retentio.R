library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(readxl)

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Retentio - Suivi Métabolomique Plasma"),
  dashboardSidebar(
    fileInput("file_upload", "Ajouter un fichier Excel (.xlsx)", accept = ".xlsx"),
    uiOutput("sheet_selector"),
    actionButton("refresh", "Rafraîchir les données"),
    pickerInput("analyte", "Choisir un composé :", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE)),
    pickerInput("sequence", "Séquences :", choices = NULL, multiple = TRUE),
    pickerInput("type", "Type de composé :", choices = c("Tous", "FAME", "Étalon Interne", "Analyte")),
    checkboxInput("flag_only", "Voir uniquement les anomalies", value = FALSE)
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("cvBox"),
      valueBoxOutput("meanBox"),
      valueBoxOutput("nSeqBox")
    ),
    fluidRow(
      tabBox(title = "Visualisation", width = 12,
             tabPanel("CV% par séquence", plotlyOutput("cvPlot")),
             tabPanel("Aires par date", plotlyOutput("areaPlot")),
             tabPanel("Tendance min/moy/max", plotlyOutput("trendPlot"))
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
  data_reactive <- reactiveVal()
  
  source("qualityChecks.R")
  
  output$sheet_selector <- renderUI({
    req(input$file_upload)
    sheets <- excel_sheets(input$file_upload$datapath)
    selectInput("sheet", "Choisir une feuille Excel :", choices = sheets)
  })
  
  preprocess_excel <- function(file_path, sheet_name) {
    df <- read_excel(file_path, sheet = sheet_name, skip = 3, col_names = FALSE)
    colnames(df)[1:5] <- c("Compound", "Ion", "RT", "RT_2D", "Other")
    
    # Détection automatique de la colonne Area si présente (6e colonne typique)
    if (ncol(df) >= 6) {
      df$Area <- as.numeric(df[[6]])
    } else {
      df$Area <- 1e6  # valeur simulée si non présente
    }
    
    df_clean <- df %>%
      select(Compound, Ion, RT, Area) %>%
      mutate(
        RT = as.numeric(RT),
        Area = as.numeric(Area),
        Sequence = sheet_name,
        Date = Sys.Date(),
        Type = case_when(
          str_detect(tolower(Compound), "fame") ~ "FAME",
          str_detect(tolower(Compound), "d[0-9]+") ~ "Étalon Interne",
          TRUE ~ "Analyte"
        ),
        Flagged = FALSE
      ) %>%
      relocate(Compound, Area, RT, Ion, Sequence, Date, Type, Flagged)
    
    return(df_clean)
  }
  
  
  observeEvent(input$refresh, {
    req(input$file_upload, input$sheet)
    df <- preprocess_excel(input$file_upload$datapath, input$sheet)
    print("==== Aperçu du dataframe ====")
    print(head(df))
    df <- flag_anomalies(df)
    data_reactive(df)
    
    updatePickerInput(session, "analyte", choices = unique(df$Compound))
    updatePickerInput(session, "sequence", choices = unique(df$Sequence))
  })
  
  filtered_data <- reactive({
    req(data_reactive(), input$analyte)
    data <- data_reactive()
    df <- data %>% filter(Compound == input$analyte)
    if (input$type != "Tous") df <- df %>% filter(Type == input$type)
    if (!is.null(input$sequence)) df <- df %>% filter(Sequence %in% input$sequence)
    if (input$flag_only) df <- df %>% filter(Flagged)
    validate(need(nrow(df) > 0, "Aucune donnée disponible avec les filtres actuels."))
    df
  })
  
  output$cvBox <- renderValueBox({
    df <- filtered_data()
    cv <- round(sd(df$Area, na.rm = TRUE) / mean(df$Area, na.rm = TRUE) * 100, 1)
    box_color <- if (is.na(cv)) "aqua" else if (cv < 30) "green" else "red"
    valueBox(ifelse(is.na(cv), "NA", cv), subtitle = "CV%", color = box_color)
  })
  
  output$meanBox <- renderValueBox({
    df <- filtered_data()
    mean_val <- round(mean(df$Area, na.rm = TRUE), 0)
    valueBox(ifelse(is.na(mean_val), "NA", mean_val), subtitle = "Aire Moyenne", color = "blue")
  })
  
  output$nSeqBox <- renderValueBox({
    df <- filtered_data()
    valueBox(length(unique(df$Sequence)), subtitle = "Séquences", color = "purple")
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
  
  output$trendPlot <- renderPlotly({
    df <- filtered_data()
    summary <- df %>% group_by(Date) %>% summarise(Min = min(Area), Max = max(Area), Mean = mean(Area), .groups = "drop")
    plot_ly(summary, x = ~Date) %>%
      add_lines(y = ~Min, name = "Min") %>%
      add_lines(y = ~Mean, name = "Moyenne") %>%
      add_lines(y = ~Max, name = "Max") %>%
      layout(yaxis = list(title = "Tendance des Aires"))
  })
  
  output$dataTable <- renderDT({ filtered_data() })
  
  output$downloadCSV <- downloadHandler(
    filename = function() paste0("donnees_filtrees_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered_data(), file)
  )
}

# ---- APP ----
shinyApp(ui, server)