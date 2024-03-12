library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(openxlsx)
library(datamods)

load("data.RData")

# Define UI
ui <- fluidPage(
     title = 'Auto Time-series',
     # add page title in the header
     tags$h1("Auto Time-series", align = "center"),
     setBackgroundColor(color = "ghostwhite"),
     useShinydashboard(),
     box(
          title = 'Step 1: Import Data',
          width = 12,
          collapsible = T,
          status = 'danger',
          box(
               width = 6,
               title = 'Data Source',
               status = 'danger',
               import_ui(
                    id = 'InputData',
                    from = c("env", "file", "copypaste", "googlesheets", "url"),
                    file_extensions = c(".csv", ".txt", ".xlsx")
               )
          ),
          box(
               width = 6,
               title = 'Data Settings',
               status = 'danger',
               tags$b("Import status:"),
               verbatimTextOutput(outputId = "status"),
               tags$b("Name:"),
               verbatimTextOutput(outputId = "name"),
               tags$b("Data:"),
               verbatimTextOutput(outputId = "data")
          )
     ),
     box(
          title = 'Step 2: Split Data',
          width = 12,
          collapsible = T,
          status = 'danger',
          box(
               width = 6,
               title = 'Data Check',
               status = 'danger',
               uiOutput(
                    outputId = "split_ui",
                    inline = T
               )
          ),
          box(
               width = 6,
               title = 'Data Settings',
               status = 'danger',
               tags$b("Split Information:"),
               verbatimTextOutput(outputId = "split_info"),
               tags$b("Data:"),
               verbatimTextOutput(outputId = "split_data"),
               tags$b("Time-series:"),
               verbatimTextOutput(outputId = "split_data_ts")
          )
     ),
     box(
          title = 'Step 3: Model Selection',
          width = 12,
          collapsible = T,
          status = 'danger',
          box(
               width = 6,
               title = 'Model Settings',
               status = 'danger',
               conditionalPanel(
                    condition = "output.split_data_ts",
                    column(
                         4,
                         selectInput(
                              inputId = "model_type",
                              label = "Model Type",
                              choices = c("SARIMA", "ETS", "Hybrid", "Bayesian structural", "Prophet", 'Bayesian structural'),
                              selected = "SARIMA",
                              multiple = T
                         )
                    ),
                    column(
                         6,
                         sliderInput(
                              inputId = "train_test_split",
                              label = "Train/Test Split",
                              min = 0.1,
                              max = 0.9,
                              value = 0.7,
                              step = 0.1
                         )
                    ),
                    column(
                         2,
                         actionButton(
                              inputId = "train_model",
                              label = "Train",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 25px;"
                         )
                    )
               )
          ),
          box(
               width = 6,
               title = 'Model Settings',
               status = 'danger',
               tags$b("Model Information:"),
               verbatimTextOutput(outputId = "model_info"),
               tags$b("Model:"),
               verbatimTextOutput(outputId = "model_data"),
               tags$b("Model Summary:"),
               verbatimTextOutput(outputId = "model_summary")
          )
     ),
     box(
          title = 'Step 4: Model Forecast',
          width = 12,
          collapsible = T,
          status = 'danger',
          box(
               width = 6,
               title = 'Forecast Settings',
               status = 'danger',
               conditionalPanel(
                    condition = "output.model_summary",
                    column(
                         4,
                         selectInput(
                              inputId = "set_model",
                              label = "Model Type",
                              choices = c("SARIMA", "ETS", "Hybrid", "Bayesian structural", "Prophet", 'Bayesian structural'),
                              multiple = F
                         )
                    ),
                    column(
                         6,
                         numericInput(
                              inputId = "forecast_period",
                              label = "Forecast Period",
                              value = 12,
                              min = 1
                         )
                    ),
                    column(
                         2,
                         actionButton(
                              inputId = "forecast_model",
                              label = "Forecast",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 25px;"
                         )
                    )
               )
          ),
          box(
               width = 6,
               title = 'Model Forecast',
               status = 'danger',
               tags$b("Model Information:"),
               verbatimTextOutput(outputId = "forecast_info"),
               tags$b("Forecast:"),
               DTOutput(outputId = "forecast_data"),
               tags$b("Forecast Visualization:"),
               plotlyOutput(outputId = "forecast_plot")
          )
     ),
     # add footer
     column(
          width = 12,
          tags$footer(
               tags$p(
                    "Author: Kangguo Li",
                    tags$br(),
                    tags$em("Version 2.0.0"),
                    tags$br(),
                    tags$em("Last updated: 2024-03-12"),
                    tags$br(),
                    tags$em("Host: shinyapps.io"),
                    tags$br(),
                    tags$a(
                         href = "https://github.com/xmusphlkg/code_PHSM",
                         icon("github"),
                         title = "Source Code",
                         style = "margin: 10px"
                    ),
                    tags$a(
                         href = "https://github.com/xmusphlkg/code_PHSM",
                         icon("file-pdf"),
                         title = "DOI",
                         style = "margin: 10px"
                    ),
                    style = "text-align: center; color: #777; font-size: 12px; margin-top: 10px;"
               )
          )
     )
)

# Define server
server <- function(input, output, session) {
     # global values
     GlobalData <- reactiveValues(
          Data = NULL,
          split_Data = NULL,
          split_Data_ts = NULL,
          splict_dates = NULL,
          optimal_model = NULL
     )
     detect_frequency <- function(date_vector) {
          date_diff <- diff(date_vector)
          min_diff <- as.integer(min(date_diff, na.rm = TRUE))
          if (min_diff %in% 365:366) {
               return(365.25)
          } else if (min_diff %in% 28:31) {
               return(12)
          } else {
               return(1)
          }
     }
     
     imported <- import_server(
          id = "InputData",
          return_class = c("data.frame", "data.table", "tbl_df", "raw"),
          read_fns = list(
               xlsx = function(file, sheet, skip, encoding) {
                    openxlsx::read.xlsx(path = file, sheet = sheet, startRow = skip + 1, detectDates = T)
               },
               csv = function(file, sheet, skip, encoding) {
                    read.csv(file, skip = skip, encoding = encoding)
               }
          )
     )
     
     observeEvent(imported$name(), {
          output$status <- renderPrint({
               Data <- imported$data()
               # check data is not null
               if (is.null(Data)) {
                    return("Import failed, because data is NULL")
               }
               # check data is not empty
               if (nrow(Data) == 0) {
                    return("Import failed, because data is empty")
               }
               # check data contains 'date' column
               if (!("date" %in% colnames(Data))) {
                    return("Import failed, because data does not contain 'date' column")
               }
               # check data contains 'value' column
               if (!("value" %in% colnames(Data))) {
                    return("Import failed, because data does not contain 'value' column")
               }
               # check 'date' column is date type
               if (!is.Date(Data$date)) {
                    return("Import failed, because 'date' column is not date type")
               }
               # check 'value' column is numeric or integer type
               if (!is.numeric(Data$value)) {
                    return("Import failed, because 'value' column is not numeric or integer type")
               }
               GlobalData$Data <- Data
               return("Import success")
          })
          output$name <- renderPrint({
               imported$name()
          })
          output$data <- renderPrint({
               head(imported$data())
          })
     })
     
     # split data
     observeEvent(GlobalData$Data, {
          # detect date is unique or not
          Data <- GlobalData$Data
          
          if (length(unique(Data$date)) != nrow(Data)) {
               output$split_ui <- renderUI({
                    date_range <- dateRangeInput(
                         inputId = "date_range",
                         label = "Date Range",
                         start = min(Data$date),
                         end = max(Data$date)
                    )
                    var_names <- colnames(Data)
                    var_names <- var_names[!var_names %in% c("date", "value")]
                    values <- unique(Data[, var_names[1]])
                    tags$div(
                         infoBox(
                              title = paste("Date Range", min(GlobalData$Data$date), " ", max(GlobalData$Data$date)),
                              value = "Input data may contains multiple records for the same date.",
                              icon = icon("warning"),
                              width = 12,
                              color = "red"
                         ),
                         column(
                              5,
                              selectInput(
                                   inputId = "split_by",
                                   label = "Filter By",
                                   choices = var_names
                              )
                         ),
                         column(
                              5,
                              selectInput(
                                   inputId = "split_type",
                                   label = "Filter Type",
                                   choices = values
                              )
                         ),
                         column(
                              2,
                              actionButton(
                                   inputId = "split_data",
                                   label = "Split",
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 25px;"
                              )
                         ),
                    )
               })
          } else {
               output$split_ui <- renderUI({
                    date_range <- dateRangeInput(
                         inputId = "date_range",
                         label = "Date Range",
                         start = min(Data$date),
                         end = max(Data$date)
                    )
                    var_names <- colnames(Data)
                    var_names <- var_names[!var_names %in% c("date", "value")]
                    values <- unique(Data[, var_names[1]])
                    GloablData$split_Data <- Data
                    tags$div(
                         infoBox(
                              title = paste("Date Range", min(GlobalData$Data$date), " ", max(GlobalData$Data$date)),
                              value = "Input data contains unique records for the same date.",
                              icon = icon("info"),
                              width = 12,
                              color = "green"
                         )
                    )
               })
          }
     })
     
     observeEvent(input$split_by, {
          Data <- GlobalData$Data
          var_names <- colnames(Data)
          var_names <- var_names[!var_names %in% c("date", "value")]
          values <- unique(Data[, input$split_by])
          updateSelectInput(
               session = session,
               inputId = "split_type",
               choices = values
          )
     })
     
     observeEvent(input$split_data, {
          Data <- GlobalData$Data
          split_Data <- Data %>% filter(get(input$split_by) == input$split_type)
          GlobalData$split_Data <- split_Data
     })
     
     observeEvent(GlobalData$split_Data, {
          Data <- GlobalData$split_Data
          # detect input$split_by is not null
          if (is.null(input$split_by)) {
               output$split_info <- renderPrint({
                    "Not required to split data"
               })
          } else {
               output$split_info <- renderPrint({
                    paste("Split by", input$split_by, "with value", input$split_type)
               })
          }
          output$split_data <- renderPrint({
               head(Data)
          })
          # auto detect frequency
          # browser()
          freq <- detect_frequency(Data$date)
          if (freq == 365.25) {
               Data <- Data |> 
                    select(date, value) |>
                    complete(date = seq.Date(min(date), max(date), by = "day"),
                             fill = list(value = 0))
               start_date <- c(year(min(Data$date)), yday(min(Data$date)))
               end_date <- c(year(max(Data$date)), yday(max(Data$date)))
          } else if (freq == 12) {
               Data <- Data |> 
                    select(date, value) |>
                    complete(date = seq.Date(min(date), max(date), by = "month"),
                             fill = list(value = 0))
               start_date <- c(year(min(Data$date)), month(min(Data$date)))
               end_date <- c(year(max(Data$date)), month(max(Data$date)))
          } else {
               Data <- Data |> 
                    select(date, value) |>
                    complete(date = seq.Date(min(date), max(date), by = "day"),
                             fill = list(value = 0))
               start_date <- year(min(Data$date))
               end_date <- year(max(Data$date))
          }
          
          tryCatch({
               GlobalData$split_Data_ts <- ts(Data$value, start = start_date, end = end_date, frequency = freq)
               output$split_data_ts <- renderPrint({
                    GlobalData$split_Data_ts
               })
          }, error = function(e) {
               output$split_data_ts <- renderPrint({
                    paste("Error:", e)
               })
          })
     })
}

# Run the Shiny app
shinyApp(ui, server)
