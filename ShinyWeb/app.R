
library(StanHeaders)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(openxlsx)
library(datamods)
library(lubridate)

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
          title = 'Step 2: Filter Data',
          width = 12,
          collapsible = T,
          status = 'danger',
          box(
               width = 6,
               title = 'Data Filter',
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
                         10,
                         selectInput(
                              inputId = "model_type",
                              label = "Model Type",
                              choices = c("Neural Network", "SARIMA", "ETS", "Hybrid", "Bayesian structural", "Prophet", 'Bayesian structural'),
                              selected = "SARIMA",
                              multiple = T
                         )
                    ),
                    column(
                         2,
                         actionButton(
                              inputId = "train_model",
                              label = "Train",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 25px;"
                         )
                    ),
                    column(
                         10,
                         tags$div(
                              dateRangeInput(
                                   inputId = "train_date",
                                   label = "Train Date"
                              ),
                              dateRangeInput(
                                   inputId = "test_date",
                                   label = "Test Date"
                              ),
                              # set input in one line
                              style = "display: flex; justify-content: space-between; align-items: center;"
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
               DTOutput(outputId = "model_data"),
               tags$b("Optimal model:"),
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
                         3,
                         selectInput(
                              inputId = "set_model",
                              label = "Model Type",
                              choices = c("Neural Network", "SARIMA", "ETS", "Hybrid", "Bayesian structural", "Prophet", 'Bayesian structural'),
                              multiple = F
                         )
                    ),
                    column(
                         4,
                         dateRangeInput(
                              inputId = "forecast_train",
                              label = "Train Date"
                         )
                    ),
                    column(
                         3,
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
               verbatimTextOutput(outputId = "forecast_info")
          ),
          box(
               width = 6,
               title = 'Forecast Data',
               status = 'danger',
               DTOutput(outputId = "forecast_data")
          ),
          box(
               width = 6,
               title = 'Forecast Plot',
               status = 'danger',
               plotOutput(outputId = "forecast_plot")
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
     
     source('./model.R')
     
     # global values
     GlobalData <- reactiveValues(
          Data = NULL,
          split_Data = NULL,
          split_Data_ts = NULL,
          optimal_model = NULL,
          forecast_data = NULL
     )
     
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
               GlobalData$Data <- Data |> 
                    arrange(date)
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
               str(Data)
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
               showNotification(
                    ui = "Data has been converted to time-series",
                    type = "message",
                    duration = 5
               )
          }, error = function(e) {
               output$split_data_ts <- renderPrint({
                    paste("Error:", e)
               })
               showNotification(
                    ui = "Data cannot be converted to time-series",
                    type = "error",
                    duration = 60
               )
          })
          
          # update date range
          train_period <- round(nrow(Data) * 0.7)
          test_period <- nrow(Data) - train_period
          train_date <- Data$date[1:train_period]
          test_date <- Data$date[(train_period + 1):nrow(Data)]
          updateDateRangeInput(
               session = session,
               inputId = "train_date",
               start = min(train_date),
               end = max(train_date)
          )
          updateDateRangeInput(
               session = session,
               inputId = "test_date",
               start = min(test_date),
               end = max(test_date)
          )
     })
     
     observeEvent(input$train_model, {
          Data <- GlobalData$split_Data
          DataTS <- GlobalData$split_Data_ts
          train_date <- as.Date(input$train_date)
          test_date <- as.Date(input$test_date)
          model_type <- input$model_type
          
          # check date range is legal
          if (test_date[1] < train_date[2]) {
               showNotification(
                    ui = "Test date should be later than train date",
                    type = "error",
                    duration = 60
               )
          } else {
               train_id <- which(Data$date >= train_date[1] & Data$date <= train_date[2])
               test_id <- which(Data$date >= test_date[1] & Data$date <= test_date[2])
               all_id <- which(Data$date >= train_date[1] & Data$date <= test_date[2])
               train_ts <- DataTS[train_id]
               test_ts <- DataTS[test_id]
               all_ts <- DataTS[all_id]
               
               outcome <- auto_select_function(train_ts, test_ts, all_ts, 0.1, model_type)
               
               output$model_info <- renderPrint({
                    paste("Model type:", paste(model_type, collapse = ", "))
               })
               
               DataIndex <- get_norm_index(outcome[['goodness']])
               output$model_data <- renderDT(DataIndex,
                                             options = list(
                                                  pageLength = 10,
                                                  autoWidth = TRUE,
                                                  ordering = TRUE,
                                                  scrollX = TRUE,
                                                  scrollY = TRUE,
                                                  fixedColumns = TRUE,
                                                  searching = TRUE,
                                                  info = TRUE,
                                                  dom = "Bfrtip",
                                                  buttons = list(
                                                       "copy",
                                                       "csv",
                                                       "excel",
                                                       "pdf",
                                                       "print"
                                                  )
                                             ),
                                             rownames = FALSE,
                                             class = "display",
                                             extensions = c("Buttons"))
               
               output$model_summary <- renderPrint({
                    DataIndex[DataIndex$Best == 1, 1:4]
               })
               
               GlobalData$optimal_model <- as.character(DataIndex[DataIndex$Best == 1, 'Method'])
               
               # update forecast model
               updateSelectInput(
                    session = session,
                    inputId = "set_model",
                    selected = GlobalData$optimal_model
               )
               updateDateRangeInput(
                    session = session,
                    inputId = "forecast_train",
                    start = min(train_date),
                    end = max(test_date)
               )
          }
     })
     
     observeEvent(input$forecast_model, {
          Data <- GlobalData$split_Data
          DataTS <- GlobalData$split_Data_ts
          train_date <- as.Date(input$forecast_train)
          forecast_period <- input$forecast_period
          model_type <- input$set_model
          
          train_id <- which(Data$date >= train_date[1] & Data$date <= train_date[2])
          train_ts <- DataTS[train_id]
          freq <- frequency(DataTS)
          if (freq == 365.25) {
               start_date <- c(year(train_date[1]), yday(train_date[1]))
               end_date <- c(year(train_date[2]), yday(train_date[2]))
          } else if (freq == 12) {
               start_date <- c(year(train_date[1]), month(train_date[1]))
               end_date <- c(year(train_date[2]), month(train_date[2]))
          } else {
               start_date <- year(train_date[1])
               end_date <- year(train_date[2])
          }
          train_ts <- ts(train_ts, start = start_date, end = end_date, frequency = freq)
          outcome <- auto_forecast_function(train_ts, forecast_period, 0.1, model_type)
          
          output$forecast_info <- renderPrint({
               summary(outcome$mod)
          })
          GlobalData$forecast_data <- outcome$outcome_plot_2
          
          output$forecast_data <- renderDT(GlobalData$forecast_data,
                                           options = list(
                                                pageLength = 5,
                                                autoWidth = TRUE,
                                                ordering = TRUE,
                                                scrollX = TRUE,
                                                scrollY = TRUE,
                                                fixedColumns = TRUE,
                                                searching = TRUE,
                                                info = TRUE,
                                                dom = "Bfrtip",
                                                buttons = list(
                                                     "copy",
                                                     "csv",
                                                     "excel",
                                                     "pdf",
                                                     "print"
                                                )
                                           ),
                                           rownames = FALSE,
                                           class = "display",
                                           extensions = c("Buttons"))
          
          output$forecast_plot <- renderPlot({
               ggplot(GlobalData$split_Data) +
                    geom_line(
                         mapping = aes(x = date,
                                       y = value,
                                       colour = "Observed"),
                         linewidth = 0.7
                    ) +
                    geom_line(
                         mapping = aes(x = date,
                                       y = mean,
                                       colour = "Forecasted"),
                         linewidth = 0.7,
                         data = outcome$outcome_plot_2
                    ) +
                    scale_x_date(
                         expand = expansion(add = c(0, 0)),
                         date_labels = "%Y"
                    ) +
                    scale_color_manual(values = c(
                         Forecasted = "#E64B35FF",
                         Observed = "#00A087FF"
                    )) +
                    theme_classic()+
                    theme(legend.position = "bottom") +
                    labs(
                         x = 'Date',
                         y = 'Value',
                         color = ""
                    )
          })
     })
}

# Run the Shiny app
shinyApp(ui, server)
