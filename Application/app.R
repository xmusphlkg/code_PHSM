library(shiny)
library(shinydashboard)
library(markdown)
library(DT)
library(dplyr)
library(plotly)

load("./.RData")

# generate split date for different periods
DataForecast <- DataForecast |>
  mutate(periods = case_when(
    date < split_date_1 ~ "PHSMs Period I",
    date >= split_date_1 & date < split_date_2 ~ "PHSMs Period II",
    date >= split_date_2 ~ "Epidemic Period",
    .default = "other"
  ))

# analysis for each disease
diseaseModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = id,
      width = 12,
      solidHeader = TRUE,
      status = "danger",
      fluidRow(
        align = "center",
        column(
          5,
          valueBoxOutput(ns("sumDiff1")),
          valueBoxOutput(ns("sumDiff2")),
          valueBoxOutput(ns("sumDiff3"))
        ),
        column(
          7,
          plotlyOutput(ns("diffPlot"), height = "170px")
        )
      )
    )
  )
}

diseaseModule <- function(input, output, session, data, disease_name) {
  filteredData <- reactive({
    data |>
      filter(diseasename == disease_name)
  })
  valueData <- filteredData() |>
    group_by(periods) |>
    summarise(
      value = sum(diff, na.rm = T),
      .groups = "drop"
    )

  # sum of diff
  output$sumDiff1 <- renderValueBox({
    valueBox(
      subtitle = "PHSMs Period I",
      value = -round(valueData$value[2]),
      icon = if (valueData$value[2] < 0) icon("arrow-up") else icon("arrow-down"),
      color = if (valueData$value[2] > 0) "green" else "orange"
    )
  })
  output$sumDiff2 <- renderValueBox({
    valueBox(
      subtitle = "PHSMs Period II",
      value = -round(valueData$value[3]),
      icon = if (valueData$value[3] < 0) icon("arrow-up") else icon("arrow-down"),
      color = if (valueData$value[3] > 0) "green" else "orange"
    )
  })
  output$sumDiff3 <- renderValueBox({
    valueBox(
      subtitle = "Epidemic Period",
      value = -round(valueData$value[1]),
      icon = if (valueData$value[1] < 0) icon("arrow-up") else icon("arrow-down"),
      color = if (valueData$value[1] > 0) "green" else "orange"
    )
  })

  # diff plot remove all axis and title
  max_y <- max(filteredData()$diff)
  min_y <- min(filteredData()$diff)
  output$diffPlot <- renderPlotly({
    plot_ly(
      filteredData(),
      x = ~date,
      y = ~ -diff,
      type = "bar",
      color = ~color
    ) |>
      layout(
        xaxis = list(title = "", ticktext = list(), tickvals = list()),
        yaxis = list(title = ""),
        showlegend = FALSE,
        hoverlabel = paste(
          "<b>Date:</b> %{x}<br>",
          "<b>Monthly Incidence:</b> %{y}<br>"
        ),
        shapes = list(
          list(
            type = "rect",
            x0 = split_date_0 - 15,
            x1 = split_date_1 - 15,
            y0 = min_y,
            y1 = max_y,
            fillcolor = "#E63833",
            opacity = 0.1,
            layer = "below"
          ),
          list(
            type = "rect",
            x0 = split_date_1 - 15,
            x1 = split_date_2 - 15,
            y0 = min_y,
            y1 = max_y,
            fillcolor = "#5E9546",
            opacity = 0.1,
            layer = "below"
          ),
          list(
            type = "rect",
            x0 = split_date_2 - 15,
            x1 = split_date_3 - 15,
            y0 = min_y,
            y1 = max_y,
            fillcolor = "#05215D",
            opacity = 0.1,
            layer = "below"
          )
        )
      )
  })
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "NIDs Analytics"
  ),
  dashboardSidebar(
    # Sidebar content
    ## Input for disease type
    tags$br(),
    tags$span("Step 1: Select disease(s)", style = "font-weight: bold; margin-left: 10px; margin-top: 40px;"),
    tags$hr(style = "margin-top: 5px;margin-left: 10px;margin-right: 10px;"),
    selectInput("disease_type", "Disease Type:",
      choices = unique(DataClass$class)
    ),
    ## Input for disease name
    uiOutput("disease_name"),
    ## Sidebar panel for inputs
    tags$br(),
    tags$span("Step 2: Explore data", style = "font-weight: bold; margin-left: 10px;"),
    tags$hr(style = "margin-top: 5px;margin-left: 10px;margin-right: 10px;"),
    sidebarMenu(
      menuItem("Historical Data", tabName = "data", icon = icon("filter")),
      menuItem("Model predict", tabName = "predict", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    ## APP version and author in bottom of sidebar
    tags$div(
      style = "position: fixed; bottom: 10px; width: 150px; margin-left: 20px; text-align: left",
      tags$em("Version 1.0.0"),
      tags$br(),
      tags$em("Author: Kangguo Li"),
      tags$div(
        style = "margin-top: 10px",
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
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Data page
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Graph",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            plotlyOutput("graph")
          ),
          box(
            title = "Historical Data",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            DTOutput("filtered_data"),
            footer = tags$div(
              style = "text-align: center; font-size: 10px; color: #777777",
              "Data source: ",
              tags$a(
                href = "https://github.com/xmusphlkg/code_PHSM",
                target = "_blank",
                "Github"
              )
            )
          )
        )
      ),

      # Model predict page
      tabItem(
        tabName = "predict",
        uiOutput("model_predict")
      ),
      # About page
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About this application",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            "This application is developed for the manuscript:",
            tags$br(),
            tags$b("Temporal trends and shifts of 24 notifiable infectious diseases in China before and during the COVID-19 epidemic."),
          ),
          ## Add github readme.md file
          box(
            title = "About this project",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            includeMarkdown("README.md")
          )
        )
      )
    )
  ),
  title = "Disease Data Dashboard",
  skin = "red"
)

# Define server
server <- function(input, output, session) {
  # setting global variables
  global <- reactiveValues(
    disease_type = NULL,
    disease_name = NULL,
    actual_data = NULL,
    forecast_data = NULL
  )

  # Render disease name based on selected disease type
  output$disease_name <- renderUI({
    selectInput(
      "disease_name",
      "Disease Name:",
      choices = if (!is.null(input$disease_type)) DataClass$diseasename[DataClass$class == input$disease_type] else "All",
      selected = NULL,
      multiple = TRUE
    )
  })

  # response to disease type change
  observeEvent(input$disease_type, {
    # update global variable
    global$disease_type <- input$disease_type
    disease_name <- DataClass$diseasename[DataClass$class == input$disease_type]
    global$disease_name <- disease_name
    # update selectInput for disease name
    updateSelectInput(
      session = session,
      inputId = "disease_name",
      choices = disease_name,
      selected = disease_name
    )
    # print message
    print(paste("Disease type changed to", input$disease_type))
  })

  # response to disease name change
  observeEvent(input$disease_name, {
    # display progress bar
    withProgress(
      message = "Loading data...",
      detail = "This may take a while...",
      value = 0,
      {
        # update actual data
        global$actual_data <- DataActual |>
          filter(diseasename %in% global$disease_name)
        incProgress(1 / 2)
        # update forecast data
        global$forecast_data <- DataForecast |>
          filter(diseasename %in% global$disease_name)
        incProgress(1 / 2)
      }
    )
    # print message
    print(paste("Disease name changed to", paste(input$disease_name, collapse = ", ")))
    # Create disease module for each disease
    output$model_predict <- renderUI({
      fluidRow(
        lapply(
          global$disease_name,
          function(disease_name) {
            diseaseModuleUI(disease_name)
          }
        )
      )
    })
    # Call disease module for each disease
    lapply(
      global$disease_name,
      function(disease_name) {
        callModule(diseaseModule, disease_name, data = global$forecast_data, disease_name = disease_name)
      }
    )
  })

  # Render filtered data
  output$filtered_data <- renderDT({
    datatable(global$actual_data[, c("class", "level", "diseasename", "disease_1", "date", "value")],
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20, 25, 30),
        autoWidth = TRUE,
        ordering = TRUE,
        scrollX = TRUE,
        scrollY = TRUE,
        fixedColumns = TRUE,
        searching = TRUE,
        info = TRUE,
        ordering = TRUE,
        order = list(list(5, "desc"), list(3, "desc")),
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
      extensions = c("Buttons"),
      colnames = c("Disease Type", "Level", "Disease Name", "Disease Name(CN)", "Date", "Monthly Incidence")
    )
  })

  # Render graph
  output$graph <- renderPlotly({
    # Code to generate the graph
    # Use global$actual_data colored by disease name
    if (is.null(global$actual_data) || nrow(global$actual_data) == 0) {
      return(NULL)
    }

    max_y <- max(global$actual_data$value)

    plot_ly(
      global$actual_data,
      x = ~date,
      y = ~value,
      text = ~class,
      color = ~diseasename,
      type = "scatter",
      mode = "lines"
    ) |>
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Monthly Incidence", range = c(0, max(global$actual_data$value))),
        showlegend = FALSE,
        hovermode = "closest",
        hoverlabel = paste(
          "<b>Date:</b> %{x}<br>",
          "<b>Monthly Incidence:</b> %{y}<br>",
          "<b>Disease Name:</b> %{marker.color}<br>",
          "<b>Disease Type:</b> %{marker.text}"
        ),
        shapes = list(
          list(
            type = "rect",
            x0 = "2007-12-15",
            x1 = split_date_0 - 15,
            y0 = 0,
            y1 = max_y,
            fillcolor = "#3381A8",
            opacity = 0.1,
            layer = "below"
          ),
          list(
            type = "rect",
            x0 = split_date_0 - 15,
            x1 = split_date_1 - 15,
            y0 = 0,
            y1 = max_y,
            fillcolor = "#E63833",
            opacity = 0.1,
            layer = "below"
          ),
          list(
            type = "rect",
            x0 = split_date_1 - 15,
            x1 = split_date_2 - 15,
            y0 = 0,
            y1 = max_y,
            fillcolor = "#5E9546",
            opacity = 0.1,
            layer = "below"
          ),
          list(
            type = "rect",
            x0 = split_date_2 - 15,
            x1 = split_date_3 - 15,
            y0 = 0,
            y1 = max_y,
            fillcolor = "#05215D",
            opacity = 0.1,
            layer = "below"
          )
        )
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
