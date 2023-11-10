library(shiny)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    # ),
    title = "Disease Data Dashboard"
  ),
  dashboardSidebar(
    # Sidebar content
    ## Input for disease type
    tags$br(),
    tags$span("Step 1: Select disease(s)", style = "font-weight: bold; margin-left: 10px; margin-top: 40px;"),
    tags$hr(style = "margin-top: 5px;margin-left: 10px;margin-right: 10px;"),
    selectInput("disease_type", "Disease Type:",
      choices = c("Type A", "Type B", "Type C")
    ),
    ## Input for disease name
    selectInput("disease_name", "Disease Name:",
      choices = c("Disease 1", "Disease 2", "Disease 3")
    ),
    ## Sidebar panel for inputs
    tags$br(),
    tags$span("Step 2: Explore data", style = "font-weight: bold; margin-left: 10px;"),
    tags$hr(style = "margin-top: 5px;margin-left: 10px;margin-right: 10px;"),
    sidebarMenu(
      menuItem("Data Filter", tabName = "data", icon = icon("filter")),
      menuItem("Graph", tabName = "trends", icon = icon("chart-bar")),
      menuItem("Model predict", tabName = "predict", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    ## APP version and author in bottom of sidebar
    tags$div(
      style = "position: fixed; bottom: 10px; width: 100%; margin-left: 20px; text-align: left",
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
            title = "Filtered Data",
            width = 8,
            solidHeader = TRUE,
            status = "success",
            tableOutput("filtered_data")
          )
        )
      ),

      # Long-term Trends page
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Graph",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            plotOutput("filtered_data")
          )
        )
      ),
      # Model predict page
      tabItem(
        tabName = "predict",
        fluidRow(
          box(
            title = "Model predict",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            plotOutput("code-branch")
          )
        )
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
            "This is a Shiny app for disease data dashboard."
          ),
          ## Add github readme.md file
          box(
            title = "About me",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            includeMarkdown("README.md")
          ),
          ## Add cite information
          box(
            title = "Cite",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            tags$b("Please cite this app as:"),
            tags$br(),
            tags$div(
              class = "quote",
              "Kangguo Li. (2020, November 20). xmusphlkg/code_PHSM: code_PHSM (Version v1.0.0). Zenodo. http://doi.org/10.5281/zenodo.4278951"
            )
          )
        )
      )
    )
  ),
  "Disease Data Dashboard",
  "red"
)

# Server
server <- function(input, output) {
  # Data filtering
  filtered_data <- reactive({
    # Perform data filtering based on the input disease_type and disease_name
    # Return the filtered data, which can be a data frame or any other data structure
    # This is just a sample, you need to modify it based on your specific data and filtering criteria
    data <- data.frame(
      disease_type = c("Type A", "Type B", "Type C"),
      disease_name = c("Disease 1", "Disease 2", "Disease 3"),
      value = c(10, 20, 30)
    )
    filtered_data <- subset(data, disease_type == input$disease_type & disease_name == input$disease_name)
    return(filtered_data)
  })

  # Display the filtered data
  output$filtered_data <- renderTable({
    filtered_data()
  })

  # Display the graph
  output$graph <- renderPlot({
    # Plot the graph based on the filtered data
    # This is just a sample, you need to modify it based on your data and graph requirements
    plot(filtered_data()$disease_name, filtered_data()$value,
      type = "bar",
      xlab = "Disease Name", ylab = "Value"
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)
