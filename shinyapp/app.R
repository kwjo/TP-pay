library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinylive)
library(httpuv)

# https://shinyapps.dreamrs.fr/shinyWidgets/ for more info on shinyWidgets

# Load CSV files once
Oriel2526 <- read.csv("ExpandedOrielDataFor2526.csv")
Oriel2425 <- read.csv("ExpandedOrielDataFor2425.csv")
Oriel2324 <- read.csv("ExpandedOrielDataFor2324.csv")

ui <- page_sidebar(
  title = "Trainee Pharmacist Salary Distribution",
  sidebar = sidebar(
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 10
    ),
    awesomeRadio(
      inputId = "EmployerType",
      label = "chosen employer type",
      choices = c("All EmployerTypes", "Primary care only", "Hospital only"),
      selected = "All EmployerTypes",
      status = "danger"
    )
  ),
  plotlyOutput(outputId = "distPlot")
)

server <- function(input, output) {
  filteredData <- reactive({
    if (input$EmployerType == "Primary care only") {
      Oriel2526 %>%
        filter(EmployerType == "Primary Care")
    } else if (input$EmployerType == "Hospital only") {
      Oriel2526 %>%
        filter(EmployerType == "Hospital")
    } else {
      Oriel2526
    }
  })

  output$distPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Salary)) +
      geom_histogram(bins = input$bins, fill = "lightblue", color = "black") +
      labs(
        title = "Trainee Pharmacist Salary Distribution",
        x = "Salary", y = "Count"
      ) +
      geom_vline(xintercept = 25600, color = "red") +
      theme_minimal()

    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
httpuv::runStaticServer("site/")
