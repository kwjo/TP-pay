library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(plotly)
library(ggplot2)
library(rsconnect)

# https://shinyapps.dreamrs.fr/shinyWidgets/ for more info on shinyWidgets

# Load CSV files once
Oriel2526 <- read.csv("ExpandedOrielDataFor2526.csv")
Oriel2425 <- read.csv("ExpandedOrielDataFor2425.csv")
Oriel2324 <- read.csv("ExpandedOrielDataFor2324.csv")

# Latest data to be displayed by default
DataBeingDisplayed <- Oriel2526

ui <- page_sidebar(
  title = "Trainee Pharmacist Salary Distribution",
  pickerInput(
    inputId = "SelectYear",
    label = "Which year?",
    choices = paste("Badge", c(
      "2025-26", "2024-25", "2023-24"
    )),
    multiple = FALSE,
    choicesOpt = list(
      content = sprintf(
        "<span class='label label-%s'>%s</span>",
        c("2025-26", "2024-25", "2023-24"),
        paste("Training ", c(
          "2025-26", "2024-25", "2023-24"
        ))
      )
    )
  ),
  sidebar = sidebar(
    sliderInput(
      inputId = "Bins",
      label = "Number of Bins:",
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
  PickYear <- reactive({
    if (input$SelectYear == "2025-26") {
      DataBeingDisplayed <- Oriel2526
    } else if (input$SelectYear == "2024-25") {
      DataBeingDisplayed <- Oriel2425
    } else {
      DataBeingDisplayed <- Oriel2324
    }
  })
  FilteredData <- reactive({
    if (input$EmployerType == "Primary care only") {
      DataBeingDisplayed %>%
        filter(EmployerType == "Primary Care")
    } else if (input$EmployerType == "Hospital only") {
      DataBeingDisplayed %>%
        filter(EmployerType == "Hospital")
    } else {
      DataBeingDisplayed
    }
  })

  output$distPlot <- renderPlotly({
    PlotDisplayed <- ggplot(FilteredData(), aes(x = Salary)) +
      geom_histogram(Bins = input$Bins, fill = "lightblue", color = "black") +
      labs(
        title = "Trainee Pharmacist Salary Distribution",
        x = "Salary", y = "Count"
      ) +
      geom_vline(xintercept = 25600, color = "red") +
      theme_minimal()

    ggplotly(PlotDisplayed)
  })
}



shinyApp(ui = ui, server = server)
