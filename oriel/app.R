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
    choices = c(
      "2025-26", "2024-25", "2023-24"
    ),
    multiple = FALSE
  ),
  sidebar = sidebar(
    sliderInput(
      inputId = "Bins",
      label = "Number of Bins:",
      min = 1,
      max = 50,
      value = 30
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
      return(Oriel2526)
    } else if (input$SelectYear == "2024-25") {
      DataBeingDisplayed <- Oriel2425
      return(Oriel2425)
    } else {
      DataBeingDisplayed <- Oriel2324
      return(Oriel2324)
    }
  })



  FilteredData <- reactive({
    DataBeingDisplayed <- PickYear()

    if (input$EmployerType == "Primary care only") {
      return(DataBeingDisplayed %>% filter(EmployerType == "Primary Care"))
    } else if (input$EmployerType == "Hospital only") {
      return(DataBeingDisplayed %>% filter(EmployerType == "Hospital"))
    } else {
      return(DataBeingDisplayed) # Return all data for "All EmployerTypes"
    }
  })

  output$distPlot <- renderPlotly({
    DataBeingDisplayed <- FilteredData()
    PlotDisplayed <- ggplot(DataBeingDisplayed, aes(x = Salary)) +
      geom_histogram(bins = input$Bins, fill = "lightblue", color = "black") +
      labs(
        title = "Trainee Pharmacist Salary Distribution",
        x = "Salary", y = "Count"
      ) +
      geom_vline(xintercept = 25600, color = "red") +
      theme_minimal()


    ggplotly(PlotDisplayed)
  })

  output$distPlot <- renderPlotly({
    DataBeingDisplayed <- FilteredData()
    PlotDisplayed <- ggplot(DataBeingDisplayed, aes(x = Salary)) +
      geom_histogram(bins = input$Bins, fill = "lightblue", color = "black") +
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
