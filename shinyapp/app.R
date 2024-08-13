library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

Oriel2526 <- read.csv("ExpandedOrielDataFor2526.csv")
Oriel2425 <- read.csv("ExpandedOrielDataFor2425.csv")
Oriel2324 <- read.csv("ExpandedOrielDataFor2324.csv")

ui <- page_sidebar(
  # App title ----
  title = "Trainee Pharmacist Salary Distribution",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 10
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- Oriel2526$Salary
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,
      breaks = bins, col = "#007bc2", border = "white",
      xlab = "Salary (£)",
      main = "Salary distribution for 2024-25 training year of Salary"
    )
  })
}


shinyApp(ui = ui, server = server)
