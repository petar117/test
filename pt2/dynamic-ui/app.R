library(shiny)
library(dplyr, warn.conflicts = FALSE)

ui <- navbarPage(
  "Page title",
  tabPanel(
    "tab 1",
    fluidPage(
      fluidRow(
        column(
          3,
          numericInput("min", "Minimum", 0),
          numericInput("max", "Maximum", 3),
          sliderInput("n", "n", min = 0, max = 3, value = 1)
        ),
        column(1, ),
        column(
          3,
          sliderInput("x1", "x1", 0, min = -10, max = 10),
          sliderInput("x2", "x2", 0, min = -10, max = 10),
          sliderInput("x3", "x3", 0, min = -10, max = 10),
          actionButton("reset", "Reset")
        ),
        column(1, ),
        column(
          3,
          numericInput("n1", "Simulations", 10),
          actionButton("simulate", "Simulate")
        )
      )
    )
  ),
  tabPanel(
    "tab 2"
  )
)
server <- function(input, output, session) {
  observeEvent(input$min, {
    updateSliderInput(inputId = "n", min = input$min)
  })
  observeEvent(input$max, {
    updateSliderInput(inputId = "n", max = input$max)
  })
  
  
  observeEvent(input$reset, {
    updateSliderInput(inputId = "x1", value = 0)
    updateSliderInput(inputId = "x2", value = 0)
    updateSliderInput(inputId = "x3", value = 0)
  })
  
  
  observeEvent(input$n1, {
    label <- paste0("Simulate ", input$n1, " times")
    updateActionButton(inputId = "simulate", label = label)
  })
}

shinyApp(ui, server)
