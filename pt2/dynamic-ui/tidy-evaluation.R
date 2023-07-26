library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

min <- 1
diamonds %>% filter(carat > min)


diamonds %>% filter(.data$carat > .env$min) 
# Now we can switch from $ to [[:
# =
var <- "carat"
diamonds %>% filter(.data[[var]] > .env$min)

num_vars <- c("carat", "depth", "table", "price", "x", "y", "z")
ui <- fluidPage(
  selectInput("var", "Variable", choices = num_vars),
  numericInput("min", "Minimum", value = 1),
  tableOutput("output")
)
server <- function(input, output, session) {
  data <- reactive(diamonds %>% filter(.data[[input$var]] > .env$input$min))
  output$output <- renderTable(head(data()))
}

shinyApp(ui, server)
