library(shiny)

df <- mtcars

ui <- fluidPage(
  selectInput("var", NULL, choices = colnames(df)),
  verbatimTextOutput("debug")
)

server <- function(input, output, session) {
  col_var <- reactive( df[input$var] )
  col_range <- reactive({ range(col_var(), na.rm = TRUE ) })
  output$debug <- renderPrint({ col_range() })
  
}

shinyApp(ui = ui, server = server)