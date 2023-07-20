library(shiny)
library(glue)

ui <- navbarPage(
  "Page title",   
  tabPanel("panel 1", "one", 
           sliderInput("x", "x", value = 1, min = 0, max = 10),
           sliderInput("y", "y", value = 2, min = 0, max = 10),
           sliderInput("z", "z", value = 3, min = 0, max = 10),
           textOutput("total")),
  tabPanel("panel 2", "two"),
  tabPanel("panel 3", "three"),
  navbarMenu("subpanels", 
             tabPanel("panel 4a", "four-a"),
             tabPanel("panel 4b", "four-b"),
             tabPanel("panel 4c", "four-c")
  )
)


server <- function(input, output, session) {
  observeEvent(input$x, {
    message(glue("Updating y from {input$y} to {input$x * 2}"))
    updateSliderInput(session, "y", value = input$x * 2)
  })

  total <- reactive({
    total <- input$x + input$y + input$z
    message(glue("New total is {total}"))
    total
  })

  output$total <- renderText({
    total()
  })
}
shinyApp(ui, server)