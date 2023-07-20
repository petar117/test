library(shiny)
library(glue)

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  "Page title",
  tabPanel(
    "panel 1", "one",
    sliderInput("x", "x", value = 1, min = 0, max = 10),
    sliderInput("y", "y", value = 2, min = 0, max = 10),
    sliderInput("z", "z", value = 3, min = 0, max = 10),
    textOutput("total")
  ),
  tabPanel(
    "panel 2", "two",
    fileInput("file", "Data", buttonLabel = "Upload..."),
    textInput("delim", "Delimiter (leave blank to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows", "Rows to preview", 10, min = 1)
  ),
  tabPanel("panel 3", "three",
           titlePanel("A themed plot"),
           plotOutput("plot"),),
  navbarMenu(
    "subpanels",
    tabPanel("panel 4a", "four-a"),
    tabPanel("panel 4b", "four-b"),
    tabPanel("panel 4c", "four-c")
  )
)


server <- function(input, output, session) {
  thematic::thematic_shiny()
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
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      geom_smooth()
  }, res = 96)
}
shinyApp(ui, server)
