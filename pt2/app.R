library(shiny)
library(ggplot2)
library(glue)

set.seed(1014)
df <- data.frame(x1 = rnorm(100), y1 = rnorm(100))

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
  tabPanel(
    "panel 3", "three",
    fluidPage(
      titlePanel("Plot"),
      fluidRow(
        column(
          5,
          plotOutput("plot", click = "plot_click", brush = "plot_brush")
        ),
        column(
          7,
          verbatimTextOutput("info"),
          tableOutput("data"),
          tableOutput("data1")
        )
      )
    )
  ),
  tabPanel(
    "panel 4", "four",
    fluidPage(
      titlePanel("Plot and reactive val"),
      fluidRow(
        column(
          4,
          sliderInput("height", "height", min = 100, max = 500, value = 250),
          sliderInput("width", "width", min = 100, max = 500, value = 250),
        ),
        column(
          8,
          plotOutput("plot1",
            brush = "plot_brush",
            dblclick = "plot_reset",
            width = 400,
            height = 400
          )
        )
      ),
      fluidRow(
        column(4, ),
        column(8, plotOutput("plot2",
          click = "plot_click",
          width = 250,
          height = 250
        ))
      )
    )
  )
)


server <- function(input, output, session) {
  thematic::thematic_shiny()

  # TAB 1 SLIDERS
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

  # TAB 3 PLOT INTERACTION AND TABLE ENTRIES FROM PLOT
  output$plot <- renderPlot(
    {
      ggplot(mtcars, aes(wt, mpg)) +
        geom_point()
    },
    res = 96
  )

  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })

  output$data <- renderTable({
    req(input$plot_click)
    nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
  })

  output$data1 <- renderTable({
    brushedPoints(mtcars, input$plot_brush, xvar = "wt", yvar = "mpg")
  })


  # TAB 4 PLOT INTERACTION AND CRATING REACTIVE VALUES
  selected <- reactiveVal(rep(FALSE, nrow(mtcars)))

  observeEvent(input$plot_brush, {
    brushed <- brushedPoints(mtcars, input$plot_brush, allRows = TRUE)$selected_
    selected(brushed | selected())
  })

  observeEvent(input$plot_reset, {
    selected(rep(FALSE, nrow(mtcars)))
  })

  output$plot1 <- renderPlot(
    {
      mtcars$sel <- selected()
      ggplot(mtcars, aes(wt, mpg)) +
        geom_point(aes(colour = sel)) +
        scale_colour_discrete(limits = c("TRUE", "FALSE"))
    },
    width = function() input$width,
    height = function() input$height,
    res = 96
  )




  dist <- reactiveVal(rep(1, nrow(df)))
  observeEvent(
    input$plot_click$plot2,
    dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)
  )

  output$plot2 <- renderPlot(
    {
      df$dist <- dist()
      ggplot(df, aes(x1, y1, size = dist)) +
        geom_point() +
        scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)
    },
    res = 96
  )
}


shinyApp(ui, server)
