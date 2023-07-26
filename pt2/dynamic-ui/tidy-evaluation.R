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

ui <- navbarPage(
  "Page title",
  tabPanel(
    "tab 1",
    fluidPage(
      selectInput("var", "Variable", choices = num_vars),
      numericInput("min", "Minimum", value = 1),
      tableOutput("output")
    ),
    fluidRow(HTML("<hr>")),
    sidebarLayout(
      sidebarPanel(
        selectInput("x", "X variable", choices = names(iris)),
        selectInput("y", "Y variable", choices = names(iris))
      ),
      mainPanel(plotOutput("plot"))
    ),
    fluidRow(HTML("<hr>")),
    sidebarLayout(
      sidebarPanel(
        selectInput("x1", "X variable", choices = names(iris)),
        selectInput("y1", "Y variable", choices = names(iris)),
        selectInput("geom", "geom", c("point", "smooth", "jitter"))
      ),
      mainPanel(plotOutput("plot1"))
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      fluidRow(
        column(4,
               selectInput("var2", "Select variable", choices = names(mtcars)),
               sliderInput("min2", "Minimum value", 0, min = 0, max = 100),
               selectInput("sort2", "Sort by", choices = names(mtcars))
               ),
        column(8,
               tableOutput("data2"))
      )
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      fluidRow(
        column(4,
               selectInput("var3", "Sort by", choices = names(mtcars)),
               checkboxInput("desc3", "Descending order?"),
        ),
        column(8,
               tableOutput("data3"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive(diamonds %>% filter(.data[[input$var]] > .env$input$min))
  output$output <- renderTable(head(data()))




  # dynamic plot where we allow the user to create a scatterplot by selecting
  # the variables to appear on the x and y axes.
  output$plot <- renderPlot(
    {
      ggplot(iris, aes(.data[[input$x]], .data[[input$y]])) +
        geom_point(position = ggforce::position_auto())
    },
    res = 96
  )




  plot_geom <- reactive({
    switch(input$geom,
      point = geom_point(),
      smooth = geom_smooth(se = FALSE),
      jitter = geom_jitter()
    )
  })

  output$plot1 <- renderPlot(
    {
      ggplot(iris, aes(.data[[input$x1]], .data[[input$y1]])) +
        plot_geom()
    },
    res = 96
  )
  
  
  
  observeEvent(input$var2, {
    rng <- range(mtcars[[input$var2]])
    updateSliderInput(
      session, "min2", 
      value = rng[[1]], 
      min = rng[[1]], 
      max = rng[[2]]
    )
  })
  
  output$data2 <- renderTable({
    mtcars %>% 
      filter(.data[[input$var2]] > input$min2) %>% 
      arrange(.data[[input$sort2]]) %>% 
      head(7)
  })
  
  
  
  sorted <- reactive({
    if (input$desc3) {
      arrange(mtcars, desc(.data[[input$var3]]))
    } else {
      arrange(mtcars, .data[[input$var3]])
    }
  })
  output$data3 <- renderTable(sorted() %>% head(8))
}

shinyApp(ui, server)
