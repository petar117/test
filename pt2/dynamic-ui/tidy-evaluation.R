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
      selectInput("var4", "Variable", choices = num_vars),
      numericInput("min4", "Minimum", value = 1),
      tableOutput("output4")
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
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      fluidRow(
        column(4,
               fileInput("data", "dataset", accept = ".tsv"),
               selectInput("var", "var", character()),
               numericInput("min", "min", 1, min = 0, step = 1),
        ),
        column(8,
               tableOutput("output"))
      )
    )
  ),
  tabPanel(
    "tab 2",
    fluidPage(
      selectInput("vars5", "Variables", names(mtcars), multiple = TRUE),
      tableOutput("data5"),
      tableOutput("count5")
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      selectInput("vars_g6", "Group by", names(mtcars), multiple = TRUE),
      selectInput("vars_s6", "Summarise", names(mtcars), multiple = TRUE),
      tableOutput("data6")
    )
  )
)

server <- function(input, output, session) {
  data4 <- reactive(diamonds %>% filter(.data[[input$var4]] > .env$input$min4))
  output$output4 <- renderTable(head(data4()))




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
  
  
  
  # user uploads data - .tsv file
  data <- reactive({
    req(input$data)
    vroom::vroom(input$data$datapath)
  })
  observeEvent(data(), {
    updateSelectInput(session, "var", choices = names(data()))
  })
  observeEvent(input$var, {
    val <- data()[[input$var]]
    updateNumericInput(session, "min", value = min(val))
  })
  
  output$output <- renderTable({
    req(input$var)
    
    data() %>% 
      filter(.data[[input$var]] > .env$input$min) %>% 
      arrange(.data[[input$var]]) %>% 
      head(10)
  })
  
  
  
  # tab 2 - tidy-selection
  output$data5 <- renderTable({
    req(input$vars5)
    mtcars %>% select(all_of(input$vars5)) %>% head(5)
  })
  
  output$count5 <- renderTable({
    req(input$vars5)
    
    mtcars %>% 
      group_by(across(all_of(input$vars5))) %>% 
      summarise(n = n(), .groups = "drop")
  })
  
  
  # following code lets the user select any number of grouping variables, 
  # and any number of variables to summarise with their means.
  output$data6 <- renderTable({
    mtcars %>% 
      group_by(across(all_of(input$vars_g6))) %>% 
      summarise(across(all_of(input$vars_s6), mean), n = n()) %>% 
      head(7)
  })
}

shinyApp(ui, server)
