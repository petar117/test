library(shiny)

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}


ui <- navbarPage(
  "Page title",
  tabPanel(
    "tab 1",
    fluidPage(
      textInput("label", "label"),
      selectInput("type", "type", c("slider", "numeric")),
      uiOutput("numeric")
      ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          numericInput("n", "Number of colours", value = 5, min = 1),
          uiOutput("col"),
        ),
        mainPanel(
          plotOutput("plot")  
        )
      )
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          make_ui(iris$Sepal.Length, "Sepal.Length"),
          make_ui(iris$Sepal.Width, "Sepal.Width"),
          make_ui(iris$Species, "Species")
        ),
        mainPanel(
          tableOutput("data")
        )
      )
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          map(names(iris), ~ make_ui(iris[[.x]], .x))
        ),
        mainPanel(
          tableOutput("data1")
        )
      )
    )
  ),
  tabPanel(
    "tab 2",
    
  )
  
)


server <- function(input, output, session) {
  
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("dynamic", input$label, value = 0, min = 0, max = 10)
    } else {
      numericInput("dynamic", input$label, value = 0, min = 0, max = 10) 
    }
  })
  
  
  col_names <- reactive(paste0("col", seq_len(input$n)))
  
  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])))
  })
  
  output$plot <- renderPlot({
    cols <- map_chr(col_names(), ~ input[[.x]] %||% "")
    # convert empty inputs to transparent
    cols[cols == ""] <- NA
    
    barplot(
      rep(1, length(cols)), 
      col = cols,
      space = 0, 
      axes = FALSE
    )
  }, res = 96)
  
  
  
  selected <- reactive({
    filter_var(iris$Sepal.Length, input$Sepal.Length) &
      filter_var(iris$Sepal.Width, input$Sepal.Width) &
      filter_var(iris$Species, input$Species)
  })
  
  output$data <- renderTable(head(iris[selected(), ], 12))
  
  
  selected1 <- reactive({
    each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
    reduce(each_var, ~ .x & .y)
  })
  
  output$data1 <- renderTable(head(iris[selected1(), ], 12))
}

shinyApp(ui, server)