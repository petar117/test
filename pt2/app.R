library(shiny)
library(ggplot2)
library(glue)
library(tidyverse)

set.seed(1014)
df <- data.frame(x1 = rnorm(100), y1 = rnorm(100))

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  "Page title",
  tabPanel(
    "panel 1",
    sliderInput(
      "x",
      "x",
      value = 1,
      min = 0,
      max = 10
    ),
    sliderInput(
      "y",
      "y",
      value = 2,
      min = 0,
      max = 10
    ),
    sliderInput(
      "z",
      "z",
      value = 3,
      min = 0,
      max = 10
    ),
    textOutput("total")
  ),
  navbarMenu(
    "subpanels",
    tabPanel("panel 2",
             fluidPage(
               titlePanel("Inputs and user feedback"),
               sidebarLayout(
                 sidebarPanel(
                   fileInput("file", "Data", buttonLabel = "Upload..."),
                   textInput("delim", "Delimiter (leave blank to guess)", ""),
                   numericInput("skip", "Rows to skip", 0, min = 0),
                   numericInput("rows", "Rows to preview", 10, min = 1)
                 ),
                 mainPanel(
                   fluidRow(
                     shinyFeedback::useShinyFeedback(),
                     numericInput("n", "Enter an even number", value = ""),
                     textOutput("half"),
                     HTML("<br><br>")
                   ),
                   fluidRow(
                     selectInput("language", "Language", choices = c("", "English", "Maori")),
                     textInput("name", "Name"),
                     textOutput("greeting"),
                     HTML("<br><br>")
                   )
                 )
               )
             )),
    tabPanel("panel 2.1",
             fluidPage(
               fluidRow(
                 column(
                   4,
                   shinyFeedback::useShinyFeedback(),
                   textInput("dataset", "Dataset name"),
                   actionButton("goodnight", "Good night"),
                   HTML("<br><br>"),
                   
                 ),
                 column(4,
                        tableOutput("data3"))
               ),
               fluidRow(
                 HTML("<br><br>"),
                 numericInput("x_sqrt", "x", value = 0),
                 selectInput(
                   "trans",
                   "transformation",
                   choices = c("square", "log", "square-root")
                 ),
                 textOutput("out"),
                 HTML("<br><br>")
               ),
               fluidRow(
                 column(
                   4,
                   numericInput("steps", "How many steps?", 10),
                   actionButton("go", "go"),
                   textOutput("result")
                 )
               ),
               fluidRow(
                 column(
                   4,
                   waiter::use_waitress(),
                   numericInput("steps1", "How many steps?", 10),
                   actionButton("go1", "go"),
                   textOutput("result1")
                 ),
                 column(
                   4,
                   waiter::use_waiter(),
                   numericInput("steps2", "How many steps?", 10),
                   actionButton("go2", "go"),
                   textOutput("result2")
                 )
               )
             ))
  ),
  tabPanel("panel 3",
           fluidPage(
             titlePanel("Plot"),
             fluidRow(
               column(5,
                      plotOutput(
                        "plot",
                        click = "plot_click", brush = "plot_brush"
                      )),
               column(
                 7,
                 verbatimTextOutput("info"),
                 tableOutput("data"),
                 tableOutput("data1")
               )
             )
           )),
  tabPanel("panel 4",
           fluidPage(
             titlePanel("Plot and reactive val"),
             fluidRow(column(
               4,
               sliderInput(
                 "height",
                 "height",
                 min = 100,
                 max = 500,
                 value = 250
               ),
               sliderInput(
                 "width",
                 "width",
                 min = 100,
                 max = 500,
                 value = 250
               ),
             ),
             column(
               8,
               plotOutput(
                 "plot1",
                 brush = "plot_brush",
                 dblclick = "plot_reset",
                 width = 250,
                 height = 250
               )
             )),
             fluidRow(column(4, ),
                      column(
                        8,
                        plotOutput(
                          "plot2",
                          click = "plot_click",
                          width = 250,
                          height = 250
                        )
                      ))
           ))
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
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point()
  },
  res = 96)
  
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  output$data <- renderTable({
    req(input$plot_click)
    nearPoints(mtcars,
               input$plot_click,
               xvar = "wt",
               yvar = "mpg")
  })
  
  output$data1 <- renderTable({
    brushedPoints(mtcars,
                  input$plot_brush,
                  xvar = "wt",
                  yvar = "mpg")
  })
  
  
  # TAB 4 PLOT INTERACTION AND CRATING REACTIVE VALUES
  selected <- reactiveVal(rep(FALSE, nrow(mtcars)))
  
  observeEvent(input$plot_brush, {
    brushed <-
      brushedPoints(mtcars, input$plot_brush, allRows = TRUE)$selected_
    selected(brushed | selected())
  })
  
  observeEvent(input$plot_reset, {
    selected(rep(FALSE, nrow(mtcars)))
  })
  
  output$plot1 <- renderPlot({
    mtcars$sel <- selected()
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point(aes(colour = sel)) +
      scale_colour_discrete(limits = c("TRUE", "FALSE"))
  },
  width = function() {
    input$width
  },
  height = function() {
    input$height
  },
  res = 96)
  
  
  
  
  dist <- reactiveVal(rep(1, nrow(df)))
  observeEvent(input$plot_click$plot2,
               dist(
                 nearPoints(
                   df,
                   input$plot_click,
                   allRows = TRUE,
                   addDist = TRUE
                 )$dist_
               ))
  
  output$plot2 <- renderPlot({
    df$dist <- dist()
    ggplot(df, aes(x1, y1, size = dist)) +
      geom_point() +
      scale_size_area(limits = c(0, 1000),
                      max_size = 10,
                      guide = NULL)
  },
  res = 96)
  
  # TAB 2 INPUTS AND USER FEEDBACK
  half <- reactive({
    even <- input$n %% 2 == 0
    shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
    req(even)
    input$n / 2
  })
  
  output$half <- renderText(half())
  
  
  greetings <- c(English = "Hello",
                 Maori = "Kia ora")
  output$greeting <- renderText({
    req(input$language, input$name)
    paste0(greetings[[input$language]], " ", input$name, "!")
  })
  
  
  
  
  # TAB 2.1
  
  notify <- function(msg, id = NULL) {
    showNotification(msg,
                     id = id,
                     duration = NULL,
                     closeButton = FALSE)
  }
  
  data3 <- reactive({
    req(input$dataset)
    
    exists <- exists(input$dataset, "package:datasets")
    
    id <- notify("Reading data...")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)
    
    notify("Reticulating splines...", id = id)
    Sys.sleep(1)
    
    shinyFeedback::feedbackDanger("dataset", !exists, "Unknown dataset")
    req(exists, cancelOutput = TRUE)
    
    get(input$dataset, "package:datasets")
  })
  
  output$data3 <- renderTable({
    data3() %>% head(2)
    # head(data3())
  })
  
  output$out <- renderText({
    if (input$x_sqrt < 0 && input$trans %in% c("log", "square-root")) {
      validate("x can not be negative for this transformation")
    }
    
    switch(
      input$trans,
      square = input$x_sqrt ^ 2,
      "square-root" = sqrt(input$x_sqrt),
      log = log(input$x_sqrt)
    )
  })
  
  #notifications tab 2.1
  observeEvent(input$goodnight, {
    showNotification("So long")
    Sys.sleep(1)
    showNotification("Farewell", type = "message")
    Sys.sleep(1)
    showNotification("Auf Wiedersehen", type = "warning")
    Sys.sleep(1)
    showNotification("Adieu", type = "error")
  })
  
  data4 <- eventReactive(input$go, {
    withProgress(message = "Computing random number", {
      for (i in seq_len(input$steps)) {
        Sys.sleep(0.5)
        incProgress(1 / input$steps)
      }
      runif(1)
    })
  })
  output$result <- renderText(round(data4(), 2))
  
  
  #progress bar 
  
  data5 <- eventReactive(input$go1, {
    waitress <- waiter::Waitress$new(max = input$steps1, 
                                     selector = "#steps1", theme = "overlay")
    on.exit(waitress$close())
    
    for (i in seq_len(input$steps1)) {
      Sys.sleep(0.5)
      waitress$inc(1)
    }
    
    runif(1)
  })
  
  output$result1 <- renderText(round(data5(), 2))
  
  data6 <- eventReactive(input$go2, {
    waiter <- waiter::Waiter$new(html = spin_ripple())
    waiter$show()
    on.exit(waiter$hide())
    
    Sys.sleep(sample(5, 1))
    runif(1)
  })
  output$result2 <- renderText(round(data6(), 2))
  
}

shinyApp(ui, server)
