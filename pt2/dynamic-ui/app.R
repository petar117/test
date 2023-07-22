library(shiny)
library(dplyr, warn.conflicts = FALSE)
sales <- vroom::vroom("sales_data_sample.csv", col_types = list(), na = "")
sales %>% 
  select(TERRITORY, CUSTOMERNAME, ORDERNUMBER, everything()) %>%
  arrange(ORDERNUMBER)

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
      ),
      fluidRow(HTML("<hr>")),
      fluidPage(
        titlePanel("Sales Dashboard"),
        sidebarLayout(
          sidebarPanel(
            selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
            selectInput("customername", "Customer", choices = NULL),
            selectInput("ordernumber", "Order number", choices = NULL, size = 5, selectize = FALSE),
          ),
          mainPanel(
            uiOutput("customer"),
            tableOutput("data")
          )
        )
      ),
      fluidRow(HTML("<hr>")),
      fluidPage(
        titlePanel("Freezing reactive inputs"),
        sidebarLayout(
          sidebarPanel(
            selectInput("dataset", "Choose a dataset", c("pressure", "cars")),
            selectInput("column", "Choose column", character(0))
          ),
          mainPanel(
            verbatimTextOutput("summary")
          )
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
  
 
  # table based on selected input / sales dashboard
  territory <- reactive({
    req(input$territory)
    filter(sales, TERRITORY == input$territory)
  })
  
  customer <- reactive({
    req(input$customername)
    filter(territory(), CUSTOMERNAME == input$customername)
  })
  
  output$customer <- renderUI({
      row <- customer()[1, ]
    tags$div(
      class = "well",
      tags$p(tags$strong("Name: "), row$CUSTOMERNAME),
      tags$p(tags$strong("Phone: "), row$PHONE),
      tags$p(tags$strong("Contact: "), row$CONTACTFIRSTNAME, " ", row$CONTACTLASTNAME)
    )
  })
  
  order <- reactive({
    req(input$ordernumber)
    customer() %>%
      filter(ORDERNUMBER == input$ordernumber) %>%
      arrange(ORDERLINENUMBER) %>%
      select(PRODUCTLINE, QUANTITYORDERED, PRICEEACH, SALES, STATUS)
  })
  
  output$data <- renderTable(order())
  
  observeEvent(territory(), {
    updateSelectInput(session, "customername", 
                      choices = unique(territory()$CUSTOMERNAME), 
                      selected = character())
  })
  observeEvent(customer(), {
    updateSelectInput(session, "ordernumber", 
                      choices = unique(customer()$ORDERNUMBER))
  })
  
  
  # freezing reactive inputs / !!! always use it when you dynamically change an input value
  dataset <- reactive(get(input$dataset, "package:datasets"))
  
  observeEvent(input$dataset, {
    freezeReactiveValue(input, "column")
    updateSelectInput(inputId = "column", choices = names(dataset()))
  })
  
  output$summary <- renderPrint({
    summary(dataset()[[input$column]])
  })
  
}

shinyApp(ui, server)
