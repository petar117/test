library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(shinyjs)

sales <- vroom::vroom("sales_data_sample.csv", col_types = list(), na = "")
sales %>% 
  select(TERRITORY, CUSTOMERNAME, ORDERNUMBER, everything()) %>%
  arrange(ORDERNUMBER)


parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean", "mean", value = 1),
           numericInput("sd", "standard deviation", min = 0, value = 1)
  ),
  tabPanel("uniform", 
           numericInput("min", "min", value = 0),
           numericInput("max", "max", value = 1)
  ),
  tabPanel("exponential",
           numericInput("rate", "rate", value = 1, min = 0),
  )
)


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
    "tab 2",
    fluidPage(
      titlePanel("Exercises"),
      useShinyjs() ,
      numericInput("year", "year", value = 2020),
      dateInput("date", "date", value = Sys.Date())
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          checkboxInput("moreControls", 
                        label = "Show advanced controls?",
                        value = FALSE
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "basic",
            type = "hidden",
            tabPanelBody("panel1",
                         numericInput("basicControl", label = "Basic:", 0),
            )
          ),
          tabsetPanel(
            id = "advanced",
            type = "hidden",
            tabPanelBody("emptyPanel", style = "display: none"),
            tabPanelBody("panel2",
                         numericInput("advancedCotrol", label = "Advanced:", 1)
            )
          )
        )
      )
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("geom", "Geom function to use", 
        choices = c("histogram", "freqpoly", "density")
      ),
      tabsetPanel(
        id = "params",
        type = "hidden",
        tabPanel("histogram",
          numericInput("hist_bw", 
            label = "Binwidth", value = 0.1, 
            min = 0.1, max = 5, step = 0.1
          )
        ),
        tabPanel("freqpoly", 
          numericInput("freqpoly_bw", 
            label = "Binwidth", value = 0.1, 
            min = 0.1, max = 5, step = 0.1
          )
        ),
        tabPanel("density",
          numericInput("density_bw", 
            label = "Standard deviation of smoothing kernel",
            value = 0.01, min = 0.01, max = 1, step = 0.01
          )
        )
      )
    ),
    mainPanel(
      plotOutput("gg")
    )
  )
)
    
  ),
  tabPanel(
    "tab 3",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("controller", "Show", choices = paste0("panel", 1:3))
        ),
        mainPanel(
          tabsetPanel(
            id = "switcher",
            type = "hidden",
            tabPanelBody("panel1", "Panel 1 content"),
            tabPanelBody("panel2", "Panel 2 content"),
            tabPanelBody("panel3", "Panel 3 content")
          )
        )
      )
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("dist", "Distribution", 
                      choices = c("normal", "uniform", "exponential")
          ),
          numericInput("n5", "Number of samples", value = 100),
          parameter_tabs,
        ),
        mainPanel(
          plotOutput("hist")
        )
      )
    ),
    fluidRow(HTML("<hr>")),
    fluidPage(
      tabsetPanel(
        id = "wizard",
        type = "hidden",
        tabPanel("page_1", 
                 "Welcome!",
                 actionButton("page_12", "next")
        ),
        tabPanel("page_2", 
                 "Only one page to go",
                 actionButton("page_21", "prev"),
                 actionButton("page_23", "next")
        ),
        tabPanel("page_3", 
                 "You're done!",
                 actionButton("page_32", "prev")
        )
      )
    ),
    fluidRow(HTML("<hr>"))
    
    
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
  
  
  # exercises
  #
  observeEvent(input$year, {
    
    req(input$year) # stop if year is blank
    daterange <- range(as.Date(paste0(input$year, "-01-01")),as.Date(paste0(input$year, "-12-31")))
    updateDateInput(session, "date", min = daterange[1], max = daterange[2] )
    delay(250,  # delay 250ms
          updateDateInput(session,"date",
                          value = daterange[1]
          ))
  })
  
  # updates input$county choices based on input$state. 
  # For an added challenge, also change the label from “County” to 
  # “Parrish” for Louisana and “Borrough” for “Alaska”.
  
  # label <- reactive({
  #   switch(input$state,
  #          "Alaska" = "Burrough",
  #          "Louisiana" = "Parish",
  #          "County")
  # })
  # 
  # observeEvent( input$state, {
  #   updateSelectInput(session, "county", label = label(),
  #                     choices = county %>% 
  #                       filter(state == input$state) %>%
  #                       select(name) %>%
  #                       distinct())
  # })
  
  
  # Use a hidden tabset to show additional controls only if the 
  # user checks an “advanced” check box.
  observeEvent(input$moreControls, {
    if (input$moreControls) {
      updateTabsetPanel(session, "advanced", selected = "panel2")
    } else {
      updateTabsetPanel(session, "advanced", selected = "emptyPanel")
    }
  })
  
  
  # DYNAMIC VISABILITY - TAB 3
  
  observeEvent(input$controller, {
    updateTabsetPanel(inputId = "switcher", selected = input$controller)
  })
  
  # !!! 
  # Imagine that you want an app that allows the user to simulate from 
  # the normal, uniform, and exponential distributions. Each distribution 
  # has different parameters, so we’ll need some way to show different 
  # controls for different distributions. Here, I’ll put the unique user 
  # interface for each distribution in its own tabPanel(), and then arrange 
  # the three tabs into a tabsetPanel().
  
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  }) 
  
  sample <- reactive({
    switch(input$dist,
           normal = rnorm(input$n5, input$mean, input$sd),
           uniform = runif(input$n5, input$min, input$max),
           exponential = rexp(input$n5, input$rate)
    )
  })
  output$hist <- renderPlot(hist(sample()), res = 96)
  
  # wizard interface
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_32, switch_page(2))
  
  
  # !!!!  EXERCISES
  
  # Create an app that plots ggplot(diamonds, aes(carat)) but allows the
  # user to choose which geom to use: geom_histogram(), geom_freqpoly(), 
  # or geom_density(). Use a hidden tabset to allow the user to select 
  # different arguments depending on the geom: geom_histogram() and 
  # geom_freqpoly() have a binwidth argument; geom_density() has a bw 
  # argument.
  
  observeEvent(input$geom, {
    updateTabsetPanel(inputId = "params", selected = input$geom)
  }) 
  
  gg_args <- reactive({
    switch(input$geom, 
           histogram = geom_histogram(binwidth = input$hist_bw),
           freqpoly = geom_freqpoly(binwidth = input$freqpoly_bw),
           density = geom_density(bw = input$density_bw)
    )
  })
  
  output$gg <- renderPlot({
    ggplot(diamonds, aes(carat)) +
      gg_args()
  })

}


shinyApp(ui, server)
