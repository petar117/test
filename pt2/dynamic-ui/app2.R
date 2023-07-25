library(shiny)

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
    )
    
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
}

shinyApp(ui, server)