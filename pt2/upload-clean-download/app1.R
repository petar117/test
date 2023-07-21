library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"), # file widget
      selectInput("variable", "Select Variable", choices = NULL) # select widget
    ),
    mainPanel(
      verbatimTextOutput("results") # t-test results
    )
  )
)

server <- function(input, output,session) {
  
  # get data from file
  data <- reactive({
    req(input$file)
    
    # as shown in the book, lets make sure the uploaded file is a csv
    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv", "Invalid file. Please upload a .csv file"))
    
    dataset <- vroom::vroom(input$file$datapath, delim = ",")
    
    # let the user know if the data contains no numeric column
    validate(need(ncol(dplyr::select_if(dataset, is.numeric)) != 0,
                  "This dataset has no numeric columns."))
    dataset
  })
  
  # create the select input based on the numeric columns in the dataframe
  observeEvent(input$file, {
    req(data())
    num_cols <- dplyr::select_if(data(), is.numeric)
    updateSelectInput(session, "variable", choices = colnames(num_cols))
  })
  
  # print t-test results
  output$results <- renderPrint({
    if(!is.null(input$variable))
      t.test(data()[input$variable])
  })
}

shinyApp(ui, server)