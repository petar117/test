library(shiny)
library(readr)
library(tidyverse)

make_dropdown <- function(name_of_vector) {
  selectInput(inputId = name_of_vector, label =  name_of_vector, choices = 
                c("numeric", "character", "logical"))
}  

ui <- fluidPage(
  tags$style("#wizard { display:none; }"),
  tabsetPanel(id = "wizard",
              tabPanel("page1", 
                       fileInput("data_input", "input"),
                       actionButton("page12", "next")
              ),
              tabPanel("page2",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("type_of")
                         ),
                         mainPanel(
                           tableOutput('type_table')
                         )),
                       actionButton("page21", "prev"),
                       actionButton("page23", "next")
              ),
              tabPanel("page3", 
                       tableOutput("summary_table"),
                       actionButton("page32", "prev")
              )
  )
)

server <- function(input, output, session) {
  
  ################ WIZARD  ###############################
  
  switch_tab <- function(page) {
    updateTabsetPanel(session, "wizard", selected = page)
  }
  
  observeEvent(input$page12, switch_tab("page2"))
  observeEvent(input$page21, switch_tab("page1"))
  observeEvent(input$page23, switch_tab("page3"))
  observeEvent(input$page32, switch_tab("page2"))
  
  ##################### FILE INPUT #######################
  
  dat <- reactive({
    req(input$data_input)
    read.csv(input$data_input$datapath)
  })
  
  ##################### TABLE TYPE #######################
  
  # make a dropdown using the names of each column
  output$type_of <- renderUI({ map(names(dat()), ~ make_dropdown(.x)) })
  
  
  # switch the type of column based on the input
  # name of vector == "Sepal.Length"
  # vector == Sepal.Length
  change_type <- function(vector, name_of_vector) {
    switch(input[[name_of_vector]],
           "numeric" = vector <- as.numeric(vector),
           "character" = vector <- as.character(vector),
           "logical" = vector <- as.complex(vector)
    )
  }
  
  # convert the supplied data to a list
  # use imap because it is a condensed version o map
  # with two arguments == x & name_of_x
  # so we don't need to supply it arguments beyond the list!
  df<- reactive({
    dat() %>% 
      as.list() %>% 
      imap(change_type) %>% 
      as_tibble()
  })
  
  # create an output of the data's names
  # and their types
  output$type_table <- renderTable(data.frame(
    names = names(df()),
    type = map_chr(df(), function(x) typeof(x)))
  )
  
  ##################### TABLE OUTPUT #####################
  
  output$summary_table <- renderTable( summary(df()) )
}

shinyApp(ui = ui, server = server)