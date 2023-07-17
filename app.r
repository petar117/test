library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss")

download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}

download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")
population


# convert the variable to a factor, order by the frequency of 
# the levels, and then lump together all levels after the top 5
# KEY POINT IS TO ROUND UP NUMBERS AND SUMMARISE OTHER IN 6TH ROW

injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))

# function that automates this:


count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%")),
    column(2, numericInput("rows", "Number of Rows",
                           min = 1, max = 10, value = 5)),
    column(2, 
           selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Previous story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  
   selected <- reactive(injuries %>% filter(prod_code == input$code))
   
   # Find the maximum possible of rows.
   max_no_rows <- reactive(
     max(length(unique(selected()$diag)),
         length(unique(selected()$body_part)),
         length(unique(selected()$location)))
   )
   
   # Update the maximum value for the numericInput based on max_no_rows().
   observeEvent(input$code, {
     updateNumericInput(session, "rows", max = max_no_rows())
   })
   
   table_rows <- reactive(input$rows - 1)
   
  
   
  output$diag <- renderTable(
    count_top(selected(), diag, n = table_rows()), width = "100%"
    )
  
  output$body_part <- renderTable(
    # selected() %>% 
    #   count(body_part, wt = weight, sort = TRUE) %>% 
    #   head(6)
    count_top(selected(), body_part, n = table_rows()), width = "100%"
  )
  
  output$location <- renderTable(
    count_top(selected(), location, n = table_rows()), width = "100%"
  )
  
  
  
  summary <- reactive({
    selected() %>% 
      count(age, sex, wt = weight) %>% 
      left_join(population, by = c("age", "sex")) %>% 
      mutate(rate = n / population * 1e4)
    })
  
  
  
  output$age_sex <- renderPlot({
    if(input$y == "count"){
      summary() %>% 
        ggplot(aes(age, n, color = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")+
        theme_grey(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")+
        theme_grey(15)
    }
  }, res = 96)
  
  # Store the maximum posible number of stories.
  max_no_stories <- reactive(length(selected()$narrative))
  
  # Reactive used to save the current position in the narrative list.
  story <- reactiveVal(1)
  
  # Reset the story counter if the user changes the product code. 
  observeEvent(input$code, {
    story(1)
  })
  
  # When the user clicks "Next story", increase the current position in the
  # narrative but never go beyond the interval [1, length of the narrative].
  # Note that the mod function (%%) is keeping `current`` within this interval.
  observeEvent(input$next_story, {
    story((story() %% max_no_stories()) + 1)
  })
  
  # When the user clicks "Previous story" decrease the current position in the
  # narrative. Note that we also take advantage of the mod function.
  observeEvent(input$prev_story, {
    story(((story() - 2) %% max_no_stories()) + 1)
  })
  
  output$narrative <- renderText({
    selected()$narrative[story()]
  })
}

shinyApp(ui, server)