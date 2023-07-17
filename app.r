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

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)

selected %>% count(location, wt = weight, sort = TRUE)

selected %>% count(body_part, wt = weight, sort = TRUE)

selected %>% count(diag, wt = weight, sort = TRUE)

summary <- selected %>% 
  count(age, sex, wt = weight)

summary %>% 
  ggplot(aes(age, n, color = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")

summary1 <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary1 %>% 
  ggplot(aes(age, rate, color = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")

selected %>% 
  sample_n(10) %>% 
  pull(narrative)


prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
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
  )
)

server <- function(input, output, session) {
   selected <- reactive(injuries %>% filter(prod_code == input$code))
  
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
  
  output$diag <- renderTable(
    count_top(selected(), diag), width = "100%"
    )
  
  output$body_part <- renderTable(
    # selected() %>% 
    #   count(body_part, wt = weight, sort = TRUE) %>% 
    #   head(6)
    count_top(selected(), body_part), width = "100%"
  )
  
  output$location <- renderTable(
    count_top(selected(), location), width = "100%"
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
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
}

shinyApp(ui, server)

