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
    column(6,
           selectInput("code", "Product", choices = prod_codes)
    )
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
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
    )
  
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )
}

shinyApp(ui, server)

