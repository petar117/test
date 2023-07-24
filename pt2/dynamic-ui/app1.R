library(shiny)
library(ggplot2)

# exercises - dynamic ui/dynamic visability

# Modify the app you created in the previous exercise to allow the user to 
# choose whether each geom is shown or not (i.e. instead of always using 
# one geom, they can picked 0, 1, 2, or 3). Make sure that you can control 
# the binwidth of the histogram and frequency polygon independently.


geom_choices <- c("histogram", "freqpoly", "density")

# ----------------------------------------------- #
#    Generate the necessary code (as a string)    #
#    for ggplot, after having chosen one of       #
#    the available geom functions in the ui       #
# ----------------------------------------------- #
# A list could be used, working only with the three
# provided geom choices, but a fuction will be used,
# to provide a template for a possible generalization 
# when working with more geom choices and parameters.

geom_choice_code <- function(geom_choice) {
  # Using %in% could be a more general case,
  # considering more possible geom options.
  if (geom_choice == "density") { 
    return(paste0(
      # Simple density
      # "geom_", geom_choice, "(bw = input$", geom_choice, "_bw)"
      
      # Match density to the histogram's y-scale
      "geom_density(
        color = 'blue',
        bw = input$density_bw,
        aes(y = ..density.. * (nrow(diamonds) * input$histogram_bw))
      )"
    ))
  }
  
  return(paste0(
    "geom_", geom_choice, "(", 
    # Improve histograms' visibility
    ifelse(geom_choice == "histogram", "fill = 'transparent', color = 'red', ", ""),
    
    "binwidth = input$", geom_choice, "_bw)"
  ))
}


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("geom", 
                  label = "Geom function to use", multiple = TRUE,
                  choices = geom_choices, selected = "histogram"
      ),
      tabsetPanel(
        id = "histogram",
        type = "hidden",
        tabPanelBody("histogram_empty", style = "display: none"),
        tabPanelBody("histogram_params",
                     # This parameter will also be used to scale the y-axis
                     # when plotting density, so that all graphics
                     # have a similar y-axis of "count"
                     numericInput("histogram_bw", 
                                  label = "Histogram's binwidth", value = 0.15, 
                                  min = 0.1, max = 5, step = 0.1
                     )
        )
      ),
      tabsetPanel(
        id = "freqpoly",
        type = "hidden",
        tabPanelBody("freqpoly_empty", style = "display: none"),
        tabPanelBody("freqpoly_params", 
                     numericInput("freqpoly_bw", 
                                  label = "freqpoly's binwidth", value = 0.15, 
                                  min = 0.1, max = 5, step = 0.1
                     )
        )
      ),
      tabsetPanel(
        id = "density",
        type = "hidden",
        tabPanelBody("density_empty", style = "display: none"),
        tabPanelBody("density_params",
                     numericInput("density_bw", 
                                  label = "Standard deviation of smoothing kernel",
                                  value = 0.15, min = 0.01, max = 1, step = 0.01
                     )
        )
      )
    ),
    mainPanel(
      plotOutput("gg")
    )
  )
)

server <- function(input, output, session) {
  # ------------------------------------------ #
  # Update inputs' visibility in sidebar panel #
  # ------------------------------------------ #
  observeEvent(input$geom, {
    # Get non selected geom functions, in order
    # to hide their respective parameters
    non_selected <- setdiff(geom_choices, input$geom)
    purrr::map(
      geom_choices, 
      ~ updateTabsetPanel(
        inputId = .x, 
        selected = paste0(
          .x, 
          ifelse(.x %in% non_selected, "_empty", "_params")
        )
      )
    )
    # Run this code also when the select input is cleared
  }, ignoreNULL = FALSE) 
  
  # ------------------------ #
  # Retrieve code for ggplot #
  # ------------------------ #
  gg_args <- reactive({
    req(input$geom)
    
    purrr::map_chr(input$geom, geom_choice_code) |>
      paste(collapse = " + ")
  })
  
  output$gg <- renderPlot({
    eval(parse(text = paste0(
      "ggplot(diamonds, aes(carat)) + ",
      gg_args(), " + ",
      "labs(y = 'Count')"
    )))
  })
}

shinyApp(ui, server)