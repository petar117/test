library(shiny)
library(officer)
library(text)
library(wordcloud2)
library(tm)
library(colourpicker)
library(tidytext)
library(sentimentr)
library(textstem)
library(tidyverse)

ui <- fluidPage(
  h1("Word Cloud"),
  # Create a "Word cloud" tab
  tabPanel(
    title = "Word cloud",
    sidebarLayout(
      sidebarPanel(
        # Wrap the file input in a conditional panel
        fileInput("file", "Upload a file"),
        hr(),
        # Add the selector for the language of the text
        selectInput(
          inputId = "language",
          label = "Remove stopwords in",
          choices = c("Danish", "Dutch", "English", "Finnish", "French", "German", "Hungarian", "Italian", "Norwegian", "Portuguese", "Russian", "Spanish", "Swedish"),
          multiple = FALSE,
          selected = "English"
        ),
        hr(),
        checkboxInput("remove_words", "Remove specific words?", FALSE),
        conditionalPanel(
          condition = "input.remove_words == 1",
          textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
          textAreaInput("words_to_remove2", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
          textAreaInput("words_to_remove3", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
          textAreaInput("words_to_remove4", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
          textAreaInput("words_to_remove5", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
          textAreaInput("words_to_remove6", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
          textAreaInput("words_to_remove7", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
          textAreaInput("words_to_remove8", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
          textAreaInput("words_to_remove9", "", rows = 1)
        ),
        conditionalPanel(
          condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
          textAreaInput("words_to_remove10", "", rows = 1)
        ),
        hr(),
        numericInput("num", "Maximum number of words",
          value = 100, min = 5
        ),
        hr(),
        textInput("custom_stopwords", "Custom Stopwords (comma-separated):"), actionButton("add", "add"),
        actionButton("update_stopwords", "Update Stopwords"),
        textOutput("names"),
        colourInput("col", "Background color", value = "white"),
        hr(),
        HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/word-cloud/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/word-cloud/blob/master/app.R">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>')
      ),
      mainPanel(
        wordcloud2Output("cloud"),
        br(),
        br()
      )
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (!is.null(input$file)) {
      if (tolower(tools::file_ext(input$file$name)) == "docx") {
        # If it's a docx file, read the text directly
        doc <- read_docx(input$file$datapath)
        data <- docx_summary(doc)$text
      } else if (tolower(tools::file_ext(input$file$name)) == "pdf") {
        # If it's a pdf file, read the text using the pdftools package
        pdf_text <- pdftools::pdf_text(input$file$datapath)
        data <- unlist(strsplit(pdf_text, "\\s+"))
      } else {
        # For other file types (e.g., csv, txt), read as-is
        text <- readLines(input$file$datapath, warn = FALSE, encoding = "UTF-8", skipNul = TRUE)
       # data <- paste(text, collapse = "\n")  # Use "\n" for Unix/Linux-style line endings
      }
    } else {
      # Handle the case when no file is selected
      data <- character(0)
    }
    return(data)
  })

  custom_stopwords <- reactiveVal(NULL)

  create_wordcloud <- function(data, num_words = 100, background = "white") {
    if (is.character(data)) {
      # corpus <- Corpus(VectorSource(data))
      # corpus <- tm_map(corpus, content_transformer(tolower))
      # corpus <- tm_map(corpus, removePunctuation)
      # corpus <- tm_map(corpus, removeNumbers)
      # corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      # tdm <- as.matrix(TermDocumentMatrix(corpus))
      # data <- sort(rowSums(tdm), decreasing = TRUE)
      # data <- data.frame(word = names(data), freq = as.numeric(data))
      cleaned_data <- data %>%
        removePunctuation() %>%
        removeNumbers()


      # if (!is.null(custom_stopwords())) {
      #   cleaned_data <- removeWords(cleaned_data, custom_stopwords())
      # }
      # custom_stopwords <- strsplit(input$custom_stopwords, ",\\s*")[[1]]
      # cleaned_data <- str_replace_all(cleaned_data, paste0("\\b(", paste(custom_stopwords, collapse = "|"), ")\\b"), "")
      #
      # # cleaned_data <- dplyr::anti_join(
      # #   data.frame(word = unlist(strsplit(cleaned_data, "\\s+")), stringsAsFactors = FALSE),
      # #   data.frame(word = custom_stopwords, stringsAsFactors = FALSE), by = "word"
      # # )
      #


      tibble_data <- tibble(text = cleaned_data)

      word_freq <- tibble_data %>%
        dplyr::filter(text != "") %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(
          wordl = lemmatize_words(word),
          words = stem_words(word)
        ) %>%
        filter(!wordl %in% custom_stopwords()) %>%
        filter(!word %in% custom_stopwords()) %>%
        count(wordl, sort = TRUE)
    }

    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }

    # Grab the top n most common words
    word_freq <- head(word_freq, n = num_words)
    if (nrow(word_freq) == 0) {
      return(NULL)
    }

    wordcloud2(word_freq, backgroundColor = background)
  }

  observeEvent(input$file, {
    custom_stopwords(NULL)
  })

  observeEvent(input$update_stopwords, {
    custom_stopwords(strsplit(input$custom_stopwords, ",\\s*")[[1]])
  })

  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
      num_words = input$num,
      background = input$col
    )
  })
}

shinyApp(ui = ui, server = server)
