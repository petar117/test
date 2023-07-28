library(officer)
library(text)
library(wordcloud2)
library(tm)
library(colourpicker)
library(tidyverse)
library(tidytext)
library(sentimentr)
library(textstem)




doc <- read_docx("pt2/proposal.docx")
text <- docx_summary(doc)$text
temp_file <- tempfile(fileext = ".txt")
writeLines(text, temp_file)
data <- readLines(temp_file)
unlink(temp_file) # Remove the temporary file

create_wordcloud <- function(data, num_words = 100, background = "white") {

}
  
if (is.character(data)) {
  data %>% select(text)
  }

text_df <- tibble(text = data)
text_df %>% 
  unnest_tokens(word,text) %>%
  mutate(wordl=lemmatize_words(word),
         words=stem_words(word)) %>% 
  count(wordl, sort = TRUE) %>% 
  wordcloud2()




data1 <- text_df %>% 
  dplyr::filter(text!="") %>% 
  unnest_tokens(word, text) %>% 
  filter(word!="web") %>%
  anti_join(stop_words) %>% 
  mutate(wordl = lemmatize_words(word),
         words = stem_words(word)) %>% 
  count(wordl, sort = TRUE) 
  
  data1 %>% head(100) %>% 
  wordcloud2()


 
