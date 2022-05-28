library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(lubridate)
library(wordcloud2)
library(tidytext) 
library(stopwords)
library(tm)
library(wordcloud)

#data_insta_completa <- read.csv("data_insta_completa.csv")
tokens_instagram <- read.csv("tokens_instagram.csv")


# primera nube exploratoria

matriz_terminos_insta <- tokens_instagram   %>%
  group_by(tokens) %>% 
  dplyr::summarise(n =  dplyr::n(),
                   sum_LIKES = sum(LIKES, na.rm=T),
                   mean_LIKES = as.integer(mean(LIKES, na.rm=T))) %>% 
  subset(!str_detect(tokens, "^[0-9]"))


wordcloud(words = matriz_terminos_insta$tokens, 
          freq = matriz_terminos_insta$mean_LIKES, 
          min.freq = 1,
          max.words=800, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

