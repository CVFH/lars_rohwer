library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(lubridate)
library(wordcloud2)
library(tidytext) 
library(stopwords)
library(tm)
library(wordcloud)

#data_facebook_completa <- read.csv("data_facebook_completa.csv")
tokens_facebook <- read.csv("tokens_facebook.csv")

# creamos variable engagement 

tokens_facebook <- tokens_facebook %>% 
  mutate(engagement = LIKES + comments + compartida)


# primera nube exploratoria

matriz_terminos_facebook <- tokens_facebook  %>%
  group_by(tokens) %>% 
  dplyr::summarise(n =  dplyr::n(),
                   sum_engagement = sum(engagement, na.rm=T),
                   mean_engagement = as.integer(mean(engagement, na.rm=T))) %>% 
  na.omit(mean_engagement) %>% 
  subset(!str_detect(tokens, "^[0-9]"))


wordcloud(words = matriz_terminos_facebook$tokens, 
          freq = matriz_terminos_facebook$mean_engagement, 
          min.freq = 1,
          max.words=400, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





 


####### BORRADORES #####################

# comparamos top post versus antitop posts

tokens_porpost <- tokens_facebook  %>%
  group_by(datahora) %>% 
  mutate()





%>% 
  acast(tokens ~ palabra_busqueda, value.var = "n", fill = 0)

wordcloud::comparison.cloud(matriz_terminos ,
                            colors = c("lightblue", "blue"),
                            max.words = 100,
                            title.size=NULL)



# creamos df que tambien vamos a usar para prox grafico
tuitsarg_tokens_year <- tuitsarg_tokens_limpios  %>% 
  mutate(year = str_sub(created_at, start = 1, end = 4)) %>% 
  mutate(engagement = public_metrics.retweet_count +
           public_metrics.reply_count +
           public_metrics.like_count) %>% 
  group_by(year, tokens) %>% 
  summarise(n=n(),
            engagement = sum(engagement),
            incidencia = n*engagement) 

plot_tokens_year <- tuitsarg_tokens_year %>% 
  subset(!str_detect(tokens, "(ciberdefensa)|(ciberseguridad)|(cybersecurity)")) %>% 
  mutate(tokens = str_replace(tokens, "mention", "@")) %>%
  arrange(desc(n)) %>% 
  head(150) %>% 
  ggplot(aes(label = tokens, 
             size = n,
             colour= year)) +
  geom_text_wordcloud() +
  facet_wrap(~year, ncol = 3) +
  scale_size_area(max_size = 4) + 
  theme_void() +
  labs(title= "Palabras más tuiteadas por año",
       caption = "Elaboración propia.
       Se cuenta la cantidad de veces que aparece una palabra en los tutis únicos")
