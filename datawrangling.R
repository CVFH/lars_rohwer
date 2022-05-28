library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(lubridate)
library(wordcloud2)
library(tidytext) 
library(stopwords)
library(tm)


#data_descarga_cruda <- read.csv("data_descarga.csv")
data_descarga <- readxl::read_xlsx("data_descarga.xlsx")
data_manual <- readxl::read_xlsx("data_manual.xlsx")

data_descarga <- data_descarga %>% 
  mutate(fecha = str_sub(datahora, 1, 10),
         hora = str_sub(datahora, 12, -1))

data_fb <- data_descarga %>% 
  subset(str_detect(data_descarga$medio, "Facebook"))

data_insta <- data_descarga %>% 
  subset(str_detect(data_descarga$medio, "Instagram")) 

data_manual <- data_manual %>% 
  mutate(TEXTO = str_replace(TEXTO, "\n", " "),
         TEXTO = str_replace_all(TEXTO, "lars.rohwer.politik", ""),
         TEXTO = str_replace(TEXTO, "Verifiziert", ""),
         TEXTO = str_replace(TEXTO, "s Profilbild", ""),
         TEXTO = str_trim(TEXTO))



# hacemos id texto para unir con fuzzy join
data_insta <- data_insta %>% 
  mutate(POSTID = str_sub(TEXTO, 7, end = 16)) %>% 
  rename("texto" = TEXTO) # %>% 
 # mutate(LIKES = as.character(LIKES))

data_manual <- data_manual %>% 
  mutate(POSTID = str_sub(TEXTO, 7, end = 16)) # %>% 
  #mutate(LIKES = as.character(LIKES))

# endlich funco!
data_insta_completa <- data_insta %>% 
  left_join(data_manual)


# preparo datos de fecha ###########

format.str <- "%Y-%m-%dT%H:%M:%S"
data_insta_completa <- data_insta_completa %>% 
  mutate(datahoraposi = as.POSIXct(datahora, format.str, tz = "GMT"))

# GUARDO ############
#data_insta_completa %>%  write.csv("data_insta_completa.csv")




#####################################
# TRABAJO CON DATA FACEBOOK ################

format.str <- "%Y-%m-%dT%H:%M:%S"
data_fb <- data_fb %>% 
  mutate(datahoraposi = as.POSIXct(datahora, format.str, tz = "GMT"))

# probandola 

test <- data_fb %>% subset(duplicated(TEXTO))
# CREO METADATA #####

metadata <- data_insta_completa %>% 
  select(POSTID, FOTO, img_word, pro_spon, TEMA1, TEMA2, N_FOTOS, Grund, datahoraposi) %>% 
  subset(!is.na(FOTO)) %>% 
  mutate(datadate = as.Date(datahoraposi)) %>% 
  select(-datahoraposi)

dafa_fb_cid <- data_fb  %>% 
  mutate(POSTID = str_sub(TEXTO, 7, end = 16)) %>% 
  rename("texto" = TEXTO) %>% 
  mutate(datadate = as.Date(datahoraposi)) 

test <- dafa_fb_cid %>% subset(duplicated(POSTID))
test <- metadata %>% subset(duplicated(POSTID))

data_facebook_completa1 <- dafa_fb_cid  %>% 
  left_join(metadata)

# hay algunos repetidos pero quede conforme

# guardo

#data_facebook_completa1 %>%  write.csv("data_facebook_completa.csv")

########################################## 
########################################### 
############################################  

# TRABAJO CON TOKENS #######

# importo data ya trabajada en lo anterior ############

data_insta_completa <- read.csv("data_insta_completa.csv")
data_facebook_completa <- read.csv("data_facebook_completa.csv")

palabras_a_borrar <- stopwords("german")  # de tm. funciona mejor

# insta , creo y guardo

df_tokens_insta <-  data_insta_completa %>% 
  unnest_tokens(output = tokens, input = texto)  %>%  
  filter(!(tokens %in% palabras_a_borrar))

#df_tokens_insta %>%  write.csv("tokens_instagram.csv")

# fb 

df_tokens_fb <-  data_facebook_completa %>% 
  unnest_tokens(output = tokens, input = texto)  %>%  
  filter(!(tokens %in% palabras_a_borrar))

#df_tokens_fb %>%  write.csv("tokens_facebook.csv")

# # BORRADORES ########
# # otros intentos de ID descartados
# 
# 
# # prueba con fuzzy join
# data_insta_completa <- data_insta %>% 
#   fuzzy_inner_join(data_manual, match_fun = str_detect)
# 
# 
# 
# # id con texto y likes, probamos
# data_insta <- data_insta %>% 
#   mutate(POSTID = paste(str_sub(texto, 25, end = 36), 
#                         LIKES))
# 
# data_manual <- data_manual %>% 
#   mutate(POSTID = paste(str_sub(TEXTO, 61, end = 72), 
#                         LIKES))
# 
# # testeamos y parece que funciono 
# test <- data_descarga$POSTID %>% duplicated() 
# sum(test)
# 
# test <- data_descarga %>% 
#   subset(duplicated(POSTID))
# 
# # unimos
# 
# data_insta_completa <- data_insta %>% 
#   left_join(data_manual)
# 
# 
# 
# # id con texto y fecha..las fechas no estn iguales hay que corregir
# # id con texto solo no funca porque hay duplicados
# #data_insta <- data_insta %>% 
# #  mutate(POSTID = str_sub(texto, 5, end = 16))
# 
# #data_manual <- data_manual %>% 
# #  mutate(POSTID = str_sub(TEXTO, 41, end = 52))
# 
# #test <- data_manual$POSTID %>% duplicated() 
# #sum(test)

# data_facebook_completa2 <- data_fb %>% 
#   rename("POSTID" = TEXTO) %>%
#      fuzzy_left_join(metadata, match_fun = str_detect)