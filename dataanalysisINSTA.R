library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(lubridate)
library(wordcloud2)
library(tidytext) 
library(stopwords)
library(tm)




# ANALISIS INSTA ########## 

data_insta_completa <- read.csv("data_insta_completa.csv")

# mini correcciones #####

data_insta_completa <- data_insta_completa %>% 
  mutate(Grund = ifelse(Grund=="böse", "Diskussion", Grund)) %>% 
  mutate(pro_spon = ifelse(pro_spon=="Profesional", "Professionell", pro_spon)) %>% 
  mutate(across(where(is.character), ~str_to_title(.)))

# DESCRIPTIVO UNIVARIADO: LIKES ###########

plot_fotos <- data_insta_completa %>% 
  group_by(img_word) %>% 
  summarise(LIKES = mean(LIKES)) %>% 
  subset(!is.na(img_word))  %>% 
  mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(img_word, LIKES, fill= img_word)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16)) + 
  labs(x = "", y = "",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Wörter vs. Bilderrn",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_fotos.png", bg = "white")

plot_pro <- data_insta_completa %>% 
  group_by(pro_spon) %>% 
  summarise(LIKES = mean(LIKES))  %>% 
  subset(!is.na(pro_spon))  %>% 
  mutate(pro_spon = fct_reorder(pro_spon, desc(LIKES)))  %>%
  ggplot(aes(pro_spon, LIKES, fill= pro_spon)) +
  geom_col()  +
  theme_minimal() +
  theme(panel.border = element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,         
        text = element_text(size = 16) ) + 
  labs(x = "", y = "",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Spontan vs. Gebastelt",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_pro.png", bg = "white")

plot_FOTO <- data_insta_completa %>% 
  group_by(FOTO) %>% 
  summarise(LIKES = mean(LIKES))  %>% 
  subset(!is.na(FOTO))  %>% 
  mutate(FOTO = fct_reorder(FOTO, desc(LIKES)))  %>%
  ggplot(aes(FOTO, LIKES, fill= FOTO)) +
  geom_col()  +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,         
        text = element_text(size = 16) ) + 
  labs(x = "", y = "",
       title = "Durchschnittliche Likes-per-Bildarten: ",
       subtitle = "Bilder Inhalt",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_FOTO.png", bg = "white")

plot_TEMA1 <- data_insta_completa %>% 
  group_by(TEMA1) %>% 
  summarise(LIKES = mean(LIKES)) %>% 
  subset(!is.na(TEMA1))  %>% 
  mutate(TEMA1 = fct_reorder(TEMA1, desc(LIKES)))  %>%
  ggplot(aes(TEMA1, LIKES, fill= TEMA1)) +
  geom_col() +
  theme_minimal() +
  theme(panel.border = element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,         
        text = element_text(size = 16) ) + 
  labs(x = "", y = "",
       title = "Durchschnittliche Likes-per-Thema ",
       subtitle = "",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_TEMA1.png", bg = "white")

# DESCRIPTIVO BIVARIADO LIKES VS N ######

plot_fotos_n <- data_insta_completa %>% 
  group_by(img_word) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(img_word))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= img_word, label = img_word)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) + 
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  theme(panel.border = element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,         
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "Likes (Durchschnitt)",
       title = "Durchschnittliche Likes-per-Bildarten: ",
       subtitle = "Wörter vs. Bilderrn",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_fotos_n.png", bg = "white")

plot_pro_n <- data_insta_completa %>% 
  group_by(pro_spon) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(pro_spon))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= pro_spon, label = pro_spon)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  theme(panel.border = element_blank(),         
        panel.grid.major = element_blank(),         
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,        
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "Likes (Durchschnitt)",
       title = "Durchschnittliche Likes-per-Bildarten: ",
       subtitle = "Spontan vs. Professionell",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_pro_n.png", bg = "white")

plot_FOTO_n <- data_insta_completa %>% 
  group_by(FOTO) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(FOTO))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= FOTO, label = FOTO)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) + 
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  theme(panel.border = element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,         
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "Likes (Durchschnitt)",
       title = "Durchschnittliche Likes-per-Bildarten: ",
       subtitle = "Inhalt des Bildern",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_FOTOS_n.png", bg = "white")

plot_THEMA_n <- data_insta_completa %>% 
  group_by(TEMA1) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(TEMA1))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= TEMA1, label = TEMA1)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) + 
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  theme(panel.border = element_blank(),         
        panel.grid.major = element_blank(),        
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,         
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "Likes (Durchschnitt)",
       title = "Durchschnittliche Likes-per-Thema: ",
       subtitle = "",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_THEMA_n.png", bg = "white")

plot_Grund_n <- data_insta_completa %>% 
  group_by(Grund) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(Grund))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= Grund, label = Grund)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) + 
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  theme(panel.border = element_blank(),        
        panel.grid.major = element_blank(),       
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,        
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "Likes (Durchschnitt)",
       title = "Durchschnittliche Likes-per-Grund ",
       subtitle = "",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_GRUND_n.png", bg = "white")

# distribuciones desvio estandar ###########


plot_TEMA1box <- data_insta_completa %>% 
  subset(!is.na(TEMA1))  %>% 
  ggplot(aes(TEMA1, LIKES, fill= TEMA1)) +
  geom_boxplot() +
  theme_minimal() +
  theme(panel.border = element_blank(),         
        panel.grid.major = element_blank(),         
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,         
        text = element_text(size = 16) ) + 
  labs(x = "", y = "",
       title = "Abweichung Likes-per-Thema ",
       subtitle = "",
       caption = "Quelle: Carolina")

ggsave("images/instagram_plot_THEMA_bxplot.png", bg = "white")

##################
# tiempo #########

evdata_insta_completa  <-  data_insta_completa  %>% 
  mutate(date = as.Date(datahoraposi)) %>%
  mutate(week = lubridate::week(date),
         year = lubridate::year(date)) %>% 
  group_by(year, week) %>% 
  summarise(posts=n(),
            likes = sum(LIKES, na.rm=T)) %>% 
  mutate(date =  strptime(paste(year, week, 1), format = "%Y %W %u")) %>%  mutate(date = as.Date(date))



plot_evdata_insta_completa <- evdata_insta_completa %>% 
  ggplot() +
  geom_col(aes(date, likes/posts), fill = "lightgreen", colour = "black") +
  geom_smooth(aes(date, likes/posts), se=F) +
  theme_minimal() +
  labs(title= "Durchschnittliche Likes per Datum",
       #subtitle = "xxxx",
       caption = "Quelle: Carolina :D") +
  theme(panel.border = element_blank(),         
        panel.grid.major = element_blank(),        
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggsave("images/instagram_plot_ev1.png", bg = "white")

plot2_evdata_insta_completa <- evdata_insta_completa %>% 
  ggplot() +
  geom_line(aes(date, likes), colour = "blue") +
  geom_smooth(aes(date, likes), colour = "lightblue", se =F) +
  geom_line(aes(date, posts), colour = "red") +
  geom_smooth(aes(date, posts), colour = "pink", se =F) +
  theme_minimal() +
  labs(title= "Likes & Posts",
       subtitle = "per Datum, Insgesamt",
       caption = "Quelle: Carolina") +
  theme(panel.border = element_blank(),         
        panel.grid.major = element_blank(),         
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggsave("images/instagram_plot_ev2.png", bg = "white")

plot3_evdata_insta_completa <- evdata_insta_completa %>% 
  ggplot() +
  geom_line(aes(date, likes/posts), colour = "darkgreen") +
  geom_smooth(aes(date, likes/posts), colour = "lightgreen", se =F) +
  geom_line(aes(date, likes), colour = "blue", alpha = 0.7) +
  geom_smooth(aes(date, likes), colour = "lightblue", se =F, alpha = 0.7) +
  geom_line(aes(date, posts), colour = "red", alpha = 0.7) +
  geom_smooth(aes(date, posts), colour = "pink", se =F, alpha = 0.7) +
  theme_minimal() +
  labs(title= "Likes & Posts",
       subtitle = "per Datum",
       caption = "Quelle: Carolina") +
  theme(panel.border = element_blank(),         
        panel.grid.major = element_blank(),         
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16),
        legend.position = "bottom")  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggsave("images/instagram_plot_ev3.png", bg = "white")


### TOP 15 ###############3

top_posts_instagram <- data_insta_completa %>% 
  arrange(desc(LIKES)) %>% 
  head(15)

least_top_post_instagram <- data_insta_completa %>% 
  arrange(LIKES) %>% 
  head(15)

top_posts_instagram %>% write.csv("top15insta.csv")
least_top_post_instagram %>% write.csv("leasttop15insta.csv")


# publico #############

publico_fb <- readxl::read_xlsx("publico_fb.xlsx")


publico_fb_long <- publico_fb %>% 
  pivot_longer(cols=c("Mujeres", "Hombres"), names_to = "Geschlecht", values_to = "Prozentanteil") %>% 
  mutate(Geschlecht = ifelse(Geschlecht=="Mujeres","Frauen", "Herren"),
         Prozentanteil = as.numeric(Prozentanteil)) 


plot_publico_genero_insta <- publico_fb_long %>% 
  subset(Plataforma=="Instagram") %>% 
  ggplot(aes(Edad, Prozentanteil, fill=Geschlecht), colour="black") +
  geom_col(position="dodge") +
  theme_minimal()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,
        text = element_text(size = 16)) + 
  labs(x = "Alter", y = "Prozentanteil",
       title = "Publikum: Instagram",
       #subtitle = "per Thema (Durchschnitt)",
       caption = "Quelle: Facebook Analyitics")

ggsave("images/publikum_insta.png", bg = "white")