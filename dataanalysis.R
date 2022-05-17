# ANALISIS INSTA ########## 

data_insta_completa <- read.csv("data_insta_completa.csv")





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
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "", y = "",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Wörter vs. Bilderrn",
       caption = "Quelle: Carolina")

plot_pro <- data_insta_completa %>% 
  group_by(pro_spon) %>% 
  summarise(LIKES = mean(LIKES))  %>% 
  subset(!is.na(pro_spon))  %>% 
  mutate(pro_spon = fct_reorder(pro_spon, desc(LIKES)))  %>%
  ggplot(aes(pro_spon, LIKES, fill= pro_spon)) +
  geom_col()  +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "", y = "",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Spontan vs. Gebastelt",
       caption = "Quelle: Carolina")

plot_FOTO <- data_insta_completa %>% 
  group_by(FOTO) %>% 
  summarise(LIKES = mean(LIKES))  %>% 
  subset(!is.na(FOTO))  %>% 
  mutate(FOTO = fct_reorder(FOTO, desc(LIKES)))  %>%
  ggplot(aes(FOTO, LIKES, fill= FOTO)) +
  geom_col()  +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "", y = "",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Bilder Inhalt",
       caption = "Quelle: Carolina")

plot_TEMA1 <- data_insta_completa %>% 
  group_by(TEMA1) %>% 
  summarise(LIKES = mean(LIKES)) %>% 
  subset(!is.na(TEMA1))  %>% 
  mutate(TEMA1 = fct_reorder(TEMA1, desc(LIKES)))  %>%
  ggplot(aes(TEMA1, LIKES, fill= TEMA1)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "", y = "",
       title = "Durchschnittlicher Likes-per-Thema ",
       subtitle = "",
       caption = "Quelle: Carolina")


# DESCRIPTIVO BIVARIADO LIKES VS N ######

plot_fotos_n <- data_insta_completa %>% 
  group_by(img_word) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(img_word))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= img_word, label = img_word)) +
  geom_label() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "n Posts", y = "Likes (durchschnitt)",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Wörter vs. Bilderrn",
       caption = "Quelle: Carolina")


plot_pro_n <- data_insta_completa %>% 
  group_by(pro_spon) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(pro_spon))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= pro_spon, label = pro_spon)) +
  geom_label() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "n Posts", y = "Likes (durchschnitt)",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Spontan vs. Profesionell",
       caption = "Quelle: Carolina")

plot_FOTO_n <- data_insta_completa %>% 
  group_by(FOTO) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(FOTO))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= FOTO, label = FOTO)) +
  geom_label() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "n Posts", y = "Likes (durchschnitt)",
       title = "Durchschnittlicher Likes-per-Bildarten: ",
       subtitle = "Inhalt des Bildern",
       caption = "Quelle: Carolina")

plot_THEMA_n <- data_insta_completa %>% 
  group_by(TEMA1) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(TEMA1))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= TEMA1, label = TEMA1)) +
  geom_label() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "n Posts", y = "Likes (durchschnitt)",
       title = "Durchschnittlicher Likes-per-Thema: ",
       subtitle = "",
       caption = "Quelle: Carolina")

plot_Grund_n <- data_insta_completa %>% 
  group_by(Grund) %>% 
  summarise(LIKES = mean(LIKES),
            N = n()) %>% 
  subset(!is.na(Grund))  %>% 
  #mutate(img_word = fct_reorder(img_word, desc(LIKES)))  %>%
  ggplot(aes(N, LIKES, colour= Grund, label = Grund)) +
  geom_label() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "n Posts", y = "Likes (durchschnitt)",
       title = "Durchschnittlicher Likes-per-Grund ",
       subtitle = "",
       caption = "Quelle: Carolina")
# distribuciones desvio estandar ###########


plot_TEMA1box <- data_insta_completa %>% 
  subset(!is.na(TEMA1))  %>% 
  ggplot(aes(TEMA1, LIKES, fill= TEMA1)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) + 
  labs(x = "", y = "",
       title = "Abweichung Likes-per-Thema ",
       subtitle = "",
       caption = "Quelle: Carolina")

##################
# tiempo #########

evdata_insta_completa  <-  data_insta_completa  %>% 
  mutate(date = as.Date(datahoraposi)) %>%
  mutate(week = lubridate::week(date),
         year = lubridate::year(date)) %>% 
  group_by(year, week) %>% 
  summarise(posts=n(),
            likes = sum(LIKES)) %>% 
  mutate(date =  strptime(paste(year, week, 1), format = "%Y %W %u")) %>%  mutate(date = as.Date(date))

plot_evdata_insta_completa <- evdata_insta_completa %>% 
  ggplot() +
  geom_col(aes(date, likes/posts), fill = "lightgreen", colour = "black") +
  geom_smooth(aes(date, likes/posts), se=F) +
  theme_minimal() +
  labs(title= "Cantidad de likes promedio recibidos por fecha",
       #subtitle = "xxxx",
       caption = "xxxx") +
  theme(axis.text.x = element_text(angle = 90))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

plot2_evdata_insta_completa <- evdata_insta_completa %>% 
  ggplot() +
  geom_line(aes(date, likes), colour = "blue") +
  geom_smooth(aes(date, likes), colour = "lightblue", se =F) +
  geom_line(aes(date, posts), colour = "red") +
  geom_smooth(aes(date, posts), colour = "pink", se =F) +
  theme_minimal() +
  labs(title= "xxxx",
       #subtitle = "xxxx",
       caption = "xxxx") +
  theme(axis.text.x = element_text(angle = 90))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

plot3_evdata_insta_completa <- evdata_insta_completa %>% 
  ggplot() +
  geom_line(aes(date, likes/posts), colour = "darkgreen") +
  geom_smooth(aes(date, likes/posts), colour = "lightgreen", se =F) +
  geom_line(aes(date, likes), colour = "blue", alpha = 0.7) +
  geom_smooth(aes(date, likes), colour = "lightblue", se =F, alpha = 0.7) +
  geom_line(aes(date, posts), colour = "red", alpha = 0.7) +
  geom_smooth(aes(date, posts), colour = "pink", se =F, alpha = 0.7) +
  theme_minimal() +
  labs(title= "xxxx",
       #subtitle = "xxxx"
       caption = "xxxx") +
  theme(axis.text.x = element_text(angle = 90))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")