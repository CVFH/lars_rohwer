library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(lubridate)
library(wordcloud2)
library(tidytext) 
library(stopwords)
library(tm)



# ANALISIS FACEBOOK ########## 

data_facebook_agregada <- readxl::read_xlsx("datielars.xlsx")
data_facebook_completa <- read.csv("data_facebook_completa.csv")

# preparo datos de fecha ###########

format.str <- "%Y-%m-%dT%H:%M:%S"
data_facebook_agregada <- data_facebook_agregada %>% 
  mutate(fecha_posix = as.POSIXct(Fecha, format.str, tz = "GMT"))

# algunas correcciones de textos. #########

data_facebook_completa$FOTO %>% unique()
data_facebook_completa$TEMA1 %>% unique()
data_facebook_completa$Grund %>% unique()
 
data_facebook_completa <- data_facebook_completa %>% 
  mutate(Grund = ifelse(Grund=="böse", "Diskussion", Grund)) %>% 
  mutate(across(where(is.character), ~str_to_title(.)))

########## ACHTUNG POR AHORA NO CAMBIE NADA SOLO COPIE Y PEGUE DE INSTA
# Y CAMBIE INSTA POR FB. HAY QUE CHIQUIAR
# Preparo data long para considerar todos los casos de reaccion en simultaneo. ######

data_facebook_long <- data_facebook_completa %>% 
  pivot_longer(cols = c(LIKES, comments, cliks, compartida), names_to = "reaccion", values_to = "n_reaccion") %>% 
  mutate(across(where(is.character), ~str_to_title(.))) %>% 
  mutate(pro_spon = ifelse(pro_spon=="Profesional", "Professionell", pro_spon))


data_facebook_engagement <- data_facebook_completa %>% 
  mutate(engagement = LIKES + comments + compartida,
         engagement_plus = engagement + cliks,
         engagement_plusplus = engagement_plus + Alcance) %>% 
  mutate(across(where(is.character), ~str_to_title(.)))

# DESCRIPTIVO UNIVARIADO: LIKES ###########

plot_fotos <- data_facebook_long %>% 
  group_by(img_word, reaccion) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  subset(!is.na(img_word))  %>% 
  ggplot(aes(img_word %>% fct_relevel(levels = c("bild", "worter", "inzwischen", "text")),
             n_, 
             fill= reaccion)) +
  geom_col() +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "", y = "", fill = "",
       title = "Durchschnittliche Reactions-per-Bildarten: ",
       subtitle = "Wörter vs. Bilderrn",
       caption = "Quelle: Carolina")

ggsave("images/fb_fotos.png", bg = "white")

plot_pro_spon <- data_facebook_long %>% 
  group_by(pro_spon, reaccion) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  subset(!is.na(pro_spon))  %>% 
  ggplot(aes(pro_spon %>% fct_relevel("Gebastelt", "Spontan"),
             n_, 
             fill= reaccion)) +
  geom_col() +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "", y = "", fill = "",
       title = "Durchschnittliche Reactions-per-Bildarten: ",
       subtitle = "Spontan vs. Gebastelt",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_prospon.png", bg = "white")

order <- data_facebook_long %>% 
  group_by(FOTO) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  arrange(desc(n_)) %>% 
  select(-n_)


plot_FOTO <- data_facebook_long %>% 
  group_by(FOTO, reaccion) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  subset(!is.na(FOTO))  %>% 
  ggplot(aes(FOTO %>% fct_relevel(order$FOTO),
             n_, 
             fill= reaccion)) +
  geom_col() +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "", y = "", fill = "",
       title = "Durchschnittliche Reactions-per-Bildarten: ",
       subtitle = "Bilder Inhalt",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_FOTO.png", bg = "white")

order <- data_facebook_long %>% 
  group_by(TEMA1) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  arrange(desc(n_)) %>% 
  select(-n_)


plot_TEMA1 <- data_facebook_long %>% 
  group_by(TEMA1, reaccion) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  subset(!is.na(TEMA1))  %>% 
  ggplot(aes(TEMA1 %>% fct_relevel(order$TEMA1),
             n_, 
             fill= reaccion)) +
  geom_col() +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,
        text = element_text(size = 16)) + 
  labs(x = "", y = "", fill = "",
       title = "Durchschnittliche Reactions-per-Post: ",
       subtitle = "Themen",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_THEMA.png", bg = "white")

order <- data_facebook_long %>% 
  group_by(Grund) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  arrange(desc(n_)) %>% 
  select(-n_)

plot_Grund <- data_facebook_long %>% 
  group_by(Grund, reaccion) %>% 
  summarise(n_ = sum(n_reaccion, na.rm=T)) %>% 
  subset(!is.na(Grund))  %>% 
  ggplot(aes(Grund %>% fct_relevel(order$Grund),
             n_, 
             fill= reaccion)) +
  geom_col() +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "", y = "", fill = "",
       title = "Durchschnittliche Reactions-per-'Grund'",
       subtitle = "Grund / Gefühl",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_GRUND.png", bg = "white")

# DESCRIPTIVO BIVARIADO LIKES VS N ######

facet_plot_fotos_n <- data_facebook_long %>% 
  group_by(img_word, reaccion) %>% 
  summarise(n_reaccion = sum(n_reaccion, na.rm=T),
            N = n()/4) %>% 
  subset(!is.na(img_word))  %>% 
  ggplot(aes( y = n_reaccion, x = N, colour= img_word, label = img_word)) +
  geom_label(size=6) +
  facet_wrap( ~ reaccion) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +    scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 250)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,
        text = element_text(size = 16)) + 
  labs(x = "n Shared Posts", y = "n Reactions (Durchschnitt)",
       title = "Durchschnittliche Reactions-per-Bildarten: ",
       subtitle = "Wörter vs. Bilderrn",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_facet_fotos_n.png", bg = "white")

plot_fotos_n <- data_facebook_long %>% 
  group_by(img_word) %>% 
  summarise(n_reaccion = sum(n_reaccion, na.rm=T),
            N = n()/4) %>% 
  subset(!is.na(img_word))  %>% 
  ggplot(aes( y = n_reaccion, x = N, colour= img_word, label = img_word)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 250)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "n Reactions (Durchschnitt)",
       title = "Durchschnittliche Reactions-per-Bildarten: ",
       subtitle = "Wörter vs. Bilderrn",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_fotos_n.png", bg = "white")

plot_pro_n <- data_facebook_long %>% 
  group_by(pro_spon) %>% 
  summarise(n_reaccion = sum(n_reaccion, na.rm=T),
            N = n()/4) %>% 
  subset(!is.na(pro_spon))  %>% 
  ggplot(aes( y = n_reaccion, x = N, colour= pro_spon, label = pro_spon)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 250)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "n Reactions (Durchschnitt)",
       title = "Durchschnittliche Reactions-per-Bildarten: ",
       subtitle = "Spontan vs. Professionell",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_prospon_n.png", bg = "white")

plot_FOTO_n <- data_facebook_long %>% 
  group_by(FOTO) %>% 
  summarise(n_reaccion = sum(n_reaccion, na.rm=T),
            N = n()/4) %>% 
  subset(!is.na(FOTO))  %>% 
  ggplot(aes( y = n_reaccion, x = N, colour= FOTO, label = FOTO)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +    scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 250)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "n Shared Posts", y = "n Reactions (Durchschnitt)",
       title = "Durchschnittliche Reactions-per-Bildarten: ",
       subtitle = "Bilder Inhalt",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_FOTO_n.png", bg = "white")

plot_THEMA_n <- data_facebook_long %>% 
  group_by(TEMA1) %>% 
  summarise(n_reaccion = sum(n_reaccion, na.rm=T),
            N = n()/4) %>% 
  subset(!is.na(TEMA1))  %>% 
  ggplot(aes( y = n_reaccion, x = N, colour= TEMA1, label = TEMA1)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 250)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16)) + 
  labs(x = "n Shared Posts", y = "n Reactions (Durchschnitt)",
       title = "Durchschnittliche Reactions-per-Post: ",
       subtitle = "Themen",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_THEMA_n.png", bg = "white")

plot_Grund_n <- data_facebook_long %>% 
  group_by(Grund) %>% 
  summarise(n_reaccion = sum(n_reaccion, na.rm=T),
            N = n()/4) %>% 
  subset(!is.na(Grund))  %>% 
  ggplot(aes( y = n_reaccion, x = N, colour= Grund, label = Grund)) +
  geom_label(size=6) +
  theme_minimal() +
  scale_x_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 250)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ,
        text = element_text(size = 16)) + 
  labs(x = "n Shared Posts", y = "n Reactions (Durchschnitt)",
       title = "Durchschnittliche Reactions-per-'Grund'",
       subtitle = "Grund / Gefühl",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_GRUND_n.png", bg = "white")

# distribuciones desvio estandar ###########


plot_TEMA1box <- data_facebook_completa %>% 
  subset(!is.na(TEMA1))  %>% 
  ggplot(aes(TEMA1, LIKES, fill= TEMA1)) +
  geom_boxplot() +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16) ) + 
  labs(x = "", y = "",
       title = "Abweichung Likes-per-Thema ",
       subtitle = "",
       caption = "Quelle: Carolina")

ggsave("images/fb_plot_THEMA_bxplot.png", bg = "white")

##################
# tiempo #########

evdata_facebook_completa  <-  data_facebook_completa  %>% 
  mutate(date = as.Date(datahoraposi)) %>%
  mutate(week = lubridate::week(date),
         year = lubridate::year(date)) %>% 
  group_by(year, week) %>% 
  summarise(posts_week=n(),
            likes = sum(LIKES, na.rm=TRUE),
            comments = sum(comments, na.rm=TRUE),
            compartida = sum(compartida, na.rm=TRUE),
            cliks = sum(cliks, na.rm=TRUE),
            Alcance = sum(Alcance, na.rm=TRUE)) %>% 
  mutate(date =  strptime(paste(year, week, 1), format = "%Y %W %u")) %>%  mutate(date = as.Date(date))

evdata_facebook_agregada  <-  data_facebook_agregada  %>% 
  mutate(date = as.Date(fecha_posix)) %>%
  mutate(week = lubridate::week(date),
         year = lubridate::year(date)) %>% 
  group_by(year, week) %>% 
  summarise(cuenta_week=n(),
            Nuevos_mg_fb = sum(as.numeric(Nuevos_mg_fb, na.rm=T)),
            Visitas = sum(Visitas, na.rm=T),
            Alcance_pg = sum(Alcance, na.rm=T)) %>% 
  mutate(date =  strptime(paste(year, week, 1), format = "%Y %W %u")) %>%  mutate(date = as.Date(date))

evdata_facebook <- evdata_facebook_agregada %>% 
  left_join(evdata_facebook_completa)

evdata_facebook_long <- evdata_facebook %>% 
  pivot_longer(
    cols = c(comments, likes, compartida, cliks, Alcance, Nuevos_mg_fb, Visitas, Alcance_pg),
    names_to = "Reaction",
    values_to = "n_Reaction"
  ) %>% 
  mutate(average_Reaction = round(n_Reaction/posts_week,3))


plot_evdata_facebook_long  <- evdata_facebook_long  %>% 
  ggplot() +
  geom_line(aes(date, n_Reaction, colour = Reaction)) +
  geom_smooth(aes(date, n_Reaction, colour = Reaction), se=F) +
  theme_minimal() +
  labs(title= "Erfolg per Datum",
       #subtitle = "xxxx",
       caption = "Quelle: Carolina") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggsave("images/fb_plot_ev1.png", bg = "white")

plotaverage_evdata_facebook_long  <- evdata_facebook_long  %>% 
  ggplot() +
  geom_line(aes(date, average_Reaction, colour = Reaction)) +
  geom_smooth(aes(date, average_Reaction, colour = Reaction), se=F) +
  theme_minimal() +
  labs(title= "Durchschnittliche Erfolg per Datum",
       #subtitle = "xxxx",
       caption = "Quelle: Carolina") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggsave("images/fb_plot_ev2.png", bg = "white")

plot2average_evdata_facebook_long  <- evdata_facebook_long  %>% 
  subset(!Reaction=="Alcance") %>% 
  subset(!Reaction=="Alcance_pg" ) %>% 
  ggplot() +
  geom_line(aes(date, average_Reaction, colour = Reaction)) +
  geom_smooth(aes(date, average_Reaction, colour = Reaction), se=F) +
  theme_minimal() +
  labs(title= "Durchschnittliche Erfolg per Datum",
       #subtitle = "xxxx",
       caption = "Quelle: Carolina") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 16))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggsave("images/fb_plot_ev3.png", bg = "white")

plot3average_evdata_facebook_long  <- evdata_facebook_long  %>% 
  subset(!Reaction=="Alcance") %>% 
  subset(!Reaction=="Alcance_pg" ) %>% 
  subset(!Reaction=="cliks" ) %>% 
  ggplot() +
  geom_line(aes(date, average_Reaction, colour = Reaction)) +
  geom_smooth(aes(date, average_Reaction, colour = Reaction), se=F) +
  theme_minimal() +
  labs(title= "Durchschnittliche Erfolg per Datum",
       #subtitle = "xxxx",
       caption = "Quelle: Carolina") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 90))  + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggsave("images/fb_plot_ev4.png", bg = "white")

######### most elfogreiche posts ############


data_facebook_post <- data_facebook_long %>% 
  group_by(texto, datahora) %>% 
  summarise(n_control = n(),
            n_reacciones = sum(n_reaccion, na.rm=T)) %>% 
  mutate(n_reacciones = ifelse(n_control==8, 
                               n_reacciones/2, 
                               n_reacciones)) %>% 
  arrange(desc(n_reacciones))

top_posts_facebook <- data_facebook_post %>% 
  head(15)

top_posts_facebook %>% write.csv("top15fb.csv")

leasttop_posts_facebook <- data_facebook_post %>% 
  tail(60)

leasttop_posts_facebook %>% write.csv("leasttop15fb.csv")

#### posts por hora ######

data_facebook_engagement  <- data_facebook_engagement %>% 
  mutate(hora = hms::as_hms(hora)) %>% 
  mutate(horario = hour(hora))

evdata_facebook_time <- data_facebook_engagement %>% 
  group_by(horario) %>% 
  summarise(mean_engagement = mean(engagement, na.rm = T),
            posts = n(),
            suma_engagement = sum(engagement, na.rm = T))

evdata_facebook_time %>% write.csv("evdata_facebook_time.csv")

plot_evdata_facebook_time <- evdata_facebook_time %>% 
  ggplot() +
 # geom_line(aes(horario, suma_engagement), colour = "blue") +
  geom_line(aes(horario, mean_engagement), colour = "red") +
  geom_line(aes(horario, posts), colour = "grey") +
  theme_minimal()

ggsave("images/fb_evdata_facebook_time.png", bg = "white")

### algunas cuentas con engagement. hay correlaciones? ##########


df_fbfiltered <- data_facebook_engagement %>% 
  select(c(
    engagement,
    engagement_plus, 
    engagement_plusplus, 
  ))

cortest <- cor(df_fbfiltered, method = "pearson", use = "complete.obs") 

