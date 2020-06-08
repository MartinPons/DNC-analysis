# 02-EDA_covid.R


# CONFIGURACION INICIAL ---------------------------------------------------

# En caso de problemas con qdap mirar como esta variable de entorno para java y cambiar
# si un caso
library(qdapRegex)
library(sentimentr)
library(syuzhet)
library(lubridate)
library(tidyverse)
library(tidytext)
library(forcats)
library(tokenizers)
library(widyr)
library(igraph)
library(ggraph)
library(topicmodels)
library(forcats)
library(scales)
library(stringr)
library(extrafont)
library(reldist)
library(grid)
library(gridExtra)
library(boot)
library(ggradar)
library(tidygraph)

# paleta
plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")

# orden comunidades
comunidad_order <- c("GOP", "Independent", "DNC", "Progressives")


# NECESITO SABER LA DISTRIBUCION DE TWEETS POR MIEMBRO DE UNA COMUNIDAD
# PARA SABER SI HAY UNA SITUACIÓN MUY DESEQUILIBRADA

# detectar emojis
# https://stackoverflow.com/questions/43359066/how-can-i-match-emoji-with-an-r-regex
library(remoji)

library(lexicon)
library(rtweet)
Sys.getenv()
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_251")

library(tm)
library(qdap)


# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))



# CARGA DE DATOS ----------------------------------------------------------
covid1 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "covid_part1.rds")) 
 

covid2 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "covid_part2.rds")) 

covid <- bind_rows(covid1, covid2) %>% 
  dplyr::distinct(status_id, .keep_all = TRUE) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives"))) %>% 
  mutate(text = str_replace_all(text, "(COVID|covid)19_?19(19|[0-9]{1,2})?s?|(COVID|covid)19pandemic|corona(19)?", "COVID19")) %>% 
  mutate(text = str_replace_all(text, "covid19covid19", "covid19"))

rm(covid1, covid2)
gc()

# tengo que tuitar pandemic luego
# DISTRIBUCION DE TWEETS --------------------------------------------------

# tweetws por comunidad
covid %>% 
  count(comunidad) %>% 
  mutate(f = percent(n / sum(n), accuracy = 0.1))


freq_tweets_usuario <- get_tweet_distribution(covid) 

covid %>% 
  count(comunidad, user_id) %>% 
  group_by(comunidad, user_id) %>% 
  summarise(tweets = sum(n))

freq_tweets_usuario %>% 
  arrange(desc(tweets)) %>% 
  mutate(tweets_acum = cumsum(tweets), 
         usuarios_acum = cumsum(usuarios)) %>% 
  mutate(por_tweets_acum = tweets_acum / sum(tweets), 
         por_usuarios_acum = usuarios_acum / sum(usuarios)) %>% 
  mutate(int_users_acum = cut(por_usuarios_acum, breaks = seq(0, 1, by = 0.05))) %>% 
  group_by(comunidad, int_users_acum) %>%  
  summarise(porc_tweets = sum(porc_tweets)) %>% 
  mutate(porc_tweets_acum = cumsum(porc_tweets)) 


# Curva de lorenz
freq_tweets_usuario %>% 
  ggplot(aes(x = porc_usuarios_acum, y = porc_tweets_acum)) + 
  geom_line(aes(color = comunidad), size = 1.3) + 
  scale_color_manual(values = plots_palette) 


# gini
gini_comunidades <- freq_tweets_usuario %>% 
  group_by(comunidad) %>% 
  summarise(gini = gini(x = tweets, weights = porc_usuarios)) %>% 
  arrange(desc(gini))

# FRECUENCIA DE PALABRAS --------------------------------------------------

covid <- covid %>% 
  filter(comunidad != "Independent")

# datos tidy
covid_unigram <- get_unigrams(covid, c("#covid19", "covid19", "pandemic", "#s"))
covid_hashtag <- get_hashtags(covid_unigram)
covid_frequency <- get_frequencies(covid_hashtag)

## plotting ##
# plot_topics(covid_unigram %>% get_frequencies(), c("#covid"))
# plot_topics(covid_frequency , "#covid19", cols = 1, colores = c(1, 3, 4))

# #fbr: hashtag anti trump https://www.urbandictionary.com/define.php?term=FBR
# # smartnews: aplicacion de curacion de contenidos, noticias. https://about.smartnews.com/en/, https://techcrunch.com/2019/08/04/news-discovery-app-smartnews-valued-at-1-1b/


## Topics con networks ##

gop_covid_net <- create_hashtag_graph(covid_hashtag, "GOP", c("#covid19", "#s"), minim_n = 5, nodos_n = 10)
dnc_covid_net <- create_hashtag_graph(covid_hashtag, "DNC", c("#covid19", "#s"), minim_n = 5, nodos_n = 10)
prg_covid_net <- create_hashtag_graph(covid_hashtag, "Progressives", c("#covid19", "#s"), minim_n = 5, nodos_n = 10)

covid_graph <- bind_graphs(gop_covid_net, dnc_covid_net, prg_covid_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))


# no hay topicos clarows
plot_topic_networks(covid_frequency, covid_graph, filtered_words = "#covid19", texto_titulo = "Hashtags más relevantes", lab_size = 10, layout_style = "nicely")

# GOp:

# wwg1wga: donde va uno vamos todos. Relacionado con la teoria conspirativa qanon 
# https://www.npr.org/2018/08/02/634749387/what-is-qanon-the-conspiracy-theory-tiptoeing-into-trump-world?t=1590769164728

# KAP: keep america great: slogan de la campaña de reeleccion de Trump

# greatawakening: tb parece estar relacionado con teorías conspirativas qanon


# Explorar: 
# - sentimiento hacia MAGA y KAP (benchmarking??)
# Sentimiento smartnews
# sentimiento



# BAG OF WORDS: TFIDF -----------------------------------------------------

covid_tfidf_hashtags <- get_tfidf(covid_frequency %>% filter(comunidad!= "Independent"))

plot_tfidf(covid_tfidf_hashtags, cols = 3, colores = c(1, 3, 4)) + 
  labs(title = "Tópicos característicos de cada comunidad", y = "TF-IDF") + 
  theme(axis.title.x= element_blank(),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank())

# HCQ es hidroxychloroquine


## Vote Blue No Matter Who ##

vote_blue <- covid %>% 
  filter(comunidad == "Progressives", str_detect(text, "#votebluenomatterwho"))

vote_blue_unigrams <- get_unigrams(vote_blue, c("#covid19", "covid19", "pandemic", "#s"))


vote_blue_frequencies <- get_frequencies(vote_blue_unigrams)

plot_topics(vote_blue_frequencies, filtered_words = "#votebluenomatterwho", cols = 1, colores = 4)


# --------- EXPLORACION SMARTNEWS ------------------ #

smart <- covid %>% filter(str_detect(text, "#smartnews"))

smart %>% 
  count(comunidad) %>% 
  mutate(f = percent(n / sum(n), accuracy = 0.1))

smart_hashtag <- smart %>% 
  get_unigrams(c("#covid19", "covid19", "pandemic", "#s")) %>%
  get_hashtags() %>% 
  get_frequencies()

# no está relacionada con otros hashtags
plot_topics(smart_hashtag, cols = 1, filtered_words = "#covid19")


## sentimiento smartnews ##

smart %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment), 
            sd = sd(ave_sentiment),
            n = n())


smart %>%
  filter(comunidad != "Independent") %>% 
  ggplot(aes(x = comunidad, y = ave_sentiment)) +
  geom_boxplot(aes(fill = comunidad)) + 
  theme_bw() + 
  scale_fill_manual(values = plots_palette[c(1, 3, 4)])

# no se halla diferencia significativa
aov(ave_sentiment ~ comunidad, data = filter(smart, comunidad != "Independent")) %>% summary()

# diferentes varianzas
oneway.test(ave_sentiment ~ comunidad, data = filter(smart, comunidad != "Independent"), var.equal = F)

library(car)
model <- lm(ave_sentiment ~comunidad, data = filter(smart, comunidad != "Independent"))

Anova(model, Type = "II", white.adjust = T)

# no hay diferencias entre grupos


# AQUÍ TODO EL SENTIMIENTO ESTARÁ CONTAMINADO POR LA NEGATIVIDAD INTRINSECA DEL COVID!!!





# POLARIZACION ------------------------------------------------------------

rm(list = ls())
gc()


source(here::here("scripts", "funciones_auxiliares.R"))

# emparejamiento comunidades
gop_dnc <- c("GOP", "DNC")
prog_dnc <- c("Progressives", "DNC")


cov_net <- lee_pol_rds("covid")
comunidad <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
  mutate(user_id = as.character(user_id))

cov_net <- cov_net %>% 
  network_data(.e = "retweet") %>% 
  left_join(comunidad, by = c("from" = "user_id")) %>% 
  left_join(comunidad, by = c("to" = "user_id"), suffix = c("_from", "_to")) %>% 
  mutate(comunidad_from = ifelse(is.na(comunidad_from), "Independent", comunidad_from),
         comunidad_to = ifelse(is.na(comunidad_to), "Independent", comunidad_to)) %>% 
  count(from, to, comunidad_from, comunidad_to) %>% 
  mutate(connection_type = ifelse(comunidad_from == comunidad_to, "conexion_interna", "conexion_frontera"))


# GOP VS DNC --------------------------------------------------------------

# filtro comunidades
cov_gop_dnc <- cov_net %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos <- get_inner_nodes(cov_gop_dnc)

# datos polaridad
cov_gop_dnc_pol <- map_df(unique(cov_gop_dnc$from), ~get_node_pol_data(cov_gop_dnc, nodos_internos, .x))

mean(cov_gop_dnc_pol$polarity_score)

  
# Progressives VS DNC --------------------------------------------------------------

# filtro comunidades
cov_prg_dnc <- cov_net %>%
  filter_communities("Progressives", "DNC")

# nodos internos
nodos_internos <- get_inner_nodes(cov_prg_dnc)

# datos polaridad
cov_prg_dnc_pol <- map_df(unique(cov_prg_dnc$from), ~get_node_pol_data(cov_prg_dnc, nodos_internos, .x))

mean(cov_prg_dnc_pol$polarity_score)

test_data <- cov_gop_dnc_pol %>% 
  mutate(comunidades = "DNC-GOP") %>% 
  bind_rows(cov_prg_dnc_pol %>% mutate(comunidades = "DNC-PRG")) %>% 
  select(nodo, comunidades, polarity_score)

library(boot)
pol_test_dif <- get_test_dif(test_data, var = "polarity_score", comunidades = "comunidades", groups = c("DNC-GOP", "DNC-PRG"))


pol_test_dif$data %>% 
  ggplot(aes(x = sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", alpha = 0.5, binwidth = 0.002) + 
  scale_fill_manual(values = plots_palette[c(1, 4)])
  
saveRDS(pol_test_dif, here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "covid_polarization.rds"))


