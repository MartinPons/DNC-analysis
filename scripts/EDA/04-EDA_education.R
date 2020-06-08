# 04-EDA_education.R


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

education <- readRDS(here::here("datos_procesados", "formated_text_df_data", "education.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))
         



# DISTRIBUCION DE TWEETS --------------------------------------------------
education %>% 
  count(comunidad) %>% 
  mutate(f = percent(n/sum(n), accuracy = 0.1))

freq_tweets_usuario <- get_tweet_distribution(education) 

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

education <- education %>% 
  filter(comunidad != "Independent")

# datos tidy
education_unigram <- get_unigrams(education, c("#education", "education"))
education_hashtag <- get_hashtags(education_unigram)
education_frequency <- get_frequencies(education_hashtag) 



## Topics con networks ##

gop_education_net <- create_hashtag_graph(education_hashtag, "GOP", c("#education"), minim_n = 3, nodos_n = 20)
dnc_education_net <- create_hashtag_graph(education_hashtag, "DNC", c("#education"), minim_n = 3, nodos_n = 20)
prg_education_net <- create_hashtag_graph(education_hashtag, "Progressives", c("#education"), minim_n = 3, nodos_n = 20)

education_graph <- bind_graphs(gop_education_net, dnc_education_net, prg_education_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives"))) %>% 
  activate(edges) %>% 
  filter(strength > 1) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree())

# El GOP TIene hashtags relacionados con home schooling, los democratas co educacion a distanica y los progerssives, los progressives feedly: una herramienta de agregación de noticias.
# además de hastgags reivindicativos como #taxtherich# makebillionarespay# pr
plot_topic_networks(education_frequency, education_graph, filtered_words = "#education", 
                    texto_titulo = "Hashtags más relevantes", lab_size = 10, layout_style = "nicely", minim_n = 3) 


education_hashtag %>% 
  filter(n() > 20) %>% 
  pairwise_cor(word, comunidad, sort = T)



## correlacion de palabras ##

gop_cor_net <- get_correlation_network(education_hashtag, education_frequency, com = "GOP", exclude_words = "#education", cor_limit = 0.15) %>% 
  activate(nodes) %>% mutate(comunidad = "GOP")
dnc_cor_net <- get_correlation_network(education_hashtag, education_frequency, com = "DNC", exclude_words = "#education", cor_limit = 0.25, top_words = 15) %>% 
  activate(nodes) %>% mutate(comunidad = "DNC")
prg_cor_net <- get_correlation_network(education_hashtag, education_frequency, com = "Progressives", exclude_words = "#education", cor_limit = 0.25, top_words = 15) %>% 
  activate(nodes) %>% mutate(comunidad = "Progressives")

education_net <- bind_graphs(gop_cor_net, dnc_cor_net, prg_cor_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))

education_net %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(pch = 21, aes(fill = sent, size = n)) +
  geom_node_text( aes(label = name, color = comunidad), repel = TRUE, size = 3) +
  facet_nodes(~comunidad, ncol = 1, scales = "free") + 
  scale_fill_gradient2(low = "red", high = "green", mid = "grey") +
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
  facet_nodes(~comunidad, ncol = 1, scales = "free") +
  theme_graph() + 
  guides(color = F) + 
  theme(strip.text = element_text(size = 16))



# BAG OF WORDS - TFIDF ----------------------------------------------------

# hashtags
education_tfidf_hashtags <- get_tfidf(education_frequency %>% filter(comunidad!= "Independent"))

plot_tfidf(education_tfidf_hashtags, cols = 3, colores = c(1, 3, 4)) + 
  labs(title = "Tópicos característicos de cada comunidad", y = "TF-IDF") +
  guides(fill = F) +
  theme(axis.title.x= element_blank(),
        axis.text.y = element_text(size = 16),
        legend.title = element_blank(),
        strip.text = element_text(size = 15))


# mentions

# gop menciona a professorcrunk: una feminista negra https://read.macmillan.com/lp/eloquent-rage/
# denc a stephaniue ruhle: periodista de MSNBC: https://twitter.com/SRuhle
# progressives: aqeny. En realidad es aqe_ny pero hemos quitado el guion!!: https://twitter.com/AQE_NY . aliance for qualiy education: https://www.aqeny.org/ coalición para la educacion publica de alta claidad en NY. Tendría que ser  un hashtag!!
# tb defensa de minorias en progressives
education_tfidf_mentions <- get_tfidf(education_unigram %>% filter(str_detect(word, "^@"),comunidad!= "Independent") %>% get_frequencies())

plot_tfidf(education_tfidf_mentions, cols = 3, colores = c(1, 3, 4)) + 
  labs(title = "Tópicos característicos de cada comunidad", y = "TF-IDF") +
  guides(fill = F) +
  theme(axis.title.x= element_blank(),
        axis.text.y = element_text(size = 16),
        legend.title = element_blank(),
        strip.text = element_text(size = 15))




# RED DE RETWEETS ---------------------------------------------------------
education_pol <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "education_pol.rds"))

education_pol <- education_pol %>% 
  left_join(education %>% 
              select(user_id, screen_name) %>% 
              distinct(.keep_all = T), 
            by = c("from" = "user_id")) %>% 
  left_join(education %>% 
              select(retweet_user_id, retweet_screen_name) %>% 
              distinct(.keep_all = T), 
            by = c("to" = "retweet_user_id"))


nodes <- data.frame(name = unique(c(education_pol$from, education_pol$to)))

ed_rt_graph <- tbl_graph(nodes = nodes, edges = education_pol, directed = T) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>% 
  arrange(desc(degree)) %>% 
  filter(degree > 1)

ggraph(ed_rt_graph, layout = "kk") + 
  geom_edge_link()  +
  geom_edge_node(aes(size = degree)) + 
  facet_graph(~comunidad)



# SENIMIENTO NRC ----------------------------------------------------------


nrc_education <- education_unigram %>% 
  filter(comunidad!= "Independent") %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(comunidad, sentiment, sort = T) %>% 
  mutate(sentiment = str_to_title(sentiment)) %>% 
  ungroup() %>% 
  group_by(comunidad) %>% 
  mutate(n = rescale(n)) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  select(comunidad, Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, Trust)

# predomina miedo
ggradar(nrc_education %>% ungroup() %>% mutate(comunidad = fct_drop(comunidad)), group.colours = plots_palette[c(1, 3, 4)], legend.position = "top",
        group.point.size = 4,
        plot.title = "Sentimientos expresados hacia el tópico 'education Change'", legend.text.size = 11) + 
  theme(plot.title = element_text(size = 12, family = "Georgia", color = "grey55"),
        axis.title = element_text(family = "Georgia"))




# CORRELACION DE TERMINOS -------------------------------------------------

gop_cor_net <- get_correlation_network(education_hashtag, education_frequency, com = "GOP", exclude_words = "#education", cor_limit = 0.2, top_words = 15) %>% 
  activate(nodes) %>% mutate(comunidad = "GOP")
dnc_cor_net <- get_correlation_network(education_hashtag, education_frequency, com = "DNC", exclude_words = "#education", cor_limit = 0.2, top_words = 15) %>% 
  activate(nodes) %>% mutate(comunidad = "DNC")
prg_cor_net <- get_correlation_network(education_hashtag, education_frequency, com = "Progressives", exclude_words = "#education", cor_limit = 0.2, top_words = 15) %>% 
  activate(nodes) %>% mutate(comunidad = "Progressives")

education_net <- bind_graphs(gop_cor_net, dnc_cor_net, prg_cor_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))

education_net %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(pch = 21, aes(fill = sent, size = n)) +
  geom_node_text( aes(label = name, color = comunidad), repel = TRUE, size = 3) +
  facet_nodes(~comunidad, ncol = 1, scales = "free") + 
  scale_fill_gradient2(low = "red", high = "green", mid = "grey") +
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
  facet_nodes(~comunidad, ncol = 1, scales = "free") +
  theme_graph() + 
  guides(color = F) + 
  theme(strip.text = element_text(size = 16))



# POLARIDAD ---------------------------------------------------------------

rm(list = ls())
gc()


# paleta
plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))

education_net <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "education_pol.rds"))
comunidad <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
  mutate(user_id = as.character(user_id))


# filtro comunidades
edu_gop_dnc <- education_net %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos <- get_inner_nodes(edu_gop_dnc)

# datos polaridad
edu_gop_dnc_pol <- map_df(unique(edu_gop_dnc$from), ~get_node_pol_data(edu_gop_dnc, nodos_internos, .x))

mean(edu_gop_dnc_pol$polarity_score)


# Progressives VS DNC --------------------------------------------------------------

# filtro comunidades
edu_prg_dnc <- education_net %>%
  filter_communities("DNC", "Progressives")

# nodos internos
nodos_internos <- get_inner_nodes(edu_prg_dnc)

# datos polaridad
edu_prg_dnc_pol <- map_df(unique(edu_prg_dnc$from), ~get_node_pol_data(edu_prg_dnc, nodos_internos, .x))

mean(edu_prg_dnc_pol$polarity_score)

test_data <- edu_gop_dnc_pol %>% 
  mutate(comunidades = "DNC-GOP") %>% 
  bind_rows(edu_prg_dnc_pol %>% mutate(comunidades = "DNC-PRG")) %>% 
  select(nodo, comunidades, polarity_score)

library(boot)
pol_test_dif <- get_test_dif(test_data, var = "polarity_score", comunidades = "comunidades", groups = c("DNC-GOP", "DNC-PRG"))

pol_test_dif[-1]

# Nivel de polarizacion mínimo en ambos casos pero se hallan diferencias significativas
pol_test_dif$data %>% 
  ggplot(aes(x = sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", alpha = 0.3, binwidth = 0.003, position = "identity") + 
  scale_fill_manual(values = c(plots_palette[1], plots_palette[4]))


saveRDS(pol_test_dif, here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "education_polarization.rds"))
