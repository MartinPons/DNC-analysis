 # 05-EDA_biden.R


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


key <- data.frame(x = c("bidendropout", "#neverbiden", "#replacebiden", "#joemustgo", "#dropoutjoe", "#timesupbiden", "#teamjoe", "gojoe", "#ibelievebiden", 
                            "#ridenwithbiden", "#ibelievejoe", "#ridinwithbiden"),
                            y = as.numeric(c(-0.5, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1)), 
                            stringsAsFactors = F)

mykey <- as_key(key)

mykey[c("bidendropout", "#replacebiden")][[2]]


biden_lexicon <- update_key(mykey, x = hash_sentiment_jockers_rinker, sentiment = F)


twitter_test <- c("is	bidendropout", "hi", "this is good", "martin es guay")

sentiment_by(twitter_test, polarity_df = biden_lexicon)

# CARGA DE DATOS  Y FORMATO  ----------------------------------------------------------

# carga y formato basico
biden1 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "biden_no_sent_part1.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

biden2 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "biden_no_sent_part2.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

biden <- bind_rows(biden1, biden2) %>% 
  distinct(status_id, .keep_all = T)

rm(biden1, biden2)
gc()

# DISTRIBUCION DE TWEETS --------------------------------------------------
biden %>% 
  count(comunidad) %>% 
  mutate(f = percent(n/sum(n), accuracy = 0.1))

freq_tweets_usuario <- get_tweet_distribution(biden) 

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
biden <- biden %>% 
  filter(comunidad != "Independent")

# adicion de sentimientos
sent_biden <- sentiment(biden$text)

biden <- biden %>% 
  mutate(ave_sentiment = sent_biden$sentiment)

# datos tidy
biden_unigram <- get_unigrams(biden, c("#biden", "biden", "#joe", "joe"))
biden_hashtag <- get_hashtags(biden_unigram)
biden_frequency <- get_frequencies(biden_hashtag) 



## Topics con networks ##

gop_biden_net <- create_hashtag_graph(biden_hashtag, "GOP", c("#biden", "#joe", "#joebiden", "joebiden"), minim_n = 3, nodos_n = 20)
dnc_biden_net <- create_hashtag_graph(biden_hashtag, "DNC",  c("#biden", "#joe", "#joebiden", "joebiden"), minim_n = 3, nodos_n = 20)
prg_biden_net <- create_hashtag_graph(biden_hashtag, "Progressives",  c("#biden", "#joe", "#joebiden", "joebiden"), minim_n = 3, nodos_n = 20)

biden_graph <- bind_graphs(gop_biden_net, dnc_biden_net, prg_biden_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives"))) %>% 
  activate(edges) %>% 
  filter(strength > 3) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree())

# El GOP TIene hashtags relacionados con home schooling, los democratas co bidcacion a distanica y los progerssives, los progressives feedly: una herramienta de agregación de noticias.
# además de hastgags reivindicativos como #taxtherich# makebillionarespay# pr
plot_topic_networks(biden_frequency, biden_graph, filtered_words = c("#biden", "#joe"),
                    texto_titulo = "Hashtags más relevantes", lab_size = 10, text_size = 4, layout_style = "nicely", minim_n = 3) 

# habria que mirar de aumentar el lexico custom en este caso con hashtags tan claros en favor y en contra de biden


biden_hashtag %>% 
  filter(n() > 20) %>% 
  pairwise_cor(word, comunidad, sort = T)



## correlacion de palabras ##

gop_cor_net <- get_correlation_network(biden_hashtag, biden_frequency, com = "GOP", exclude_words =  c("#biden", "#joe", "#joebiden", "joebiden"), cor_limit = 0.35, top_words = 10) %>% 
  activate(nodes) %>% mutate(comunidad = "GOP")
dnc_cor_net <- get_correlation_network(biden_hashtag, biden_frequency, com = "DNC", exclude_words =  c("#biden", "#joe", "#joebiden", "joebiden"), cor_limit = 0.35, top_words = 10) %>% 
  activate(nodes) %>% mutate(comunidad = "DNC")
prg_cor_net <- get_correlation_network(biden_hashtag, biden_frequency, com = "Progressives", exclude_words =  c("#biden", "#joe", "#joebiden", "joebiden"), cor_limit = 0.35, top_words = 10) %>% 
  activate(nodes) %>% mutate(comunidad = "Progressives")

biden_net <- bind_graphs(gop_cor_net, dnc_cor_net, prg_cor_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))

biden_net %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(pch = 21, aes(fill = sent, size = n)) +
  geom_node_text( aes(label = name, color = comunidad), repel = TRUE, size = 3) +
  scale_fill_gradient2(low = "red", high = "green", mid = "grey") +
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
  facet_nodes(~comunidad, ncol = 1, scales = "free") +
  theme_graph() + 
  guides(color = F) + 
  theme(strip.text = element_text(size = 16))




# BAG OF WORDS - TFIDF ------------------------------------------------------------

# hashtags
biden_tfidf_hashtags <- get_tfidf(biden_frequency %>% filter(comunidad!= "Independent"))

plot_tfidf(biden_tfidf_hashtags, cols = 3, colores = c(1, 3, 4)) + 
  labs(title = "Tópicos característicos de cada comunidad", y = "TF-IDF") +
  guides(fill = F) +
  theme(axis.title.x= element_blank(),
        axis.text.y = element_text(size = 16),
        legend.title = element_blank(),
        strip.text = element_text(size = 15))


# mentions

#
biden_tfidf_mentions <- get_tfidf(biden_unigram %>% filter(str_detect(word, "^@"),comunidad!= "Independent") %>% get_frequencies())

plot_tfidf(biden_tfidf_mentions, cols = 3, colores = c(1, 3, 4)) + 
  labs(title = "Tópicos característicos de cada comunidad", y = "TF-IDF") +
  guides(fill = F) +
  theme(axis.title.x= element_blank(),
        axis.text.y = element_text(size = 16),
        legend.title = element_blank(),
        strip.text = element_text(size = 15))




# SENTIMIENTO POLARITY ----------------------------------------------------

# limpeza de datos: se elimina trump y sanders
biden_for_sent <- biden %>% 
  filter(str_detect(text, "#?([Bb]ernie|[Ss]anders|[Dd]onald|[Tt]rump)", negate = T))

biden_for_sent %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment))

biden_for_sent %>% 
  ggplot(aes(x = comunidad, y = ave_sentiment)) + 
  geom_boxplot(aes(fill = comunidad)) + 
  scale_fill_manual(values = plots_palette[c(1, 3, 4)])


# DNC - PRG
test_sent_dnc_prg <- get_test_dif(biden_for_sent, group = c("DNC", "Progressives"))
test_sent_dnc_prg[-1]

# DNC - GOP
test_sent_dnc_gop <- get_test_dif(biden_for_sent, group = c("DNC", "GOP"))
test_sent_dnc_gop[-1]

sent_test_data <- bind_rows(test_sent_dnc_prg$data, test_sent_dnc_gop$data)

test_sent_dnc_prg$data %>% 
  ggplot(aes(x = sentimiento)) + 
  geom_histogram(aes(fill = comunidad), color = "white", binwidth = 0.0005) + 
  scale_fill_manual(values = plots_palette[c(3, 4)])


# geografia
biden_for_sent <- biden_for_sent %>% 
  bind_cols(map_df(.$place_full_name, ~get_state_code(.x)))


biden_sent_states <- biden_for_sent %>% 
  filter(comunidad %in% c("DNC", "Progressives")) %>% 
  group_by(comunidad, State) %>% 
  filter(str_detect(State, "[A-Z]{2}")) %>% 
  summarise(sent = mean(ave_sentiment),
            n = n()) %>% 
  filter(n > 3)




state_names <- character(length = length(biden_sent_states$State))

for (idx in 1:length(biden_sent_states$State)) {
  
  state_code <- biden_sent_states$State[idx]
  
  s_name <- state.name[which(state.abb == state_code)]
  
  if (length(s_name) == 0)
  {
    print(state_code)
    state_names[idx] <- NA
    
  } else {

  
  state_names[idx] <- s_name
  }
}


biden_sent_states$state_name <- state_names

biden_sent_states <- biden_sent_states %>% 
  filter(!is.na(state_name)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = comunidad, values_from = sent) %>% 
  mutate(dif_sent = DNC - Progressives)





us_states <- us_states %>% 
  left_join(biden_sent_states, by = c("NAME" = "state_name"))

tm_shape(us_states) + 
  tm_borders(col = "grey60", lwd = 0.5) + 
  tm_fill(col = "dif_sent") + 
  tm_text(text = "State", fontfamily = "Georgia", col = "grey55") + 
  tm_layout(title = "Diferencia de sentimiento entre Demócratas y Progresistas")


# SENIMIENTO NRC ----------------------------------------------------------


nrc_biden <- biden_for_sent %>% get_unigrams() %>% 
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

# el grafico se parece más entre Progresitas y Republicanos que entre demócratas y Republicanos

ggradar(nrc_biden %>% ungroup() %>% mutate(comunidad = fct_drop(comunidad)), group.colours = plots_palette[c(1, 3, 4)], legend.position = "top",
        group.point.size = 4,
        plot.title = "Sentimientos expresados hacia el tópico 'biden Change'", legend.text.size = 11) + 
  theme(plot.title = element_text(size = 12, family = "Georgia", color = "grey55"),
        axis.title = element_text(family = "Georgia"))


# POLARIDAD ---------------------------------------------------------------

rm(list = ls())
gc()


# paleta
plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))

biden_net <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "biden_pol.rds"))
comunidad <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
  mutate(user_id = as.character(user_id))


# filtro comunidades
bid_gop_dnc <- biden_net %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos <- get_inner_nodes(bid_gop_dnc)

# datos polaridad
bid_gop_dnc_pol <- map_df(unique(bid_gop_dnc$from), ~get_node_pol_data(bid_gop_dnc, nodos_internos, .x))

mean(bid_gop_dnc_pol$polarity_score)


# Progressives VS DNC --------------------------------------------------------------

# filtro comunidades
bid_prg_dnc <- biden_net %>%
  filter_communities("DNC", "Progressives")

# nodos internos
nodos_internos <- get_inner_nodes(bid_prg_dnc)

# datos polaridad
bid_prg_dnc_pol <- map_df(unique(bid_prg_dnc$from), ~get_node_pol_data(bid_prg_dnc, nodos_internos, .x))

mean(bid_prg_dnc_pol$polarity_score)

test_data <- bid_gop_dnc_pol %>% 
  mutate(comunidades = "DNC-GOP") %>% 
  bind_rows(bid_prg_dnc_pol %>% mutate(comunidades = "DNC-PRG")) %>% 
  select(nodo, comunidades, polarity_score)

library(boot)
pol_test_dif <- get_test_dif(test_data, var = "polarity_score", comunidades = "comunidades", groups = c("DNC-GOP", "DNC-PRG"))

pol_test_dif[-1]

# Nivel de polarizacion mínimo en ambos casos pero se hallan diferencias significativas
pol_test_dif$data %>% 
  ggplot(aes(x = sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", alpha = 0.5, binwidth = 0.003) + 
  scale_fill_manual(values = c(plots_palette[1], plots_palette[4]))


# grabacion datos test de polaridad
saveRDS(pol_test_dif, here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "biden_polarization.rds"))
