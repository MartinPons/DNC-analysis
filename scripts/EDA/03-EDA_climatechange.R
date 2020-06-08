# 03-EDA_climatechange.R

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

climate <- readRDS(here::here("datos_procesados", "formated_text_df_data", "climatechange.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")))



# DISTRIBUCION DE TWEETS --------------------------------------------------
freq_tweets_usuario <- get_tweet_distribution(climate) 

climate %>% 
  count(comunidad) %>% 
  mutate(f = percent(n/sum(n), accuracy = 0.1))


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

climate <- climate %>% 
  filter(comunidad != "Independent")

# datos tidy
climate_unigram <- get_unigrams(climate, c("#climatechange", "#globalwarming"))
climate_hashtag <- get_hashtags(climate_unigram) %>% 
  mutate(word = ifelse(word %in% c("#earthday2020", "#earthday50"), "#earthday", word))
climate_frequency <- get_frequencies(climate_hashtag) 



## Topics con networks ##

gop_climate_net <- create_hashtag_graph(climate_hashtag, "GOP", c("#climatechange", "#s"), minim_n = 5, nodos_n = 20)
dnc_climate_net <- create_hashtag_graph(climate_hashtag, "DNC", c("#climatechange", "#s"), minim_n = 5, nodos_n = 20)
prg_climate_net <- create_hashtag_graph(climate_hashtag, "Progressives", c("#covid19", "#s"), minim_n = 5, nodos_n = 20)

climate_graph <- bind_graphs(gop_climate_net, dnc_climate_net, prg_climate_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives"))) %>% 
  activate(edges) %>% 
  filter(strength > 1, from != to) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree())
  


# no hay topicos claros
plot_topic_networks(climate_frequency, climate_graph, filtered_words = "#climatechange", 
                    texto_titulo = "Hashtags más relevantes", lab_size = 10, layout_style = "nicely")



# A explorar

# top20_hash <- climate_frequency %>%
#   group_by(comunidad) %>%
#   top_n(20, wt = n)
# 
# top_gop <- top20_hash$word[top20_hash$comunidad == "GOP"]
# top_dnc <- top20_hash$word[top20_hash$comunidad == "DNC"]
# top_prg <- top20_hash$word[top20_hash$comunidad == "Progressives"]
# 
# 
# climate_hashtag %>%
#   filter(comunidad == "GOP" & word %in% top_gop|
#          comunidad == "DNC" & word %in% top_dnc|
#          comunidad == "Progressives" & word %in% top_prg) %>%
#   group_by(comunidad, status_id) %>%
#   summarise(hash_group = paste(word, collapse = " "),
#             n_hashtags = length(word)) %>%
#   ungroup() %>%
#   group_by(comunidad, hash_group, n_hashtags) %>%
#   count() %>%
#   View()


# neverbiden entre progresives. ¿Con cuantos hashtags sale neverbiden? ¿Es un eslogan y sale con todos?



# ------------------## greennewdeal ##

# exploracion de graphos
pr <- climate_graph %>%
  activate(edges) %>%
  filter(from == 11 | to == 11) %>%
  activate(nodes)

pr  %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(width = I(sqrt(strength)))) +
  geom_node_point(aes()) +
  geom_node_text(aes(label = nodes), nudge_y = 0.3)



# hay que mirar correlaciones bag of words

# he filtrado fuera temas algunos temas políticos porque hay demasiada ironía
green_sent <- readRDS(here::here("datos_procesados", "formated_sent_df_data", "greennewdeal_sent.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic", negate = T))

# probamos funciones y opciones alternativas para calcular sentimiento
sent2 <- sentiment(green_sent$text, neutral.nonverb.like = T)

green_sent2 <- select(green_sent, status_id, is_retweet, comunidad) %>% 
  bind_cols(sent2)

green_sent2 %>% 
  filter(!is_retweet) %>% 
  group_by(comunidad) %>% 
  summarise(sent = round(mean(sentiment), 7), 
            n = n())

# mismo resultado

# filtrar politicos: tweets ironicos
# llion82 @mickeyalex3 @cdamichael @nightline @aoc @jujuchangabc aoc seriously thinks that cow farts lead to climate change her green new deal is fantasy land good for her for winning her seat but just bc people think her ideas are moronic doesnt mean they are racist misogynist etc
# sentimiento

# ponerlo como ejemplo en conclusiones del paper

green_sent %>% 
  filter(!is_retweet) %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment), 
            n = n())

green_unigram <- green_sent %>% filter(!is_retweet) %>% get_unigrams(filter_regular_expression = "Trump|Biden|Sanders")
green_hashtag <- get_hashtags(green_unigram)
green_frequency <- get_frequencies(green_hashtag)

plot_topics(green_unigram %>% get_frequencies(), filtered_words = c("#greennewdeal", "green", "deal", "new"))


gop_green_net <- create_hashtag_graph(green_hashtag, "GOP", c( "#s"), minim_n = 1, nodos_n = 20)
dnc_green_net <- create_hashtag_graph(green_hashtag, "DNC", c("#s"), minim_n = 1, nodos_n = 20)
prg_green_net <- create_hashtag_graph(green_hashtag, "Progressives", c( "#s"), minim_n = 1, nodos_n = 20)

green_graph <- bind_graphs(gop_green_net, dnc_green_net, prg_green_net) 




## Radar ##

# climatechangte
nrc_climate <- climate_unigram %>% 
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
ggradar(nrc_climate %>% ungroup() %>% mutate(comunidad = fct_drop(comunidad)), group.colours = plots_palette[c(1, 3, 4)], legend.position = "top",
        group.point.size = 4,
        plot.title = "Sentimientos expresados hacia el tópico 'Climate Change'", legend.text.size = 11) + 
  theme(plot.title = element_text(size = 12, family = "Georgia", color = "grey55"),
        axis.title = element_text(family = "Georgia"))


# greennewdeal
nrc_green <- green_unigram %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(comunidad, sentiment, sort = T) %>% 
  mutate(sentiment = str_to_title(sentiment)) %>% 
  ungroup() %>% 
  group_by(comunidad) %>% 
  mutate(n = rescale(n)) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  select(comunidad, Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, Trust)

# predomina miedo: NO ES FIABLE. IRONIA
ggradar(nrc_green %>% ungroup() %>% mutate(comunidad = fct_drop(comunidad)), group.colours = plots_palette[c(1, 3, 4)], legend.position = "top",
        group.point.size = 4,
        plot.title = "Sentimientos expresados hacia el tópico 'Climate Change'", legend.text.size = 11) + 
  theme(plot.title = element_text(size = 12, family = "Georgia", color = "grey55"),
        axis.title = element_text(family = "Georgia"))





# BAG OF WORDS ------------------------------------------------------------


## HACER TB MENCIONES PARA EXPLORACION DE MODELO!!! A QUIEN MENCIONA DNC ETC, TB CON TF-IDF
## TB CORRELACIONES BAG OF WORDS PARA MODELAJE

climate_tfidf_hashtags <- get_tfidf(climate_frequency)
climate_tfidf <- get_tfidf(climate_unigram %>% get_frequencies())
climate_tfidf_nomentions <- get_tfidf(climate_unigram %>% filter(str_detect(word, "^@", negate = T)) %>% get_frequencies())

plot_tfidf(climate_tfidf, colores = c(1, 3, 4))
plot_tfidf(climate_tfidf_nomentions)
plot_tfidf(climate_tfidf_hashtags)


# correlaciones


library(janeaustenr)
library(widyr)

climate_sent_hashtag <- climate_hashtag %>% 
  filter(comunidad == "Progressives") %>% 
  group_by(comunidad, word) %>% 
  summarise(sent = mean(ave_sentiment)) %>% 
  select(-comunidad)

cor_gop <- climate_hashtag %>% 
  filter(word != "#greennewdeal", comunidad == "Progressives") %>% 
  pairwise_cor(word, status_id, sort = T)


cor_net <- cor_gop %>%
  inner_join(climate_frequency %>% 
               filter(n_usuarios > 1, comunidad == "Progressives") %>% 
               top_n(20, wt = n), #filter(n > 20, n_usuarios > 1),  
               by = c("item1" = "word", "item2" = "word")) %>% 
  filter(correlation > .35) %>%
  # left_join(climate_sent_hashtag %>% 
  #             filter(comunidad == "Progressives"),
  #           by = c("item1" = "word")) %>% 
  rename(from = item1, to = item2)

cor_nodes <- unique(c(cor_net$from, cor_net$to))

cor_graph <- tbl_graph(nodes = data.frame(name = cor_nodes), edges = cor_net , directed = F) %>% 
  activate(nodes) %>% 
  left_join(climate_sent_hashtag, by = c("name" = "word")) 


cor_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(aes(color = sent), size = 5) +
  geom_node_text(aes(label = name), color = plots_palette[4], repel = TRUE) +
  scale_color_gradient2(low = "red", high = "green", mid = "white") +
  
  theme_void()

plot_correlation_network(climate_hashtag, climate_frequency, com = "DNC", exclude_words = "#greennewdeal")


plot_correlation_network <- function(dat_hashtag, dat_frequency, com, exclude_words, top_words = 20, cor_limit = 0.35) {
  
 dat_sent <- dat_hashtag %>% 
    filter(comunidad == com) %>% 
    group_by(comunidad, word) %>% 
    summarise(sent = mean(ave_sentiment)) %>% 
    select(-comunidad)
  
  dat_cor <- dat_hashtag %>% 
    filter(!word %in% exclude_words, comunidad == com) %>% 
    pairwise_cor(word, status_id, sort = T)
  
  dat_net <-  dat_cor %>%
    inner_join(dat_frequency %>% 
                 filter(n_usuarios > 1, comunidad == com) %>% 
                 top_n(top_words, wt = n),
               by = c("item1" = "word", "item2" = "word")) %>% 
    filter(correlation > cor_limit) %>%
    rename(from = item1, to = item2)
  
  dat_nodes <- data.frame(name = unique(c(dat_net$from, dat_net$to)))
  
  dat_graph <- tbl_graph(nodes = dat_nodes, edges = dat_net , directed = F) %>% 
    activate(nodes) %>% 
    left_join(climate_sent_hashtag, by = c("name" = "word")) %>% 
    mutate(name = str_to_upper(str_remove(name, "#")))
  
  dat_graph %>% 
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(aes(color = sent), size = 5) +
    geom_node_text(aes(label = name), color = plots_palette[4], repel = TRUE) +
    scale_color_gradient2(low = "red", high = "green", mid = "white") +
    
    theme_void()
}

climate_sent <- climate_hashtag %>% 
  filter(comunidad == "GOP") %>% 
  group_by(comunidad, word) %>% 
  summarise(sent = mean(ave_sentiment)) %>% 
  ungroup() %>% 
  select(-comunidad)


climate_cor <- climate_hashtag %>% 
  filter(!word %in% "greennewdeal", comunidad == "GOP") %>% 
  pairwise_cor(word, status_id, sort = T)


climate_net <-  climate_cor %>%
  rename(from = item1, to = item2)

climate_nodes <- data.frame(name = unique(c(climate_net$from, climate_net$to)))

climate_graph <- tbl_graph(nodes = climate_nodes, edges = climate_net , directed = F) %>% 
  activate(nodes) %>% 
  inner_join(climate_frequency %>% 
               filter(n_usuarios > 1, comunidad == "GOP"), 
             by = c("name" = "word")) %>% 
            top_n(20, wt = n) %>% 
  left_join(climate_sent, by = c("name" = "word")) %>% 
  mutate(name = str_to_upper(str_remove(name, "#"))) %>% 
  activate(edges) %>% 
  filter(correlation > 0.15) 
  
climate_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(show.legend = FALSE, aes(alpha = correlation)) +
  geom_node_point(pch = 21, aes(fill = sent, size = n), show.legend = F) +
  geom_node_text( aes(label = name, color = comunidad), repel = TRUE, size = 3) +
  scale_fill_gradient2(low = "red", high = "green", mid = "grey") +
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
  theme_void() + 
  guides(color = F, fill = F)

# POLARIZACION ------------------------------------------------------------

rm(list = ls())
gc()

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))

climate_net <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "climate_pol.rds"))
comunidad <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
  mutate(user_id = as.character(user_id))



# GOP VS DNC --------------------------------------------------------------

# filtro comunidades
clim_gop_dnc <- climate_net %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos <- get_inner_nodes(clim_gop_dnc)

# datos polaridad
clim_gop_dnc_pol <- map_df(unique(clim_gop_dnc$from), ~get_node_pol_data(clim_gop_dnc, nodos_internos, .x))

mean(clim_gop_dnc_pol$polarity_score)


# Progressives VS DNC --------------------------------------------------------------

# filtro comunidades
clim_prg_dnc <- climate_net %>%
  filter_communities("DNC", "Progressives")

# nodos internos
nodos_internos <- get_inner_nodes(clim_prg_dnc)

# datos polaridad
clim_prg_dnc_pol <- map_df(unique(clim_prg_dnc$from), ~get_node_pol_data(clim_prg_dnc, nodos_internos, .x))

mean(clim_prg_dnc_pol$polarity_score)

test_data <- clim_gop_dnc_pol %>% 
  mutate(comunidades = "DNC-GOP") %>% 
  bind_rows(clim_gop_dnc_pol %>% mutate(comunidades = "DNC-PRG")) %>% 
  select(nodo, comunidades, polarity_score)

library(boot)
pol_test_dif <- get_test_dif(test_data, var = "polarity_score", comunidades = "comunidades", groups = c("DNC-GOP", "DNC-PRG"))


pol_test_dif$data %>% 
  ggplot(aes(x = sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", alpha = 0.5, binwidth = 0.003) + 
  scale_fill_manual(values = plots_palette[c(1, 4)])


saveRDS(pol_test_dif, here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "climatechange_polarization.rds"))






