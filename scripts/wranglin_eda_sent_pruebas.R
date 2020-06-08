

# CONFIGURACION INICIAL ---------------------------------------------------

# En caso de problemas con qdap mirar como esta variable de entorno para java y cambiar
# si un caso
library(qdapRegex)
library(sentimentr)
library(syuzhet)
library(lubridate)01
library(tidyverse)
library(tidytext)
library(forcats)
library(tokenizers)
library(widyr)
library(igraph)
library(ggraph)
library(topicmodels)

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

# sentimiento
medicare <- lee_datos_rds("healthcare")
comunidades <- readRDS(here::here("datos_procesados", "comunidades.rda")) 

medicare_cleaned <- clean_tweets_df(medicare, comunidades)

medicare_cleaned_text <- medicare_cleaned %>% 
  mutate(text = clean_text(text))

sent <- sentiment_by(medicare_cleaned_text$text)

medicare_cleaned_text <- medicare_cleaned_text %>% 
  bind_cols(sent)


# DISTRIBUCION DE SENTIMIENTOS --------------------------------------------------------

# media por grupo
medicare_cleaned_text %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment),
            n = n())


# distribuciones
medicare_cleaned_text %>% 
  ggplot(aes(x = comunidad, y = ave_sentiment)) + 
  geom_boxplot()

medicare_cleaned_text %>% 
  ggplot(aes(ave_sentiment)) + 
  geom_histogram() + 
  facet_wrap(~comunidad, scale = "free_y")

# test de diferencia de medias DNC GOP
dnc_prog <- filter(medicare_cleaned_text, comunidad %in% c("DNC", "Progressives"))
t.test(ave_sentiment ~ comunidad, data = dnc_prog)




# SENTIMIENTOS TIDY -------------------------------------------------------

medicare_tidy <- medicare_cleaned_text %>% 
  unnest_tokens(word, text)

# sent affin
medicare_tidy_afinn <- medicare_tidy %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(status_id, comunidad) %>% 
  summarise(afinn_sent = mean(value))

# media por grupo
medicare_tidy_afinn %>% 
  group_by(comunidad) %>% 
  summarise(afinn_sent = mean(afinn_sent))

# distribucion densidad
medicare_tidy_afinn %>% 
  ggplot(aes(x = afinn_sent)) + 
  geom_density(aes(fill = comunidad), alpha = 0.5)

# distribucion boxplot
medicare_tidy_afinn %>% 
ggplot(aes(x = comunidad, y = afinn_sent)) + 
  geom_boxplot(aes(fill = comunidad), varwidth = T, notch = T)



# TOPICS ------------------------------------------------------------------

# palabras mas utilizadas
medicare_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!(word %in% c("medicareforall", "healthcare", "medicare", "care", "health", "amp"))) %>% 
  group_by(comunidad, word) %>% 
  count(sort = T) %>% 
  group_by(comunidad) %>% 
  top_n(10, wt = n) %>% 
  ggplot(aes(x = n, y = fct_reorder2(word, comunidad, -n))) + 
  geom_col() + 
  facet_wrap(~comunidad, scales = "free")

# podemos intentar bigrams
# biden y joebiden hay que juntarlos, etc
# que es amp?

medicare_cleaned_text %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = F) %>% 
  count(comunidad, bigram, sort = T) %>% 
  filter(str_detect(bigram, "medicareforall|healthcare|medicare|care|health|amp", negate = T)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ", remove = F) %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  group_by(comunidad) %>% 
  top_n(10, wt = n) %>% 
  mutate(bigram = reorder_within(bigram, n, comunidad)) %>% 
  ggplot(aes(x = n, y = fct_reorder2(bigram, comunidad, -n))) + 
  geom_col(aes(fill = comunidad)) + 
  facet_wrap(~comunidad, scales = "free") + 
  scale_y_reordered()

# limpiar mas. que covid 19 sea una palabra

# hay que hacer un descriptor de palabras de como ven a joe biden y a tara reade progrevives vs DNC

# comunidad alternativa: seguidores en usa de Bernie sanders que no lo son de Joe Biden ni de DNC!!!!!!


# parece que muchos terminos están correlacionados. Vamos a ver la correlación

medicare_tidy_filtered <- medicare_tidy %>% 
  filter(!word %in% stop_words$word)


medicare_word_pairs <- medicare_tidy_filtered %>% 
  pairwise_count(word, status_id, sort = TRUE)

medicare_bigrams_filtered <- medicare_cleaned_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = F) %>% 
  filter(str_detect(bigram, "medicareforall|healthcare|medicare|care|health|amp|joe biden|bernie sanders|donald trump", negate = T)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ", remove = F) %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)
  
medicare_bigram_pairs <- medicare_bigrams_filtered %>% 
  pairwise_count(bigram, status_id, sort = T)

# coeficiente Phi correlacion binaria
bigram_cor <- medicare_bigrams_filtered %>% 
  group_by(bigram) %>% 
  filter(n() > 20) %>% 
  pairwise_cor(bigram, status_id, sort = TRUE)

bigram_cor

bigram_cor %>% 
  filter(correlation > 0.25) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), repel = TRUE) + 
  theme_void()

# contar cuantas veces hablaan de public option y single payer democratas y progresives y GOP