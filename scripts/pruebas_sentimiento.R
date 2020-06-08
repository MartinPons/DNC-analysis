# sentiment_explore.R

# exploracion de tweets para analisis de sentimiento

# librerias
library(tidyverse)
library(qdapRegex)
library(rtweet)
library(tm)
library(qdap)
library(topicmodels)
library(syuzhet)

# CLEANING



# CARGA DE DATOS ----------------------------------------------------------

health <- readRDS(here::here("datos_raw", "#healthcare_tweet_extraction__2020-04-28.rda"))



# CORPUS ------------------------------------------------------------------
health_corpus <- get_tweets_corpus(health)



# TERM FREQUENCY ---------------------------------------------------------

term_count <- freq_terms(health_corpus, 60) # no funciona qdap por rJava. Revisar

# creamos custom stop words
custom_stop <- c("healthcare", "health", "us", "re", "m", "dr", "care", 
                 "s", "can")

# quitando las custom_stop
health_corpus_refined <- tm_map(health_corpus, removeWords, custom_stop)

# recuento de frecuencias
term_count_clean <- freq_terms(health_corpus_refined, 20)
term_count_clean
# TOPIC ANALYSIS ----------------------------------------------------------

# Explicacion del LDA
# https://medium.com/@lettier/how-does-lda-work-ill-explain-using-emoji-108abf40fa7d

# document term matrix
dtm <- DocumentTermMatrix(health_corpus_refined)
inspect(dtm)

# filtramos para que los totales de la fila sean > 0
rowTotals <- apply(dtm, 2, sum)
tweet_dtm_new <- dtm[rowTotals > 0, ]

lda_4 <- LDA(tweet_dtm_new, k = 3)
terms(lda_4, 10)



# SENTIMENT ANALYSIS ------------------------------------------------------

# tweets de biden
biden <- search_tweets("Biden", n = 5000, 
                       lang = "en", include_rts = FALSE)

# analisis de sentimiento
sent_an <- get_nrc_sentiment(biden$text)

# suma de scores y transformacion en data.frame
score <- colSums(sent_an[,])
score_df <- data.frame(score)

# se convierten los nombres en columna
sa_score <- cbind(sentiment = row.names(score_df), 
                  score_df, row.names = NULL)

sa_score %>% 
  ggplot(aes(x = sentiment, y = score, 
             fill = sentiment)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
