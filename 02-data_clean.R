# 02-data_clean.R


# CONFIGURACION INICIAL ---------------------------------------------------

# librerias
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
library(remoji)
library(lexicon)
library(rtweet)

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))



# PROCESAMIENTO DE DATOS --------------------------------------------------

# #healthcare
healthcare <- format_twitter_df_data("#healthcare", seguidores = T)
saveRDS(healthcare, here::here("datos_procesados", "formated_text_df_data", "#healthcare.rds"))

# healthcare
healthcare <- format_twitter_df_data("healthcare", seguidores = T)
saveRDS(healthcare, here::here("datos_procesados", "formated_text_df_data", "healthcare.rds"))

healthcare <- readRDS(here::here("datos_procesados", "formated_text_df_data", "healthcare.rds"))

# covid
covid_part1 <- format_twitter_df_data("covid", seguidores = T, idx_arch = 1:2, rm_ret = T)
saveRDS(covid_part1, here::here("datos_procesados", "formated_text_df_data", "covid_part1.rds"))

covid_part2 <- format_twitter_df_data("covid", seguidores = T, idx_arch = 3, rm_ret = T)
saveRDS(covid_part2, here::here("datos_procesados", "formated_text_df_data", "covid_part2.rds"))


# medicareforall
medicareforall<- format_twitter_df_data("medicareforall", seguidores = T, rm_ret = T)
saveRDS(medicareforall, here::here("datos_procesados", "formated_text_df_data", "medicareforall.rds"))

# #medicareforall
medicareforall<- format_twitter_df_data("#medicareforall", seguidores = T, rm_ret = T)
saveRDS(medicareforall, here::here("datos_procesados", "formated_text_df_data", "#medicareforall.rds"))

# education
education <- format_twitter_df_data("education", seguidores = T, rm_ret = T)
saveRDS(education, here::here("datos_procesados", "formated_text_df_data", "education.rds"))

# #education
education <- format_twitter_df_data("#education", seguidores = T, rm_ret = T)
saveRDS(education_part1, here::here("datos_procesados", "formated_text_df_data", "#education.rds"))


# singlepayer
singlepayer <- format_twitter_df_data("singlepayer", seguidores = T, rm_ret = T)
saveRDS(singlepayer, here::here("datos_procesados", "formated_text_df_data", "singlepayer.rds"))


# publicoption
publicoption <- format_twitter_df_data("publicoption", seguidores = T, rm_ret = T)
saveRDS(publicoption, here::here("datos_procesados", "formated_text_df_data", "publicoption.rds"))

# climatechange
climatechange <- format_twitter_df_data("climatechange", seguidores = T, rm_ret = T)
saveRDS(climatechange, here::here("datos_procesados", "formated_text_df_data", "climatechange.rds"))


# joe biden
biden1<- format_twitter_df_data("JoeBiden", seguidores = T, idx_arch = c(1, 2, 4), rm_ret = T, compute_sentiment = F)
saveRDS(biden1, here::here("datos_procesados", "formated_text_df_data", "biden_no_sent_part1.rds"))

rm(biden1)
gc()

# joe biden
biden2<- format_twitter_df_data("JoeBiden", seguidores = T, idx_arch = c(5, 7), rm_ret = T, compute_sentiment = F)
saveRDS(biden2, here::here("datos_procesados", "formated_text_df_data", "biden_no_sent_part2.rds"))


# greennewdeal
greennewdeal<- format_twitter_df_data("greennewdeal", seguidores = T,  rm_ret = T, compute_sentiment = T)
saveRDS(greennewdeal, here::here("datos_procesados", "formated_text_df_data", "greennewdeal.rds"))


# DonadDTrump
trump1<- format_twitter_df_data("DonaldTrump", seguidores = T, idx_arch = 1:4, rm_ret = T, compute_sentiment = F)
saveRDS(trump1, here::here("datos_procesados", "formated_text_df_data", "trump_no_sent_part1.rds"))

rm(trump1)
gc()

trump2<- format_twitter_df_data("DonaldTrump", seguidores = T, idx_arch = c(5, 6, 8), rm_ret = T, compute_sentiment = F)
saveRDS(trump2, here::here("datos_procesados", "formated_text_df_data", "trump_no_sent_part2.rds"))

rm(trump2)
gc()

# Bernie Sanders
bernie<- format_twitter_df_data("BernieSanders", seguidores = T, idx_arch = c(1, 2, 4, 5, 7), rm_ret = T, compute_sentiment = F)
saveRDS(bernie, here::here("datos_procesados", "formated_text_df_data", "bernie_no_sent.rds"))

rm(bernie)
gc()

# Tara Reade
tara<- format_twitter_df_data("TaraReade", seguidores = T, rm_ret = T, compute_sentiment = F)
saveRDS(tara, here::here("datos_procesados", "formated_text_df_data", "taradreade_no_sent.rds"))
