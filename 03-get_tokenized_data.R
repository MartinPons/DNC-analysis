# 03-get_tokenized_data.R


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
library(topicmodels)
library(forcats)
library(scales)
library(stringr)
library(reldist)
library(ggradar)
library(tidygraph)
library(remoji)
library(lexicon)
library(rtweet)
library(tm)
library(qdap)


# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))


# CARGA DE DATOS ---------------------------------------------------------

# healthcare
healthcare <- readRDS(here::here("datos_procesados", "formated_text_df_data", "healthcare.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")))

# climatechange
climate <- readRDS(here::here("datos_procesados", "formated_text_df_data", "climatechange.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#chinavirus|#wuhanvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")))

# education
education <- readRDS(here::here("datos_procesados", "formated_text_df_data", "education.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

# biden
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


# trump
trump1 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "trump_no_sent_part1.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

trump2 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "trump_no_sent_part2.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

trump <- bind_rows(trump1, trump2) %>% 
  distinct(status_id, .keep_all = T)

rm(trump1, trump2)
gc()

# trump
trump1 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "trump_no_sent_part1.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

trump2 <- readRDS(here::here("datos_procesados", "formated_text_df_data", "trump_no_sent_part2.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

trump <- bind_rows(trump1, trump2) %>% 
  distinct(status_id, .keep_all = T)

rm(trump1, trump2)
gc()


# sanders
sanders <- readRDS(here::here("datos_procesados", "formated_text_df_data", "bernie_no_sent.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))


# reade
reade <- readRDS(here::here("datos_procesados", "formated_text_df_data", "taradreade_no_sent.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#trumpvirus", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")),
         user_id = as.character(user_id))

# tweets_por_comundidad

healthcare %>% 
  group_by(comunidad) %>% 
  count() %>% 
  saveRDS(here::here("datos_procesados", "n_tweets_comunidad", "n_tweets_healthcare.rds"))


# OBTENCION HASHTAGS TOKENZADOS -------------------------------------------

# healthcare
healthcare_unigram <- get_unigrams(healthcare,c("#healthcare", "healthcare"))
healthcare_hashtag <- get_hashtags(healthcare_unigram)
healthcare_frequency <- get_frequencies(healthcare_hashtag)

saveRDS(healthcare_hashtag, here::here("datos_procesados", "topics_processed", "healthcare_hashtag.rds"))
saveRDS(healthcare_frequency, here::here("datos_procesados", "topics_processed", "healthcare_hashtag_frequency.rds"))

# climatechange
climate_unigram <- get_unigrams(climate, c("#climatechange", "#globalwarming"))
climate_hashtag <- get_hashtags(climate_unigram)
climate_frequency <- get_frequencies(climate_hashtag)

saveRDS(climate_frequency, here::here("datos_procesados", "topics_processed", "climatechange_hashtag_frequency.rds"))

# education
education_unigram <- get_unigrams(education, c("#education", "education"))
education_hashtag <- get_hashtags(education_unigram)
education_frequency <- get_frequencies(education_hashtag) 

saveRDS(education_frequency, here::here("datos_procesados", "topics_processed", "education_hashtag_frequency.rds"))


# biden
biden <- biden %>% 
  filter(comunidad != "Independent")

biden_unigram <- get_unigrams(biden, c("#biden", "biden", "#joe", "joe"))
biden_hashtag <- get_hashtags(biden_unigram)
biden_frequency <- get_frequencies(biden_hashtag) 

saveRDS(biden_hashtag, here::here("datos_procesados", "topics_processed", "biden_hashtag.rds"))
saveRDS(biden_frequency, here::here("datos_procesados", "topics_processed", "biden_hashtag_frequency.rds"))


# trump
trump <- trump %>% 
  filter(comunidad != "Independent")

trump_unigram <- get_unigrams(trump, c("#trump", "trump", "#donald", "donald"))
trump_hashtag <- get_hashtags(trump_unigram)
trump_frequency <- get_frequencies(trump_hashtag) 

saveRDS(trump_hashtag, here::here("datos_procesados", "topics_processed", "trump_hashtag.rds"))
saveRDS(trump_frequency, here::here("datos_procesados", "topics_processed", "trump_hashtag_frequency.rds"))

# sanders
sanders <- sanders %>% 
  filter(comunidad != "Independent")

sanders_unigram <- get_unigrams(sanders, c("#sanders", "sanders", "#bernie", "bernie"))
sanders_hashtag <- get_hashtags(sanders_unigram)
sanders_frequency <- get_frequencies(sanders_hashtag) 

saveRDS(sanders_hashtag, here::here("datos_procesados", "topics_processed", "sanders_hashtag.rds"))
saveRDS(sanders_frequency, here::here("datos_procesados", "topics_processed", "sanders_hashtag_frequency.rds"))


# reade
reade <- reade %>% 
  filter(comunidad != "Independent")

reade_unigram <- get_unigrams(reade, c("#reade", "reade", "#tara", "tara"))
reade_hashtag <- get_hashtags(reade_unigram)
reade_frequency <- get_frequencies(reade_hashtag) 

saveRDS(reade_hashtag, here::here("datos_procesados", "topics_processed", "reade_hashtag.rds"))
saveRDS(reade_frequency, here::here("datos_procesados", "topics_processed", "reade_hashtag_frequency.rds"))