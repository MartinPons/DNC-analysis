# 09-modeling_data_processing.R


# CONFIGURACION INICIAL ---------------------------------------------------

# En caso de problemas con qdap mirar como esta variable de entorno para java y cambiar
# si un caso
library(tidyverse)
library(tidymodels)
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

sanders <- readRDS(here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "bernie_sanders.rds")) %>% 
  select(-screen_name, -retweet_user_id, -retweet_screen_name, -country_code, -place_name, -coords_coords, -element_id,
         -word_count, -sd, -comunidad)
biden <- readRDS(here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "joe_biden.rds")) %>% 
  select(-screen_name, -retweet_user_id, -retweet_screen_name, -country_code, -place_name, -coords_coords, -element_id,
         -word_count, -sd, -comunidad)
trump <- readRDS(here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "donald_trump.rds"))%>% 
  select(-screen_name, -retweet_user_id, -retweet_screen_name, -country_code, -place_name, -coords_coords, -element_id,
         -word_count, -sd, -comunidad)
reade <- readRDS(here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "tara_reade.rds")) %>% 
  select(-screen_name, -retweet_user_id, -retweet_screen_name, -country_code, -place_name, -coords_coords, -element_id,
         -word_count, -sd, -comunidad)

comunidades <- readRDS(here::here("datos_procesados", "comunidades.rda"))



# MODEL 1 DNC - GOP -------------------------------------------------------
set.seed(123)

dnc_gop <- comunidades %>% 
  filter(comunidad != "Progressives")

data_split <- initial_split(dnc_gop, prop = 3/4)

train_data <- training(data_split)
test_data <- testing(data_split)




# TOP HASHTAGS ------------------------------------------------------------


## sanders ##
sanders_hashtag <- sanders %>% 
  inner_join(train_data, by = "user_id") %>% 
  get_unigrams(c("#covid19", "COVID19", "#pandemic", "pandemic")) %>% get_hashtags("#[Bb]erniesanders|#[Bb]ernie|#[Ss]anders") %>% 
  get_frequencies() %>% 
  filter(n_usuarios > 10) %>% 
  group_by(comunidad) %>% 
  top_n(40, wt = n)

dnc_sanders_hashtag <- sanders_hashtag$word[sanders_hashtag$comunidad == "DNC"]
gop_sanders_hashtag <- sanders_hashtag$word[sanders_hashtag$comunidad == "GOP"]

saveRDS(dnc_sanders_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "sanders_dnc_top_hashtag.rds"))
saveRDS(gop_sanders_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "sanders_gop_top_hashtag.rds"))

rm(sanders)
gc()



## biden ##

biden_hashtag_gop <- biden %>% 
  inner_join(train_data %>% filter(comunidad == "GOP"), by = "user_id") %>% 
  get_unigrams(c("#covid19", "COVID19", "#pandemic", "pandemic")) %>% get_hashtags("#[Jj]oe[Bb]iden|#[Jj]oe|#[Bb]iden") %>% 
  get_frequencies() %>% 
  filter(n_usuarios > 10) %>% 
  group_by(comunidad) %>% 
  top_n(40, wt = n)

gop_biden_hashtag <- biden_hashtag_gop$word
saveRDS(gop_biden_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "biden_gop_top_hashtag.rds"))

rm(biden_hashtag_gop)
gc()

biden_hashtag_dnc <- biden %>% 
  inner_join(train_data %>% filter(comunidad == "DNC"), by = "user_id") %>% 
  get_unigrams(c("#covid19", "COVID19", "#pandemic", "pandemic")) %>% get_hashtags("#[Jj]oe[Bb]iden|#[Jj]oe|#[Bb]iden") %>% 
  get_frequencies() %>% 
  filter(n_usuarios > 10) %>% 
  group_by(comunidad) %>% 
  top_n(40, wt = n)

dnc_biden_hashtag <- biden_hashtag_dnc$word
saveRDS(dnc_biden_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "biden_dnc_top_hashtag.rds"))

rm(biden_hashtag_dnc)
gc()



## trump ##

trump_hashtag <- trump %>% 
  inner_join(train_data, by = "user_id") %>% 
  get_unigrams(c("#covid19", "COVID19", "#pandemic", "pandemic")) %>% get_hashtags("#[Dd]onaldtrump|#[Dd]onald|#[Tt]rump") %>% 
  get_frequencies() %>% 
  filter(n_usuarios > 10) %>% 
  group_by(comunidad) %>% 
  top_n(40, wt = n)

dnc_trump_hashtag <- trump_hashtag$word[trump_hashtag$comunidad == "DNC"]
gop_trump_hashtag <- trump_hashtag$word[trump_hashtag$comunidad == "GOP"]

saveRDS(dnc_trump_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "trump_dnc_top_hashtag.rds"))
saveRDS(gop_trump_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "trump_gop_top_hashtag.rds"))

rm(trump)
gc()



## reade ##
reade_hashtag <- reade %>% 
  inner_join(train_data, by = "user_id") %>% 
  get_unigrams(c("#covid19", "COVID19", "#pandemic", "pandemic")) %>% get_hashtags("#[Dd]onaldreade|#[Dd]onald|#[Tt]rump") %>% 
  get_frequencies() %>% 
  filter(n_usuarios > 10) %>% 
  group_by(comunidad) %>% 
  top_n(40, wt = n)

dnc_reade_hashtag <- reade_hashtag$word[reade_hashtag$comunidad == "DNC"]
gop_reade_hashtag <- reade_hashtag$word[reade_hashtag$comunidad == "GOP"]

saveRDS(dnc_reade_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "reade_dnc_top_hashtag.rds"))
saveRDS(gop_reade_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "reade_gop_top_hashtag.rds"))



# TF-IDF ------------------------------------------------------------------


## sanders ##
sanders_hashtag <- sanders %>% 
  inner_join(train_data, by = "user_id") %>% 
  get_unigrams(c("#covid19", "COVID19", "#pandemic", "pandemic")) %>% get_hashtags("#[Bb]erniesanders|#[Bb]ernie|#[Ss]anders") %>% 
  get_frequencies() %>% 
  filter(n_usuarios > 10) %>% 
  group_by(comunidad) %>% 
  top_n(40, wt = n)

dnc_sanders_hashtag <- sanders_hashtag$word[sanders_hashtag$comunidad == "DNC"]
gop_sanders_hashtag <- sanders_hashtag$word[sanders_hashtag$comunidad == "GOP"]

saveRDS(dnc_sanders_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "sanders_dnc_top_hashtag.rds"))
saveRDS(gop_sanders_hashtag, here::here("datos_procesados", "modeling_data", "objetos_para_featuring", "sanders_gop_top_hashtag.rds"))

rm(sanders)
gc()


rm(reade)
gc()


train_data <- train_data %>% 
  left_join()

# EXTRACCION DE TOP Y TF-IDF ----------------------------------------------



# DATA SPLITTING ----------------------------------------------------------






