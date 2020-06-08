# 08-modeling_data_wrangling.R



# CONFIGURACION INICIAL ---------------------------------------------------

# librerias
library(tidyverse)

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))


# CARGA DE DATOS ----------------------------------------------------------

## Bernie sanders

bernie_files <- str_subset(list.files(here::here("datos_procesados", "modeling_data", "datos_sin_independientes")), "Bernie")

bernie <- map_df(bernie_files, 
                 function(x) {
                   readRDS(here::here("datos_procesados", "modeling_data", "datos_sin_independientes", x))
                 }) %>% 
  distinct(status_id, .keep_all = T)


## Joe Biden ##

biden_files <- str_subset(list.files(here::here("datos_procesados", "modeling_data", "datos_sin_independientes")), "JoeBiden")

biden <- map_df(biden_files, 
                 function(x) {
                   readRDS(here::here("datos_procesados", "modeling_data", "datos_sin_independientes", x))
                 }) %>% 
  distinct(status_id, .keep_all = T)


## Donald Trump ##

trump_files <- str_subset(list.files(here::here("datos_procesados", "modeling_data", "datos_sin_independientes")), "DonaldTrump")

trump <- map_df(trump_files, 
                function(x) {
                  readRDS(here::here("datos_procesados", "modeling_data", "datos_sin_independientes", x))
                }) %>% 
  distinct(status_id, .keep_all = T)


## Tara Reade ##

tara_files <- str_subset(list.files(here::here("datos_procesados", "modeling_data", "datos_sin_independientes")), "TaraReade")

tara <- map_df(tara_files, 
                function(x) {
                  readRDS(here::here("datos_procesados", "modeling_data", "datos_sin_independientes", x))
                }) %>% 
  distinct(status_id, .keep_all = T)



# GRABACION DE DATOS ------------------------------------------------------

saveRDS(bernie, here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "bernie_sanders.rds"))
saveRDS(biden, here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "joe_biden.rds"))
saveRDS(trump, here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "donald_trump.rds"))
saveRDS(tara, here::here("datos_procesados", "modeling_data", "02-data_preprocessed", "tara_reade.rds"))

