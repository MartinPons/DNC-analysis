

# librerias
library(tidyverse)
library(rtweet)
library(igraph)
library(tidytext)




source(here::here("scripts", "funciones_auxiliares.R"))

# tweet object
# https://developer.twitter.com/en/docs/tweets/data-dictionary/overview/tweet-object

comunidades <- readRDS(here::here("datos_procesados", "comunidades.rda")) 

health <- readRDS(here::here("datos_raw", "#healthcare_tweet_extraction__2020-04-28.rda")) 

health_cleaned <- clean_tweets_df(health, comunidades)



# EXPLICACION DE HIPOTESISIS -----------------------------------------------

# Hay una separación entre democratas y progresistas. Para comprobarlo
# - Diferenicas en percepciones de eventos/personalidades politicas
# - Polarización de redes sociales

# A LA SEPARACIÓN LA LLAMAREMOS DIVERGENCIA ENTRE COMUNIDADES

# EXPLORACION. ESTADISTICOS -----------------------------------------------

# descriptivos

# - tamaño de cada comunidad
# - tamaño de tweeet en cada comunidad
# - momento de publicación del tweet (fecha, dia de la semana, hora)
# - como de compactas son las comunidades (closed triangle??)
# - actividad en cada comunidad (nº de tweets por usuario por comunidad)

# 1. Tamaño de cada comunidad
health_cleaned %>% 
  group_by(comunidad) %>% 
  count()

# 1.1 Comunidad por estado
health_cleaned %>% 
  group_by(State, comunidad) %>% 
  count() %>% 
  pivot_wider(names_from = "comunidad", values_from = "n") %>% View()

## sentimiento positivo - negativo ##

# sentimiento positivo /negativo por comunidad
# sentimiento positivo /negativo por comunidad y fecha
# diferencia de sentimiento positivo y negativo por estado

# DINAMISMO DEL DASHBOARD
# - Cambio de comparación de comunidades
# - Seleccion de fechas
# - Anchura de beans en histogramas???


## Espectro de sentimientos ##

# grafico pentagono espectro de sentimientos por comunidad

## Modelo de predicción de pertenencia a comunidad ##

# en funcion de #

# - estado
# - sentimientos hacia todos los efentos analizados
# - analisis imf: frecuencia en documentos inverso IMF
# - momento de publicación?? (publican mas unas comunidades )
# - n de seguidores, n de retwwets, n de tweets que publica (lo activo que es)



# DASHBOARD ---------------------------------------------------------------

# - KPIs de polarización, modularidad y alguna cosa mas (a lo mejor degree)
# - distribución de polarización
# - Evolutivo de sentimiento hacia una personalidad/evento: general y comunidades
# - Mapa de diferencia de sentimientos por comunidades
# - Pentagono de sentimientos
# - grafico (area?) de hora de dia sobre en general y de distintos eventos




# Diferencia de sentimiento por comunidad

# pr <- map_df(health_cleaned$place_full_name, ~get_state_code(.x))
