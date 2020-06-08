# 00-creacion_comunidades.R




# CONFIGURACION INICIAL ---------------------------------------------------

# librerias
library(tidyverse)

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))


# Comunidad 1: cuentas oficiales de partidos / movimientos
gop <- read_rds(here::here("datos_raw", "comunidades", "gop.rda"))
dnc <- read_rds(here::here("datos_raw", "comunidades", "dnc.rda"))
jus_dem <- read_rds(here::here("datos_raw", "comunidades", "justicedems.rda"))

# Comunidad 2: cuentas oficiales de candidatos
biden <- read_rds(here::here("datos_raw", "comunidades", "joe_biden.rds"))
sanders <- read_rds(here::here("datos_raw", "comunidades", "bernie_sanders.rds"))
trump <- read_rds(here::here("datos_raw", "comunidades", "donald_trump.rds"))




## definicion final de comunidates ##

# comunidad partidos 1 #
dnc$user_id <- as.numeric(dnc$user_id)
gop$user_id <- as.numeric(gop$user_id)
jus_dem$user_id <- as.numeric(jus_dem$user_id)

dnc_final <- dnc[!(dnc$user_id %in% gop$user_id | dnc$user_id %in% jus_dem$user_id), ]
gop_final <- gop[!(gop$user_id %in% dnc$user_id | gop$user_id %in% jus_dem$user_id), ]
jusdem_final <- jus_dem[!(jus_dem$user_id %in% gop$user_id), ] 


# comunidad partidos 2 #
biden$user_id <- as.numeric(biden$user_id)
sanders$user_id <- as.numeric(sanders$user_id)
trump$user_id <- as.numeric(trump$user_id)

trump_final <- trump[!(trump$user_id %in% biden$user_id | trump$user_id %in% sanders$user_id), ]
biden_final <- biden[!(biden$user_id %in% trump$user_id | biden$user_id %in% sanders$user_id), ]
sanders_final <- sanders[!(sanders$user_id %in% trump$user_id | sanders$user_id %in% biden$user_id), ]


# creacion de data.frame de comunidades
comunidades <- bind_rows(
  dnc_final %>% mutate(comunidad = "DNC"),
  gop_final %>% mutate(comunidad = "GOP"),
  jusdem_final %>% mutate(comunidad = "Progressives")
)

seguidores <- bind_rows(
  biden_final %>% mutate(siguiendo = "Biden"),
  trump_final %>% mutate(siguiendo = "Trump"),
  sanders_final %>% mutate(siguiendo = "Sanders")
)

# comprobacion de ids unicos
nrow(comunidades)
length(unique(comunidades$user_id))

nrow(comunidades2)
length(unique(comunidades2$user_id))

duplic <- duplicated(comunidades$user_id)
duplic <- comunidades$user_id[duplic]

View(comunidades %>% filter(user_id %in% duplic))
View(dnc_final %>% filter(user_id %in% duplic))

# hay algunos duplicados, por alguna razon, en los ids de las comunidades DNC y GOP
# se procede a elmimarlos
dnc_final <- dnc_final[!duplicated(dnc_final$user_id), ]
gop_final <- gop_final[!duplicated(gop_final$user_id), ]

comunidades <- comunidades[!duplicated(comunidades$user_id), ]
seguidores <- seguidores[!duplicated(seguidores$user_id), ]

saveRDS(seguidores, here::here("datos_procesados", "comunidades", "seguidores.rda"))


