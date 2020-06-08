# 00-datos_seguidores.R


# DATOS DE TWITTER --------------------------------------------------------

library(rtweet)
library(igraph)

options(scipen=999)

# tl <- get_timeline("@AOC", n = 500)
# med_tweets <- search_tweets("#medicareforall", n = 18000)
# red <- network_data(pr, .e = "retweet")
# aoc_nd <- network_data(tl, .e = "retweet")
# network_graph(tl, .e = "retweet") 

# cuentas de twitter

# Republicados: @GOP
# Democratas: @TheDemocrats
# Progresitas: @USProgressives, @OurRevolution, @justicedems

# para elegir cual de las cuentas asociamos a los progresistas
# podemos, si no las combinamos, medir el numero de seguidores
# y los seguidores que tienen los seguidores de esta cuenta
# Si cojo justice democrats tengo que demostrar con bibilografia
# que el nuevo movimiento progresista tira por justice democrats

# https://www.justicedemocrats.com/home

# COMUNIDADES POR SEGUIDORES DE CUENTAS DE TWITTER ------------------------

# seguidores de la cuenta oficial de los progresistas @USProgressives
# progressives <- get_followers("USProgressives", n = 70000, verbose = T)

# justice_democracts
jus_dem <- get_followers("justicedems", n = 282000, retryonratelimit = T)
saveRDS(jus_dem, here::here("datos_raw", "twitter", "justicedems.rda"))

# democratas
dnc <- get_followers("TheDemocrats", n = 1800000, retryonratelimit = T)
saveRDS(dnc, here::here("datos_raw", "twitter", "dnc.rda"))

# republicanos
gop <- get_followers("GOP", n = 2400000, retryonratelimit = T)
saveRDS(gop, here::here("datos_raw", "twitter", "gop.rda"))


# definicion final de comunidates
dnc$user_id <- as.numeric(dnc$user_id)
gop$user_id <- as.numeric(gop$user_id)
jus_dem$user_id <- as.numeric(jus_dem$user_id)

dnc_final <- dnc[!(dnc$user_id %in% gop$user_id | dnc$user_id %in% jus_dem$user_id), ]
gop_final <- gop[!(gop$user_id %in% dnc$user_id | gop$user_id %in% jus_dem$user_id), ]
jusdem_final <- jus_dem[!(jus_dem$user_id %in% gop$user_id), ] 

# creacion de data.frame de comunidades
comunidades <- bind_rows(
  dnc_final %>% mutate(comunidad = "DNC"),
  gop_final %>% mutate(comunidad = "GOP"),
  jusdem_final %>% mutate(comunidad = "Progressives")
)

# comprobacion de ids unicos
nrow(comunidades)
length(unique(comunidades$user_id))

duplic <- duplicated(comunidades$user_id)
duplic <- comunidades$user_id[duplic]

View(comunidades %>% filter(user_id %in% duplic))
View(dnc_final %>% filter(user_id %in% duplic))

# hay algunos duplicados, por alguna razon, en los ids de las comunidades DNC y GOP
# se procede a elmimarlos
dnc_final <- dnc_final[!duplicated(dnc_final$user_id), ]
gop_final <- gop_final[!duplicated(gop_final$user_id), ]

comunidades <- comunidades[!duplicated(comunidades$user_id), ]

# grabacion comunidades finales
saveRDS(jusdem_final, here::here("datos_procesados", "comunidades", "jusdem_final.rda"))
saveRDS(dnc_final, here::here("datos_procesados", "comunidades", "dnc_final.rda"))
saveRDS(gop_final, here::here("datos_procesados", "comunidades", "gop_final.rda"))

saveRDS(comunidades, here::here("datos_procesados", "comunidades", "comunidades.rda"))
saveRDS(comunidades2, here::here("datos_procesados", "comunidades", "seguidores.rda"))

