

# caraga de librerias
library(tidyverse)
library(rtweet)
library(igraph)

# funciones_auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))

# emparejamiento comunidades
gop_dnc <- c("GOP", "DNC")
prog_dnc <- c("Progressives", "DNC")

# CARGA DE DATOS ----------------------------------------------------------

health <- readRDS(here::here("datos_raw",
                             "JoeBiden_tweet_extraction__2020-04-27.rda"))

comunidades <- readRDS(here::here("datos_procesados", 
                                  "comunidades.rda")) 

# WRANGLING ---------------------------------------------------------------

# transformacion en red
health_net <- network_graph(health, .e = "retweet")

# pre-procesamiento
dat <- pre_processing(health_net, comunidades)

nodes_df <- igraph::as_data_frame(health_net, what = "vertices")
ties_df <- igraph::as_data_frame(health_net, what = "edges")

g <- graph_from_data_frame(d= ties_df, 
                           directed = TRUE, 
                           vertices = nodes_df)
# GOP VS DNC --------------------------------------------------------------

# filtro comunidades
dat_gop_dnc <- dat %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos <- get_inner_nodes(dat_gop_dnc)

# datos polaridad
data_pol <- map_df(unique(dat_gop_dnc$from), ~get_node_pol_data(dat_gop_dnc, nodos_internos, .x))

mean(data_pol$polarity_score)


# PROGRESSIVES VS DNC -----------------------------------------------------

# filtro comunidades
dat_prog_dnc <- dat %>% 
  filter_communities("DNC", "Progressives")

# nodos internos
nodos_internos <- get_inner_nodes(dat_prog_dnc)

# datos polaridad
data_pol <- map_df(unique(dat_prog_dnc$from), 
                   ~get_node_pol_data(dat_prog_dnc, nodos_internos, .x))


mean(data_pol$polarity_score)
hist(data_pol$polarity_score)



# GOP VS DNC CON UNIQUE --------------------------------------------------------------

# filtro comunidades
dat_gop_dnc_un <- dat %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos_un <- get_inner_nodes(dat_gop_dnc_un)

# datos polaridad
data_pol_un <- map_df(unique(dat_gop_dnc$from), ~get_node_pol_data(dat_gop_dnc_un, nodos_internos_un, .x))

mean(data_pol_un$polarity_score)