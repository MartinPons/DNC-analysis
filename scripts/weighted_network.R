# weighted_network.R

health <- readRDS(here::here("datos_raw",
                             "JoeBiden_tweet_extraction__2020-04-27.rda"))

comunidades <- readRDS(here::here("datos_procesados", 
                                  "comunidades.rda")) 

health <- health %>% 
  filter(is_retweet) %>% 
  select(screen_name, retweet_screen_name) %>% 
  group_by(screen_name, retweet_screen_name) %>% 
  count(name = "weight") %>% 
  rename("from" = screen_name, 
         "to" = retweet_screen_name)

health_net <- graph_from_data_frame(d = health, 
                                    directed = T)


ties_df <- igraph::as_data_frame(health_net, what = "edges")
nodes_df <- igraph::as_data_frame(health_net, what = "vertices")

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




######################################
# PRUEBAS QUE ESTABAN EN OTRO SCRIPT #
######################################

library(rtweet)
library(igraph)


pr <- readRDS(here::here("datos_raw", "#healthcare_tweet_extraction__2020-04-28.rda"))
gop <- readRDS(here::here("datos_raw", "gop.rda"))
dnc <- readRDS(here::here("datos_raw", "dnc.rda"))

g <- network_graph(pr, .e = "retweet")


ties_df <- igraph::as_data_frame(health_net, what = "edges")

ties_df_group <- ties_df %>% 
  select(-type) %>% 
  group_by(from, to) %>% 
  count(name = "weight")

weight <- ties_df_group$count


g_proc <- graph_from_data_frame(d = ties_df_group, 
                                directed = TRUE)

E