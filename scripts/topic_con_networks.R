# topic_con_networks.R

# definiciones de hashtags
# https://www.hashtags.org/definition/

# ejemplo de tidygraph y ggraph
# https://github.com/thomasp85/ggraph

# turorial tidygraph
# https://www.data-imaginist.com/2017/introducing-tidygraph/

# layouts en ggraph
# https://cran.r-project.org/web/packages/ggraph/vignettes/Layouts.html
  
library(igraph)
library(tidyverse)
library(ggraph)
library(grid)
library(gridExtra)
library(tidygraph)

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))

healthcare_unigram <- get_unigrams(healthcare, c("#healthcare", "healthcare"))
healthcare_hashtag <- get_hashtags(healthcare_unigram)

users_with_hashtags <- length(unique(healthcare_unigram$user_id[healthcare_unigram$word %in% pr$word]))

pr <- pr %>% 
  mutate(perc_users = n_usuarios / users_with_hashtags)


# Redes de hashtags

gop_hash_net <- create_hashtag_graph(healthcare_hashtag, "GOP", c("#health", "#covid19"), minim_n = 3)
dnc_hash_net <- create_hashtag_graph(healthcare_hashtag, "DNC", c("#health", "#covid19"), minim_n = 3)
prg_hash_net <- create_hashtag_graph(healthcare_hashtag, "Progressives", c("#health", "#covid19"), minim_n = 3)
ind_hash_net <- create_hashtag_graph(healthcare_hashtag, "Independent", c("#health", "#covid19"), 15, minim_n = 3)

hash_net <- bind_graphs(gop_hash_net, dnc_hash_net, prg_hash_net, ind_hash_net)

hash_net %>% 
  activate(nodes) %>% 
  filter(comunidad %in% c("GOP", "DNC")) %>% 
  ggraph(layout = "kk") +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_label(aes(label = nodes, fill = comunidad), size = 2.8, family = "Candara", color = "white") + 
  facet_nodes(~comunidad, ncol = 2, scales = "free") +
  theme_void() + 
  guides(fill = F) +
  scale_fill_manual(values = plots_palette)
  
p



get_users_hasthtag <- function(dat_unigram, hashtag, com = ".*") {
  
  unique(dat_unigram[dat_unigram$word == hashtag & str_detect(dat_unigram$comunidad, com), "user_id", drop = T])
}





pr <- get_users_hasthtag(healthcare_unigram, "#doctors", com = "GOP")
