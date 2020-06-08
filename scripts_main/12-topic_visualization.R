# 12-topic_visualization.R




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



get_correlation_data <- function(dat_hashtag, dat_frequency, com, cor_limit = 0.1, user_limit = 4, top = 40) {
  dat_hashtag %>% 
    filter(comunidad == com) %>% 
    inner_join(dat_frequency %>%
                 filter(comunidad == "Progressives", n_usuarios >= user_limit) %>% 
                 top_n(top, wt = n),
               by = c("word")) %>% 
    mutate(word = str_to_upper(str_remove(word, "^#"))) %>% 
    pairwise_cor(word, status_id, sort = T) %>% 
    filter(correlation > cor_limit)  %>% 
    mutate(comunidad = com)
}

get_correlation_net<- function(cor_data) {
  
  nodes <- unique(c(cor_data$item1, cor_data$item2))
  
  nodes_df <- data.frame(hashtag = nodes, comunidad = cor_data$comunidad[1])
  
  tbl_graph(nodes = nodes_df, edges = cor_data)
  
}

plot_topics <- function(dat, filtered_words, cols = 2, colores = 1:4, label_size = 8, min_users = 1, top_words = 20) {
  
  
  dat %>% 
    filter(!word %in% filtered_words) %>% 
    filter(n_usuarios >= min_users) %>% 
    ungroup() %>% 
    group_by(comunidad) %>% 
    top_n(top_words, n) %>% 
    mutate(word = str_to_upper(str_remove(word, "^#"))) %>% 
    ggplot(aes(x = n, y = reorder_within(word, n, within = comunidad))) + 
    geom_col(aes(fill = comunidad), width = 0.75) + 
    scale_y_reordered() + 
    facet_wrap(~comunidad, scale = "free", ncol = cols) + 
    scale_fill_manual(values = plots_palette[colores]) + 
    labs(x = "nº hashtags") +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          text = element_text(family = "Georgia", color = "grey55", size = label_size), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          strip.text = element_text(color = "white"),
          axis.title.y = element_blank(),
          legend.position = "top") +
    guides(fill = F)
}


plot_correlation_network <- function(net_data, node_size = 3, text_size = 2, colores = c(1, 3, 4)) {
  
  net_data %>%   
    ggraph(layout = "fr") + 
    geom_edge_link(aes(edge_alpha = correlation), show.legend = F) + 
    geom_node_point(aes(color = comunidad),size = node_size) + 
    geom_node_text(aes(label = hashtag, color = comunidad), repel = TRUE, size = text_size) + 
    facet_nodes(~comunidad, scales = "free", ncol = 1) + 
    scale_color_manual(values = plots_palette[colores]) + 
    theme_void() +
    guides(color = F) + 
    theme(strip.text = element_text(color = "white"))
}

plot_topic_networks <- function(dat_frequency, dat_graph, filtered_words = "", texto_titulo = "",
                                minim_n = 3, lab_size = 8, text_size = 5, layout_style = "fr", top_words = 20, 
                                node_size = 3, random_state = 123, title_size = 1.2, legend_text_size = 1) {
  
  set.seed(random_state)
  
  grob_bars <- ggplotGrob(plot_topics(dat_frequency %>% filter(comunidad != "Independent"), 
                                      filtered_words, cols = 1, min_users = minim_n,
                                      colores = c(1, 3, 4), label_size = lab_size,
                                      top_words = top_words)) 
  
  
  
  grob_graph <- ggplotGrob(dat_graph %>% plot_correlation_network(text_size = text_size, node_size = node_size))
  
  titulo <-  textGrob(label = texto_titulo, gp = gpar(fontfamily = "Georgia", cex = title_size))
  
  leyenda <- legendGrob(labels = c("GOP",  "DNC", "Progressives"),  pch = 21, 
                        gp = gpar(fill = plots_palette[c(1, 3, 4)], fontfamily = "Georgia", cex = legend_text_size), nrow = 1)
  
  plots_grob = arrangeGrob(grob_bars, grob_graph, nrow = 1, widths = c(4, 6))
  
  
  grid.arrange(titulo, leyenda, plots_grob, heights = c(1, 1, 15))
  
}


get_tfidf <- function(frequency_dat, filter_regular_expression = NULL, min_users = 2) {
  
  
  if (!is.null(filter_regular_expression)) {
    
    frequency_dat <- frequency_dat %>% 
      filter(!str_detect(word, filter_regular_expression))
    
  }
  
  frequency_dat %>% 
    filter(n_usuarios > 2) %>% 
    bind_tf_idf(word, comunidad, n)
  
  
}


plot_tfidf <- function(tfidf_data, top = 20, cols = 2, colores = 1:4) {
  
  tfidf_data %>% 
    group_by(comunidad) %>% 
    top_n(top, tf_idf) %>% 
    ggplot(aes(x = tf_idf, y = reorder_within(word, tf_idf, within = comunidad))) + 
    geom_col(aes(fill = comunidad), width = 0.75) + 
    scale_y_reordered() + 
    facet_wrap(~comunidad, scale = "free", ncol = cols) + 
    scale_fill_manual(values = plots_palette[colores]) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          text = element_text(family = "Georgia", color = "grey35", size = 14), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          legend.position = "top") 
  
}


# CARGA DE DATOS ----------------------------------------------------------

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
biden_hashtag <- readRDS(here::here("datos_procesados", "topics_processed", "biden_hashtag.rds"))
biden_frequency <- readRDS(here::here("datos_procesados", "topics_processed", "biden_hashtag_frequency.rds"))

# trump
trump_hashtag <- readRDS(here::here("datos_procesados", "topics_processed", "trump_hashtag.rds"))
trump_frequency <- readRDS(here::here("datos_procesados", "topics_processed", "trump_hashtag_frequency.rds"))

# DATA WRANGLING ----------------------------------------------------------

# healthcare
healthcare_unigram <- get_unigrams(healthcare,c("#healthcare", "healthcare"))
healthcare_hashtag <- get_hashtags(healthcare_unigram)
healthcare_frequency <- get_frequencies(healthcare_hashtag)


# climatechange
climate_unigram <- get_unigrams(climate, c("#climatechange", "#globalwarming"))
climate_hashtag <- get_hashtags(climate_unigram)
climate_frequency <- get_frequencies(climate_hashtag)


# education
education_unigram <- get_unigrams(education, c("#education", "education"))
education_hashtag <- get_hashtags(education_unigram)
education_frequency <- get_frequencies(education_hashtag) 



# CREACION DE REDES -------------------------------------------------------

# healthcare
gop_cor_data_healthcare <- get_correlation_data(healthcare_hashtag, healthcare_frequency, "GOP")
gop_cor_net_healthcare  <- get_correlation_net(gop_cor_data_healthcare)

dnc_cor_data_healthcare  <- get_correlation_data(healthcare_hashtag, healthcare_frequency, "DNC")
dnc_cor_net_heatlthcare  <- get_correlation_net(dnc_cor_data_healthcare)

prg_cor_data_healthcare  <- get_correlation_data(healthcare_hashtag, healthcare_frequency, "Progressives")
prg_cor_net_healthcare  <- get_correlation_net(prg_cor_data_healthcare)

healthcare_net <- bind_graphs(gop_cor_net_healthcare, dnc_cor_net_healthcare, prg_cor_net_healthcare) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))


# climatechange
gop_cor_data_climate <- get_correlation_data(climate_hashtag, climate_frequency, "GOP", top = 25)
gop_cor_net_climate  <- get_correlation_net(gop_cor_data_climate)

dnc_cor_data_climate  <- get_correlation_data(climate_hashtag, climate_frequency, "DNC", top = 25)
dnc_cor_net_climate  <- get_correlation_net(dnc_cor_data_climate)

prg_cor_data_climate  <- get_correlation_data(climate_hashtag, climate_frequency, "Progressives", top = 25)
prg_cor_net_climate  <- get_correlation_net(prg_cor_data_climate)

climate_net <- bind_graphs(gop_cor_net_climate, dnc_cor_net_climate, prg_cor_net_climate) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))


# education
gop_cor_data_education <- get_correlation_data(education_hashtag, education_frequency, "GOP", top = 30)
gop_cor_net_education  <- get_correlation_net(gop_cor_data_education)

dnc_cor_data_education  <- get_correlation_data(education_hashtag, education_frequency, "DNC", top = 30)
dnc_cor_net_education  <- get_correlation_net(dnc_cor_data_education)

prg_cor_data_education  <- get_correlation_data(education_hashtag, education_frequency, "Progressives", top = 30)
prg_cor_net_education  <- get_correlation_net(prg_cor_data_education)

education_net <- bind_graphs(gop_cor_net_education, dnc_cor_net_education, prg_cor_net_education) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))


# biden
gop_cor_data_biden <- get_correlation_data(biden_hashtag, biden_frequency, "GOP", top = 30)
gop_cor_net_biden  <- get_correlation_net(gop_cor_data_biden)

dnc_cor_data_biden  <- get_correlation_data(biden_hashtag, biden_frequency, "DNC", top = 30)
dnc_cor_net_biden  <- get_correlation_net(dnc_cor_data_biden)

prg_cor_data_biden  <- get_correlation_data(biden_hashtag, biden_frequency, "Progressives", top = 30)
prg_cor_net_biden  <- get_correlation_net(prg_cor_data_biden)

biden_net <- bind_graphs(gop_cor_net_biden, dnc_cor_net_biden, prg_cor_net_biden) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))


# trump
gop_cor_data_trump <- get_correlation_data(trump_hashtag, trump_frequency, "GOP", top = 30)
gop_cor_net_trump  <- get_correlation_net(gop_cor_data_trump)

dnc_cor_data_trump  <- get_correlation_data(trump_hashtag, trump_frequency, "DNC", top = 30)
dnc_cor_net_trump  <- get_correlation_net(dnc_cor_data_trump)

prg_cor_data_trump  <- get_correlation_data(trump_hashtag, trump_frequency, "Progressives", top = 30)
prg_cor_net_trump  <- get_correlation_net(prg_cor_data_trump)

trump_net <- bind_graphs(gop_cor_net_trump, dnc_cor_net_trump, prg_cor_net_trump) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))

# VISUALIZCIÓN DE TÓPICOS -------------------------------------------------

set.seed(123)
plot_topic_networks(healthcare_frequency, cor_net, text_size = 3, lab_size = 10, top_words = 15, node_size = 4, 
                    texto_titulo = "Tópicos relacionados con el topico healthcare")

plot_topic_networks(climate_frequency, climate_net, text_size = 4, lab_size = 12, top_words = 15, node_size = 4, 
                    texto_titulo = "Tópicos relacionados con el topico Climate Change", title_size = 1.4, legend_text_size = 1.2)

plot_topic_networks(education_frequency, education_net, text_size = 3.5, lab_size = 12, top_words = 15, node_size = 4, 
                    texto_titulo = "Tópicos relacionados con el tópico Education", title_size = 1.4, legend_text_size = 1.2)


plot_topic_networks(biden_frequency, biden_net, text_size = 3.2, lab_size = 12, top_words = 15, node_size = 4, 
                    texto_titulo = "Tópicos relacionados con el tópico Joe Biden", title_size = 1.4, legend_text_size = 1.2)

plot_topic_networks(trump_frequency, trump_net, text_size = 3.5, lab_size = 12, top_words = 15, node_size = 4, 
                    texto_titulo = "Tópicos relacionados con el tópico Donald Trump", title_size = 1.4, legend_text_size = 1.2)

# BAG OF WORDS - TFIDF ----------------------------------------------------

#climatechange
climate_tfidf_hashtags <- get_tfidf(climate_frequency %>% 
                                         filter(comunidad!= "Independent")) %>% 
  mutate(word = str_to_upper(str_remove(word, "#")))

plot_tfidf(climate_tfidf_hashtags, cols = 3, colores = c(1, 3, 4), top = 15) + 
  labs(title = "Climate change: tópicos característicos de cada comunidad", x = "TF-IDF")  + 
  theme(axis.title.y= element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 9),
        legend.title = element_blank(), 
        strip.text = element_text(size = 14)) + 
  guides(fill = F)


# education
education_tfidf_hashtags <- get_tfidf(education_frequency %>% 
                                      filter(comunidad!= "Independent")) %>% 
  mutate(word = str_to_upper(str_remove(word, "#")))

plot_tfidf(education_tfidf_hashtags, cols = 3, colores = c(1, 3, 4), top = 15) + 
  labs(title = "Education: tópicos característicos de cada comunidad", x = "TF-IDF") + 
  theme(axis.title.y= element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 9),
        legend.title = element_blank(), 
        strip.text = element_text(size = 14)) + 
  guides(fill = F)


# joe biden
# education
biden_tfidf_hashtags <- get_tfidf(biden_frequency %>% 
                                        filter(comunidad!= "Independent")) %>% 
  mutate(word = str_to_upper(str_remove(word, "#")))

plot_tfidf(biden_tfidf_hashtags, cols = 3, colores = c(1, 3, 4), top = 15) + 
  labs(title = "Joe Biden: tópicos característicos de cada comunidad", x = "TF-IDF") + 
  theme(axis.title.y= element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        legend.title = element_blank(), 
        strip.text = element_text(size = 14), 
        plot.title = element_text(size = 18)) + 
  guides(fill = F)

