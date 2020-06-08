# 04-get_topic_data.R


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


# CORRELACION DE TERMINOS -------------------------------------------------

## correlacion de palabras ##

# datos de correlacion puros (sin filtrar por limites de palabras etc)

gop_cor_net <- get_correlation_network(healthcare_hashtag, healthcare_frequency, com = "GOP", exclude_words = c("#healthcare", "#health"), cor_limit = 0.25) %>% 
  activate(nodes) %>% mutate(comunidad = "GOP")
dnc_cor_net <- get_correlation_network(healthcare_hashtag, healthcare_frequency, com = "DNC", exclude_words = c("#healthcare", "#health"), cor_limit = 0.25, top_words = 15) %>% 
  activate(nodes) %>% mutate(comunidad = "DNC")
prg_cor_net <- get_correlation_network(healthcare_hashtag, healthcare_frequency, com = "Progressives", exclude_words = c("#healthcare", "#health"), cor_limit = 0.25, top_words = 15) %>% 
  activate(nodes) %>% mutate(comunidad = "Progressives")

healthcare_net <- bind_graphs(gop_cor_net, dnc_cor_net, prg_cor_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))

healthcare_net %>% 
  activate(edges) %>% 
  filter(correlation > 0.15) %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(pch = 21, aes(size = n, fill = comunidad)) +
  geom_node_text( aes(label = name, color = comunidad), repel = TRUE, size = 3) +
  facet_nodes(~comunidad, ncol = 1, scales = "free") + 
  scale_fill_manual(values = plots_palette[c(1, 3, 4)]) +
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
  facet_nodes(~comunidad, ncol = 1, scales = "free") +
  theme_void() + 
  guides(color = F, fill = F) + 
  theme(strip.text = element_text(size = 16))

set.seed(123)
plot_topic_networks(healthcare_frequency, healthcare_net, layout_style = "fr", text_size = 3)



healthcare_hashtag %>% 
  filter(comunidad == "GOP") %>% 
  inner_join(healthcare_frequency %>%
               filter(comunidad == "GOP", n_usuarios > 4) %>% 
               top_n(40, wt = n),
             by = c("word")) %>% 
  pairwise_cor(word, status_id, sort = T) %>% 
  filter(correlation > 0.1) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = correlation), show.legend = F) + 
  geom_node_point(color = "steelblue", size = 5) + 
  geom_node_text(aes(label = name), repel = TRUE) + 
  theme_void()


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

cor_net <- bind_graphs(gop_cor_net, dnc_cor_net, prg_cor_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))

plot_correlation_network(cor_net)


gop_cor_data <- get_correlation_data(healthcare_hashtag, healthcare_frequency, "GOP")
gop_cor_net <- get_correlation_net(gop_cor_data)

dnc_cor_data <- get_correlation_data(healthcare_hashtag, healthcare_frequency, "DNC")
dnc_cor_net <- get_correlation_net(dnc_cor_data)

prg_cor_data <- get_correlation_data(healthcare_hashtag, healthcare_frequency, "Progressives")
prg_cor_net <- get_correlation_net(prg_cor_data)




plot_topics(healthcare_frequency %>% filter(comunidad != "Independent"),
            filtered_words = "#healthcare", cols = 1, colores = c(1, 3, 4))



plot_topic_networks <- function(dat_frequency, dat_graph, filtered_words = "", texto_titulo = "",
                                minim_n = 3, lab_size = 8, text_size = 5, layout_style = "fr", top_words = 20, 
                                node_size = 3, random_state = 123) {
  
  set.seed(random_state)
  
  grob_bars <- ggplotGrob(plot_topics(dat_frequency %>% filter(comunidad != "Independent"), 
                                      filtered_words, cols = 1, min_users = minim_n,
                                      colores = c(1, 3, 4), label_size = lab_size,
                                      top_words = top_words)) 

  
  
  grob_graph <- ggplotGrob(dat_graph %>% plot_correlation_network(text_size = text_size, node_size = node_size))

  titulo <-  textGrob(label = texto_titulo, gp = gpar(fontfamily = "Georgia", cex = 1.2))
  
  leyenda <- legendGrob(labels = c("GOP",  "DNC", "Progressives"),  pch = 21, 
                        gp = gpar(fill = plots_palette[c(1, 3, 4)], fontfamily = "Georgia", cex = 1), nrow = 1)
  
  plots_grob = arrangeGrob(grob_bars, grob_graph, nrow = 1, widths = c(4, 6))
  
  
  grid.arrange(titulo, leyenda, plots_grob, heights = c(1, 1, 15))
  
}

plot_topic_networks(healthcare_frequency, cor_net, text_size = 3, lab_size = 10, top_words = 15, node_size = 4, 
                    texto_titulo = "Tópicos relacionados con el hashtag #healthcare")





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



cum_freq <- healthcare_frequency %>% 
  arrange(desc(n)) %>% 
  mutate(cum_n = cumsum(n)) %>% 
  mutate(cum_f = cum_n / sum(n))




# NEDUCARE FOR ALL --------------------------------------------------------

# carga de datos
mfa <- readRDS(here::here("datos_procesados", "formated_text_df_data", "medicareforall.rds"))

# Exclusion de tweets con terminos referentes a otras políticas sanitarias
exclusion_mfa <- "#medicaid|#aca|[Aa]ffordable ?[Cc]are ?[Aa]ct|#publicoption|[Pp]ublic ?[Oo]ption|#medicare( |$|#)|#?COVID|#[Pp]andemic"
mfa <- mfa %>% 
  filter(discriminate_term(text, "#medicareforall|#m4a", exclusion_mfa))

mfa_sent <- mfa %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment), 
            n = n()) %>% 
  mutate(f = percent(n/sum(n), accuracy = 0.1))
mfa_sent


# limpieza de topicos
mfa_healthcare_clean <- mfa %>% 
  mutate(text = str_replace_all(text, "single payer", "singlepayer")) %>% 
  mutate(text = str_replace_all(text, "public option", "publicoption")) %>% 
  mutate(text = str_replace_all(text, "(obama ?care)|(affordable ?care ?act)", "aca")) %>% 
  mutate(text = str_replace_all(text, "#", "")) %>% 
  filter(str_detect(text, "singlepayer|publicoption|aca|medicaid|medicare"))


# creacion de tabla de unigrams
mfa_clean_unigram <- get_unigrams(mfa_healthcare_clean %>% filter(comunidad != "Independent")) %>% 
  filter(word %in% c("singlepayer", "publicoption", "aca", "medicaid", "medicare"))

# tabla de frecuencias de los uunigrams
mfa_clean_freq <- get_frequencies(mfa_clean_unigram) %>% 
  left_join(mfa %>% 
              group_by(comunidad) %>% 
              count(name = "tweets"), by = "comunidad") %>% 
  mutate(f = n / tweets)
  
mfa_clean_freq %>% 
  group_by(comunidad, word) %>% 
  summarise(n = sum(n)) %>% 
  mutate(f = n / sum(n))


mfa_clean_freq %>% 
  ungroup() %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP",  "DNC", "Progressives"))) %>% 
  ggplot() + 
  geom_rect(data = data.frame(xmin = 0.67, 
                              xmax = 1.33,  ymin = 0,
                              ymax = 1, 
                              comunidad = factor(c("GOP",  "DNC", "Progressives"))), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = plots_palette[c(1, 3, 4)], alpha = 1,
            color = "white") + 
  geom_col(aes(x = 1, y = f,fill = fct_reorder2(word, f, comunidad)), position = "fill",
           color = "white", width = 0.6) + 
  geom_text(x = 1, y = 0.9, aes(label = comunidad, color = comunidad), 
            family = "Georgia", 
            size = 4) +
  labs(fill = "Topic") +
  facet_wrap(~comunidad, nrow = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_fill_brewer() + 
  guides(color = F) +
  theme_bw() +
  theme(
    text = element_text(family = "Georgia", color = "grey35"),
    plot.title = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_blank(), 
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "top",
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank()) + 
  scale_x_continuous(limits = c(0.5, 1.5)) + 
  scale_y_continuous(label = scales::percent) + 
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
  labs(title = "De qué otras políticas sanitaras hablan las comunidades cuando hablan de Medicare for All")


# BAG OF WORDS - TF-IDF ---------------------------------------------------

healthcare_tfidf_hashtags <- get_tfidf(healthcare_frequency %>% 
                                         filter(comunidad!= "Independent")) %>% 
  mutate(word = str_to_upper(str_remove(word, "#")))


plot_tfidf(healthcare_tfidf_hashtags, cols = 3, colores = c(1, 3, 4), top = 15) + 
  labs(title = "Tópicos característicos de cada comunidad", x = "TF-IDF") + 
  theme(axis.title.y= element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.title = element_blank())

# DIFERENCIA MEDIAS SENTIMIENTO ------------------------------

## SINGLE PAYER ##

# carga de datos
sp <- readRDS(here::here("datos_procesados", "formated_text_df_data", "singlepayer.rds")) %>% 
  mutate(comunidad = fct_relevel(comunidad, comunidad_order))

# filtro de tweets con politicas sanitarias no compatibles con sigle payer
exclusion_sp <- "#medicaid|#aca|[Aa]ffordable ?[Cc]are ?[Aa]ct|#publicoption|[Pp]ublic ?[Oo]ption|#medicare( |$|#)|#COVID|COVID"
sp <- sp %>% 
  filter(discriminate_term(text, "[Ss]ingle ?[Pp]ayer", exclusion_sp))


# test diferencias DNC Progressives
test_sp_dnc_prog <- get_test_dif(sp, groups = c("DNC", "Progressives"))
test_sp_dnc_prog[-1]

test_sp_gop_prog <- get_test_dif(sp, groups = c("GOP", "Progressives"))
test_sp_gop_prog[-1]


plot_gop_prog <- test_sp_gop_prog$data %>% 
  ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.002) + 
  scale_fill_manual(values = plots_palette[c(1, 4)]) +
  labs(x = "Puntuacion sentimiento medio por tweet", 
       title = "Replicaciónes bootstrap del sentimiento medio para GOP y Progressives") + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "top",
        axis.title.y = element_blank(),
        text = element_text(family = "Georgia", color = "grey55")) +
  geom_vline(xintercept = 0, lty = "dashed") +
  scale_x_continuous(limits = c(-0.1, 0.1)) + 
  theme(plot.title = element_blank()) + 
  guides(fill = F)


plot_dnc_prog <- test_sp_dnc_prog$data %>% 
  ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.002) + 
  scale_fill_manual(values = plots_palette[3:4]) + 
  labs(x = "Puntuacion sentimiento medio por tweet", 
       title = "Replicaciónes bootstrap del sentimiento medio para DNC y Progressives") + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "top",
        axis.title.y = element_blank(),
        text = element_text(family = "Georgia", color = "grey55")) +
  geom_vline(xintercept = 0, lty = "dashed") +
  scale_x_continuous(limits = c(-0.1, 0.1))+ 
  theme(plot.title = element_blank(), 
        axis.title.x = element_blank()) + 
  guides(fill = F)

titulo <-  textGrob(label = "Distribución de muestreo empírica del scoring de sentimiento medio", gp = gpar(fontfamily = "Georgia", cex = 1.2))

leyenda <- legendGrob(labels = c("GOP",  "DNC", "Progressives"),  pch = 21, 
                      gp = gpar(fill = plots_palette[c(1, 3, 4)], fontfamily = "Georgia", cex = 1), nrow = 1)


grid.arrange(titulo, leyenda, plot_dnc_prog, plot_gop_prog, heights = c(1, 1, 5, 5))


## GREEN NEW DEAL ##

# he filtrado fuera temas algunos temas políticos porque hay demasiada ironía
green_sent <- readRDS(here::here("datos_procesados", "formated_sent_df_data", "greennewdeal_sent.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic|#chinavirus|#wuhamvirus", negate = T))

green_sent %>% 
  filter(!is_retweet) %>% 
  group_by(comunidad) %>% 
  summarise(n = n(),
            sent = mean(ave_sentiment)) %>% 
  mutate(f = n/sum(n)) %>% 
  select(comunidad, n, f, sent)

green_sent %>% 
  ggplot(aes(comunidad, ave_sentiment, fill = comunidad)) + 
  geom_boxplot()

# test diferencias DNC Progressives
test_sp_dnc_prog <- get_test_dif(green_sent, groups = c("DNC", "Progressives"))
test_sp_dnc_prog[-1]

test_sp_gop_prog <- get_test_dif(green_sent, groups = c("GOP", "Progressives"))
test_sp_gop_prog[-1]

# RUEDA DE EMOCIONES ------------------------------------------------------

sp_unigram <- get_unigrams(sp %>% filter(comunidad != "Independent"), filter_regular_expression = exclusion_sp) %>% 
  mutate(word = str_remove(word, "^#"))

sp_nrc <- sp_unigram %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(comunidad, sentiment, sort = T) %>% 
  mutate(sentiment = str_to_title(sentiment)) %>% 
  ungroup() %>% 
  group_by(comunidad) %>% 
  mutate(n = rescale(n)) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  select(comunidad, Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, Trust) %>% 
  ungroup() %>% 
  mutate(comunidad = fct_drop(comunidad))

ggradar(sp_nrc, group.colours = plots_palette[c(1, 3, 4)], legend.position = "top",
        group.point.size = 4,
        plot.title = "Sentimientos expresados hacia el tópico 'Single payer'", 
        legend.text.size = 11, axis.label.size = 4) + 
  theme(plot.title = element_text(size = 14, family = "Georgia", color = "grey35"),
        axis.title = element_text(family = "Georgia", size = 9))




# POLARIZACIÓN ------------------------------------------------------------


pol_test_dif <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "health_polarization.rds"))


# Nivel de polarizacion mínimo en ambos casos pero se hallan diferencias significativas
pol_test_dif$data %>% 
  ggplot(aes(x = sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", alpha = 0.5, binwidth = 0.0017, position = "identity") + 
  scale_fill_manual(values = c(plots_palette[1], plots_palette[4])) + 
  labs(x = "Polarization score", title = "Distribución empírica del scoring medio de polarización para los nodos frontera") + 
  theme_bw() + 
  labs(x = "Scoring de polarización", y = "Conteo") +
  geom_vline(xintercept = 0, lty = "dashed") +
  theme(text = element_text(family = "Georgia", color = "grey35"),
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "top")



# HASHTAGS COMUNES --------------------------------------------------------

top_health <- healthcare_frequency %>% 
  group_by(comunidad) %>% 
  filter(n_usuarios > 3) %>% 
  top_n(40, n) %>% 
  arrange(desc(n))

top_gop <- top_health[top_health$comunidad == "GOP", "word", drop = T][1:40]
top_dnc <- top_health[top_health$comunidad == "DNC", "word", drop = T][1:40]
top_prg <- top_health[top_health$comunidad == "Progressives", "word", drop = T][1:40]


get_common_terms <- function(set1, set2) {
  
  if (length(set1) != length(set2)) stop("Los vectores tienen que ser de la misma longitud")
  
  common_words <- integer(length = length(set1))
  
  for (idx in 1:length(set1)) {
    
    subset1 <- set1[1:idx]
    subset2 <- set2[1:idx]
    
    common_words[idx] <- length(intersect(subset1, subset2))
    
    
  }
  
  common_words
}


common_dnc_gop <- get_common_terms(top_dnc, top_gop)
common_dnc_prg <- get_common_terms(top_dnc, top_prg)

common <- data.frame(
  top_words = rep(1:length(common_dnc_gop), 2),
  common_words = c(common_dnc_gop, common_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Healthcare"
)


common %>% 
  ggplot(aes(top_words, round(common_words), color = comparacion)) + 
  geom_step(size = 1.3) + 
  # geom_point() + 
  scale_color_manual(values = plots_palette[c(1, 4)]) + 
  labs(title = "Hashtags comunes de entre DNC y las otras dos comunidades", 
       x = "Ranking de los 40 hashtags más usados", 
       y = "Número de hashtags comunes") + 
  theme_bw() + 
  theme(text = element_text(family = "Georgia", color = "grey45"), 
        legend.title = element_blank(),
        legend.position = "top",
        panel.border = element_blank()) + 
  scale_x_continuous(limits = c(1, 40), breaks = seq(0, 40, by = 10)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2))
