# 01-EDA_healthcare.R

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


# CARGA DE DATOS ----------------------------------------------------------
healthcare <- readRDS(here::here("datos_procesados", "formated_text_df_data", "healthcare.rds")) %>% 
  filter(str_detect(text, "#?covid19|#?COVID|#?[Pp]andemic", negate = T)) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")))


# DISTRIBUCION DE TWEETS --------------------------------------------------
freq_tweets_usuario <- get_tweet_distribution(healthcare) 

healthcare %>% 
  count(comunidad,) %>% 
  group_by(comunidad) %>% 
  summarise(tweets = sum(n))

freq_tweets_usuario %>% 
  arrange(desc(tweets)) %>% 
  mutate(tweets_acum = cumsum(tweets), 
            usuarios_acum = cumsum(usuarios)) %>% 
  mutate(por_tweets_acum = tweets_acum / sum(tweets), 
         por_usuarios_acum = usuarios_acum / sum(usuarios)) %>% 
  mutate(int_users_acum = cut(por_usuarios_acum, breaks = seq(0, 1, by = 0.05))) %>% 
  group_by(comunidad, int_users_acum) %>%  
  summarise(porc_tweets = sum(porc_tweets)) %>% 
  mutate(porc_tweets_acum = cumsum(porc_tweets)) 


# Curva de lorenz
freq_tweets_usuario %>% 
  ggplot(aes(x = porc_usuarios_acum, y = porc_tweets_acum)) + 
  geom_line(aes(color = comunidad), size = 1.3) + 
  scale_color_manual(values = plots_palette) 


# gini
gini_comunidades <- freq_tweets_usuario %>% 
  group_by(comunidad) %>% 
  summarise(gini = gini(x = tweets, weights = porc_usuarios)) %>% 
  arrange(desc(gini))
  

# FRECUENCIA DE PALABRAS --------------------------------------------------


# datos tidy
healthcare_unigram <- get_unigrams(healthcare,c("#healthcare", "healthcare"))
healthcare_hashtag <- get_hashtags(healthcare_unigram)
healthcare_frequency <- get_frequencies(healthcare_hashtag)

## plotting ##
plot_topics(healthcare_unigram %>% get_frequencies(), c("#health"))
plot_topics(healthcare_frequency, c("#covid19", "#health"))

# healthcare_bigram <- healthcare  %>% 
#   unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>% 
#   separate(bigram, c("word1", "word2"), sep = " ", remove = F) %>% 
#   filter(!word1 %in% stop_words$word) %>% 
#   filter(!word2 %in% stop_words$word)

ggsave(here::here("figuras_exploratorias", "healthcare", "bar_hashtag.png"), width = 14, height = 10)

## Topics con networks ##

gop_hash_net <- create_hashtag_graph(healthcare_hashtag, "GOP", c("#health"), minim_n = 3)
dnc_hash_net <- create_hashtag_graph(healthcare_hashtag, "DNC", c("#health"), minim_n = 3)
prg_hash_net <- create_hashtag_graph(healthcare_hashtag, "Progressives", c("#health"), minim_n = 3)
# ind_hash_net <- create_hashtag_graph(healthcare_hashtag, "Independent", c("#health"), 15, minim_n = 3)

health_graph <- bind_graphs(gop_hash_net, dnc_hash_net, prg_hash_net) %>% 
  activate(nodes) %>% 
  mutate(comunidad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))

grob_graph <- ggplotGrob(health_graph %>% 
  ggraph(layout = "nicely") +
  geom_edge_link(alpha = 0.3) + 
  geom_node_point(alpha = 0.3) + 
  # geom_node_label(aes(label = nodes, fill = comunidad), size = 2.5, family = "Candara", color = "white") + 
    geom_node_text(aes(label = nodes, color = comunidad), size = 3, family = "Candara", repel = T) +
  facet_nodes(~comunidad, ncol = 1, scales = "free") +
  theme_bw() + 
  guides(fill = F) +
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(family = "Georgia", size = 11, color = "white"),
          panel.border = element_blank(),
          axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank()))


grob_bars <- ggplotGrob(plot_topics(healthcare_frequency %>% filter(comunidad != "Independent"), c("#health"), cols = 1, colores = c(1, 3, 4)) +
                          guides(fill = F))

titulo <-  textGrob(label = "Hastags más mencionados con el tópico 'Healthcare'", gp = gpar(fontfamily = "Georgia", cex = 1.5))
leyenda <- legendGrob(labels = c("GOP",  "DNC", "Progressives"),  pch = 21, 
                      gp = gpar(fill = plots_palette[c(1, 3, 4)], fontfamily = "Georgia", cex = 1), nrow = 1)

plots_grob = arrangeGrob(grob_bars, grob_graph, nrow = 1, widths = c(4, 6))

grid.arrange(titulo, leyenda, plots_grob, heights = c(1, 1, 8))
# En el Gop: politica. temas de china. Truth: teorias de la conspiracion
# Independents: tecnología, telemedicina
# DNC: Medicaid y medicare. Politica. Promocion democratas
# Ptrogrsives: medicare for all. Atención domiciliaria: homecare workers (rise up home care es agencia de at. dom.)
# reivindicacion. Pagar a los healthcare workers atención domiciliaria


# SCATTERPLOTS ------------------------------------------------------------

# scatterplot DNC vs Progressives
healthcare_frequency <- healthcare_unigram %>% 
  group_by(comunidad) %>% 
  count(word, sort = TRUE) %>% 
  left_join(healthcare_unigram %>% 
              group_by(comunidad) %>% 
              summarise(total = n())) %>% 
  mutate(freq = n / total)%>% 
  select(-n, -total)

 # scatterplot
healthcare_frequency %>% 
  filter(str_detect(word, "#")) %>% 
  

  pivot_wider(names_from = comunidad, values_from = freq, values_fill = list(freq = 0)) %>% 
  ggplot(aes(DNC, Progressives)) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = -1) + 
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) + 
  geom_abline(color = "red")
  

# word ratio bars
healthcare_ratios <- healthcare_unigram %>% 
  filter(!str_detect(word, "@")) %>% 
  count(word, comunidad) %>% 
  group_by(word) %>% 
  filter(sum(n) >= 10) %>% 
  ungroup() %>% 
  pivot_wider(names_from = comunidad, values_from = n, values_fill = list(n = 0)) %>% 
  mutate_if(is.numeric, list(~(. + 1) / sum(.) + 1)) %>% 
  mutate(logratio = log(DNC / Progressives)) %>% 
  arrange(desc(logratio))

healthcare_ratios %>% 
    group_by(logratio < 0) %>% 
    top_n(20, abs(logratio)) %>% 
    ungroup() %>% 
    mutate(word = reorder(word, logratio)) %>% 
  ggplot(aes(logratio, word, fill = logratio < 0)) + 
  geom_col(show.legend = FALSE) + 
  scale_fill_manual(values = plots_palette[3:4]) 
 
  

  
# word ratio bars hasthtags
healthcare_ratios %>% 
  filter(str_detect(word, "^#")) %>% 
  group_by(logratio < 0) %>% 
  top_n(20, abs(logratio)) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, logratio)) %>% 
  ggplot(aes(logratio, word, fill = logratio < 0)) + 
  geom_col(show.legend = FALSE) + 
  scale_fill_manual(values = plots_palette[3:4]) +
  theme_tfm() 

healthcare_bigram %>%
  count(comunidad, bigram) %>%
  group_by(comunidad) %>%
  top_n(20, n) %>%
  ggplot(aes(x = n, y = reorder_within(bigram, n, within = comunidad))) +
  geom_col(aes(fill = comunidad)) +
  scale_y_reordered() +
  facet_wrap(~comunidad, scale = "free")



# BAG OF WORDS ------------------------------------------------------------

# ESTO IRA BIEN PARA MODELIZR
# PODRIAMOS MODELIZAR EN BASE A TWEETS Y NO A USUARIOS
# HACER FEATURING: SI CON ESTE TOPIC (p. ej healthcare) HABLA TAMBIEN DE ESTE TEMA (p ej, workers)

# A QUIEN RETUITEAN Y AQUIEN MENCIONAN TB PUEDE SER  UTIL PARA MODELO DE PREDICICON
# TB CUANTOS TWEETS SCRIBEN EN PROPORCION AL TOTAL
# TB CUANTO HASHTAGS METEN EN CADA TWEET

healthcare_tfidf_hashtags <- get_tfidf(healthcare_frequency)
healthcare_tfidf <- get_tfidf(healthcare_unigram %>% get_frequencies())

healthcare_tfidf_nomentions <- get_tfidf(healthcare_unigram %>% filter(str_detect(word, "^@", negate = T)) %>% get_frequencies())
plot_tfidf(healthcare_tfidf)
plot_tfidf(healthcare_tfidf_nomentions)
plot_tfidf(healthcare_tfidf_hashtags)



# TOPIC MODELING ----------------------------------------------------------

## lda sobre todos los grupos ##

# matriz dtm
healthcare_dtm <- healthcare_unigram %>% 
  
  # eliminamos temporalmente covid
  filter(str_detect(word, "covid", negate = TRUE)) %>% 
  count(status_id, word) %>% 
  filter(str_detect(word, "^#")) %>% 
  cast_dtm(status_id, word, n)


# lda
healthcare_lda <- LDA(healthcare_dtm, k = 5, control = list(seed = 1234))


# lda_tidy
healthcare_topics <- tidy(healthcare_lda, matrix = "beta")

# lda per topic
healthcare_topics_document <- tidy(healthcare_lda, matrix = "gamma") %>% 
  left_join(healthcare_cleaned_text %>% 
              select(status_id, comunidad), by = c("document" = "status_id"))
  

# grafico de toppalabras para cada topico
healthcare_topics %>% 
  group_by(topic) %>% 
  top_n(20, beta) %>% 
  ggplot(aes(x = beta, y = reorder_within(term, beta, within =topic))) + 
  geom_col(aes(fill = as.factor(topic))) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered() + 
#  scale_fill_manual(values = plots_palette) + 
  theme_tfm()


# boxplot con probabilidades de cada topico para cada comunidad
healthcare_topics_document %>% 
  ggplot(aes(x = factor(topic), y = gamma)) + 
  geom_boxplot() + 
  facet_wrap(~comunidad)

# cuatro temas: covid19 politica, trabajadores sanitarios, salud en general, telesalud, tecnología





# -------------------- medicareforall ------------------------------


# medicare_unigram <- medicare %>% 
#   mutate(text = str_remove_all(text, remove_reg)) %>% 
#   unnest_tokens(word, text, token = "tweets") %>% 
#   filter(!word %in% stop_words$word) %>% 
#   filter(!word %in% c("#healthcare", "healthcare")) %>% 
#   filter(!str_detect(word, "#aca.+"))
# 
# medicare_frequency <- medicare_unigram %>% 
#   count(comunidad, word)

# carga de datos
mfa <- readRDS(here::here("datos_procesados", "formated_text_df_data", "medicareforall.rds"))

# filtro de terminos

exclusion_mfa <- "#medicaid|#aca|[Aa]ffordable ?[Cc]are ?[Aa]ct|#publicoption|[Pp]ublic ?[Oo]ption|#medicare( |$|#)|#?COVID|#[Pp]andemic"
mfa <- mfa %>% 
  filter(discriminate_term(text, "#medicareforall|#m4a|#singlepayer", exclusion_mfa))

mfa_sent <- mfa %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment), 
            n = n()) %>% 
  mutate(f = percent(n/sum(n), accuracy = 0.1))



# NO HAY DIFERENCIAS

# ----------- ## singlepayer ## -----------

sp <- readRDS(here::here("datos_procesados", "formated_text_df_data", "singlepayer.rds")) %>% 
  mutate(comunidad = fct_relevel(comunidad, comunidad_order))
exclusion_sp<- "#medicaid|#aca|[Aa]ffordable ?[Cc]are ?[Aa]ct|#publicoption|[Pp]ublic ?[Oo]ption|#medicare( |$|#)|#COVID|COVID"

sp <- sp %>% 
  filter(discriminate_term(text, "[Ss]ingle ?[Pp]ayer", exclusion_sp))


sp_sent <- sp %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment), 
            n = n()) %>% 
  mutate(f = percent(n / sum(n), accuracy = 0.1))

# Boxplogs
sp %>% 
  ggplot(aes(x = comunidad, y = ave_sentiment)) + 
  geom_boxplot(aes(fill = comunidad), alpha = 0.7) + 
  scale_fill_manual(values = plots_palette)
  
# test diferencias DNC Progressives
test_sp_dnc_prog <- get_test_dif(sp, c("DNC", "Progressives"))

test_sp_dnc_prog$data %>% 
ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.002) + 
  scale_fill_manual(values = plots_palette[3:4])

# test diferencias GOP Progressives
set.seed(123)
test_sp_gop_prog <- get_test_dif(sp, c("GOP", "Progressives"))

test_sp_gop_prog$data %>% 
  ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.002) + 
  scale_fill_manual(values = plots_palette[c(1, 4)])


sp_unigram <- get_unigrams(sp, filter_regular_expression = exclusion_sp) %>% 
  mutate(word = str_remove(word, "^#"))

# health topics
mfa_healthcare_clean <- mfa %>% 
  mutate(text = str_replace_all(text, "single payer", "singlepayer")) %>% 
  mutate(text = str_replace_all(text, "public option", "publicoption")) %>% 
  mutate(text = str_replace_all(text, "(obama ?care)|(affordable ?care ?act)", "aca")) %>% 
  mutate(text = str_replace_all(text, "#", "")) %>% 
  filter(str_detect(text, "singlepayer|publicoption|aca|medicaid|medicare"))
  
mfa_clean_unigram <- get_unigrams(mfa_healthcare_clean %>% filter(comunidad != "Independent")) %>% 
  filter(word %in% c("singlepayer", "publicoption", "aca", "medicaid", "medicare"))

mfa_clean_freq <- get_frequencies(mfa_clean_unigram) %>% 
  left_join(mfa %>% 
             group_by(comunidad) %>% 
             count(name = "tweets"), by = "comunidad") %>% 
  mutate(f = n / tweets)




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
            size = 6) +
  labs(fill = "Topic") +
  facet_wrap(~comunidad, nrow = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  scale_fill_brewer() + 
  guides(color = F) +
  theme_bw() +
  theme(
    text = element_text(family = "Georgia", color = "grey55"),
    panel.grid = element_blank(),
        panel.border = element_blank(), 
        strip.background = element_blank(),
    strip.text = element_blank(),
        legend.position = "left",
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
    axis.title.y = element_blank()) + 
  scale_x_continuous(limits = c(0.5, 1.5)) + 
  scale_y_continuous(label = scales::percent) + 
  scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
  labs(title = "De qué otras políticas sanitaras hablan las comunidades cuando hablan de Medicare for All")



healthcare_topics <- c("#?medicare|#?medicaid|#?obamacare|#?aca|#?public ?option", "#?siglepayer")


sp_nrc <- sp_unigram %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(comunidad, sentiment, sort = T) %>% 
  mutate(sentiment = str_to_title(sentiment)) %>% 
  ungroup() %>% 
  group_by(comunidad) %>% 
  mutate(n = rescale(n)) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  select(comunidad, Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, Trust)

ggradar(sp_nrc, group.colours = plots_palette, legend.position = "top",
        group.point.size = 4,
        plot.title = "Sentimientos expresados hacia el tópico 'Single payer'", legend.text.size = 11) + 
  theme(plot.title = element_text(size = 12, family = "Georgia", color = "grey55"),
        axis.title = element_text(family = "Georgia"))


## public option ##

# POCAS VARIABLES PARA ANALISIS
po <- readRDS(here::here("datos_procesados", "formated_text_df_data", "publicoption.rds")) %>% 
  mutate(comunidad = fct_relevel(comunidad, comunidad_order))
exclusion_po <- "#?[Mm]edicare ?[Ff]or ?[Aa]ll|#mfa|#?[Ss]ingle ?[Pp]ayer"

po <- po %>% 
  filter(discriminate_term(text, "[Ss]ingle ?[Pp]ayer|", exclusion_mfa))


# ALTERNATIVA: FILTRAMOS EN HEALTHCARE PALABRAS PRODEMOCRATAS Y PALABRAS PRO PROGRESIVES

dnc_terms <- "#?[Pp]ublic ?[Oo]ption|#?[Oo]bama ?[Cc]are|#aca|#?[Mm]edicare( |$|#)|#?|#?[Mm]edicaid"
prog_terms <- "#?[Mm]edicare ?[Ff]or ?[Aa]ll|#m[f4]a|#?[Ss]ingle ?[Pp]ayer"


# PRO DNC HEALTHCARE #
dnc_health <- healthcare %>% 
  filter(discriminate_term(text, dnc_terms, paste(prog_terms, "|COVID")))

dnc_health_sent <- dnc_health %>% 
  group_by(comunidad) %>% 
  summarise(sent = mean(ave_sentiment))

# Boxplots
dnc_health %>% 
  ggplot(aes(x = comunidad, y = ave_sentiment)) + 
  geom_boxplot(aes(fill = comunidad), alpha = 0.7) + 
  scale_fill_manual(values = plots_palette)

# test diferencias DNC Progressives
test_sp_dnc_health <- get_test_dif(dnc_health, c("DNC", "Progressives"))

test_sp_dnc_health$data %>% 
  ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.0002) + 
  scale_fill_manual(values = plots_palette[3:4])

# test diferencias GOP Progressives
test_sp_dnc_gop_health <- get_test_dif(sp, c("GOP", "Progressives"))

test_sp_dnc_gop_health$data %>% 
  ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.002) + 
  scale_fill_manual(values = plots_palette[c(1, 4)])




# --------------- ## aca ## ---------------------------

# HAY QUE QUITAR COVID Y ASOCIADOS POR CONNOTACIONES NEGATIVAS

aca <- healthcare %>% 
  filter(discriminate_term(text, "#aca", "#medicareforall|#covid|#pandemic"))

aca %>% 
  group_by(comunidad) %>% 
  mutate(text = str_replace(text, "#", "")) %>% 
  summarise(sentimiento = mean(ave_sentiment), 
            n = n())

aca %>% 
  ggplot(aes(comunidad, ave_sentiment)) +
  geom_boxplot()


sample_dnc <- boot(aca$ave_sentiment[aca$comunidad == "DNC"], function(x, i) mean(x[i]), R = 5000)
sample_prog <- boot(aca$ave_sentiment[aca$comunidad == "Progressives"], function(x, i) mean(x[i]), R =5000)

# no hay efecto significativo
quantile(sample_dnc$t, probs = c(0.025, 0.975))
quantile(sample_prog$t, probs = c(0.025, 0.975))


aca_boot <- data.frame(comunidad = rep(c("DNC", "Progresives"), each = 5000), sentimiento = c(sample_dnc$t, sample_prog$t))

aca_boot %>% 
  ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.0008) + 
  scale_fill_manual(values = plots_palette[3:4])





aca_boot <- data.frame(comunidad = rep(c("DNC", "Progresives"), each = 5000), sentimiento = c(sample_dnc$t, sample_prog$t))

aca_boot %>% 
  ggplot(aes(sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", position = "identity", alpha = 0.6, binwidth =0.0008) + 
  scale_fill_manual(values = plots_palette[3:4])



# POLARIZACIÓN ------------------------------------------------------------

hlt_net <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "healthcare_pol.rds"))
comunidad <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
  mutate(user_id = as.character(user_id))


# paleta
plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))


# filtro comunidades
hlt_gop_dnc <- hlt_net %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos <- get_inner_nodes(hlt_gop_dnc)

# datos polaridad
hlt_gop_dnc_pol <- map_df(unique(hlt_gop_dnc$from), ~get_node_pol_data(hlt_gop_dnc, nodos_internos, .x))

mean(hlt_gop_dnc_pol$polarity_score)


# Progressives VS DNC --------------------------------------------------------------

# filtro comunidades
hlt_prg_dnc <- hlt_net %>%
  filter_communities("DNC", "Progressives")

# nodos internos
nodos_internos <- get_inner_nodes(hlt_prg_dnc)

# datos polaridad
hlt_prg_dnc_pol <- map_df(unique(hlt_prg_dnc$from), ~get_node_pol_data(hlt_prg_dnc, nodos_internos, .x))

mean(hlt_prg_dnc_pol$polarity_score)

test_data <- hlt_gop_dnc_pol %>% 
  mutate(comunidades = "DNC-GOP") %>% 
  bind_rows(hlt_prg_dnc_pol %>% mutate(comunidades = "DNC-PRG")) %>% 
  select(nodo, comunidades, polarity_score)

library(boot)
pol_test_dif <- get_test_dif(test_data, var = "polarity_score", comunidades = "comunidades", groups = c("DNC-GOP", "DNC-PRG"))

pol_test_dif[-1]

# Nivel de polarizacion mínimo en ambos casos pero se hallan diferencias significativas
pol_test_dif$data %>% 
  ggplot(aes(x = sentimiento)) + 
  geom_histogram(aes(group = comunidad, fill = comunidad), color = "white", alpha = 0.5, binwidth = 0.002) + 
  scale_fill_manual(values = c(plots_palette[1], plots_palette[4]))


# grabacion datos test de polaridad
saveRDS(pol_test_dif, here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "health_polarization.rds"))
