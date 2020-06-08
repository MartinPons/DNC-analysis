# funciones_auxiliares.R



# DATA WRANGLING ---------------------------------------------------------------

format_twitter_df_data <- function(fil,  seguidores = F, idx_arch = NULL, rm_ret = F, compute_sentiment = T) {
  
  
  inicio <- Sys.time()
  cat("Inicio", inicio, fill = T)
  # character, logical -> data.frame
  
  # Carga datos de carpetas de archivos de twitter, los formatea, limpia el texto
  # y calcula sentimiento de cata tweet
  
  require(dplyr)
  require(sentimentr)
  
  cat("Carga datos principales", fill = T)
  dat <- lee_datos_rds(fil, idx_archivos = idx_arch, rm_retweets = rm_ret)
  
  cat("Datos leidos.  ", "Filas:", nrow(dat), fill = T)
  
  cat("Carga comunidades", fill = T)
  comunidades <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) 
  
  cat("Limpieza de data frame", fill = T)
  dat_cleaned <- clean_tweets_df(dat, comunidades)
  rm(comunidades)
  gc()
  
  cat("Limpieza de texto", fill = T)
  dat_cleaned_text <- dat_cleaned %>% 
    mutate(text = clean_text(text))
  
  rm(dat_cleaned)
  gc()
  
 
  if (compute_sentiment) {
    cat("Calculo de sentimiento", fill = T)
    
  sent <- sentiment_by(dat_cleaned_text$text)
  
  dat_cleaned_text <- dat_cleaned_text %>% 
    bind_cols(sent)
  
  rm(sent)
  gc()
  }
  
  cat("Ordenacion orden comunidades", fill = T)
  dat_cleanded_text <- bind_rows(dat_cleaned_text) %>% 
    mutate(comunidad = fct_relevel(comunidad, c("GOP", "Independent", "DNC", "Progressives")))
  
  if (seguidores) {
    
    cat("Carga y formato de datos de seguidores", fill = T)
    seguidores <- readRDS(here::here("datos_procesados", "comunidades", "seguidores.rda")) 
    dat_cleanded_text <- left_join(dat_cleaned_text, seguidores, by = "user_id") %>% 
      mutate(siguiendo = fct_relevel(siguiendo, c("Trump", "Independent", "Biden", "Sanders")))
    
  }
  
  cat("Libearacion de espacio comunidades y seguidores", fill = T)
  rm(seguidores)
  gc()
  
  cat("Devolucion de datos limpios", fill = T)
  
  fin <- Sys.time()
  cat("Fin", fin, fill = T)
  
  cat("Duracion: ", fin - inicio)
  
  dat_cleanded_text
  
}

lee_datos_sent <- function(fil, idx_arch = NULL) {
  
  
  cat("Carga datos principales", fill = T)
  dat <- lee_datos_rds(fil, idx_archivos = idx_arch, rm_retweets = F)
  
  cat("Datos leidos.  ", "Filas:", nrow(dat), fill = T)
  
  cat("Carga comunidades", fill = T)
  comunidades <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
    mutate(user_id = as.character(user_id))
  
  cat("Fusion comunidades", fill = T)
  # fusion con comunidades. no se incluyen independientes
  dat <- dat %>% 
    select(status_id, user_id, created_at, text, is_retweet, place_full_name, coords_coords) %>% 
    inner_join(comunidades, by = c("user_id")) %>% 
    
    # limpieza de texto
    mutate(text = clean_text(text))
  
  cat("Calcluo sentimiento", fill = T)
  # calculo de  sentimiento
  sent <- sentiment_by(dat$text)
  
  dat <- dat %>% bind_cols(sent)
  
  rm(sent)
  gc()
  
  cat("Calcluo estado", fill = T)
  # calculo de estado
  dat <- dat %>% 
    bind_cols(map_df(.$place_full_name, ~get_state_code(.x)))
  
  
  
  dat %>% 
    mutate(comundad = fct_relevel(comunidad, c("GOP", "DNC", "Progressives")))
  
}



lee_datos_rds <- function(carpeta, idx_archivos = NULL, rm_retweets = F) {
  
  cat("Lectura de archivos de la carpeta", carpeta, fill = T)
  # listado de archivos en la carpeta
  archivos <- list.files(here::here("datos_raw", "extraccion_tweets", carpeta))
  
  
  if (!is.null(idx_archivos)) archivos <- archivos[idx_archivos]
  cat("Archivos para leer:", archivos, fill = T)
  
  n_archivos <- 0
  
  # carga de archivos
  dat <- map_df(archivos, function(x) {
    
    print("")
    print(paste("Leyendo archivo:", x))
    subdat <- readRDS(here::here("datos_raw", "extraccion_tweets", carpeta, x))
  
    print(paste("Leido archivo:", x))
    print(paste("Nº filas", nrow(subdat)))
  
  
    
    n_archivos <- n_archivos + 1
    
    if (rm_retweets == T) subdat <- filter(subdat, !is_retweet)
    
    print(paste("Nº filas finales", nrow(subdat)))

    subdat
  })

  print(paste("Total archvios:", n_archivos))
  
  duplicados <- duplicated(dat$status_id, fromLast = T)
  print(paste("Registros duplicados", sum(duplicados)))
  
  print("Eliminando tweets duplicados")
  dat[!duplicados, ]
}



get_tweets_corpus <- function(tweets_df) {
  
  
  # extraccion de texto
  tweets_text <- tweets_df$text
  
  # eliminacion de urls
  tweets_url <- rm_twitter_url(tweets_text)
  
  # eliminacion de caracteres especiales
  tweets_chr <- str_replace_all(tweets_url, "[^A-Za-z]", " ")
  
  
  # conversion en corpus
  tweets_corpus <- tweets_chr %>% 
    VectorSource() %>% 
    Corpus()
  
  # convert to lowercase
  tweets_corpus_lwr <- tm_map(tweets_corpus, tolower)
  
  # quitar stop words
  tweets_corpus_stop <- tm_map(tweets_corpus_lwr, removeWords, stopwords("english"))
  
  
  # remove additional spaces
  tm_map(tweets_corpus_stop, stripWhitespace)
  
}


clean_tweets_df <- function(tweets_df, comunidades) {
  
  tweets_df %>% 

    # cambio de class de user_id para fusionar con comunidades
    mutate(user_id = as.numeric(user_id)) %>% 
    
    # seleccion de variables que interesan
    select(user_id, status_id, created_at, screen_name, followers_count, retweet_user_id, retweet_screen_name, 
           is_retweet, text, country_code, place_name, place_full_name, coords_coords) %>% 
    
    # Obteniendo ciudad y estado a partir de place_full_name
    # bind_cols(map_df(.$place_full_name, ~get_state_code(.x))) %>% 
    
    # obteniendo comunidad fusionando con tabla de comunidades
    left_join(comunidades, by = "user_id") %>% 
    
    # identificacion de comunidades independientes
    mutate(comunidad = ifelse(is.na(comunidad), "Independent", comunidad))
    
  
}


clean_text <- function(text){
  
  text <- replace_abbreviation(text)
  text <- replace_contraction(text)
  text <- rm_url(text)
  text <- replace_ordinal(text)
  text <- rm_email(text)
  text <- replace_emoji(text)
  text <- str_replace_all(text, "[^aA-zZ0-9#@ ]", "")
  
  #  text_no_number <- replace_number(text_no_emot)
  text <- str_to_lower(text)
  
  # limpieza palabras recurrentes propias del texto a analizar
  text <- str_replace_all(text, "[Jj]oe ?[Bb]iden", "Joe Biden")
  text <- str_replace_all(text, "[Bb]ernie ?[Ss]anders", "Bernie Sanders")
  text <- str_replace_all(text, "[Dd]onald ?[Tt]rump", "Donald Trump")
  text <- str_replace_all(text, "((COVID)|([Cc]ovid))(19)?_?(e383bc)?(19)?([Pp]andemic)?|[Cc]oronavirus(19)?", "COVID19")
  text <- str_replace_all(text, "amp", "")
  text <- str_replace_all(text, "#(m4a|medicare4all)", "#medicareforall")
  text <- str_replace_all(text, "#[Oo]bama[Cc]are|#[Aa]ffordable[Cc]are[Aa]ct", "#aca")
  
  text
  
}

get_state_code <- function(place_string) {
  
  
  place_vector <- unlist(str_split(place_string, ", "))
  
  
  if (is.na(place_string) | is.na(place_vector[2]) | length(place_vector) == 1) {
    
    return(data.frame(City = "No city", State = "No state"))
    
  }
  
  
  if (place_vector[2] == "USA") {
    state <- state.abb[which(state.name == place_vector[1])]
    
    if (length(state) == 0) state <- NA
    
    return(data.frame(City = "No city", State = state))
    
  } else {
    
    location <- data.frame(City = place_vector[1], State = place_vector[2])
    return(location)
  }
  
  
}


# EDA ---------------------------------------------------------------------



discriminate_term <- function(text, term1, term2) {
  
  # character, character, character -> logical
  
  ## fitlter occurrences in a caracter vector where term1 appears but term2 doesn't
  ## term 1 and term2 can be regular expressions
  
  # contains term1
  contains_term1 <- str_detect(text, term1)
  
  # doesn't contain term2
  not_contains_term2 <- !str_detect(text, term2)
  
  # filter text
  contains_term1 & not_contains_term2
  
}


get_tweet_distribution <- function(dat, bin_width = 0.05) {
  
  dat %>%  
    count(comunidad, user_id, name = "tweets") %>% 
    count(comunidad, tweets, name = "usuarios") %>% 
    group_by(comunidad) %>% 
    arrange(tweets) %>% 
    mutate(
      porc_tweets = tweets/sum(tweets),
      porc_usuarios = usuarios/sum(usuarios),
      tweets_acumulados = cumsum(tweets),
      usuarios_acumulados = cumsum(usuarios)) %>% 
    mutate(porc_tweets_acum = tweets_acumulados /sum(tweets), 
           porc_usuarios_acum = usuarios_acumulados / sum(usuarios)
    ) %>% 
    mutate(int_usuarios_acum = cut(porc_usuarios_acum, seq(0, 1, by = bin_width)),
           int_tweets_acum = cut(porc_tweets_acum, seq(0, 1, by = bin_width))) 
  
    
}

get_unigrams <- function(dat, custom_stop_words = "", filter_regular_expression = NULL) {
  
  remove_reg <- "&amp;|&lt;|&gt;"
  
  dat <- dat %>% 
    mutate(text = str_remove_all(text, remove_reg)) %>% 
    unnest_tokens(word, text, token = "tweets") %>% 
    filter(!word %in% stop_words$word) %>% 
    filter(!word %in% custom_stop_words)
  
  if (!is.null(filter_regular_expression)) {
    
    dat <- dat %>% 
    filter(!str_detect(word, filter_regular_expression))
    
  }

    
  dat
}

# pr <- get_unigrams(healthcare, custom_stop_words = c("#healthcare", "healthcare"))

get_hashtags <- function(unigram_data, filter_regular_expression = NULL) {
  
  hash_data <- unigram_data %>% 
    filter(str_detect(word, "^#"))
  
  if (!is.null(filter_regular_expression)) {
    
    hash_data <- hash_data %>% 
      filter(!str_detect(word, filter_regular_expression))
  }
  
  hash_data
    
}

# hash <- get_hashtags(pr)

get_frequencies <- function(tokened_data, com = "comunidad") {
  
  tokened_data %>% 
    group_by_(com, "word") %>% 
    summarise(n = n(), 
              n_usuarios = length(unique(user_id))) %>% 
    setNames(c("comunidad", "word", "n", "n_usuarios")) %>% 
    arrange(comunidad, desc(n))
  
  
}

# freq <- get_frequencies(hash)

plot_topics <- function(dat, filtered_words, cols = 2, colores = 1:4, label_size = 8, min_users = 1) {
  
  
  dat %>% 
    filter(!word %in% filtered_words) %>% 
    filter(n_usuarios >= min_users) %>% 
    ungroup() %>% 
    group_by(comunidad) %>% 
    top_n(20, n) %>% 
    mutate(word = str_to_upper(str_remove(word, "^#"))) %>% 
    ggplot(aes(x = n, y = reorder_within(word, n, within = comunidad))) + 
    geom_col(aes(fill = comunidad), width = 0.75) + 
    scale_y_reordered() + 
    facet_wrap(~comunidad, scale = "free", ncol = cols) + 
    scale_fill_manual(values = plots_palette[colores]) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          text = element_text(family = "Georgia", color = "grey55", size = label_size), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          legend.position = "top") +
    guides(fill = F)
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

# tfitd <- get_tfidf(healthcare_frequency)


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
          text = element_text(family = "Georgia", color = "grey55", size = 14), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          legend.position = "top") 
  
}

# plot_tfidf(healthcare_tfidf)

create_hashtag_graph <- function(dat_hashtag, com, custom_stop, nodos_n = 20, minim_n = 2) {
  
  net_df <- create_hashtag_network(filter(dat_hashtag, !word %in% custom_stop, comunidad == com), nodos_n, minim_n)
  
  nodes_df <- data.frame(nodes = unique(c(net_df$from, net_df$to)))
  edges_df <- select(net_df, from, to)
  
  tbl_graph_dnc <- tbl_graph(nodes = nodes_df, edges = edges_df) %>% 
    activate(edges) %>% 
    mutate(strength = net_df$strength) %>% 
    activate(nodes) %>% 
    mutate(comunidad = com)
  
}


create_hashtag_network <- function(dat, n_nodos = 20, min_n = 2) {
  
  cat("Iniciacion data frame de links", fill = TRUE)
  hashtag_ties <- data.frame(comunidad = character(0), from = character(0), to = character(0))
  
  cat("Obtencion de data frame con conteo de hashtag  por status_id", fill = TRUE)
  dat_topic <- dat %>% 
    count(comunidad, status_id, word)
  
  cat("Obtencion de data frame de frequencias de hasthags", fill = TRUE)
  dat_frequency <- dat %>% 
    group_by(comunidad, word) %>% 
    summarise(total = n(), 
              n_usuarios = length(unique(user_id))) %>% 
    arrange(desc(total)) 
  
  
  cat("Obtencion del porcentade de usuarios del hashtag", fill = TRUE)
  users_with_hashtags <- length(unique(dat$user_id[dat$word %in% dat_frequency$word]))
  
  dat_frequency <- dat_frequency %>% 
    mutate(perc_users = n_usuarios / users_with_hashtags) %>% 
    filter(n_usuarios > min_n) %>% 
    dplyr::top_n(n_nodos, wt = total)
  
  cat("Filtro de datos por nodos mas frecuentes", fill = TRUE)
  nodos <- unique(dat_frequency$word)
  dat_topic <- dat_topic %>% 
    filter(word %in% nodos)
  
  cat("Obtencion de tweets unicos", fill = TRUE)
  tweets <- unique(dat_topic$status_id)
  
  
  cat("Inicio de loop para obtener data frame de links", fill = TRUE)
  n_nodo <- 1
  
  for (nod in nodos) {
    
    print(paste("Nodo:", n_nodo, " - ", nod))
    
    for (tweet in tweets) {
      
      hashtags <- dat_topic[dat_topic$status_id == tweet, "word", drop = T]
      
      comunidad <- dat_topic[dat_topic$status_id == tweet, "comunidad", drop = T]
      
      if (nod %in% hashtags) {
        
        nashtags <- setdiff(hashtags, nod)
        
        hashtag_ties <- hashtag_ties %>% 
          bind_rows(
            data.frame(status_id = tweet,
                       comunidad = comunidad[1],
                       from =      hashtags, 
                       to =        nod)) # %>% 
       #   filter(from != to)
      }
    }
    
    n_nodo <- n_nodo + 1
  }
  
  cat("Fin de loop", fill = TRUE)
  
  cat("Adicion de nodos sin links", fill = T)
  
  nodos_sin_links <- setdiff(nodos, unique(hashtag_ties$to))
  
  # cat("Nº de Nodos sin links:", length(nodos_sin_links), fill = T)
  
  # if (length(nodos_sin_links) > 0) {
  #   hashtag_ties <- hashtag_ties %>% 
  #     bind_rows(
  #       data.frame(status_id = NA, 
  #                  comunidad = comunidad[1], 
  #                  from = nodos_sin_links, 
  #                  to = nodos_sin_links
  #       ))
  #   
  # }
  
  cat("Wrangling final del data frame", fill = TRUE)
  
  hashtag_ties <- hashtag_ties %>% 
    group_by(comunidad, from, to) %>% 
    summarise(strength = n(),
              tweets = paste(status_id, collapse = ",")) %>%
    left_join(select(dat_frequency, word, total), by = c("from" = "word")) %>%
    rowwise() %>% 
    mutate(ordered_from_to = paste(sort(c(from, to)), collapse = " ")) %>% 
    ungroup() %>% 
    distinct(ordered_from_to, .keep_all = T) %>% 
    select(-ordered_from_to) %>% 
    mutate(from = str_replace(from, "#", "") %>% str_to_upper(),
           to = str_replace(to, "#", "") %>% str_to_upper())
  
  cat("Devolucion del data frame", fill = TRUE)
  hashtag_ties
  
}



plot_topic_networks <- function(dat_frequency, dat_graph, filtered_words = "", texto_titulo = "",
                                minim_n = 3, lab_size = 8, text_size = 5, layout_style = "nicely") {
  
  grob_bars <- ggplotGrob(plot_topics(dat_frequency %>% filter(comunidad != "Independent"), 
                                      c(filtered_words), cols = 1, min_users = minim_n, colores = c(1, 3, 4), label_size = lab_size) +
                            guides(color = F , fill = F))
  
  grob_graph <- ggplotGrob(dat_graph %>% 
                             ggraph(layout = layout_style) +
                             geom_edge_link(aes(edge_alpha = correlation)) + 
                             geom_node_point(alpha = 0.3) + 
                             geom_node_text(aes(label = name, color = comunidad), size = text_size, family = "Candara", repel = T) +
                             facet_nodes(~comunidad, ncol = 1, scales = "free") +
                             theme_bw() + 
                             guides(fill = F, alpha = F, color = F) +
                             scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
                             theme(panel.grid = element_blank(),
                                   strip.background = element_blank(),
                                   strip.text = element_text(family = "Georgia", size = 11, color = "white"),
                                   panel.border = element_blank(),
                                   axis.title = element_blank(), 
                                   axis.text = element_blank(),
                                   axis.ticks = element_blank()))
  
  titulo <-  textGrob(label = texto_titulo, gp = gpar(fontfamily = "Georgia", cex = 1.5))
  
  leyenda <- legendGrob(labels = c("GOP",  "DNC", "Progressives"),  pch = 21, 
                        gp = gpar(fill = plots_palette[c(1, 3, 4)], fontfamily = "Georgia", cex = 1), nrow = 1)
  
  plots_grob = arrangeGrob(grob_bars, grob_graph, nrow = 1, widths = c(4, 6))
  
  
  grid.arrange(titulo, leyenda, plots_grob, heights = c(1, 1, 8))
  
}



get_correlation_network <- function(dat_hashtag, dat_frequency, com, exclude_words,
                                     top_words = 20, cor_limit = 0.35, text_size = 10, point_size = 5, 
                                     text_color = "grey") {
  
  # dat_sent <- dat_hashtag %>% 
  #   filter(comunidad == "GOP") %>% 
  #   group_by(comunidad, word) %>% 
  #   summarise(sent = mean(ave_sentiment)) %>% 
  #   ungroup() %>% 
  #   select(-comunidad)
  # 
  
  dat_cor <- dat_hashtag %>% 
    filter(!word %in% exclude_words, comunidad == com) %>% 
    pairwise_cor(word, status_id, sort = T)
  
  
  dat_net <-  dat_cor %>%
    rename(from = item1, to = item2)
  
  dat_nodes <- data.frame(name = unique(c(dat_net$from, dat_net$to)))
  
  dat_graph <- tbl_graph(nodes = dat_nodes, edges = dat_net , directed = F) %>% 
    activate(nodes) %>% 
    inner_join(dat_frequency %>% 
                 filter(n_usuarios > 1, comunidad == com), 
               by = c("name" = "word")) %>% 
    top_n(top_words, wt = n) %>% 
 #   left_join(dat_sent, by = c("name" = "word")) %>% 
    mutate(name = str_to_upper(str_remove(name, "#"))) %>% 
    activate(edges) %>% 
    filter(correlation > cor_limit) 
  
  dat_graph %>% 
    ggraph(layout = "fr") +
    geom_edge_link(show.legend = FALSE, aes(alpha = correlation)) +
    geom_node_point(pch = 21, aes(size = n), show.legend = F) +
    geom_node_text( aes(label = name, color = comunidad), repel = TRUE, size = 3) +
    scale_fill_gradient2(low = "red", high = "green", mid = "grey") +
    scale_color_manual(values = plots_palette[c(1, 3, 4)]) + 
    theme_void() + 
    guides(color = F, fill = F)
  
  dat_graph
  # 
  # dat_graph %>% 
  #   ggraph(layout = "fr") +
  #   geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  #   geom_node_point(aes(color = sent), size = point_size) +
  #   geom_node_text(aes(label = name), color = text_color, repel = TRUE, size = text_size) +
  #   scale_color_gradient2(low = "red", high = "green", mid = "grey") +
  #   guides(color = F) + 
  #   
  #   theme_void()
}

# ANALISIS ----------------------------------------------------------------

get_test_dif <- function(dat, var = "ave_sentiment", comunidades = "comunidad", groups, replic = 5000) {
  
  sample_dnc <- boot(dat[[var]][dat[[comunidades]] == groups[1]], function(x, i) mean(x[i]), R = replic)
  sample_prog <- boot(dat[[var]][dat[[comunidades]] == groups[2]], function(x, i) mean(x[i]), R = replic)
  
  q_group1 <- quantile(sample_dnc$t, probs = c(0.025, 0.975))
  q_group2 <- quantile(sample_prog$t, probs = c(0.025, 0.975))
  
  ic <- rbind(q_group1, q_group2)
  rownames(ic) <- groups
  
  resultado <- ifelse(max(q_group1) < min(q_group2) | max(q_group2) < min(q_group1),
                      "Diferencia significativa en medias", "No se encuentran diferencias en medias")
  
 dat_boot <- data.frame(comunidad = rep(groups, each = replic), sentimiento = c(sample_dnc$t, sample_prog$t))
 
 list(data = dat_boot, 
      IC = ic, 
      resultado = resultado)
  
}

# POLARIZACION ------------------------------------------------------------

# lectura de datos
lee_pol_rds <- function(carpeta, idx_archivos = NULL) {
  
  cat("Lectura de archivos de la carpeta", carpeta, fill = T)
  
  # listado de archivos en la carpeta
  archivos <- list.files(here::here("datos_raw", "extraccion_tweets", carpeta))
  
  
  # seleccion de archivos de la carptea
  if (!is.null(idx_archivos)) archivos <- archivos[idx_archivos]
  cat("Archivos para leer:", archivos, fill = T)
  
  n_archivos <- 0
  
  # carga de archivos
  dat <- map_df(archivos, function(x) {
    
    print("")
    print(paste("Leyendo archivo:", x))
    subdat <- readRDS(here::here("datos_raw", "extraccion_tweets", carpeta, x)) %>% 
      filter(is_retweet) %>% 
      select(user_id, status_id, screen_name, retweet_count, retweet_status_id, retweet_user_id, retweet_screen_name)
    
    print(paste("COlumnas: ", names(subdat)))
    
    gc()
    print(paste("Leido archivo:", x))
    print(paste("Nº filas", nrow(subdat)))
    
    
    n_archivos <- n_archivos + 1
    
    print(paste("Nº filas finales", nrow(subdat)))
    
    subdat
  })
  
  print(paste("Total archvios:", n_archivos))
  
  duplicados <- duplicated(dat$status_id, fromLast = T)
  print(paste("Registros duplicados", sum(duplicados)))
  
  print("Eliminando tweets duplicados")
  
  
  dat[!duplicados, ]
}


# clean_polarization_data
clean_polarization_data <- function(archivo) {
  
  cat("Carga de datos", fil = T)
  
  cat("Datos tweet", fill = T)
  dat_net <- lee_pol_rds(archivo)

  cat("Datos comunidades")
  comunidad <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
    mutate(user_id = as.character(user_id))
  
  
dat_net %>% 
  network_data(.e = "retweet") %>% 
    left_join(comunidad, by = c("from" = "user_id")) %>% 
    filter(!is.na(comunidad)) %>% 
    left_join(comunidad, by = c("to" = "user_id"), suffix = c("_from", "_to")) %>% 
    filter(!is.na(comunidad_to)) %>% 
    count(from, to, comunidad_from, comunidad_to) %>% 
    mutate(connection_type = ifelse(comunidad_from == comunidad_to,
                                    "conexion_interna",
                                    "conexion_frontera"))
}



pre_processing <- function(network, comunidades) {
  
  
  ties_df <- igraph::as_data_frame(network, what = "edges") %>% 
    select(-type)
  
  nodes_df <- igraph::as_data_frame(network, what = "vertices") %>% 
    mutate(id = as.numeric(id)) %>% 
    left_join(comunidades, by = c("id" = "user_id")) %>% 
    mutate(comunidad = ifelse(is.na(comunidad), "Independent", comunidad))
  
  ties_df <- ties_df %>% 
    left_join(nodes_df, by = c("from" = "name")) %>% 
    left_join(nodes_df, by = c("to" = "name"), suffix = c("_from", "_to")) %>% 
    mutate(connection_type = ifelse(comunidad_from == comunidad_to,
                                    "conexion_interna", 
                                    "conexion_frontera"))
  
  ties_df
}


get_node_pol_data <- function(dat, ni, n) {
  
  # data.frame, character, numeric -> data,frame
  
  # Calcula el scoring de polarización de un nodo de la frontera
  
  # obtención de vector con lconexiones frontera
  conexiones_externas <- unique(dat[dat$from == n &
                               dat$connection_type == "conexion_frontera",
                             "to", 
                             drop = T])
  
  
  # calculo de conexines internas del nodo con nodos internos
  conexiones_internas <- unique(dat[dat$from == n &
                               dat$connection_type == "conexion_interna",
                             "to", 
                             drop = T])
  
  # calculo de las conexiones con los nodos internos (solo una parte de las conexiones internas
  # serán conexiones con los nodos internos)
  conexiones_nodos_internos <- intersect(nodos_internos, conexiones_internas)
  
  # si no hay conexiones con nodos frontera o conexiones con nodos internos, 
  # no se puede calcular el scoring
  if (length(conexiones_externas) == 0 | 
      (length(conexiones_externas) > 0 & length(conexiones_nodos_internos) == 0)) {
    return(NULL)
    
  } else {
    
    # calculo de degree interno y externo del nodo analizado
    degree_externo <- length(conexiones_externas)
    degree_interno <- length(conexiones_nodos_internos)
    
    # aplicacion de la ecuacion para el calculo del scoring de polaridad
    polarity_score <- degree_interno / (degree_interno + degree_externo) - 0.5
    
    # se juntan todos los datos relevantes en forma de data.frame de una linea
    data.frame(nodo = n, 
               degree_interno = degree_interno, 
               degree_externo = degree_externo, 
               polarity_score = polarity_score)
    
  }
  



get_inner_nodes <- function(dat) {
  
  # data.frame -> characer
  
  # calcula los nodos internos de un data.frame que representa las relaciones
  # entre dos comunidades
  # ASUME: dos columnas que forman las relaciones de los nodos tienen el nombre "from" y "to"
  # ASUME: el data.frame tiene una columna llamada connection_type con los valores "conexion_interna", "conexion_frontera"
  
  dat %>% 
    
    # se agrupan los datos por nodo
    group_by(from) %>% 
    
    # se determina para cada uno de los nodos from, si tienen alguna "conexion frontera"
    summarise(tiene_con_externa = any(connection_type == "conexion_frontera")) %>% 
    
    # se eliminan los nodos que tienen conexión frontera
    filter(!tiene_con_externa) %>% 
    
    # se devuelven los nodos que quedan: los que no tienen conexión frontera
    .$from
}


filter_communities <- function(dat, com1, com2) {
  
  dat %>% 
    filter(comunidad_from %in% c(com1, com2), 
           comunidad_to %in% c(com1, com2))
}

# get_node_pol_data(ties_df,nodos_internos, "a")
# get_node_pol_data(ties_df, nodos_internos, "b")
# get_node_pol_data(ties_df, nodos_internos, "2")
# get_node_pol_data(ties_df, nodos_internos, "e")




# MODELIMG ---------------------------------------------------------------

# preprocesamiento:

# eliminación de independientes

guarda_sin_independientes <- function(carpeta, idx_archivos = NULL) {
  
  
  cat("Lectura de archivos de la carpeta", carpeta, fill = T)
  
  # listado de archivos en la carpeta
  archivos <- list.files(here::here("datos_raw", "extraccion_tweets", carpeta))
  
  # carchivos para leer
  if (!is.null(idx_archivos)) archivos <- archivos[idx_archivos]

  
  comunidades <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda"))

  
    walk(archivos, function(x) {
    
    print("")
    print(paste("Leyendo archivo:", x))
    subdat <- readRDS(here::here("datos_raw", "extraccion_tweets", carpeta, x)) %>% 
      mutate(user_id = as.numeric(user_id)) %>% 
      inner_join(comunidades, by = "user_id")
    
    cat("N de filas del fichero", nrow(subdat), fill = T)
    
    cat("Limpieza de data frame", fill = T)
    dat_cleaned <- clean_tweets_df(subdat, comunidades)
    rm(comunidades)
    gc()
    
    cat("Limpieza de texto", fill = T)
    dat_cleaned_text <- dat_cleaned %>% 
      mutate(text = clean_text(text))
    
    rm(dat_cleaned)
    gc()
    
    cat("Calculo de sentimientto", fill = T)
    sent <- sentiment_by(dat_cleaned_text$text)
    
    dat_cleaned_text <- dat_cleaned_text %>% 
      bind_cols(sent)
    
    rm(sent)
    gc()
    
    saveRDS(dat_cleaned_text, here::here("datos_procesados", "modeling_data", "datos_sin_independientes", x))
    
    rm(dat_cleaned_text)
    gc()
    
  })
}
    
    
    



