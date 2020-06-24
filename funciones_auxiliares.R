# funciones_auxiliares.R


get_common_terms <- function(set1, set2) {
  
  # character, character -> integer
  
  # Devuelve un vector con el conteo acumulativo de elementos
  # comunes entre dos vectores character de igual longitud
  
  # comprobacion de igualdad de longitud
  if (length(set1) != length(set2)) stop("Los vectores tienen que ser de la misma longitud")
  
  # inicio del vector integer donde se guardan los resultados
  common_words <- integer(length = length(set1))
  
  # bucle para aumentar aumentar un subconjunto de los vectores: te tamaño 1 a su tamaño original
  for (idx in 1:length(set1)) {
    
    # subconjunto de los vectores
    subset1 <- set1[1:idx]
    subset2 <- set2[1:idx]
    
    # se asigna el numero de palabras comunes del subconjunto al correspondiente elemento
    # del vector de integers
    common_words[idx] <- length(intersect(subset1, subset2))
    
    
  }
  
  # se devuelve el resultado
  common_words
}


discriminate_term <- function(text, term1, term2) {
  
  # character, character, character -> logical
  
  # filtra ocucrencias de un vector character, donde aparece term1 pero no term2
  # term1 y term2 pueden ser expresiones regulares
  
  # contiene term1
  contains_term1 <- str_detect(text, term1)
  
  # no contiene term2
  not_contains_term2 <- !str_detect(text, term2)
  
  # filtro de terminos
  contains_term1 & not_contains_term2
  
}



get_unigrams <- function(dat, custom_stop_words = "", filter_regular_expression = NULL) {
  
  # data.frame, character, logical -> data.frame
  
  # tokeniza un data.frame con texto de twitter en unigramos
  # ASUME: el texto está guardado en una columna con nombre text
  
  # se recoge texto defectuoso del output de twitter para eliminarlo
  remove_reg <- "&amp;|&lt;|&gt;"
  
  dat <- dat %>% 
    
    # se elimina texto de fectuoso de twitter
    mutate(text = str_remove_all(text, remove_reg)) %>% 
    
    # tokenización a unigramos
    unnest_tokens(word, text, token = "tweets") %>% 
    
    # filtro de stop words comunes en ingles
    filter(!word %in% stop_words$word) %>% 
    
    # filtro de stop words personalizadas
    filter(!word %in% custom_stop_words)
  
  # filtro de terminos adicionales a través de expresión regular
  if (!is.null(filter_regular_expression)) {
    
    dat <- dat %>% 
      filter(!str_detect(word, filter_regular_expression))
    
  }
  
  
  dat
}


get_hashtags <- function(unigram_data, filter_regular_expression = NULL) {
  
  # data.frame, character -> data.frame
  
  # filtra los hashtags de un data.frame con texto de Twitter, previamentee
  # tokenizado en unigramos.
  # ASUME: los unigramos están en una columna de nombre "word"
  
  hash_data <- unigram_data %>% 
    
    # filtro de hashtags
    filter(str_detect(word, "^#"))
  
  # filtro por expresiones regulares adicionales
  if (!is.null(filter_regular_expression)) {
    
    hash_data <- hash_data %>% 
      filter(!str_detect(word, filter_regular_expression))
  }
  
  hash_data
  
}


get_frequencies <- function(tokened_data, com = "comunidad") {
  
  # data.frame, character -> data.frame
  # hace un conteo de términos de un data.frame con objetos tweet previamente tokenizado
  # agrupando el conteo de acuardo con "com".
  # ASUME: los unigramos están en una columna de nombre "word"
  # ASUME: el data.frame contiene una id de usuario guardada en la columna user_id
  tokened_data %>% 
    
    # agrupacion por com
    group_by_(com, "word") %>% 
    
    # conteo de hashtags y número de usuarios diferentes que lo emplean
    summarise(n = n(), 
              n_usuarios = length(unique(user_id))) %>% 
    
    # renombre de columnas
    setNames(c("comunidad", "word", "n", "n_usuarios")) %>% 
    
    # ordenación por frecuencia, de mayor a menor
    arrange(comunidad, desc(n))
  
  
}


get_test_dif <- function(dat, var = "ave_sentiment", comunidades = "comunidad", groups, replic = 5000) {
  
  # data.frame, character, character, character, character, integer -> list
  
  # efectua un test de diferencias de  una variable cuantitativa a través de un número de replicaciones
  # bootstrap determinadas por "replic"
  # "var" determina la variable cuantitativa del data.frame para la cual se hace el text
  # "comunidades" es la variable de agrupacion de la variable cuantitativa
  # "groups es un character vector de longitud 2 indicando los valores a comparar en "comunidades"
  
  # ejecución de muestras bootstrap para los datos filtrados
  sample_dnc <- boot(dat[[var]][dat[[comunidades]] == groups[1]], function(x, i) mean(x[i]), R = replic)
  sample_prog <- boot(dat[[var]][dat[[comunidades]] == groups[2]], function(x, i) mean(x[i]), R = replic)
  
  # calculo de cuantiles 0.05 y 0.975
  q_group1 <- quantile(sample_dnc$t, probs = c(0.025, 0.975))
  q_group2 <- quantile(sample_prog$t, probs = c(0.025, 0.975))
  
  # se guardan intervalos de confianza para los dos grupos en una matriz
  ic <- rbind(q_group1, q_group2)
  rownames(ic) <- groups
  
  # se determina el resultado del text
  resultado <- ifelse(max(q_group1) < min(q_group2) | max(q_group2) < min(q_group1),
                      "Diferencia significativa en medias", "No se encuentran diferencias en medias")
  
  # se guardan los datos co las replicas bootstrap
  dat_boot <- data.frame(comunidad = rep(groups, each = replic), sentimiento = c(sample_dnc$t, sample_prog$t))
  
  # se devuelve una lista con los datos, los intervalos de confianza y el resultados del text
  list(data = dat_boot, 
       IC = ic, 
       resultado = resultado)
  
}

# VISUALIZACIONES ---------------------------------------------------------

## TOPICS ##


get_correlation_data <- function(dat_hashtag, dat_frequency, com, cor_limit = 0.1, user_limit = 4, top = 40) {
  
  # data.frame, data.frame, character, numeric, integer, integer -> data.frame
  # calcula un data.frame de correlaciones entre hashtags a partir de un data frame
  # tokenizado con get_hashtags y un data.frarme de frecuencias de estos hashtags 
  # calculado con get_frequency
  
  # Se incluyen solo los hashtags que cumplen
  # coeficiente phi superior a cor_limit
  # número de usuarios superior a user_limit
  # hashtag entre los top más usados
  
  dat_hashtag %>% 
    
    # filtro de comunidad
    filter(comunidad == com) %>% 
    
    # union de df de hashtags con el de frecuencias para poder filtar n usuarios y top hashtags
    inner_join(dat_frequency %>%
                 filter(comunidad == com, n_usuarios >= user_limit) %>% 
                 top_n(top, wt = n),
               by = c("word")) %>% 
    
    # eliminación de "#" del texto
    mutate(word = str_to_upper(str_remove(word, "^#"))) %>% 
    
    # calculo de correlacion entre hashtags en base a los tweets (status_id)
    pairwise_cor(word, status_id, sort = T) %>% 
    
    # filtro de correlación superior a cor limit
    filter(correlation > cor_limit)  %>% 
    
    # adición de la comunidad
    mutate(comunidad = com)
}


get_correlation_net<- function(cor_data) {
  
  # data.frame -> tbl_graph
  
  # convierte una data.frame de correlaciones calculado con get_correlation_data 
  # en un data.frame grafo (objeto tbl_graph)
  
  # obtencion de nodos a partir dde los hashtags
  nodes <- unique(c(cor_data$item1, cor_data$item2))
  nodes_df <- data.frame(hashtag = nodes, comunidad = cor_data$comunidad[1])
  
  # creación del objeto tbl_graph
  tbl_graph(nodes = nodes_df, edges = cor_data)
  
}

plot_topics <- function(dat, filtered_words, cols = 2, colores = 1:4, label_size = 8, min_users = 1, top_words = 20) {
  
  # data.frame, character, integer, integer, integer, integer, integer -> ggplot
  
  # grafica los primeros top_words hashtags a partir de un data.frame de hashtags obtenido con la función 
  # get_hashtags
  
  # filtered_words: vector de hashtags que no apareceran en el grafico
  # cols: numero de columnas en los facets creados para la visualización
  # colores: elección de los colores de la paleta generada en esta función
  # label_size: tamaño de las etiquetas de las barras en el eeje y
  # min_users: filtro de número mínimo de usuarios que han empleado un hashtag, para que aparezca en el grafico
  # top_words: número de hashtags a incluir en el gráfico de acuerdo a su uso
  
  # paleta de hasta cuatro colores para visualizar las diferentes comunidades
  plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")
  
  dat %>% 
    
    # filtro de hashtags
    filter(!word %in% filtered_words) %>% 
    
    # filtro de numero d usuarios míimo
    filter(n_usuarios >= min_users) %>% 
    ungroup() %>% 
    
    # optencion de los hashtags mas usados de cada comunidad
    group_by(comunidad) %>% 
    top_n(top_words, n) %>% 
    
    # eliminación de "#" 
    mutate(word = str_to_upper(str_remove(word, "^#"))) %>% 
    
    ## visualizacion ##
    ggplot(aes(x = n, y = reorder_within(word, n, within = comunidad))) + 
    
    # columnas
    geom_col(aes(fill = comunidad), width = 0.75) + 
    
    # reorden de columnas
    scale_y_reordered() + 
    
    # creacion de facets
    facet_wrap(~comunidad, scale = "free", ncol = cols) + 
    
    # especificación de colores
    scale_fill_manual(values = plots_palette[colores]) + 
    
    # titulo eje x
    labs(x = "nº hashtags") +
    
    # tema del grafico
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
  
  # tbl_graph, integer, integer, integer -> ggplot
  
  # dibuja una grafo correlaciones de hashtags en un mismo tweet, a partir de un data.frame de
  # un tbl_graph de correlaciones de hashtags calculado con la función get_correlation_net
  
  # generacion de paleta de hasta cuatro colores
  plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")
  
  net_data %>%   
    
    # grafo
    ggraph(layout = "fr") + 
    geom_edge_link(aes(edge_alpha = correlation), show.legend = F, size = 1.3) + 
    geom_node_point(aes(color = comunidad),size = node_size) + 
    
    # etiquetas de los nodos
    geom_node_text(aes(label = hashtag, color = comunidad), repel = TRUE, size = text_size) + 
    
    # facets
    facet_nodes(~comunidad, scales = "free", ncol = 1) + 
    
    # color de los nodos
    scale_color_manual(values = plots_palette[colores]) + 
    
    # tema
    theme_void() +
    guides(color = F) + 
    theme(strip.text = element_text(color = "white"))
}

plot_topic_networks <- function(dat_frequency, dat_graph, filtered_words = "", texto_titulo = "",
                                minim_n = 3, lab_size = 8, text_size = 5, layout_style = "fr", top_words = 20, 
                                node_size = 3, random_state = 123, title_size = 1.2, legend_text_size = 1) {
  
  # data.frame, data.frame, character, character, integer, integer, integer, character, integer, 
  # integer, integer, numeric, integer -> ggGrob
  
  # Muestra un display de dos graficos generados con las funciones plot_topics y plot_correlation_network
  # filtered_words: hashtags que no apareceran en el grafico
  # texto_titulo: título del grafico
  # minim_n = 3: número mínimo de usuarios por hashtag, que apareceran en el gráfico
  # lab_size: tamaño de las etiquetas de las barras
  # text_size: tamaño de las etiquetas de los nodos
  # layout_style: layout del grafo
  # top_words: top hashtags a filtrar
  # node size: tamaño del nodo
  # random_sate: semilla para generar el grafo
  # title_size: tamaño del título del gráfico
  # legend_text_size: tamaño de la leyenda
  
  # generacion de paleta de hasta cuatro colores
  plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")
  
  # generacion de semilla
  set.seed(random_state)
  
  # generacion de grob para las barras
  grob_bars <- ggplotGrob(plot_topics(dat_frequency %>% filter(comunidad != "Independent"), 
                                      filtered_words, cols = 1, min_users = minim_n,
                                      colores = c(1, 3, 4), label_size = lab_size,
                                      top_words = top_words)) 
  
  
  # generacion de grob para el grafo
  grob_graph <- ggplotGrob(dat_graph %>% plot_correlation_network(text_size = text_size, node_size = node_size))
  
  # generacion de grob para el titulo
  titulo <-  textGrob(label = texto_titulo, gp = gpar(fontfamily = "Georgia", cex = title_size))
  
  # generacion de grop para la leyenda
  leyenda <- legendGrob(labels = c("GOP",  "DNC", "Progressives"),  pch = 21, 
                        gp = gpar(fill = plots_palette[c(1, 3, 4)], fontfamily = "Georgia", cex = legend_text_size), nrow = 1)
  
  # distribucion de los grobs del gráfico de barras y del grafo
  plots_grob = arrangeGrob(grob_bars, grob_graph, nrow = 1, widths = c(4, 6))
  
  # distribución y dibujo de todos los grobs
  grid.arrange(titulo, leyenda, plots_grob, heights = c(1, 1, 15))
  
}


get_tfidf <- function(frequency_dat, filter_regular_expression = NULL, min_users = 2) {
  
  # data.frame, character, numeric -> data.frame
  
  # Obtiene los td-idf de un data.frame de frecuencias de unigramos obtenido con get_frequencies
  # se pueden filtrar términos con filter_regular_expression y controlar el número de usuarios mínimos
  # que ha de tener un término con min_users
  
  # filtro de terminos
  if (!is.null(filter_regular_expression)) {
    
    frequency_dat <- frequency_dat %>% 
      filter(!str_detect(word, filter_regular_expression))
    
  }
  
  # filtro de usuarios
  frequency_dat %>% 
    filter(n_usuarios > 2) %>% 
    
    # calculo de tf-idf
    bind_tf_idf(word, comunidad, n)
  
  
}


plot_tfidf <- function(tfidf_data, top = 20, cols = 2, colores = 1:4) {
  
  # data.frame, integer, integer, integer -> ggplot
  
  # Dibuja un gráfico de barras con los tf-idf generados poir la función
  # get_dfidf
  # ASUME: el data.frame tiene una columna con nombre "comunidad", que servirá
  # para generar los facets del gráfico
  
  # top: filtro para los términos con mayor tfidf
  # colores: filtro para la paleta de colores generada en la funcion
  
  # generacion de paleta de hasta cuatro colores
  plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")
  
  tfidf_data %>% 
    
    # agrupacion por comuniudad
    group_by(comunidad) %>% 
    
    # filtro para los terminos con mayor tfidf
    top_n(top, tf_idf) %>% 
    
    ## visualizacion ##
    ggplot(aes(x = tf_idf, y = reorder_within(word, tf_idf, within = comunidad))) + 
    
    # barras
    geom_col(aes(fill = comunidad), width = 0.75) + 
    
    # roerdenacion de eje y de acuerdo con las frecuencias de los terminos en cada grupo
    scale_y_reordered() + 
    
    # facets
    facet_wrap(~comunidad, scale = "free", ncol = cols) + 
    
    # colores de las barras
    scale_fill_manual(values = plots_palette[colores]) + 
    
    # tema
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          text = element_text(family = "Georgia", color = "grey35", size = 14), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          legend.position = "top") 
  
}



# CARGA Y PREPROCESAMIENTO DE DATOS ---------------------------------------

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
