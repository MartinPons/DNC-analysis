# funciones_auxiliares.R



# SENTIMENT ---------------------------------------------------------------
lee_datos_rds <- function(carpeta) {
  
  print(paste("Lectura de archivos de la carpeta", carpeta))
  # listado de archivos en la carpeta
  archivos <- list.files(here::here("datos_raw", "extraccion_tweets", carpeta))
  
  print(archivos)
  
  n_archivos <- 0
  
  # carga de archivos
  dat <- map_df(archivos, function(x) {
    
    print("")
    print(paste("Leyendo archivo:", x))
    subdat <- readRDS(here::here("datos_raw", "extraccion_tweets", carpeta, x))
  
    print(paste("Leido archivo:", x))
    print(paste("Nº filas", nrow(subdat)))
  
  
    
    n_archivos <- n_archivos + 1

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
    
    # Se excluyen retweets
    filter(!is_retweet) %>% 
    
    # cambio de class de user_id para fusionar con comunidades
    mutate(user_id = as.numeric(user_id)) %>% 
    
    # seleccion de variables que interesan
    select(user_id, status_id, created_at, screen_name, retweet_user_id, 
           retweet_screen_name, is_retweet, text, country_code, place_name, 
           place_full_name, verified, geo_coords, coords_coords) %>% 
    
    # Obteniendo ciudad y estado a partir de place_full_name
    bind_cols(map_df(.$place_full_name, ~get_state_code(.x))) %>% 
    
    # obteniendo comunidad fusionando con tabla de comunidades
    left_join(comunidades, by = "user_id") %>% 
    
    # identificacion de comunidades independientes
    mutate(comunidad = ifelse(is.na(comunidad), "Independent", comunidad))
    
  
}


clean_text <- function(text){
  
  text_no_abbr <- replace_abbreviation(text)
  text_no_cont <- replace_contraction(text_no_abbr)
  text_no_url <- rm_url(text_no_cont)
  text_no_ordinal <- replace_ordinal(text_no_url)

#  text_no_number <- replace_number(text_no_emot)
  text_no_mail <- rm_email(text_no_ordinal)
  text_no_hash <-str_remove_all(text_no_mail, "#")
  text_words_only <- rm_non_words(text_no_hash)
  text_no_emot <- replace_emoji(text_words_only)
  text_lower <- str_to_lower(text_no_hash)
  
}

get_state_code <- function(place_string) {
  
  #  print(paste("Place strint", place_string))
  place_vector <- unlist(str_split(place_string, ", "))
  
  
  #  print(paste("Place vector", place_vector))
  
  if (is.na(place_string) | is.na(place_vector[2]) | length(place_vector) == 1) {
    
    return(data.frame(City = "No city", State = "No state"))
    
  }
  
  
  if (place_vector[2] == "USA") {
    
    return(data.frame(City = "No city", State = state.abb[which(state.name == place_vector[1])]))
    
  } else {
    
    location <- data.frame(City = place_vector[1], State = place_vector[2])
    return(location)
  }
  
  
}



# POLARIZACION ------------------------------------------------------------


get_node_pol_data <- function(dat, ni, n) {
  
  conexiones_externas <- unique(dat[dat$from == n &
                               dat$connection_type == "conexion_frontera",
                             "to", 
                             drop = T])
  
  
  # calculo de conexines internas del nodo con nodos internos
  conexiones_internas <- unique(dat[dat$from == n &
                               dat$connection_type == "conexion_interna",
                             "to", 
                             drop = T])
  
  conexiones_nodos_internos <- intersect(nodos_internos, conexiones_internas)
  
  if (length(conexiones_externas) == 0 | 
      (length(conexiones_externas) > 0 & length(conexiones_nodos_internos) == 0)) {
    return(NULL)
  } else 
    
    degree_externo <- length(conexiones_externas)
    degree_interno <- length(conexiones_nodos_internos)
    polarity_score <- degree_interno / (degree_interno + degree_externo) - 0.5
    
    data.frame(nodo = n, 
               degree_interno = degree_interno, 
               degree_externo = degree_externo, 
               polarity_score = polarity_score)
    
  }
  



get_inner_nodes <- function(dat) {
  
  dat %>% 
    group_by(from) %>% 
    summarise(tiene_con_externa = any(connection_type == "conexion_frontera")) %>% 
    filter(!tiene_con_externa) %>% 
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




# FUNCIONES GRAFICAS ------------------------------------------------------

# tema

theme_tfm <- function() {
  
  theme_bw() + 
  theme(
    text = element_text(family = "Georgia", color = "grey55"),
    panel.grid = element_blank(), 
    panel.border = element_blank(), 
    strip.background = element_blank()
  )
}
