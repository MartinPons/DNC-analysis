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