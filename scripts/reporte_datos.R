

# data.frame de datos

library(tidyverse)
comunidades <- data.frame(comunidad = character(0), 
                         n = numeric())


com <- list.files(here::here("datos_raw", "comunidades"))

com_df <- map_df(
  com, function(x) {
    dat <- readRDS(here::here("datos_raw", "comunidades", x))
    comunidades <- data.frame(comunidad = x, n = nrow(dat))
    rm(dat)
    gc()
    comunidades
    })

write_csv2(com_df, path = here::here("datos_procesados", "reporte_datos", "comunidades.csv"))

carpetas_extraccion <- list.files(here::here("datos_raw", "extraccion_tweets"))


ext_df <- map_df(carpetas_extraccion, 
                 function(x) {
                   
                   print(paste("Lectura carpeta:", x))
                   ids <- numeric(0)
                   
                   current_files <- list.files(here::here("datos_raw", "extraccion_tweets", x))
                   for(file in current_files) {
        
                        current_data <- readRDS(here::here("datos_raw", "extraccion_tweets", x, file))$status_id 
                        ids <- c(ids, current_data)
                        ids <- ids[!duplicated(ids)]
                        
                        rm(current_data)
                        gc()
                        
                        
                         
                   }
                   

                   n_rows <- length(ids)
                   rm(ids)
                   gc()

                   data.frame(topic = x, n = n_rows)
                   

                 })


write_csv2(ext_df, path = here::here("datos_procesados", "reporte_datos", "extraccion.csv"))
