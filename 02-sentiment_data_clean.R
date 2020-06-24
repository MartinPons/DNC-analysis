# 02-sentiment_data_clean.R



# CONFIGURACION INICIAL ---------------------------------------------------

# librerias
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
library(remoji)
library(lexicon)
library(rtweet)
Sys.getenv()
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_251")

library(tm)
library(qdap)

# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))



# LIMPIEZA ----------------------------------------------------------------

# climatechange
climate_sent <- lee_datos_sent("climatechange")
saveRDS(climate_sent, here::here("datos_procesados", "formated_sent_df_data", "climatechange_sent.rds"))

# greennewdeal
greennewdeal_sent <- lee_datos_sent("greennewdeal")
saveRDS(greennewdeal_sent, here::here("datos_procesados", "formated_sent_df_data", "greennewdeal_sent.rds"))

# healthcare
healthcare <- lee_datos_sent("healthcare")
saveRDS(healthcare, here::here("datos_procesados", "formated_sent_df_data", "healthcare_sent.rds"))

# biden
biden <- lee_datos_sent("JoeBiden")
saveRDS(biden, here::here("datos_procesados", "formated_sent_df_data", "healthcare_sent.rds"))