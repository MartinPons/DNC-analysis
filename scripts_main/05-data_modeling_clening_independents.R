# 05-data_modeling_clening_independents.R



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



# GRABACION DATOS SIN INDEPENDIENTES --------------------------------------

guarda_sin_independientes("JoeBiden", idx_arch = c(1, 2, 4, 5, 7))
guarda_sin_independientes("DonaldTrump", idx_arch = c(2, 3, 4, 5, 6, 8))
guarda_sin_independientes("BernieSanders")
guarda_sin_independientes("TaraReade")
