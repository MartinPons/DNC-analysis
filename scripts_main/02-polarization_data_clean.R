# 02-polarization_data_clean.R



# CONFIGURACION INICIAL ---------------------------------------------------

# librerias
library(tidyverse)
library(igraph)
library(rtweet)


# funciones_auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))



# carga y limpieza de datos
covid_pol <- clean_polarization_data("covid")
saveRDS(covid_pol, here::here("datos_procesados", "formated_polarization_df_data", "covid_pol.rds"))


# climatechange
climate_pol <- clean_polarization_data("climatechange")
saveRDS(climate_pol, here::here("datos_procesados", "formated_polarization_df_data", "climate_pol.rds"))


# education
education_pol <- clean_polarization_data("education")
saveRDS(education_pol, here::here("datos_procesados", "formated_polarization_df_data", "education_pol.rds"))


# biden
biden_pol <- clean_polarization_data("JoeBiden")
saveRDS(biden_pol, here::here("datos_procesados", "formated_polarization_df_data", "biden_pol.rds"))


# trump
trump_pol <- clean_polarization_data("DonaldTrump")
saveRDS(trump_pol, here::here("datos_procesados", "formated_polarization_df_data", "trump_pol.rds"))


# sand4ers
sanders_pol <- clean_polarization_data("BernieSanders")
saveRDS(sanders_pol, here::here("datos_procesados", "formated_polarization_df_data", "sanders_pol.rds"))


# tarareade
tara_pol <- clean_polarization_data("TaraReade")
saveRDS(tara_pol, here::here("datos_procesados", "formated_polarization_df_data", "tara_pol.rds"))

# healthcare
healthcare_pol <- clean_polarization_data("healthcare")
saveRDS(healthcare_pol, here::here("datos_procesados", "formated_polarization_df_data", "healthcare_pol.rds"))