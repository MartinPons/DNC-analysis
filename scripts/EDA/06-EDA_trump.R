# 06-EDA_trump.R



# CONFIGURACION INICIAL ---------------------------------------------------

#librerias
library(tidyverse)
library(boot)


# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))

# POLARIZACIÓN ------------------------------------------------------------

# carga de datos
trump_net <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "trump_pol.rds"))
comunidad <- readRDS(here::here("datos_procesados", "comunidades", "comunidades.rda")) %>% 
  mutate(user_id = as.character(user_id))

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

trump %>% 
  count(comunidad) %>% 
  mutate(f = percent(n/sum(n), accuracy = 0.1))


# paleta
plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")



# filtro comunidades
trp_gop_dnc <- trump_net %>%
  filter_communities("DNC", "GOP")

# nodos internos
nodos_internos <- get_inner_nodes(trp_gop_dnc)

# datos polaridad
trp_gop_dnc_pol <- map_df(unique(trp_gop_dnc$from), ~get_node_pol_data(trp_gop_dnc, nodos_internos, .x))

mean(trp_gop_dnc_pol$polarity_score)


# Progressives VS DNC --------------------------------------------------------------

# filtro comunidades
trp_prg_dnc <- trump_net %>%
  filter_communities("DNC", "Progressives")

# nodos internos
nodos_internos <- get_inner_nodes(trp_prg_dnc)

# datos polaridad
trp_prg_dnc_pol <- map_df(unique(trp_prg_dnc$from), ~get_node_pol_data(trp_prg_dnc, nodos_internos, .x))

mean(trp_prg_dnc_pol$polarity_score)

test_data <- trp_gop_dnc_pol %>% 
  mutate(comunidades = "DNC-GOP") %>% 
  bind_rows(trp_prg_dnc_pol %>% mutate(comunidades = "DNC-PRG")) %>% 
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
saveRDS(pol_test_dif, here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "trump_polarization.rds"))

pol_test_dif <- readRDS(here::here("datos_procesados", "formated_polarization_df_data", "polarization_testing", "trump_polarization.rds"))

pol_test_dif[-1]
