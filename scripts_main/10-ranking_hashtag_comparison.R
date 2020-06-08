# 11-ranking_hashtag_comparison.R



# CONFIGURACION INIICAL ---------------------------------------------------

# librerias
library(tidyverse)


# funciones auxiliares

get_common_terms <- function(set1, set2) {
  
  if (length(set1) != length(set2)) stop("Los vectores tienen que ser de la misma longitud")
  
  common_words <- integer(length = length(set1))
  
  for (idx in 1:length(set1)) {
    
    subset1 <- set1[1:idx]
    subset2 <- set2[1:idx]
    
    common_words[idx] <- length(intersect(subset1, subset2))
    
    
  }
  
  common_words
}

# constantes
plots_palette <- c("#ad5d51", "grey55", "#2b559e", "#947240")


# CARGA DE DATOS ----------------------------------------------------------

climate <- readRDS(here::here("datos_procesados", "topics_processed", "climatechange_hashtag_frequency.rds")) %>% 
  filter(n_usuarios > 3) %>% 
  group_by(comunidad) %>% arrange(desc(n))

education <- readRDS(here::here("datos_procesados", "topics_processed", "education_hashtag_frequency.rds")) %>% 
  filter(n_usuarios > 3) %>% 
  group_by(comunidad) %>% arrange(desc(n))

healthcare <- readRDS(here::here("datos_procesados", "topics_processed", "healthcare_hashtag_frequency.rds")) %>% 
  filter(n_usuarios > 3) %>% 
  group_by(comunidad) %>% arrange(desc(n))

biden <- readRDS(here::here("datos_procesados", "topics_processed", "biden_hashtag_frequency.rds")) %>% 
  filter(n_usuarios > 3) %>% 
  group_by(comunidad) %>% arrange(desc(n))

trump <- readRDS(here::here("datos_procesados", "topics_processed", "trump_hashtag_frequency.rds")) %>% 
  filter(n_usuarios > 3) %>% 
  group_by(comunidad) %>% arrange(desc(n))

sanders <- readRDS(here::here("datos_procesados", "topics_processed", "sanders_hashtag_frequency.rds")) %>% 
  filter(n_usuarios > 3) %>% 
  group_by(comunidad) %>% arrange(desc(n))



# WRANGLING ---------------------------------------------------------------

# climate
top_climate_gop <- climate[climate$comunidad == "GOP", "word", drop = T][1:40]
top_climate_dnc <- climate[climate$comunidad == "DNC", "word", drop = T][1:40]
top_climate_prg <- climate[climate$comunidad == "Progressives", "word", drop = T][1:40]

common_climate_dnc_gop <- get_common_terms(top_climate_dnc, top_climate_gop)
common_climate_dnc_prg <- get_common_terms(top_climate_dnc, top_climate_prg)

common_climate <- data.frame(
  top_words = rep(1:length(common_climate_dnc_gop), 2),
  common_words = c(common_climate_dnc_gop, common_climate_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Climate Change"
)


# education
top_education_gop <- education[education$comunidad == "GOP", "word", drop = T][1:40]
top_education_dnc <- education[education$comunidad == "DNC", "word", drop = T][1:40]
top_education_prg <- education[education$comunidad == "Progressives", "word", drop = T][1:40]

common_education_dnc_gop <- get_common_terms(top_education_dnc, top_education_gop)
common_education_dnc_prg <- get_common_terms(top_education_dnc, top_education_prg)

common_education <- data.frame(
  top_words = rep(1:length(common_education_dnc_gop), 2),
  common_words = c(common_education_dnc_gop, common_education_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Education"
)

# education
top_education_gop <- education[education$comunidad == "GOP", "word", drop = T][1:40]
top_education_dnc <- education[education$comunidad == "DNC", "word", drop = T][1:40]
top_education_prg <- education[education$comunidad == "Progressives", "word", drop = T][1:40]

common_education_dnc_gop <- get_common_terms(top_education_dnc, top_education_gop)
common_education_dnc_prg <- get_common_terms(top_education_dnc, top_education_prg)

common_education <- data.frame(
  top_words = rep(1:length(common_education_dnc_gop), 2),
  common_words = c(common_education_dnc_gop, common_education_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Education"
)


# healthcare
top_healthcare_gop <- healthcare[healthcare$comunidad == "GOP", "word", drop = T][1:40]
top_healthcare_dnc <- healthcare[healthcare$comunidad == "DNC", "word", drop = T][1:40]
top_healthcare_prg <- healthcare[healthcare$comunidad == "Progressives", "word", drop = T][1:40]

common_healthcare_dnc_gop <- get_common_terms(top_healthcare_dnc, top_healthcare_gop)
common_healthcare_dnc_prg <- get_common_terms(top_healthcare_dnc, top_healthcare_prg)

common_healthcare <- data.frame(
  top_words = rep(1:length(common_healthcare_dnc_gop), 2),
  common_words = c(common_healthcare_dnc_gop, common_healthcare_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Healthcare"
)

# biden
top_biden_gop <- biden[biden$comunidad == "GOP", "word", drop = T][1:40]
top_biden_dnc <- biden[biden$comunidad == "DNC", "word", drop = T][1:40]
top_biden_prg <- biden[biden$comunidad == "Progressives", "word", drop = T][1:40]

common_biden_dnc_gop <- get_common_terms(top_biden_dnc, top_biden_gop)
common_biden_dnc_prg <- get_common_terms(top_biden_dnc, top_biden_prg)

common_biden <- data.frame(
  top_words = rep(1:length(common_biden_dnc_gop), 2),
  common_words = c(common_biden_dnc_gop, common_biden_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Joe Biden"
)


# trump
top_trump_gop <- trump[trump$comunidad == "GOP", "word", drop = T][1:40]
top_trump_dnc <- trump[trump$comunidad == "DNC", "word", drop = T][1:40]
top_trump_prg <- trump[trump$comunidad == "Progressives", "word", drop = T][1:40]

common_trump_dnc_gop <- get_common_terms(top_trump_dnc, top_trump_gop)
common_trump_dnc_prg <- get_common_terms(top_trump_dnc, top_trump_prg)

common_trump <- data.frame(
  top_words = rep(1:length(common_trump_dnc_gop), 2),
  common_words = c(common_trump_dnc_gop, common_trump_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Donald Trump"
)


# sanders
top_sanders_gop <- sanders[sanders$comunidad == "GOP", "word", drop = T][1:40]
top_sanders_dnc <- sanders[sanders$comunidad == "DNC", "word", drop = T][1:40]
top_sanders_prg <- sanders[sanders$comunidad == "Progressives", "word", drop = T][1:40]

common_sanders_dnc_gop <- get_common_terms(top_sanders_dnc, top_sanders_gop)
common_sanders_dnc_prg <- get_common_terms(top_sanders_dnc, top_sanders_prg)

common_sanders <- data.frame(
  top_words = rep(1:length(common_sanders_dnc_gop), 2),
  common_words = c(common_sanders_dnc_gop, common_sanders_dnc_prg),
  comparacion = rep(c("DNC-GOP", "DNC-PRG"), each = 40),
  topic = "Bernie sanders"
)


#


# VISUALIZACIÓN -----------------------------------------------------------

## Topicos ##

# union de data frames: topicos
common_words <- bind_rows(common_healthcare, common_climate, common_education)

common_words %>% 
  ggplot(aes(top_words, common_words, color = comparacion)) + 
  geom_step(size = 1.2) + 
  geom_text(data = common_words %>% filter(top_words == 40), 
            aes(label = common_words), show.legend = F, nudge_x = 2, fontface = "bold", size = 5) + 
  facet_wrap(~topic, scales = "free") + 
  labs(x = "Ranking de los hashtags más empleados", 
       y = "Hashtags comunes", 
       title = "Número de hashtags comunes entre DNC con las otras dos comunidades") +
  scale_color_manual(values = plots_palette[c(1, 4)]) +
  scale_x_continuous(limits = c(1, 43)) +
  scale_y_continuous(limits = c(0, 25)) + 
 
  theme_bw() + 
  theme(text = element_text(family = "Georgia", color = "grey35"),
        panel.border = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 13),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(color = "grey35", size = 15),
        axis.line = element_line(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 17))



common_healthcare %>% 
  ggplot(aes(top_words, common_words, color = comparacion)) + 
  geom_step(size = 1.2) + 
  geom_text(data = common_healthcare %>% filter(top_words == 40), 
            aes(label = common_words), show.legend = F, nudge_x = 2, fontface = "bold", size = 5) + 
  labs(x = "Ranking de los hashtags más empleados", 
       y = "Hashtags comunes", 
       title = "Healthcare: número de hashtags comunes entre DNC con las otras dos comunidades") +
  scale_color_manual(values = plots_palette[c(1, 4)]) +
  scale_x_continuous(limits = c(1, 43)) +
  
  theme_bw() + 
  theme(text = element_text(family = "Georgia", color = "grey35"),
        panel.border = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 13),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(color = "grey35", size = 15),
        axis.line = element_line(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 14))


## Personalidades ##

# union de data frames: personalidades
common_words <- bind_rows(common_biden, common_sanders, common_trump)

common_words %>% 
  ggplot(aes(top_words, common_words, color = comparacion)) + 
  geom_step(size = 1.2) + 
  geom_text(data = common_words %>% filter(top_words == 40), 
            aes(label = common_words), show.legend = F, nudge_x = 2, fontface = "bold", size = 5) + 
  facet_wrap(~topic, scales = "free") + 
  labs(x = "Ranking de los hashtags más empleados", 
       y = "Hashtags comunes", 
       title = "Número de hashtags comunes entre DNC con las otras dos comunidades") +
  scale_color_manual(values = plots_palette[c(1, 4)]) +
  scale_x_continuous(limits = c(1, 43)) +
  scale_y_continuous(limits = c(0, 25)) + 
  
  theme_bw() + 
  theme(text = element_text(family = "Georgia", color = "grey35"),
        panel.border = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 13),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(color = "grey35", size = 15),
        axis.line = element_line(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 17))
