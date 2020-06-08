# limpieza_de_tweets.R


# libraries

# En caso de problemas con qdap mirar como esta variable de entorno para java y cambiar
# si un caso
library(qdapRegex)
library(sentimentr)
library(syuzhet)
library(lubridate)
library(tidyverse)
library(tidytext)

# detectar emojis
# https://stackoverflow.com/questions/43359066/how-can-i-match-emoji-with-an-r-regex
library(remoji)

library(lexicon)
library(rtweet)
Sys.getenv()
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_251")

library(tm)
library(qdap)

# lista de emojis
emo <- emoji(list_emoji(), TRUE)
# funciones auxiliares
source(here::here("scripts", "funciones_auxiliares.R"))


comunidades <- readRDS(here::here("datos_procesados", "comunidades.rda")) 
health <- readRDS(here::here("datos_raw", "JoeBiden_tweet_extraction__2020-04-27.rda")) 




health_cleaned <- clean_tweets_df(health, comunidades)




health_cleaned_text <- health_cleaned %>% 
  mutate(text = clean_text(text))

text <- health_cleaned_text$text


# CALCULO SENTIMIENTO CON sentimentr --------------------------------------

sent <- sentiment_by(health_cleaned_text$text)

# jutamos tentimento con data frame
health_cleaned_text <- health_cleaned_text %>% 
  bind_cols(sent)


# test de diferencia de medias
dnc_prog <- filter(health_cleaned_text, comunidad %in% c("DNC", "Progressives"))
t.test(ave_sentiment ~ comunidad, data = dnc_prog)

# diseñamos para 80 % de potencia y 0.05 de alpha??
# tamaño del efecto
# comparar con control: donald trump

# histograma
health_cleaned_text %>% 
  filter(comunidad %in% c("DNC", "Progressives")) %>% 
  ggplot(aes(x = ave_sentiment)) + 
  geom_density(aes(fill = comunidad)) + 
  theme_bw()

# boxplot
health_cleaned_text %>% 
  ggplot(aes(x = comunidad, y = ave_sentiment)) + 
  geom_boxplot(aes(fill = comunidad))


# serie temporal
health_cleaned_text %>% 
  mutate(fecha = as.Date(created_at)) %>%
  group_by(fecha, comunidad) %>% 
  summarise(score = mean(ave_sentiment)) %>% 
  ggplot(aes(x = fecha, y = score)) + 
  geom_line(aes(color = comunidad, group = comunidad))





# SENTIMIENTO CON qdap ----------------------------------------------------
polar <- polarity(health_cleaned_text$text)
saveRDS(polar, here::here("datos_procesados", "qdap_polarity_test.rda"))
health_cleaned_text$qdap_polarity <- polar$all$polarity

# media por grupos
health_cleaned_text %>% 
  group_by(comunidad) %>% 
  summarise(pol = mean(qdap_polarity))

# dostribucion
health_cleaned_text %>% 
#  filter(comunidad %in% c("DNC", "Progressives")) %>% 
  ggplot(aes(x = qdap_polarity)) + 
  geom_density(aes(fill = comunidad)) + 
  theme_bw()

# boxplot
health_cleaned_text %>% 
  ggplot(aes(x = comunidad, y = qdap_polarity)) + 
  geom_boxplot(aes(fill = qdap_polarity))

# serie temporal
health_cleaned_text %>% 
  mutate(fecha = as.Date(created_at)) %>%
  group_by(fecha, comunidad) %>% 
  summarise(score = mean(qdap_polarity)) %>% 
  ggplot(aes(x = fecha, y = score)) + 
  geom_line(aes(color = comunidad, group = comunidad))



# TIDYTEXT ----------------------------------------------------------------

# tidytext
health_tidy <- health_cleaned_text %>% 
  unnest_tokens(word, text)
  
# --------------- ## Afinn sent ## -----------------------

# afinn score
health_tidy_afinn <- health_tidy %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(status_id, comunidad) %>% 
  summarise(afinn_sent = mean(value))

# media por grupo
health_tidy_afinn %>% 
  group_by(comunidad) %>% 
  summarise(afinn_sent = mean(afinn_sent)) 

# distribucion densidad
health_tidy_afinn %>% 
  ggplot(aes(x = afinn_sent)) + 
  geom_density(aes(fill = comunidad), alpha = 0.5)

# distribucion boxplot
health_tidy_afinn %>% 
  ggplot(aes(x = comunidad, y = afinn_sent)) + 
  geom_boxplot(aes(fill = comunidad), varwidth = T)

## agrupacion por user ##
health_tidy_afinn_user <- health_tidy %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(user_id, comunidad) %>% 
  summarise(afinn_sent = mean(value))

# media por grupo
health_tidy_afinn_user %>% 
  group_by(comunidad) %>% 
  summarise(afinn_sent = mean(afinn_sent))
  
# distribucion densidad
health_tidy_afinn_user %>% 
  ggplot(aes(x = afinn_sent)) + 
  geom_density(aes(fill = comunidad), alpha = 0.5)

# distribucion boxplot
health_tidy_afinn_user %>% 
  ggplot(aes(x = comunidad, y = afinn_sent)) + 
  geom_boxplot(aes(fill = comunidad), varwidth = T)

# ----------------- ## Bing sent ## ------------------------

health_tidy_bing <- health_tidy %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  group_by(status_id, comunidad, sentiment) %>% 
  count() %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(diff_sent = positive - negative)

# media por grupo
health_tidy_bing %>% 
  group_by(comunidad) %>% 
  summarise(diff_sent = mean(diff_sent))

health_tidy_bing %>% 
  ggplot(aes(x = comunidad, y = diff_sent)) + 
  geom_boxplot()


# ---------------- ## nrc sent ## ---------------------------

health_tidy_nrc <- health_tidy %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  group_by(status_id, comunidad, sentiment) %>% 
  count() 

health_tidy_nrc %>% 
  group_by(comunidad, sentiment) %>% 
  summarise(nrc_sent = mean(n)) %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  pivot_wider(names_from = comunidad, values_from = nrc_sent) %>% 
  chartJSRadar()


# PRUEBAS ----------------------------------------------------------------

