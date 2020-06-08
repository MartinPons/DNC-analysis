# deteccion_bots.R

library(tidyverse)
library(lubridate)

average_diftime <- 
healthcare_cleaned_text %>% 
  group_by(comunidad, user_id, screen_name) %>% 
  arrange(created_at) %>% 
#  rowwise() %>% 
 mutate(timediff = difftime(created_at, lag(created_at), units = "hours")) %>% 
  summarise(mean_timediff = round(mean(as.numeric(timediff), na.rm = T), 2),
            sd_timediff = round(sd(as.numeric(timediff), na.rm = T), 2),
            n = n(),
            days_tweeting = round(as.numeric(difftime(max(created_at), min(created_at) - 3600 * 24, units = "days"))), 
            tweets_per_day = round(n / days_tweeting, 2)) %>% 
  ungroup() %>% 
  mutate(stand_tweets_per_day = scale(tweets_per_day)) %>% 
  filter(n > 10) 
 



# avg_ntweets <- healthcare_cleaned_text %>% 
#   count(user_id, screen_name, comunidad, hora = format(created_at, "%y%m%d-%H")) %>% 
#   ungroup() %>% 
#   group_by(user_id, screen_name, comunidad) %>% 
#   summarise(tweets_hour = mean(n, na.rm = T), 
#             n = n()) %>% 
#   filter(n > 5)
#   


average_diftime %>% 
  ggplot(aes(x = mean_timediff)) + 
  geom_histogram(color = "white")




# EXPLORACION POTENCIALES BOTS --------------------------------------------

healthcare_cleaned_text %>% 
  filter(screen_name %in% c("CVSHealthJobs", "CVSHealthJobs")) %>% 
  mutate(hora = floor_date(created_at, unit = "hour")) %>% 
  complete(hora = seq(floor_date(min(created_at), "hour"), floor_date(max(created_at), "hour"), by = "hour")) %>% 
  count(screen_name, hora) %>% 
  ggplot(aes(x = hora, y = n, group = screen_name)) + 
  geom_line()
 
  
plot_ntweets_timeline <- function(dat, user, un = "hour", fun = geom_point) {
healthcare_cleaned_text %>% 
      filter(screen_name %in% user) %>% 
      mutate(tiempo = floor_date(created_at, unit = un)) %>% 
      complete(tiempo = seq(floor_date(min(created_at), un), floor_date(max(created_at), un), by = un)) %>% 
      count(screen_name, tiempo) %>% 
      ggplot(aes(x = tiempo, y = n, group = screen_name, color = screen_name)) + 
      fun()
    
  }
  
plot_ntweets_timeline(healthcare_cleaned_text, c("UChicagoMedJobs", "CVSHealthJobs", "dinfomall", "jenny_hegel"), "day", geom_line)

