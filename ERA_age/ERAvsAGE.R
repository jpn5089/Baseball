library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)

ages <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/ERA_age/Ages.csv")

files <- list.files("C://Users/johnp/Documents/GitHub/Baseball/ERA_age") %>%
  data.frame(files = .) %>%
  filter(grepl("20", files)) %>%
  pull(.)

stats <- list()

for(i in 1:length(files)) {
  print(i)
  x <- read.csv(sprintf("C://Users/johnp/Documents/GitHub/Baseball/ERA_age/%s", files[i]))
  stats[[paste(gsub(".rds" , replacement = "", files[i]))]] <- x
  
}

out <- list()
x <- seq(2006, 2017, 1)

for (i in 1:length(stats)) {
  
  year_name <- names(stats)[i]
  
  year <- stats[[year_name]]
  
  fcast_data <- Reduce(left_join, list(year)) %>% 
    rename(name = ï..Name) %>%
    mutate(year = x[i]) %>% 
    select(name, IP, ER, year)
  
  out[[year_name]] <- fcast_data
  
}

stats_all <- do.call(rbind, out) %>% 
  left_join(., ages, by = c("name" = "Player")) %>% 
  filter(DOB != "") %>% 
  mutate(birthday = mdy(DOB), 
         age_season_adder = if_else(month(birthday) >= 4, 0, 1),
         birth_year = year(birthday),
         age_season = year - birth_year + age_season_adder) %>% 
  filter(age_season > 21 & age_season < 36) %>% 
  group_by(age_season) %>% 
  summarise(IP_total = sum(IP),
            ER_total = sum(ER),
            num_seasons = n()) %>% 
  ungroup() %>% 
  mutate(ERA = (ER_total*9) / IP_total)

# https://rpubs.com/MarkusLoew/226759

ggplot() +
  #geom_ribbon(aes(ymin = 0, ymax = num_players), fill = "grey70") +
  geom_line(data = stats_all, aes(x = age_season, y = ERA)) +
  # geom_line(data = stats_all, aes(x = age_season, y = num_seasons/35)) +
  # scale_y_continuous(sec.axis = sec_axis(~.*35, name = "Relative humidity [%]")) +
  theme_bw()
  

# ggplot(stats_all, aes(ERA, fill = age_season)) +
#   geom_histogram()


