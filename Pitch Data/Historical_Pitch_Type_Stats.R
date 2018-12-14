library(lubridate)
library(dplyr)
library(rvest)
library(httr)
library(stringr)
library(tidyr)
library(RcppRoll)

#Fix Age Season Next time (Use July 1)

# data collection -------------------------------------------------------------

url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"

chadwick_player_lu_table <- readr::read_csv(url) %>%
  filter(!is.na(key_fangraphs))

fangraphs_ids <- chadwick_player_lu_table %>%
  filter(birth_year > 1965) %>%
  select(name_last, name_first, key_fangraphs, birth_year, birth_month, birth_day, pro_played_first,
         mlb_played_first, mlb_played_last) %>%
  mutate(name = paste0(name_first, " ", name_last),
         DOB = ymd(paste(birth_year, birth_month, birth_day)),
         minors = mlb_played_first - pro_played_first,
         seasons = mlb_played_last - mlb_played_first)  %>%
  filter(key_fangraphs < 30000) %>% 
  filter(mlb_played_last > 2006) %>% 
  mutate(early_birthday = ifelse(month(DOB) < 7, "yes", "no"))

# pitch_stats <- list()
# 
# # for (i in 1:nrow(fangraphs_ids)) {
# for (i in 3907:nrow(fangraphs_ids)) {
#   
#   url <- paste0("https://www.fangraphs.com/statss.aspx?playerid=", fangraphs_ids$key_fangraphs[i], "&position=C")
#   
#   player_page <- GET(as.character(url))
#   
#   position <- str_sub(player_page$url, -1) 
#   
#   if (position == "P") {
#     
#     url = paste0("https://www.fangraphs.com/pitchfx.aspx?playerid=", fangraphs_ids$key_fangraphs[i], "&position=P&pitch=all")
#     
#     pfx <- read_html(url) %>%
#       html_node(xpath = '//*[@id="PFXOverview1_dgSeason4_ctl00"]') %>%
#       html_table() %>% 
#       mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
#              early_birthday = fangraphs_ids$early_birthday[i],
#              age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
#                                   as.numeric(Season) - birth_year - 1),
#              name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
#     
#     pitch_stats[[i]] <- pfx
#     
#   } else 
#     next
#   
#   print(paste0(i, " - ", fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
# }
# 
# all_pitch_data <- do.call(rbind, pitch_stats)
# 
# saveRDS(all_pitch_data, "C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/historical_pitch_type_data.rds")

all_pitch_data <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/historical_pitch_type_data.rds") %>% 
  filter(Season != "Total") %>% 
  filter(Pitches > 200) %>% 
  mutate(`SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `O-Swing%` = as.numeric(gsub("%", "", as.character(`O-Swing%`))),
         `Swing%` = as.numeric(gsub("%", "", as.character(`Swing%`))),
         `Z-Swing%` = as.numeric(gsub("%", "", as.character(`Z-Swing%`))),
         `Z-Contact%` = as.numeric(gsub("%", "", as.character(`Z-Contact%`))),
         `O-Contact%` = as.numeric(gsub("%", "", as.character(`O-Contact%`))),
         `Contact%` = as.numeric(gsub("%", "", as.character(`Contact%`))),
         `Zone%` = as.numeric(gsub("%", "", as.character(`Zone%`)))) %>% 
  select(name, everything())

avg <- all_pitch_data %>% 
  group_by(Pitch) %>% 
  summarise(total_pitches = sum(Pitches),
            SwStr_avg = mean(`SwStr%`),
            O_Swing_avg = mean(`O-Swing%`)) %>% 
  ungroup()

total_pitches <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/historical_pitch_type_data.rds") %>% 
  filter(Season != "Total") %>% 
  group_by(name, Season) %>%
  summarise(total_pitches = sum(Pitches)) %>% 
  ungroup()
  
pitch_percentages <- all_pitch_data %>% 
  left_join(., total_pitches) %>%  
  mutate(pitch_pct = Pitches/total_pitches)
  

improvers <- all_pitch_data %>% 
  filter(Season > 2016) 

rolling_3yr_best_pitch_oswing <- all_pitch_data %>% 
  select(name, `O-Swing%`, Season, Pitch) %>% 
  mutate(Season = as.numeric(Season)) %>% 
  arrange(Season) %>% 
  group_by(name, Pitch) %>% 
  mutate(roll_3yr_oswing = roll_mean(`O-Swing%`, 3)) %>% 
  ungroup() %>% 
  filter(`O-Swing%` != roll_3yr_oswing)

