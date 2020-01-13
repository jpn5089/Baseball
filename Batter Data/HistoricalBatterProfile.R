library(lubridate)
library(dplyr)
library(rvest)
library(httr)
library(stringr)
library(tidyr)
library(RcppRoll)

fangraphs_ids <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Batter Data/2019BatterData.csv")


# data pull ---------------------------------------------------------------

batter_stats <- list()

for (i in 17:nrow(fangraphs_ids)) {
  
  url = paste0("https://www.fangraphs.com/statss.aspx?playerid=", fangraphs_ids$playerid[i])
  
  plate_discipline <- read_html(url) %>%
    html_node(xpath = '//*[@id="SeasonStats1_dgSeason7_ctl00"]') %>%
    html_table() %>% 
    mutate(name = fangraphs_ids$ï..Name[i]) %>% 
    filter(!Season %in% c('Total', 'Postseason')) %>% 
    filter(!grepl('\\(', Team))
  
  batted_ball <- read_html(url) %>%
    html_node(xpath = '//*[@id="SeasonStats1_dgSeason3_ctl00"]') %>%
    html_table() %>% 
    mutate(name = fangraphs_ids$ï..Name[i]) %>% 
    filter(!Season %in% c('Total', 'Postseason')) %>% 
    filter(!grepl('\\(', Team)) %>% 
    filter(`Soft%` != "")
  
  plate_appearances <- read_html(url) %>%
    html_node(xpath = '//*[@id="SeasonStats1_dgSeason1_ctl00"]') %>%
    html_table() %>% 
    mutate(name = fangraphs_ids$ï..Name[i]) %>% 
    filter(!Season %in% c('Total', 'Postseason')) %>%
    filter(!Team %in% c('Steamer', 'Depth Charts')) %>% 
    filter(!grepl('\\(', Team)) %>% 
    group_by(Season, Team) %>% 
    mutate(n = row_number()) %>% 
    filter(n == min(n)) %>% 
    ungroup() %>% 
    mutate(dummy_var = case_when(Team == '3 Teams' ~ 3,
                                 Team == '2 Teams' ~ 2,
                                 TRUE ~ 1)) %>% 
    group_by(name, Season) %>% 
    filter(dummy_var == max(dummy_var)) %>% 
    ungroup() %>% 
    select(name, Season, PA)
  
  data <- left_join(plate_discipline, batted_ball) %>% 
    left_join(., plate_appearances)
  
  batter_stats[[i]] <- data
  
  
  print(paste0(i, " - ", fangraphs_ids$ï..Name[i]))
}

all_batter_data <- do.call(rbind, batter_stats)

saveRDS(all_batter_data, "C://Users/johnp/Documents/GitHub/Baseball/Batter Data/2019_batter_type_data.rds")


# analysis ----------------------------------------------------------------

all_batter_data <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Batter Data/2019_batter_type_data.rds")

# MLB plate discipline averages

plate_discipline_avg <- all_batter_data %>% 
  filter(Team == "Average") %>% 
  select(1:11) %>% 
  distinct() %>% 
  filter(Season == "2019") %>% 
  select(-c(Season, Team))

remove_percent_sign <- function(x, na.rm = FALSE) trimws(gsub("%", "", x))

batter_data <- all_batter_data %>% 
  mutate_if(is.character, remove_percent_sign) %>% 
  mutate_at(vars(contains("%")), as.numeric) %>% 
  mutate_at(vars(contains("FB")), as.numeric) %>% 
  mutate_at(vars(contains("Season")), as.numeric) %>% 
  filter(Team != "Average") 

# recent trends

last_3_years <- batter_data %>% 
  filter(Season > 2016) %>% 
  mutate(dummy_var = case_when(Team == '3 Teams' ~ 3,
                               Team == '2 Teams' ~ 2,
                               TRUE ~ 1)) %>% 
  group_by(name, Season) %>% 
  filter(dummy_var == max(dummy_var)) %>% 
  ungroup()

two_year_avg <- last_3_years %>% 
  filter(Season %in% c(2017,2018)) %>% 
  group_by(name) %>% 
  summarise(`O-Swing%_avg` = round(weighted.mean(`O-Swing%`, PA),1),
            `GB/FB_avg` = round(weighted.mean(`GB/FB`, PA),1),
            `Hard%_avg` = round(weighted.mean(`Hard%`, PA),1),
            PA_17_18 = sum(PA)) %>% 
  ungroup()

comparisons_w_2019 <- left_join(two_year_avg, last_3_years %>% filter(Season == 2019) %>% 
                                  select(name, PA_19 = PA, `O-Swing%`, `GB/FB`, `Hard%`)) %>% 
  mutate(o_swing_diff = `O-Swing%` - `O-Swing%_avg`,
         gb_fb_diff = `GB/FB` - `GB/FB_avg`,
         hard_diff = `Hard%` - `Hard%_avg`) 

last_season <- batter_data %>% 
  filter(Season == 2019) %>% 
  mutate(dummy_var = case_when(Team == '3 Teams' ~ 3,
                               Team == '2 Teams' ~ 2,
                               TRUE ~ 1)) %>% 
  group_by(name) %>% 
  filter(dummy_var == max(dummy_var)) %>% 
  ungroup() %>% 
  select(name, everything()) %>% 
  select(-c(Season, Team, `Swing%`, `Contact%`, `Zone%`, `IFFB%`, `IFH%`, `BUH%`, dummy_var))


