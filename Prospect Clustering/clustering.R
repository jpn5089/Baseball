library(lubridate)
library(dplyr)
library(rvest)
library(httr)
library(stringr)
library(tidyr)

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
  filter(minors > 1) %>%
  filter(seasons > 3) %>%
  filter(key_fangraphs < 30000) %>%
  mutate(early_birthday = ifelse(month(DOB) < 4, "yes", "no"))

standard_batter <- list()
advanced_batter <- list()
batted_ball_batter <- list()
standard_pitcher <- list()
advanced_pitcher <- list()
batted_ball_pitcher <- list()


for (i in 1227:1227) {

    url <- paste0("https://www.fangraphs.com/statss.aspx?playerid=", fangraphs_ids$key_fangraphs[i], "&position=C")
  
    player_page <- GET(as.character(url))
    
    position <- str_sub(player_page$url, -1) 
    
    if (position == "P") {
      
      url = paste0("https://www.fangraphs.com/statss.aspx?playerid=", fangraphs_ids$key_fangraphs[i], "&position=P")
      
      standard <- read_html(url) %>%
        html_node(xpath = '//*[@id="SeasonStats1_dgSeason1_ctl00"]') %>%
        html_table() %>% 
        filter(!grepl("Total|Postseason", Season)) %>% 
        mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
               early_birthday = fangraphs_ids$early_birthday[i],
               age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                                    as.numeric(Season) - birth_year - 1),
               name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
      
      standard_pitcher[[i]] <- standard
      
      advanced <- read_html(url) %>% 
        html_node(xpath = '//*[@id="SeasonStats1_dgSeason2_ctl00"]') %>% 
        html_table() %>% 
        filter(Team != "Average") %>% 
        filter(!grepl("Total|Postseason", Season)) %>% 
        mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
               early_birthday = fangraphs_ids$early_birthday[i],
               age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                                    as.numeric(Season) - birth_year - 1),
               name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
      
      advanced_pitcher[[i]] <- advanced
      
      batted_ball <- read_html(url) %>% 
        html_node(xpath = '//*[@id="SeasonStats1_dgSeason3_ctl00"]') %>% 
        html_table() %>% 
        filter(!grepl("Total|Postseason", Season)) %>% 
        mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
               early_birthday = fangraphs_ids$early_birthday[i],
               age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                                    as.numeric(Season) - birth_year - 1),
               name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
      
      batted_ball_pitcher[[i]] <- batted_ball
      
    } else {
      
      url = paste0("https://www.fangraphs.com/statss.aspx?playerid=", fangraphs_ids$key_fangraphs[i], "&position=C")
    
    
      standard <- read_html(url) %>%
          html_node(xpath = '//*[@id="SeasonStats1_dgSeason1_ctl00"]') %>%
          html_table() %>% 
        filter(!grepl("Total|Postseason", Season)) %>% 
        mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
               early_birthday = fangraphs_ids$early_birthday[i],
               age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                                    as.numeric(Season) - birth_year - 1),
               name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
      
      standard_batter[[i]] <- standard
      
      advanced <- read_html(url) %>% 
        html_node(xpath = '//*[@id="SeasonStats1_dgSeason2_ctl00"]') %>% 
        html_table() %>% 
        filter(Team != "Average") %>% 
        filter(!grepl("Total|Postseason", Season)) %>% 
        mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
               early_birthday = fangraphs_ids$early_birthday[i],
               age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                                    as.numeric(Season) - birth_year - 1),
               name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
      
      advanced_batter[[i]] <- advanced
      
      batted_ball <- read_html(url) %>% 
        html_node(xpath = '//*[@id="SeasonStats1_dgSeason3_ctl00"]') %>% 
        html_table() %>% 
        filter(!grepl("Total|Postseason", Season)) %>% 
        mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
               early_birthday = fangraphs_ids$early_birthday[i],
               age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                                    as.numeric(Season) - birth_year - 1),
               name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
      
      batted_ball_batter[[i]] <- batted_ball
    
    }
    print(paste0(i, " - ", fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
}

standard_p_raw <- do.call(rbind, standard_pitcher) %>% 
  select(-c(early_birthday, birth_year)) %>% 
  filter(!Team %in% c("ATC", "Depth Charts", "Steamer", "ZiPS", "Steamer (R)", "THE BAT", "Depth Charts (R)",
                      "ZiPS (R)", "THE BAT (R)")) %>% 
  group_by(Team, Season, name) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

advanced_p_raw <- do.call(rbind, advanced_pitcher) %>% 
  select(-c(early_birthday, birth_year)) %>% 
  filter(!Team %in% c("ATC", "Depth Charts", "Steamer", "ZiPS", "Steamer (R)", "THE BAT", "Depth Charts (R)",
                      "ZiPS (R)", "THE BAT (R)")) %>%
  group_by(Team, Season, name) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

batted_ball_p_raw <- do.call(rbind, batted_ball_pitcher) %>% 
  select(-c(early_birthday, birth_year)) %>% 
  group_by(Team, Season, name) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

standard_b_raw <- do.call(rbind, standard_batter) %>% 
  select(-c(early_birthday, birth_year)) %>% 
  filter(!Team %in% c("ATC", "Depth Charts", "Steamer", "ZiPS", "Steamer (R)", "THE BAT", "Depth Charts (R)",
                      "ZiPS (R)", "THE BAT (R)")) %>% 
  group_by(Team, Season, name) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

advanced_b_raw <- do.call(rbind, advanced_batter) %>% 
  select(-c(early_birthday, birth_year, AVG)) %>% 
  filter(!Team %in% c("ATC", "Depth Charts", "Steamer", "ZiPS", "Steamer (R)", "THE BAT", "Depth Charts (R)",
                      "ZiPS (R)", "THE BAT (R)")) %>% 
  group_by(Team, Season, name) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

batted_ball_b_raw <- do.call(rbind, batted_ball_batter) %>% 
  group_by(Team, Season, name) %>% 
  filter(row_number() == 1) %>% 
  ungroup()


# Minor League Player Pull ------------------------------------------------

url = paste0("https://www.fangraphs.com/statss.aspx?playerid=sa739599&position=P")
DOB = ymd('1995-11-23')
early_birthday = "no"
name = "Lewis Thorpe"

standard <- read_html(url) %>%
  html_node(xpath = '//*[@id="SeasonStats1_dgSeason1_ctl00"]') %>%
  html_table() %>% 
  filter(!grepl("Total|Postseason", Season)) %>% 
  mutate(birth_year  = as.numeric(year(DOB)),
         early_birthday = early_birthday,
         age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                              as.numeric(Season) - birth_year - 1),
         name = name) %>% 
  select(-c(early_birthday, birth_year)) %>% 
  filter(!Team %in% c("ATC", "Depth Charts", "Steamer", "ZiPS", "Steamer (R)", "THE BAT", "Depth Charts (R)",
                      "ZiPS (R)", "THE BAT (R)"))

advanced <- read_html(url) %>% 
  html_node(xpath = '//*[@id="SeasonStats1_dgSeason2_ctl00"]') %>% 
  html_table() %>% 
  filter(Team != "Average") %>% 
  filter(!grepl("Total|Postseason", Season)) %>% 
  mutate(birth_year  = as.numeric(year(DOB)),
         early_birthday = early_birthday,
         age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                              as.numeric(Season) - birth_year - 1),
         name = name) %>% 
  select(-c(early_birthday, birth_year)) %>% 
  filter(!Team %in% c("ATC", "Depth Charts", "Steamer", "ZiPS", "Steamer (R)", "THE BAT", "Depth Charts (R)",
                      "ZiPS (R)", "THE BAT (R)"))

batted_ball <- read_html(url) %>% 
  html_node(xpath = '//*[@id="SeasonStats1_dgSeason3_ctl00"]') %>% 
  html_table() %>% 
  filter(!grepl("Total|Postseason", Season)) %>% 
  mutate(birth_year  = as.numeric(year(DOB)),
         early_birthday = early_birthday,
         age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
                              as.numeric(Season) - birth_year - 1),
         name = name) %>% 
  select(-c(early_birthday, birth_year)) %>% 
  filter(!Team %in% c("ATC", "Depth Charts", "Steamer", "ZiPS", "Steamer (R)", "THE BAT", "Depth Charts (R)",
                      "ZiPS (R)", "THE BAT (R)"))

MILB_final <- Reduce(left_join, list(standard, advanced, batted_ball)) %>%
  separate(Team, c("Team", "Level"), sep ="[(]") %>% 
  mutate(Level = gsub("\\)","", Level)) %>% 
  select(name, age_season, Season, Team, Level, ERA, IP, `K/9`, `BB/9`, `K/BB`, `HR/9`, `K%`, `BB%`, `K/BB`,
         AVG, WHIP, BABIP, `LOB%`, FIP, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, xFIP)

# Data Prep --------------------------------------------------------------

all_pitching <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Prospect Clustering/all_pitching_data.rds") %>% 
  filter(!grepl("Fans", Team)) %>% 
  filter(IP > 5) %>% 
  select(name, everything())

minors <- c("\\(A\\)","\\(A\\+\\)","\\(A-\\)","\\(AA\\)","\\(AAA\\)","\\(R\\)")

minor_league_pitching <- all_pitching %>%
  filter(grepl(paste(minors, collapse = "|"), Team)) %>%
  separate(Team, c("Team", "Level"), sep ="[(]") %>% 
  mutate(Level = gsub("\\)","", Level))
  
major_league_pitching <- all_pitching %>% 
  filter(!grepl(paste(minors, collapse = "|"), Team)) %>% 
  group_by(name, Season) %>% 
  mutate(teams_per_season = n()) %>% 
  select(teams_per_season, everything()) %>% 
  ungroup() 

major_league_pitching_1_team <- major_league_pitching %>% 
  filter(teams_per_season == 1)

pitching_2_teams <- major_league_pitching %>% 
  filter(teams_per_season > 1) %>% 
  group_by(name, Season) %>% 
  filter(grepl("Team", Team)) %>% 
  ungroup() 
  
pitching_final <- rbind(major_league_pitching_1_team, pitching_2_teams) %>% 
  mutate(Level = "MLB") %>% 
  select(-teams_per_season) %>% 
  rbind(minor_league_pitching) %>% 
  select(Level, everything()) %>% 
  select(name, age_season, Season, Team, Level, ERA, IP, `K/9`, `BB/9`, `K/BB`, `HR/9`, `K%`, `BB%`, `K/BB`,
         AVG, WHIP, BABIP, `LOB%`, FIP, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, xFIP)
  
all_batting <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Prospect Clustering/all_batting_data.rds") %>% 
  filter(!grepl("Fans", Team))


# Clustering --------------------------------------------------------------

like_players <- pitching_final %>% 
  rbind(., MILB_final) %>% 
  # filter((Level == "AA" & age_season %in% c(23,24)) || (Level == "AAA" & age_season %in% c(24,25)))
  filter(Level == "AA" & age_season %in% c(21,22)) %>% 
  group_by(Season, name, Level) %>% 
  mutate(rank = rank(IP)) %>% 
  filter(rank == 1) %>% 
  select(-rank) %>%
  ungroup()

filtered_df <- like_players %>% 
  select(-c(`LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, xFIP)) %>% 
  select(-c(Season, Level, Team, IP)) %>% 
  gather(var, value, -c(name, age_season)) %>% 
  mutate(name = paste0(name, "_", age_season)) %>% 
  select(-c(age_season)) %>% 
  spread(var, value)

final_df <- filtered_df[-1]

row.names(final_df) <- filtered_df$name

mat <- as.matrix(final_df)

d <- dist(mat)

hc <- hclust(d)
plot(hc)
hcd = as.dendrogram(hc)
plot(cut(hcd, h = 15)$lower[[2]])

group <- cutree(hc, k = 9) %>% 
  as_data_frame() %>%
  mutate(var = row.names(.)) %>%
  select(var, cluster = value)
