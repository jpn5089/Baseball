library(dplyr)
library(tidyr)

league_players <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/War_Input_2018.csv") %>% 
  mutate(playerid = as.character(playerid)) %>% 
  mutate(Team = as.character(Team)) %>% 
  select(playerid, Team)

steamer_pitchers <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/SteamerPitchers.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid)   
steamer_hitters <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/SteamerHitters.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid) %>% 
  filter(playerid != 19755)

steamer <- rbind(steamer_hitters, steamer_pitchers) %>% 
  mutate(playerid = as.character(playerid))

zips_pitchers <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/ZipsPitchers.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid) 
zips_hitters <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/ZipsBatters.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid) %>% 
  filter(playerid != 19755)

zips <- rbind(zips_hitters, zips_pitchers) %>% 
  mutate(playerid = as.character(playerid)) 

FG_pitchers <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/FanGraphsPitchers.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid) 

FG_hitters <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/FanGraphsHitters.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid) %>% 
  filter(playerid != 19755)

FG <- rbind(FG_hitters, FG_pitchers) %>% 
  mutate(playerid = as.character(playerid))

FGdepth_pitchers <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/FGDepthPitchers.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid) 

FGdepth_hitters <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/FGDepthHitters.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, WAR, playerid) %>% 
  filter(playerid != 19755)

FG_depth <- rbind(FGdepth_pitchers, FGdepth_hitters) %>% 
  mutate(playerid = as.character(playerid))
  
all <- left_join(league_players, steamer) %>% 
  rename(Steamer_WAR = WAR) %>% 
  mutate(name = as.character(name)) %>% 
  select(-name) %>% 
  left_join(., zips) %>% 
  mutate(name = as.character(name)) %>% 
  rename(zips_WAR = WAR) %>% 
  select(-name) %>%
  left_join(., FG_depth) %>% 
  mutate(name = as.character(name)) %>% 
  rename(FG_Depth_WAR = WAR) %>% 
  select(-playerid) %>% 
  gather(projection, WAR, -Team, -name) %>% 
  group_by(projection, Team) %>% 
  summarise(WAR_Total = sum(WAR)) %>% 
  spread(projection, WAR_Total) %>% 
  ungroup() 

mean_sd <- all %>%
  gather(projection, WAR, -Team) %>%
  group_by(Team) %>% 
  summarise(mean = mean(WAR),
            sd = sd(WAR)) %>% 
  ungroup()

final <- left_join(all, mean_sd)

write.csv(final, "C://Users/johnp/Documents/GitHub/Baseball/2018 WAR Projections/league_projections.csv")
