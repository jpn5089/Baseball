library(rvest)
library(dplyr)
library(lubridate)

ages <- read.csv("C://Users/johnp/Downloads/SGP Rankings Tool_19Mar11 - PLAYERIDMAP.csv") %>% 
  select(name = PLAYERNAME, DOB = BIRTHDATE) %>% 
  mutate(DOB = mdy(DOB),
         age = round(difftime(today(), DOB, units = 'weeks')/52,1))

steamer_bat <- read.csv('C:/Users/johnp/Downloads/steamer_bat.csv') %>% 
  mutate(name = ï..Name) %>% 
  select(name, PA, H, BB, SB, RBI, HR, R) %>% 
  mutate(pts = H + BB + (SB*3) + (HR*3) + RBI + R,
         pts_per_PA = pts/PA,
         pos = 'Batter') %>% 
  arrange(desc(pts))


steamer_pitch <- read.csv('C:/Users/johnp/Downloads/steamer_pitch.csv') %>% 
  mutate(name = ï..Name) %>% 
  select(name, IP, ER, BB, H, SV, SO, W) %>% 
  mutate(pts = (W*3) + (SV*6) - ((H + BB)*0.5) - (ER*1.5) + (IP*1.5) + (SO*1.5),
         pts_per_IP = pts/IP,
         pos = 'Pitcher') %>% 
  arrange(desc(pts))

all <- rbind(steamer_bat %>% select(name, pts, pos), steamer_pitch %>% select(name, pts, pos)) %>% 
  arrange(desc(pts)) %>% 
  left_join(., ages)