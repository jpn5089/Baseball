library(XML)
library(dplyr)
library(lubridate)
library(httr)

#File with Player Fangraphs URL and xpath
test <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/2018/WAR_Input_2018.csv") %>% 
  select(1:12) %>% 
  mutate(Name = as.character(Name),
         value = as.character(value),
         value = gsub("[[:punct:]]", " ", value))

#Use merge statement if you just grab csv's with 2018 stats straight from FanGraphs 
#Merge needed bc those datasets may not include all of our 360 players

#Injured, Suspended, Minors

#Team Arfin                -> Nelson
#The Fireballing Five      -> Lamet
#Team Martin               -> No One
#Team Heckmann             -> No One
#Nuck Bob Futting          -> No One
#Team Brothers             -> No One
#Team Baumgardner          -> No One
#Team Fink                 -> Salazar
#Ringin' Dingers           -> No One
#Spend Nutting Win Nutting -> Kopech
#Crash Davis' Team         -> No One

#Create empty list  
war <- list()

# https://stackoverflow.com/questions/16255988/htmlparse-errors-on-accessing-google-search-is-their-an-alternative-approach
# I think i just need to change everything to start with https

for (i in 1:335){
  print(i)
  print(test[i,2])
  player_page <- GET(as.character(test$url[i]))
  data <- htmlParse(content(player_page, as = 'text')) #URL for parsing WAR
  war_path <- xpathSApply(data, as.character(test[i,9]), xmlValue)
  PA_IP_path <- xpathSApply(data, as.character(test[i,10]), xmlValue)
  #Choose values you want to keep from CSV 
  war[[i]] <- data.frame(Player = test[i,2], Price = test[i,5], Position = test[i,4], 
                         Team = test[i,6], WAR = war_path, PA_IP = PA_IP_path, Status = test[i,11]) 
}

#Create your data table
war_final <- do.call(rbind,war) %>%
  mutate(WAR   = as.numeric(as.character(WAR)),
         Price   = as.numeric(as.character(Price)),
         PA_IP   = as.numeric(as.character(PA_IP)),
         WAR = if_else(Player %in% c('Corey Seager', 'Gregory Polanco', 'Tommy Kahnle', 'Dinelson Lamet',
                                     'Domingo Santana', 'Jimmy Nelson',
                                     'Danny Salazar', 'Jay Bruce'), 0, WAR),
         PA_IP = if_else(Player %in% c('Corey Seager', 'Gregory Polanco', 'Tommy Kahnle', 'Dinelson Lamet',
                                       'Domingo Santana', 'Jimmy Nelson', 
                                       'Danny Salazar','Jay Bruce'), 0, PA_IP),
         WARperDollar = round(WAR/Price,4),
         WAR = case_when(Status == "Bench" ~ WAR*0.5,
                         Status == "Starter" ~ WAR),
         PA_IP = case_when(Status == "Bench" ~ PA_IP*0.5,
                           Status == "Starter" ~ PA_IP))

#Set their WAR and PA_IP value to 0
# war_final$WAR[114] <- 0.0
# war_final$PA_IP[114] <- 74
# war_final$WARperDollar[114] <- 0

#Total War by Team
total_war <- group_by(war_final, Team) %>%
  summarize(total_war_sum = sum(WAR))

#Filter Pitchers
pitchers <- filter(war_final, Position %in% c("SP", "RP"))

#Sum by War
standings_PWar <- group_by(pitchers, Team) %>%
  summarize(pitcher_war_sum = sum(WAR)) 

#Sum by IP
standings_P_IP <- group_by(pitchers, Team) %>%
  summarize(IP_sum  = sum(PA_IP))

#Merge all Pitcher stats
pitch <- merge(standings_PWar, standings_P_IP, by = "Team") %>%
  mutate(WARPerIP = round(pitcher_war_sum/IP_sum,3),
         pitcher_war_sum = case_when(Team == "Team Martin" ~ pitcher_war_sum - 2.2 - 0.1, #for Kahnle's -0.1 WAR as of 6/23
                                     TRUE ~ pitcher_war_sum))

#Filter Batters
batters <- filter(war_final, !Position %in% c("SP", "RP"))

#Sum by War
standings_BatWar <- group_by(batters, Team) %>%
  summarize(batter_war_sum = sum(WAR)) 

#Sum by IP
standings_Bat_IP <- group_by(batters, Team) %>%
  summarize(PA_sum  = sum(PA_IP))

#Merge all Batter stats
bat <- merge(standings_BatWar, standings_Bat_IP, by = "Team") %>%
  mutate(WARPerPA = round(batter_war_sum/PA_sum,3),
         batter_war_sum = case_when(Team == "Crash Davis' Team" ~ batter_war_sum - 0.3 + 0.5, #for Seager's 0.5 WAR as of 5/1
                                    Team == "The Fireballing Five" ~ batter_war_sum - 2.1 - 0.5, #for Polanco's -0.5 WAR as of 6/13
                                    Team == "Team Brothers" ~ batter_war_sum - 1.2 + 0.2, #for Santana's 0.2 WAR as of 6/25
                                    Team == "Team Arfin" ~ batter_war_sum - 1.3 - 0.3, #for Bruce's -0.3 WAR as of 6/30
                                    TRUE ~ batter_war_sum)) 

#Merge total, pitchers, and batters
ordered_standings <- Reduce(left_join, list(total_war, pitch, bat)) %>% 
  rename("Total WAR" = total_war_sum) %>% 
  mutate(`Total WAR` = case_when(Team == "Crash Davis' Team" ~ `Total WAR` - 0.3 + 0.5, #for Seager's 0.5 WAR as of 5/1
                                 Team == "The Fireballing Five" ~ `Total WAR` - 2.1 - 0.5, #for Polanco's -0.5 WAR as of 6/13
                                 Team == "Team Martin" ~ `Total WAR` - 2.2 - 0.1, #for Kahnle's -0.1 WAR as of 6/23
                                 Team == "Team Brothers" ~ `Total WAR` - 1.2 + 0.2, #for Santana's 0.2 WAR as of 6/25
                                 Team == "Team Arfin" ~ `Total WAR` - 1.3 - 0.3, #for Bruce's -0.3 WAR as of 6/30
                                 TRUE ~ `Total WAR`)) %>% 
  arrange(desc(`Total WAR`)) 

write.csv(war_final, paste0("C://Users/johnp/Documents/GitHub/Baseball/WAR League/2018/WAR_Output_", today()-days(1), ".csv"))
write.csv(ordered_standings, paste0("C://Users/johnp/Documents/GitHub/Baseball/WAR League/2018/WAR_Standings_", today()-days(1), ".csv"))
