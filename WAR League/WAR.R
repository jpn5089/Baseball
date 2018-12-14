library(XML)
library(dplyr)
library(lubridate)

#File with Player Fangraphs URL and xpath
test <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/WAR_Input.csv")

#Use merge statement if you just grab csv's with 2017 stats straight from FanGraphs 
#Merge needed bc those datasets may not include all of our 360 players

#Injured, Suspended, Minors

#Big Sexy          -> DeSclafani
#Doubles or Nothin -> Thornburg
#Billy Heywood     -> A. Reyes, Smyly, Kang, Kazmir
#Bluff Bunters     -> No one
#Rockford Peaches  -> No one
#Baumgardner       -> M. Cabrera
#Boyles            -> No One
#Ellis             -> Dahl
#Tierney           -> No One
#Tebowie Baysox    -> No One
#Natural           -> No One
#War Tour          -> No one

#Create empty list  
war <- list()

for (i in 1:360){
  print(i)
  print(test[i,2])
  data_war <- htmlParse(as.character(test[i,7]), isURL = T) #URL for parsing WAR
  #Using that URL as well as xpath to grab 2017 WAR value
  war_path <- xpathSApply(data_war, as.character(test[i,8]), xmlValue)
  data_PA_IP <- htmlParse(as.character(test[i,7]), isURL = T) #URL for parsing PA or IP
  #Using that URL as well as xpath to grab 2017 PA or IP value
  PA_IP_path <- xpathSApply(data_PA_IP, as.character(test[i,9]), xmlValue)
  #Choose values you want to keep from CSV 
  war[[i]] <- data.frame(test[i,2], test[i,4], test[i,5], war_path, PA_IP_path, test[i,10]) 
}

#Create your data table
war_final <- do.call(rbind,war) %>%
  mutate(war_path   = as.numeric(as.character(war_path))) %>%
  mutate(PA_IP_path   = as.numeric(as.character(PA_IP_path))) %>%
  mutate(test.i..2. = as.character(test.i..2.)) %>%
  mutate(test.i..4. = as.character(test.i..4.))

#Rename columns
colnames(war_final) <- c("Player", "Position", "Team", "WAR", "PA_IP", "Status")

#List players who haven't played a game in 2017

void <- war_final$Player %in% c('Anthony DeSclafani', 'Tyler Thornburg', 'Alex Reyes', 'Drew Smyly',
                                'Jung Ho Kang', 'Scott Kazmir', 'Mauricio Cabrera', 'David Dahl')
#Set their WAR and PA_IP value to 0
war_final$WAR[void] <- 0
war_final$PA_IP[void] <- 0
#war_final$WAR[294] <- -0.4
#war_final$PA_IP[294] <- 181
# war_final$WAR[68] <- 0.4
# war_final$PA_IP[68] <- 37.1
# war_final$WAR[16] <- 1.8
# war_final$PA_IP[16] <- 37.2
    
#Account for bench player's 50% weight
for (i in 1:360) {
  
  if (war_final$Status[i] == "Bench") {
    war_final$WAR[i] = war_final$WAR[i]*0.5
  } else {
    war_final$WAR[i] = war_final$WAR[i]
  }
}

for (i in 1:360) {
  
  if (war_final$Status[i] == "Bench") {
    war_final$PA_IP[i] = war_final$PA_IP[i]*0.5
  } else {
    war_final$PA_IP[i] = war_final$PA_IP[i]
  }
}

#Total War by Team
total_war <- group_by(war_final, Team) %>%
  summarize(total_war_sum = sum(WAR))

#Filter Pitchers
pitchers <- filter(war_final, Position == "P")

#Sum by War
standings_PWar <- group_by(pitchers, Team) %>%
  summarize(pitcher_war_sum = sum(WAR)) 

#Sum by IP
standings_P_IP <- group_by(pitchers, Team) %>%
  summarize(IP_sum  = sum(PA_IP))

#Merge all Pitcher stats
pitch <- merge(standings_PWar, standings_P_IP, by = "Team") %>%
  mutate(WARPerIP = pitcher_war_sum/IP_sum)

#Filter Batters
batters <- filter(war_final, Position != "P")

#Sum by War
standings_BatWar <- group_by(batters, Team) %>%
  summarize(batter_war_sum = sum(WAR)) 

#Sum by IP
standings_Bat_IP <- group_by(batters, Team) %>%
  summarize(PA_sum  = sum(PA_IP))

#Merge all Batter stats
bat <- merge(standings_BatWar, standings_Bat_IP, by = "Team") %>%
  mutate(WARPerPA = batter_war_sum/PA_sum)

#Merge total, pitchers, and batters
first <- merge(total_war, pitch, by = "Team")
standings <- merge(first, bat, by = "Team")
  
colnames(standings)[2] <- "Total WAR"
ordered_stand <- standings[order(-standings$`Total WAR`),]

write.csv(war_final, paste0("C://Users/johnp/Documents/GitHub/Baseball/WAR League/WAR_Output_", today()-days(1), ".csv"))
write.csv(ordered_stand, paste0("C://Users/johnp/Documents/GitHub/Baseball/WAR League/WAR_Standings_", today()-days(1), ".csv"))
