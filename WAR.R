library(XML)
library(dplyr)

#File with Player Fangraphs URL and xpath
test <- read.csv("C:\\Users\\John\\Documents\\GitHub\\Baseball\\WAR_Input.csv")

#Use merge statement if you just grab csv's with 2017 stats straight from FanGraphs 
#Merge needed bc those datasets may not include all of our 360 players

#Injured, Suspended, Minors

#Big Sexy          -> DeSclafani, Rodon
#Doubles or Nothin -> Price, Giolito, Thornburg, Ramos
#Billy Heywood     -> Beltre, A. Reyes, Smyly, De Leon, Kang, Kazmir
#Bluff Bunters     -> No one
#Rockford Peaches  -> Matz
#Baumgardner       -> Capps, M. Cabrera, T. Murphy
#Boyles            -> No One
#Ellis             -> McHugh, Dahl
#Tierney           -> T. Ross
#Tebowie Baysox    -> No One
#Natural           -> No One
#War Tour          -> No one

#Create empty list  
war <- list()

for (i in 1:360){
  print(test[i,2])
  data_war <- htmlParse(as.character(test[i,7])) #URL for parsing WAR
  #Using that URL as well as xpath to grab 2017 WAR value
  war_path <- xpathSApply(data_war, as.character(test[i,8]), xmlValue)
  data_PA_IP <- htmlParse(as.character(test[i,7])) #URL for parsing PA or IP
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

void <- war_final$Player %in% c('Anthony DeSclafani', 'Carlos Rodon', 'David Price',
                                'Lucas Giolito', 'Tyler Thornburg', 'Wilson Ramos', 'Adrian Beltre',
                                'Alex Reyes', 'Drew Smyly', 'Jose De Leon', 'Jung Ho Kang', 'Scott Kazmir', 
                                'Steven Matz', 'Carter Capps',
                                'Mauricio Cabrera', 'Tom Murphy', 'Collin McHugh', 'David Dahl', 
                                'Tyson Ross')
#Set their WAR and PA_IP value to 0
war_final$WAR[void] <- 0
war_final$PA_IP[void] <- 0

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


write.csv(war_final, "C:\\Users\\John\\Documents\\GitHub\\Baseball\\WAR_Output.csv")
write.csv(ordered_stand, "C:\\Users\\John\\Documents\\GitHub\\Baseball\\WAR_Standings.csv")
