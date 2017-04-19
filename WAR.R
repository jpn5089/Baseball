library(XML)
library(dplyr)

#File with Player Fangraphs URL and xpath
test <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\WAR_Input.csv")

#Use merge statement if you just grab csv's with 2017 stats straight from FanGraphs 
#Merge needed bc those datasets may not include all of our 360 players

#Injured, Suspended, Minors

#Big Sexy          -> DeSclafani, Rodon, Familia
#War Tour          -> No one
#Doubles or Nothin -> Price, Giolito, Thornburg, Ramos
#Billy Heywood     -> Beltre, A. Reyes, Smyly, De Leon, Kang, Prado, Kazmir, S. Gray
#Bluff Bunters     -> No one
#Rockford Peaches  -> Matz
#Baumgardner       -> Capps, Tillman, Gregorius, M. Cabrera, T. Murphy
#Boyles            -> No One
#Ellis             -> McHugh, Dahl
#Tierney           -> Mesoraco, J.D. Martinez, T. Ross
#Tebowie Baysox    -> Desmond, Kipnis, J. Ross, Urias 
#Natural           -> Berrios

#Create empty list  
war <- list()

for (i in 1:360){
  print(test[i,2])
  data <- htmlParse(as.character(test[i,7])) #URL for parsing
  #Using that URL as well as xpath to grab 2017 WAR value
  war_path <- xpathSApply(data, as.character(test[i,8]), xmlValue)
  #Choose values you want to keep from CSV 
  war[[i]] <- data.frame(test[i,2], test[i,5], war_path, test[i,9]) 
}

#Create your data table
war_final <- do.call(rbind,war) %>%
  mutate(war_path   = as.numeric(as.character(war_path))) %>%
  mutate(test.i..2. = as.character(test.i..2.))

#Rename columns
colnames(war_final) <- c("Player", "Team", "WAR", "Status")

#List players who haven't played a game in 2017

void <- war_final$Player %in% c('Anthony DeSclafani', 'Carlos Rodon', 'Jeurys Familia', 'David Price',
                                'Lucas Giolito', 'Tyler Thornburg', 'Wilson Ramos', 'Adrian Beltre',
                                'Alex Reyes', 'Drew Smyly', 'Jose De Leon', 'Jung Ho Kang', 'Martin Prado',
                                'Scott Kazmir', 'Sonny Gray', 'Steven Matz', 'Carter Capps', 'Chris Tillman',
                                'Didi Gregorius', 'Mauricio Cabrera', 'Tom Murphy', 'Collin McHugh',
                                'David Dahl', 'Devin Mesoraco', 'J.D. Martinez', 'Tyson Ross',
                                'Ian Desmond', 'Jason Kipnis', 'Joe Ross', 'Julio Urias', 'Jose Berrios')
#Set their WAR value to 0
war_final$WAR[void] <- 0

#Account for bench player's 50% weight
for (i in 1:360) {
  
  if (war_final$Status[i] == "Bench") {
    war_final$WAR[i] = war_final$WAR[i]*0.5
  } else {
    war_final$WAR[i] = war_final$WAR[i]
  }
}

#Sum by Team
standings <- group_by(war_final, Team) %>%
  summarize(war_sum = sum(WAR))
  
colnames(standings)[2] <- "Total WAR"


write.csv(war_final, "C:\\Users\\John\\Desktop\\R\\Baseball\\WAR_Output.csv")
write.csv(standings, "C:\\Users\\John\\Desktop\\R\\Baseball\\WAR_Standings.csv")
