library(dplyr)
library(lubridate)
library(data.table)

fg_batters <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\FanGraphs_Batters.csv") %>%
  select(-playerid, -ADP) %>%
  mutate(source = "FanGraphs")

fg_pitchers <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\FanGraphs_Pitchers.csv") %>%
  select(-playerid, -SV) %>%
  mutate(source = "FanGraphs")

steamer_batters <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Steamer_Batters.csv") %>%
  select(-X.1, -X.1.1, -X.1.2, -X.1.3, -playerid, -ADP, -Off, -Def, -wRC.) %>%
  mutate(source = "Steamer")

steamer_pitchers <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Steamer_Pitchers.csv") %>%
  select(-playerid, -SV, -RA9.WAR) %>%
  mutate(source = "Steamer")

#Batters (180 batters drafted; baseline is WAR from 181st (1.3 WAR) best projected player)

zips_batters <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_Batters.csv") %>%
  select(-playerid, -ADP) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 1.3) 

colnames(zips_batters)[1] <- "Name"

zips_batters <- subset(zips_batters, WAR >= 1.3) %>%
  select(Name, surplus_WAR, WAR)

B_WAR <- 211.8

pitchers <- rbind(steamer_pitchers, fg_pitchers, zips_pitchers)

#write.csv(pitchers, "C:\\Users\\John\\Desktop\\R\\Baseball\\Pitchers2017.csv")

pitchers <- subset(pitchers, IP > 5) %>%
  mutate(warPerIP = round(WAR/IP, 6))

batters <- rbind(steamer_batters, fg_batters, zips_batters)

#write.csv(batters, "C:\\Users\\John\\Desktop\\R\\Baseball\\Batters2017.csv")

batters <- subset(batters, PA > 200) %>%
  mutate(warPerAB = round(WAR/PA, 6))

############################################################################################
#Zips by position
############################################################################################

#Total Money = (260 - 30) x 12 = $2760
#Subtract 30 bc thats the minimum you'd spend on 30 players
TotalMoney <- 2760

#Pitchers (144 pitchers drafted; baseline is WAR from 145th (1.4 WAR) best projected player)
#Pitchers (157 is 1.4 too) 

zips_pitchers <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_Pitchers.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 1.4)

colnames(zips_pitchers)[1] <- "Name"

P_WAR = 162.7

dol_war <- TotalMoney/(P_WAR+B_WAR)

zips_pitchers <- subset(zips_pitchers, WAR >1.3) %>%
  mutate(value = (Pdol_war*surplus_WAR)+1) %>%
  select(Name, WAR, surplus_WAR)

crude_proj <- rbind(zips_batters, zips_pitchers) %>%
  mutate(value = (surplus_WAR * dol_war)+1)

#Pitchers (24 catchers drafted; baseline is WAR from 25th (1.1 WAR) best projected player)

zips_C <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_C.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 1.1)

C_WAR <- 13.9
Cdol_war <- TotalMoney/C_WAR

zips_C <- subset(zips_C, WAR >= 1.1) %>%
  mutate(value = Cdol_war*surplus_WAR)



#1B (12 1B drafted; baseline is WAR from 13th (1.7 WAR) best projected player)

zips_1B <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_1B.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 1.7)

#2B (12 2B drafted; baseline is WAR from 13th (2.3 WAR) best projected player)

zips_2B <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_2B.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 2.3)

#SS (12 SS drafted; baseline is WAR from 13th (2.2 WAR) best projected player)

zips_SS <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_SS.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 2.2)

#3B (12 3B drafted; baseline is WAR from 13th (2.6 WAR) best projected player)

zips_3B <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_3B.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 2.6)

#MI (12 MI drafted; baseline is WAR from 13th (2.9 WAR) best projected player)

zips_MI <- rbind(zips_2B, zips_SS) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 2.9)

#CI (12 CI drafted; baseline is WAR from 13th (3.3 WAR) best projected player)

zips_CI <- rbind(zips_3B, zips_1B) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 3.3)

#RF (12 RF drafted; baseline is WAR from 13th (2.3 WAR) best projected player)

zips_RF <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_RF.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 2.3)

#CF (12 CF drafted; baseline is WAR from 13th (2.8 WAR) best projected player)

zips_CF <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_CF.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 2.8)

#LF (12 LF drafted; baseline is WAR from 13th (1.6 WAR) best projected player)

zips_LF <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_LF.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 1.6)

#OF (12 OF drafted; baseline is WAR from 13th (1.7 WAR) best projected player)

zips_OF <- read.csv("C:\\Users\\John\\Desktop\\R\\Baseball\\Zips_OF.csv") %>%
  select(-playerid) %>%
  mutate(source = "Zips") %>%
  mutate(surplus_WAR = WAR - 1.4)






