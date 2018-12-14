library(dplyr)
library(ggplot2)
library(ggrepel)

adv_2017 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Advanced_2017.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, K.BB., ERA., FIP., xFIP., FIP, E.F, SIERA)) 

bb_2017 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Batted_Ball_2017.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, BABIP, RS, Balls, Strikes, Pitches))

pl_2017 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Plate_Discipline_2017.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-Team)

pitch_2017 <- Reduce(left_join, list(adv_2017, bb_2017, pl_2017)) %>%
  mutate(name_year = paste0(name, "_2017")) %>% 
  rename(`K/9` = K.9, `BB/9` = BB.9, `K/BB` = K.BB, `HR/9` = HR.9, `K%` = K., `BB%` = BB., "OPP_AVG" = AVG,
         `LOB%` = LOB., `GB/FB` = GB.FB, `LD%` = LD., `GB%` = GB.,
         `FlyB%` = FB., `InfFlyB%` = IFFB., `HR/FB%` = HR.FB, `RunSupport/9` = RS.9,
         `Pull%` = Pull., `Cent%` = Cent., `Oppo%` = Oppo., `Soft%` = Soft., `Med%` = Med., `Hard%` = Hard.,
         `O_Swing%` = O.Swing., `Z_Swing%` = Z.Swing., `Swing%` = Swing., `O_Contact%` = O.Contact.,
         `Z_Contact%` = Z.Contact., `Contact%` = Contact., `Zone%` = Zone., `First_Pitch_Str%` = F.Strike., 
         `SwStr%` = SwStr.) %>% 
  mutate(`K%` = gsub("%", '', `K%`),
         `BB%` = gsub("%", '', `BB%`),
         `LOB%` = gsub("%", '', `LOB%`),
         `LD%` = gsub("%", '', `LD%`),
         `GB%` = gsub("%", '', `GB%`),
         `FlyB%` = gsub("%", '', `FlyB%`),
         `InfFlyB%` = gsub("%", '', `InfFlyB%`),
         `Pull%` = gsub("%", '', `Pull%`),
         `Cent%` = gsub("%", '', `Cent%`),
         `Oppo%` = gsub("%", '', `Oppo%`),
         `Soft%` = gsub("%", '', `Soft%`),
         `Med%` = gsub("%", '', `Med%`),
         `Hard%` = gsub("%", '', `Hard%`),
         `O_Swing%` = gsub("%", '', `O_Swing%`),
         `Z_Swing%` = gsub("%", '', `Z_Swing%`),
         `Swing%` = gsub("%", '', `Swing%`),
         `O_Contact%` = gsub("%", '', `O_Contact%`),
         `Z_Contact%` = gsub("%", '', `Z_Contact%`),
         `Contact%` = gsub("%", '', `Contact%`),
         `Zone%` = gsub("%", '', `Zone%`),
         `First_Pitch_Str%` = gsub("%", '', `First_Pitch_Str%`),
         `SwStr%` = gsub("%", '', `SwStr%`),
         `HR/FB%` = gsub("%", '', `HR/FB%`)) %>% 
  mutate_at(vars(`K/9`:`SwStr%`), funs(as.numeric))

adv_2016 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Advanced_2016.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, K.BB., ERA., FIP., xFIP., FIP, E.F, SIERA)) 

bb_2016 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Batted_Ball_2016.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, BABIP, RS, Balls, Strikes, Pitches))

pl_2016 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Plate_Discipline_2016.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-Team)

pitch_2016 <- Reduce(left_join, list(adv_2016, bb_2016, pl_2016)) %>% 
  mutate(name_year = paste0(name, "_2016")) %>% 
  rename(`K/9` = K.9, `BB/9` = BB.9, `K/BB` = K.BB, `HR/9` = HR.9, `K%` = K., `BB%` = BB., "OPP_AVG" = AVG,
         `LOB%` = LOB., `GB/FB` = GB.FB, `LD%` = LD., `GB%` = GB.,
         `FlyB%` = FB., `InfFlyB%` = IFFB., `HR/FB%` = HR.FB, `RunSupport/9` = RS.9,
         `Pull%` = Pull., `Cent%` = Cent., `Oppo%` = Oppo., `Soft%` = Soft., `Med%` = Med., `Hard%` = Hard.,
         `O_Swing%` = O.Swing., `Z_Swing%` = Z.Swing., `Swing%` = Swing., `O_Contact%` = O.Contact.,
         `Z_Contact%` = Z.Contact., `Contact%` = Contact., `Zone%` = Zone., `First_Pitch_Str%` = F.Strike., 
         `SwStr%` = SwStr.) %>% 
  mutate(`K%` = gsub("%", '', `K%`),
         `BB%` = gsub("%", '', `BB%`),
         `LOB%` = gsub("%", '', `LOB%`),
         `LD%` = gsub("%", '', `LD%`),
         `GB%` = gsub("%", '', `GB%`),
         `FlyB%` = gsub("%", '', `FlyB%`),
         `InfFlyB%` = gsub("%", '', `InfFlyB%`),
         `Pull%` = gsub("%", '', `Pull%`),
         `Cent%` = gsub("%", '', `Cent%`),
         `Oppo%` = gsub("%", '', `Oppo%`),
         `Soft%` = gsub("%", '', `Soft%`),
         `Med%` = gsub("%", '', `Med%`),
         `Hard%` = gsub("%", '', `Hard%`),
         `O_Swing%` = gsub("%", '', `O_Swing%`),
         `Z_Swing%` = gsub("%", '', `Z_Swing%`),
         `Swing%` = gsub("%", '', `Swing%`),
         `O_Contact%` = gsub("%", '', `O_Contact%`),
         `Z_Contact%` = gsub("%", '', `Z_Contact%`),
         `Contact%` = gsub("%", '', `Contact%`),
         `Zone%` = gsub("%", '', `Zone%`),
         `First_Pitch_Str%` = gsub("%", '', `First_Pitch_Str%`),
         `SwStr%` = gsub("%", '', `SwStr%`),
         `HR/FB%` = gsub("%", '', `HR/FB%`)) %>% 
  mutate_at(vars(`K/9`:`SwStr%`), funs(as.numeric))

adv_2015 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Advanced_2015.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, K.BB., ERA., FIP., xFIP., FIP, E.F, SIERA)) 

bb_2015 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Batted_Ball_2015.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, BABIP, RS, Balls, Strikes, Pitches)) 

pl_2015 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Plate_Discipline_2015.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-Team)

pitch_2015 <- Reduce(left_join, list(adv_2015, bb_2015, pl_2015)) %>% 
  mutate(name_year = paste0(name, "_2015")) %>% 
  rename(`K/9` = K.9, `BB/9` = BB.9, `K/BB` = K.BB, `HR/9` = HR.9, `K%` = K., `BB%` = BB., "OPP_AVG" = AVG,
         `LOB%` = LOB., `GB/FB` = GB.FB, `LD%` = LD., `GB%` = GB.,
         `FlyB%` = FB., `InfFlyB%` = IFFB., `HR/FB%` = HR.FB, `RunSupport/9` = RS.9,
         `Pull%` = Pull., `Cent%` = Cent., `Oppo%` = Oppo., `Soft%` = Soft., `Med%` = Med., `Hard%` = Hard.,
         `O_Swing%` = O.Swing., `Z_Swing%` = Z.Swing., `Swing%` = Swing., `O_Contact%` = O.Contact.,
         `Z_Contact%` = Z.Contact., `Contact%` = Contact., `Zone%` = Zone., `First_Pitch_Str%` = F.Strike., 
         `SwStr%` = SwStr.) %>% 
  mutate(`K%` = gsub("%", '', `K%`),
         `BB%` = gsub("%", '', `BB%`),
         `LOB%` = gsub("%", '', `LOB%`),
         `LD%` = gsub("%", '', `LD%`),
         `GB%` = gsub("%", '', `GB%`),
         `FlyB%` = gsub("%", '', `FlyB%`),
         `InfFlyB%` = gsub("%", '', `InfFlyB%`),
         `Pull%` = gsub("%", '', `Pull%`),
         `Cent%` = gsub("%", '', `Cent%`),
         `Oppo%` = gsub("%", '', `Oppo%`),
         `Soft%` = gsub("%", '', `Soft%`),
         `Med%` = gsub("%", '', `Med%`),
         `Hard%` = gsub("%", '', `Hard%`),
         `O_Swing%` = gsub("%", '', `O_Swing%`),
         `Z_Swing%` = gsub("%", '', `Z_Swing%`),
         `Swing%` = gsub("%", '', `Swing%`),
         `O_Contact%` = gsub("%", '', `O_Contact%`),
         `Z_Contact%` = gsub("%", '', `Z_Contact%`),
         `Contact%` = gsub("%", '', `Contact%`),
         `Zone%` = gsub("%", '', `Zone%`),
         `First_Pitch_Str%` = gsub("%", '', `First_Pitch_Str%`),
         `SwStr%` = gsub("%", '', `SwStr%`),
         `HR/FB%` = gsub("%", '', `HR/FB%`)) %>% 
  mutate_at(vars(`K/9`:`SwStr%`), funs(as.numeric))

pitch_data <- rbind(pitch_2017, pitch_2016) %>% 
  rbind(., pitch_2015) 

cor_pitch <- round(cor(pitch_data %>% select(-name, -name_year)), 2)

ggplot(pitch_data, aes(x= `HR/9`, y = ERA)) +
  geom_point() +
  geom_label_repel(data = pitch_data %>% filter(ERA < 3.2), aes(x = `HR/9`, y = ERA, label = name_year),
                   size = 2.5,
                   box.padding = unit(0.75, "lines"),
                   point.padding = unit(0.2, "lines"),
                   arrow = arrow(length = unit(0.01, 'npc')),
                   segment.color = 'black', segment.size = 1) +
  theme(axis.text.x = element_text(angle=0.9))


averages <- pitch_data %>% 
  select(-name_year) %>% 
  group_by(name) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  setNames(c(names(.)[1], paste0(names(.)[-1],"_avg")))

### NEED IP TOTAL TO CALCULATE WEIGHTED AVERAGES 
full <- left_join(pitch_data, averages, by = "name") %>% 
  select(-playerid) %>% 
  mutate(`K/9_diff` = `K/9` - `K/9_avg`,
         BABIP_diff = BABIP - BABIP_avg,
         `BB/9_diff` = `BB/9` - `BB/9_avg`,
         `LOB%_diff` = `LOB%` - `LOB%_avg`,
         ERA_diff = ERA - ERA_avg,
         WHIP_diff = WHIP - WHIP_avg,
         `K/BB_diff` = `K/BB` - `K/BB_avg`,
         `GB%_diff` = `GB%` - `GB%_avg`) %>% 
  select(name, name_year, contains("K/9"), contains("K/BB"), contains("GB%"), contains("WHIP"))
           
ggplot(full, aes(x= `LOB%_diff`, y = ERA_diff)) +
  geom_point() +
  geom_label_repel(data = full %>% filter(`LOB%_diff` < -5), aes(x= `LOB%_diff`, y = ERA_diff, label = name_year),
                   size = 2.5,
                   box.padding = unit(0.75, "lines"),
                   point.padding = unit(0.2, "lines"),
                   arrow = arrow(length = unit(0.01, 'npc')),
                   segment.color = 'black', segment.size = 1) +
  theme(axis.text.x = element_text(angle=0.9))
