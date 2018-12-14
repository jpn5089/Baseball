library(dplyr)
library(ggplot2)
library(ggrepel)

# https://www.fangraphs.com/library/offense/offensive-statistics-list/

hitter_adv_2017 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Advanced_2017.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, Spd, UBR, wGDP, wSB, wRC, wRAA, playerid))

hitter_bb_2017 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Batted_Ball_2017.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, BABIP, `IFFB.`, BUH, `BUH.`, playerid))

hitter_pl_2017 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Plate_Discipline_2017.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-Team, -playerid)

hitter_standard_2017 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Standard_2017.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, R, HR, RBI, SB)

hitters_2017 <- Reduce(left_join, list(hitter_adv_2017, hitter_bb_2017, hitter_pl_2017, hitter_standard_2017)) %>%
  mutate(name_year = paste0(name, "_2017")) %>% 
  rename(`BB/K` = BB.K, `K%` = K., `BB%` = BB., `wRC+` = wRC., `GB/FB` = GB.FB, `LD%` = LD., `GB%` = GB.,
         `FlyB%` = FB., `HR/FB%` = HR.FB, `IFH%` = `IFH.`, `Pull%` = Pull., `Cent%` = Cent., `Oppo%` = Oppo., 
         `Soft%` = Soft., `Med%` = Med., `Hard%` = Hard., `O_Swing%` = O.Swing., `Z_Swing%` = Z.Swing., 
         `Swing%` = Swing., `O_Contact%` = O.Contact., `Z_Contact%` = Z.Contact., `Contact%` = Contact., 
         `Zone%` = Zone., `First_Pitch_Str%` = F.Strike., `SwStr%` = SwStr.) %>% 
  mutate(`K%` = gsub("%", '', `K%`),
         `BB%` = gsub("%", '', `BB%`),
         `LD%` = gsub("%", '', `LD%`),
         `GB%` = gsub("%", '', `GB%`),
         `FlyB%` = gsub("%", '', `FlyB%`),
         `HR/FB%` = gsub("%", '', `HR/FB%`),
         `IFH%` = gsub("%", '', `IFH%`),
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
         `SwStr%` = gsub("%", '', `SwStr%`)) %>% 
  mutate_at(vars(PA:`SwStr%`), funs(as.numeric))

hitter_adv_2016 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Advanced_2016.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, Spd, UBR, wGDP, wSB, wRC, wRAA, playerid))

hitter_bb_2016 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Batted_Ball_2016.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, BABIP, `IFFB.`, BUH, `BUH.`, playerid))

hitter_pl_2016 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Plate_Discipline_2016.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-Team, -playerid)

hitter_standard_2016 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Standard_2016.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, R, HR, RBI, SB)

hitters_2016 <- Reduce(left_join, list(hitter_adv_2016, hitter_bb_2016, hitter_pl_2016, hitter_standard_2016)) %>%
  mutate(name_year = paste0(name, "_2016")) %>% 
  rename(`BB/K` = BB.K, `K%` = K., `BB%` = BB., `wRC+` = wRC., `GB/FB` = GB.FB, `LD%` = LD., `GB%` = GB.,
         `FlyB%` = FB., `HR/FB%` = HR.FB, `IFH%` = `IFH.`, `Pull%` = Pull., `Cent%` = Cent., `Oppo%` = Oppo., 
         `Soft%` = Soft., `Med%` = Med., `Hard%` = Hard., `O_Swing%` = O.Swing., `Z_Swing%` = Z.Swing., 
         `Swing%` = Swing., `O_Contact%` = O.Contact., `Z_Contact%` = Z.Contact., `Contact%` = Contact., 
         `Zone%` = Zone., `First_Pitch_Str%` = F.Strike., `SwStr%` = SwStr.) %>% 
  mutate(`K%` = gsub("%", '', `K%`),
         `BB%` = gsub("%", '', `BB%`),
         `LD%` = gsub("%", '', `LD%`),
         `GB%` = gsub("%", '', `GB%`),
         `FlyB%` = gsub("%", '', `FlyB%`),
         `HR/FB%` = gsub("%", '', `HR/FB%`),
         `IFH%` = gsub("%", '', `IFH%`),
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
         `SwStr%` = gsub("%", '', `SwStr%`)) %>% 
  mutate_at(vars(PA:`SwStr%`), funs(as.numeric))

hitter_adv_2015 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Advanced_2015.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, Spd, UBR, wGDP, wSB, wRC, wRAA, playerid))

hitter_bb_2015 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Batted_Ball_2015.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-c(Team, BABIP, `IFFB.`, BUH, `BUH.`, playerid))

hitter_pl_2015 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Plate_Discipline_2015.csv") %>% 
  rename(name = ï..Name) %>% 
  select(-Team, -playerid)

hitter_standard_2015 <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Regression_Comeback_Candidates/Hitters/Standard_2015.csv") %>% 
  rename(name = ï..Name) %>% 
  select(name, R, HR, RBI, SB)

hitters_2015 <- Reduce(left_join, list(hitter_adv_2015, hitter_bb_2015, hitter_pl_2015, hitter_standard_2015)) %>%
  mutate(name_year = paste0(name, "_2015")) %>% 
  rename(`BB/K` = BB.K, `K%` = K., `BB%` = BB., `wRC+` = wRC., `GB/FB` = GB.FB, `LD%` = LD., `GB%` = GB.,
         `FlyB%` = FB., `HR/FB%` = HR.FB, `IFH%` = `IFH.`, `Pull%` = Pull., `Cent%` = Cent., `Oppo%` = Oppo., 
         `Soft%` = Soft., `Med%` = Med., `Hard%` = Hard., `O_Swing%` = O.Swing., `Z_Swing%` = Z.Swing., 
         `Swing%` = Swing., `O_Contact%` = O.Contact., `Z_Contact%` = Z.Contact., `Contact%` = Contact., 
         `Zone%` = Zone., `First_Pitch_Str%` = F.Strike., `SwStr%` = SwStr.) %>% 
  mutate(`K%` = gsub("%", '', `K%`),
         `BB%` = gsub("%", '', `BB%`),
         `LD%` = gsub("%", '', `LD%`),
         `GB%` = gsub("%", '', `GB%`),
         `FlyB%` = gsub("%", '', `FlyB%`),
         `HR/FB%` = gsub("%", '', `HR/FB%`),
         `IFH%` = gsub("%", '', `IFH%`),
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
         `SwStr%` = gsub("%", '', `SwStr%`)) %>% 
  mutate_at(vars(PA:`SwStr%`), funs(as.numeric))

hitter_data <- rbind(hitters_2017, hitters_2016) %>% 
  rbind(., hitters_2015) 

#cor_1 <- round(cor(hitter_data), 2)

averages <- hitter_data %>% 
  select(-name_year) %>% 
  group_by(name) %>% 
  summarise(total_PA = sum(PA)) %>% 
  ungroup() %>% 
  left_join(., hitter_data %>% select(-name_year)) %>% 
  mutate(weight = PA/total_PA) %>% 
  select(-PA, -total_PA) %>% 
  group_by(name) %>% 
  summarise_all(funs(sum(.*weight))) %>%  
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  setNames(c(names(.)[1], paste0(names(.)[-1],"_avg"))) %>% 
  select(-weight_avg)

### NEED IP TOTAL TO CALCULATE WEIGHTED AVERAGES 
full <- left_join(hitter_data, averages, by = "name") %>% 
  mutate(`K%_diff` = `K%` - `K%_avg`,
         BABIP_diff = BABIP - BABIP_avg,
         AVG_diff = AVG - AVG_avg,
         HR_diff = HR - HR_avg,
         R_diff = R - R_avg,
         RBI_diff = RBI - RBI_avg,
         OBP_diff = OBP - OBP_avg,
         `wRC+_diff` = `wRC+` - `wRC+_avg`,
         wOBA_diff = wOBA - wOBA_avg,
         `BB/K_diff` = `BB/K` - `BB/K_avg`,
         `First_Pitch_Str%_diff` = `First_Pitch_Str%` - `First_Pitch_Str%_avg`,
         `O_Swing%_diff` = `O_Swing%` - `O_Swing%_avg`,
         `BB%_diff` = `BB%` - `BB%_avg`,
         OPS_diff = OPS - OPS_avg,
         `Hard%_diff` = `Hard%` - `Hard%_avg`,
         `GB/FB_diff` = `GB/FB` - `GB/FB_avg`,
         ISO_diff = ISO - ISO_avg) %>% 
  select(name, name_year, contains("wRC+"))


ggplot(full, aes(x=`GB/FB_diff`, y = HR_diff)) +
  geom_point() +
  geom_label_repel(data = full %>% filter(`GB/FB_diff` < -0.4), aes(x = `GB/FB_diff`, y = HR_diff, label = name_year),
                   size = 2.5,
                   box.padding = unit(0.75, "lines"),
                   point.padding = unit(0.2, "lines"),
                   arrow = arrow(length = unit(0.01, 'npc')),
                   segment.color = 'black', segment.size = 1) +
  theme(axis.text.x = element_text(angle=0.9))
