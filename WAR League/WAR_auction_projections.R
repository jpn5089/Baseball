library(dplyr)
library(tidyr)

# https://www.fangraphs.com/fantasy/value-above-replacement-part-one/
# https://www.fangraphs.com/fantasy/value-above-replacement-part-two/
# https://www.fangraphs.com/fantasy/value-above-replacement-part-three/

# data --------------------------------------------------------------------

no_teams <- 11
salary_cap <- 275

catchers <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/C.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(AB > 200)   
first_base <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/1B.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(AB > 200)  
second_base <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/2B.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(AB > 200) 
third_base <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/3B.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(AB > 200) 
shortstop <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/SS.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(AB > 200) 
outfield <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/OF.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(AB > 200) 
DH <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/DH.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(AB > 200) 
P <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/WAR League/Depth Projections/P.csv") %>% 
  rename(name = ï..Name) %>% 
  filter(IP > 30) 

# roto --------------------------------------------------------------------

roto_c <- catchers %>% 
  select(name, HR, R, RBI, SB, AVG, OBP, WAR) %>% 
  gather(stat, value, -name, -WAR) 

catcher_stats <- roto_c %>% 
  group_by(stat) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  ungroup()

catchers_final <- left_join(roto_c, catcher_stats, by = "stat") %>% 
  mutate(z_score = (value-mean)/sd)

roto_1B <- first_base %>% 
  select(name, AB, HR, R, RBI, SB, AVG, OBP, WAR) %>% 
  gather(stat, value, -name, -WAR, -AB) 

first_base_stats <- roto_1B %>% 
  group_by(stat) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  ungroup()

first_base_z <- left_join(roto_1B, first_base_stats, by = "stat") %>% 
  mutate(z_score = (value-mean)/sd) %>% 
  select(name, stat, z_score, AB) %>% 
  spread(stat, z_score) %>% 
  mutate(AVG_wt = AVG * AB,
         OBP_wt = OBP * AB) %>% 
  gather(stat, value, -name, -AB)

first_base_wt <- first_base_z %>% 
  filter(stat %in% c("AVG_wt", "OBP_wt")) %>% 
  select(-AB) %>% 
  group_by(stat) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  ungroup()

first_base_z_wt <- left_join(first_base_wt, first_base_z, by = "stat") %>% 
  mutate(z_score = (value-mean)/sd) %>% 
  select(name, stat, z_score) %>% 
  spread(stat, z_score)

first_base_final <- left_join(first_base_z %>% spread(stat, value) %>% 
                                select(-c(AB, AVG, AVG_wt, OBP, OBP_wt)), first_base_z_wt, by = "name")


roto_2B <- second_base %>%  
  select(name, HR, R, RBI, SB, AVG, OBP, WAR)

roto_SS <- shortstop %>% 
  select(name, HR, R, RBI, SB, AVG, OBP, WAR)

roto_3B <- third_base %>% 
  select(name, HR, R, RBI, SB, AVG, OBP, WAR)

roto_OF <- outfield %>% 
  select(name, HR, R, RBI, SB, AVG, OBP, WAR)

roto_DH <- DH %>% 
  select(name, HR, R, RBI, SB, AVG, OBP, WAR)
  
roto_P <- P %>%  
  filter(IP > 30) %>% 
  select(name, W, SV, HLD, ERA, SO, WHIP, WAR)



# By WAR ------------------------------------------------------------------

war_C <- catchers %>% 
  select(name, WAR) %>% 
  mutate(position = "C")

war_1B <- first_base %>% 
  select(name, WAR) %>% 
  mutate(position = "1B")

war_2B <- second_base %>% 
  select(name, WAR) %>% 
  mutate(position = "2B")

war_3B <- third_base %>% 
  select(name, WAR) %>% 
  mutate(position = "3B")

war_SS <- shortstop %>% 
  select(name, WAR) %>% 
  mutate(position = "SS")

war_OF <- outfield %>% 
  select(name, WAR) %>% 
  mutate(position = "OF")

war_DH <- DH %>% 
  select(name, WAR) %>% 
  mutate(position = "DH")

war_P <- P %>% 
  select(name, WAR) %>% 
  mutate(position = "P")

war_all <- Reduce(rbind, list(war_C, war_1B, war_2B, war_SS, war_3B, war_OF, war_DH, war_P))

war_stats <- war_all %>% 
  group_by(position) %>% 
  summarise(mean = mean(WAR),
            sd = sd(WAR)) %>% 
  ungroup()

war_z_score <- left_join(war_all, war_stats, by = "position") %>% 
  mutate(z_score = (WAR-mean)/sd) %>% 
  group_by(position) %>% 
  mutate(rank = rank(desc(z_score),ties.method = "first")) %>% 
  ungroup()

c_replacement <- filter(war_z_score, position == "C" & rank == no_teams*2) %>% select(z_score) %>% pull()
first_base_replacement <- filter(war_z_score, position == "1B" & rank == no_teams*2) %>% select(z_score) %>% pull()
second_base_replacement <- filter(war_z_score, position == "2B" & rank == no_teams*2) %>% select(z_score) %>% pull()
third_base_replacement <- filter(war_z_score, position == "3B" & rank == no_teams*2) %>% select(z_score) %>% pull()
ss_replacement <- filter(war_z_score, position == "SS" & rank == no_teams*2) %>% select(z_score) %>% pull()
of_replacement <- filter(war_z_score, position == "OF" & rank == no_teams*5) %>% select(z_score) %>% pull()
dh_replacement <- filter(war_z_score, position == "DH" & rank == no_teams*1) %>% select(z_score) %>% pull()
p_replacement <- filter(war_z_score, position == "P" & rank == no_teams*12) %>% select(z_score) %>% pull()

c_value <- war_z_score %>% 
  filter(position == "C") %>% 
  mutate(above_replacement = z_score - c_replacement)

first_base_value <- war_z_score %>% 
  filter(position == "1B") %>% 
  mutate(above_replacement = z_score - first_base_replacement)

second_base_value <- war_z_score %>% 
  filter(position == "2B") %>% 
  mutate(above_replacement = z_score - second_base_replacement)

third_base_value <- war_z_score %>% 
  filter(position == "3B") %>% 
  mutate(above_replacement = z_score - third_base_replacement)

ss_value <- war_z_score %>% 
  filter(position == "SS") %>% 
  mutate(above_replacement = z_score - ss_replacement)

of_value <- war_z_score %>% 
  filter(position == "OF") %>% 
  mutate(above_replacement = z_score - of_replacement)

dh_value <- war_z_score %>% 
  filter(position == "DH") %>% 
  mutate(above_replacement = z_score - dh_replacement)

p_value <- war_z_score %>% 
  filter(position == "P") %>% 
  mutate(above_replacement = z_score - p_replacement)

value_all <- Reduce(rbind, list(c_value, first_base_value, second_base_value, ss_value, third_base_value,
                                of_value, dh_value, p_value)) 

above_replacement_avg <- filter(value_all, above_replacement > 0) %>% mutate(avg = mean(above_replacement)) %>% 
  select(avg) %>% unique() %>% pull()

final_value <- value_all %>% 
  mutate(price = ((salary_cap - (1*30))/30)*(above_replacement/above_replacement_avg) + 1)

