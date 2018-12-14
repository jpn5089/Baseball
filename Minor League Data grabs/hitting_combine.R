library(dplyr)

AA_hitting <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/AA_hitting.rds") %>% 
  mutate(Age = as.numeric(Age))

high_A_hitting <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/highA_hitting.rds") %>% 
  mutate(Age = as.numeric(Age))

mid_A_hitting <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/midA_hitting.rds") %>% 
  mutate(Age = as.numeric(Age))

BP_AA_hitting <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/AA_hitting_BP.rds") 

BP_high_A_hitting <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/highA_hitting_BP.rds")

BP_mid_A_hitting <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/midA_hitting_BP.rds")

AA_all <- left_join(AA_hitting, BP_AA_hitting)
saveRDS(AA_all, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/AA_hitting_all.rds")


high_A_all <- left_join(high_A_hitting, BP_high_A_hitting)
saveRDS(high_A_all, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/highA_hitting_all.rds")

low_A_all <- left_join(mid_A_hitting, BP_mid_A_hitting)
saveRDS(low_A_all, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/midA_hitting_all.rds")
