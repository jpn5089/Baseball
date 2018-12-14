library(dplyr)
library(ggplot2)
library(ggrepel)

pitch <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/pitchersdata.csv") %>%
  rename(name = ï..Name) %>%
  select(name, Team, IP, WAR) 

pitch$Team[34] = "Rangers"
pitch$Team[28] = "Athletics"
pitch$Team[24] = "White Sox"
pitch$Team[122] = "White Sox"
pitch$Team[40] = "Cardinals"
pitch$Team[89] = "Braves"
pitch$Team[221] = "Padres"
pitch$Team[180] = "Blue Jays"
pitch$Team[271] = "Phillies"
pitch$Team[200] = "Braves"
pitch$Team[15] = "Tigers"
pitch$Team[182] = "Rays"

rank <- pitch %>%
  arrange(desc(IP)) %>%
  group_by(Team) %>%
  mutate(rank = order(IP, decreasing = T)) %>%
  filter(rank <= 5) %>%
  filter(Team != "- - -")


ggplot(rank, aes(x = IP, y = WAR, label = name)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_point(data = rank[rank$WAR < 0,], color = "red", size = 2) +
  geom_text_repel(data = rank[rank$WAR < 100,], size = 2.5) +
  facet_wrap(~ Team) 

ggsave(filename = "C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/SP_WAR.jpeg", width = 10, height = 7)
