library(dplyr)
library(ggplot2)

a <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/Pitchers2018.csv", check.names = F) %>%
  select(1,8, 16, 17) %>%
  mutate(ERA_minus_FIP = ERA - FIP)

colnames(a) <- c("Name", "IP", "ERA", "FIP", "ERA_minus_FIP")  
  
b <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/Pitches2018.csv", check.names = FALSE) %>%
  select(1, Pitches)

colnames(b) <- c("Name", "Pitches")  

whip <- read.csv("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/whip2018.csv", check.names = F) %>%
  select(1,11)

colnames(whip) <- c("Name", "WHIP") 

c <- merge(a, b, by = "Name") %>%
  mutate(pPerIn = round(Pitches/IP, digits = 1))

final <- merge(c, whip, by = "Name") %>%
  # filter(IP >= 20) %>%
  mutate(SHORTNAME = paste0(Name, " - ", IP, " IP")) %>%
  mutate(name_ip = paste0(Name, " - ", pPerIn, " Pitches/IP"))

ggplot(final, aes(IP , Pitches, label = name_ip)) +
  geom_point() +
  geom_point(data = final[final$pPerIn < 14,], color = "red", size = 2) +
  geom_text(data= final[final$pPerIn < 14,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  labs(title = "Number of Pitches vs Innings Pitched in 2017", 
       subtitle = "Pitchers with less than 14 pitches per inning are highlighted",
       caption = "Source: Fangraphs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) 
ggsave(filename = "C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/Pitches.jpeg", width = 10, height = 7)


ggplot(final, aes(pPerIn , WHIP, label = SHORTNAME)) +
  geom_point() +
  #geom_smooth(method=lm, se = FALSE) +
  geom_point(data = final[final$WHIP < 1,], color = "red", size = 2) +
  geom_text(data= final[final$WHIP < 1,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  geom_point(data = final[final$ERA < 2,], color = "green", size = 2) +
  geom_text(data= final[final$ERA < 2,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  geom_point(data = final[final$ERA < 2 & final$WHIP < 1,], color = "purple", size = 2) +
  geom_text(data= final[final$ERA < 2 & final$WHIP < 1,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  geom_point(data = final[final$pPerIn < 14,], color = "blue", size = 2) +
  geom_text(data= final[final$pPerIn < 14,], hjust = 0.3, vjust = 1.6, size = 2.5) +
  labs(x = "Pitches per Inning", title = "Pitches per Inning vs WHIP in 2017", 
       subtitle = "Blue: less than 14 pitchers per inning ~ Red: less than 1.00 WHIP 
       Green: Less than 2.00 ERA ~ Purple: Less than 1.00 WHIP AND 2.00 ERA 
       (**Minimum of 50 IP**)", 
       caption = "Source: Fangraphs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) 
ggsave(filename = "C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/Pitches_WHIP.jpeg", width = 12, height = 8)


ggplot(final, aes(pPerIn , ERA, label = SHORTNAME)) +
  geom_point() +
  #geom_smooth(method=lm, se = FALSE) +
  geom_point(data = final[final$WHIP < 1,], color = "red", size = 2) +
  geom_text(data= final[final$WHIP < 1,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  geom_point(data = final[final$ERA < 2,], color = "green", size = 2) +
  geom_text(data= final[final$ERA < 2,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  geom_point(data = final[final$ERA < 2 & final$WHIP < 1,], color = "purple", size = 2) +
  geom_text(data= final[final$ERA < 2 & final$WHIP < 1,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  geom_point(data = final[final$pPerIn < 14,], color = "blue", size = 2) +
  geom_text(data= final[final$pPerIn < 14,], hjust = 0.3, vjust = 1.6, size = 2.5) +
  labs(x = "Pitches per Inning", title = "Pitches per Inning vs ERA in 2017", 
       subtitle = "Blue: less than 14 pitchers per inning ~ Red: less than 1.00 WHIP 
       Green: Less than 2.00 ERA ~ Purple: Less than 1.00 WHIP AND 2.00 ERA 
       (**Minimum of 50 IP**)", 
       caption = "Source: Fangraphs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) 
ggsave(filename = "C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/Pitches_ERA.jpeg", width = 12, height = 8)
