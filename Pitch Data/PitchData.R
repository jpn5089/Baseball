library(dplyr)
library(ggplot2)

a <- read.csv("C:\\Users\\John\\Desktop\\Fangraphs.csv", check.names = F) %>%
  select(1,8)

colnames(a) <- c("Name", "IP")  
  
b <- read.csv("C:\\Users\\John\\Desktop\\FangraphsLeaderboard.csv", check.names = FALSE) %>%
  select(1, Pitches)

colnames(b) <- c("Name", "Pitches")  

c <- merge(a, b, by = "Name") %>%
  filter(IP >= 30) %>%
  mutate(pPerIn = Pitches/IP)

ggplot(c, aes(IP ,Pitches, label = Name)) +
  geom_point() +
  geom_point(data = c[c$pPerIn < 14,], color = "red", size = 2) +
  geom_text(data= c[c$pPerIn < 14,], hjust = 0.5, vjust = 1.6, size = 2.5) +
  labs(title = "Number of Pitches vs Innings Pitched in 2017", subtitle = "Pitchers with less than 14 pitches per inning are highlighted") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) 
ggsave(filename = "C:\\Users\\John\\Desktop\\Pitches.jpeg", width = 10, height = 7)
