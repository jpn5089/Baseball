library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

# devtools::install_github('hadley/ggplot2')

files_draught <- list.files("C://Users/johnp/Documents/GitHub/Baseball/Draught/2019/") %>% 
  data.frame(files = .) %>%
  filter(grepl(".csv", files)) %>%
  pull(.)

dates <- data.frame(dates = seq(ymd("2019-03-27"), ymd("2019-09-08"), by = 1)) %>% 
  filter(!row_number() %in% c(22,28,30,37,39,44,46,47,51,54,55,57,63,65,66,68:72,74:84,86,88:94,96,98,
                              103:113,115:118,121:127,129:140,142,144:151,153,155:156,160:161,163:164)) %>%
  pull()


overall <- list()
BA <- list()
HR <- list()
OBP <- list()
R <- list()
RBI <- list()
SB <- list()
ERA <- list()
WHIP <- list()
S <- list()
K <- list()
QS <- list()
HD <- list()


for (i in 1:length(files_draught)) {
  
  raw_data <- read.csv(paste0("C://Users/johnp/Documents/GitHub/Baseball/Draught/2019/", files_draught[i]))
  
  overall[[i]] <- raw_data[1:14,1:5] %>% 
    mutate(date = dates[i])
  
  BA[[i]] <- raw_data[17:30,1:3] %>% 
    mutate(date = dates[i],
           stat = "BA") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  HR[[i]] <- raw_data[32:45,1:3] %>% 
    mutate(date = dates[i],
           stat = "HR") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  OBP[[i]] <- raw_data[47:60,1:3] %>% 
    mutate(date = dates[i],
           stat = "OBP") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  R[[i]] <- raw_data[62:75,1:3] %>% 
    mutate(date = dates[i],
           stat = "R") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  RBI[[i]] <- raw_data[77:90,1:3] %>% 
    mutate(date = dates[i],
           stat = "RBI") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  SB[[i]] <- raw_data[92:105,1:3] %>% 
    mutate(date = dates[i],
           stat = "SB") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  ERA[[i]] <- raw_data[107:120,1:3] %>% 
    mutate(date = dates[i],
           stat = "ERA") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  HD[[i]] <- raw_data[122:135,1:3] %>% 
    mutate(date = dates[i],
           stat = "HD") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  K[[i]] <- raw_data[137:150,1:3] %>% 
    mutate(date = dates[i],
           stat = "K") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  QS[[i]] <- raw_data[152:165,1:3] %>% 
    mutate(date = dates[i],
           stat = "QS") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  S[[i]] <- raw_data[167:180,1:3] %>% 
    mutate(date = dates[i],
           stat = "S") %>% 
    rename(Team = Rank, value = Team, Pts = Batting)
  
  WHIP[[i]] <- raw_data[182:195,1:3] %>% 
    mutate(date = dates[i],
           stat = "WHIP") %>% 
    rename(Team = Rank, value = Team, Pts = Batting) 
}

overall_standings <- do.call(rbind, overall)

saveRDS(overall_standings, paste0("C://Users/johnp/Documents/GitHub/Baseball/Draught_Stats/standings_", last(dates), ".rds"))

weighted_pts <- overall_standings %>%
  group_by(Team) %>%
  summarise(avg_pts = round(mean(Total), 1)) %>% 
  ungroup() %>% 
  arrange(desc(avg_pts))

# p_overall <- ggplot(overall_standings, aes(date, Total, color = Team)) +
#   geom_line() +
#   labs(title = "Total Pts") +
#   theme(axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# 
# ggplotly(p_overall)

BA_all <- do.call(rbind, BA) %>% 
  mutate(value = round(as.numeric(as.character(value)),3))

# BA_overall <- ggplot(BA_all, aes(date, BA, color = Team)) +
#   geom_line() +
#   labs(title = "Batting Average") +
#   theme(axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# 
# ggplotly(BA_overall)

HR_all <- do.call(rbind, HR) %>% 
  mutate(value = as.numeric(as.character(value)))
OBP_all <- do.call(rbind, OBP) %>% 
  mutate(value = round(as.numeric(as.character(value)),3))
R_all <- do.call(rbind, R) %>% 
  mutate(value = as.numeric(as.character(value)))
RBI_all <- do.call(rbind, RBI) %>% 
  mutate(value = as.numeric(as.character(value)))
SB_all <- do.call(rbind, SB) %>% 
  mutate(value = as.numeric(as.character(value)))
ERA_all <- do.call(rbind, ERA) %>% 
  mutate(value = round(as.numeric(as.character(value)),2))
WHIP_all <- do.call(rbind, WHIP) %>% 
  mutate(value = round(as.numeric(as.character(value)),2))
S_all <- do.call(rbind, S) %>% 
  mutate(value = as.numeric(as.character(value)))
K_all <- do.call(rbind, K) %>% 
  mutate(value = as.numeric(as.character(value)))
QS_all <- do.call(rbind, QS) %>% 
  mutate(value = as.numeric(as.character(value)))
HD_all <- do.call(rbind, HD) %>%  
  mutate(value = as.numeric(as.character(value)))

stats <- Reduce(rbind, list(BA_all, HR_all, OBP_all, R_all, RBI_all, SB_all, ERA_all, WHIP_all, S_all,
                            K_all, QS_all, HD_all))

saveRDS(stats, paste0("C://Users/johnp/Documents/GitHub/Baseball/Draught_Stats/stats_", last(dates), ".rds"))
# HD_overall <- ggplot(HD_all, aes(date, HD, color = Team)) +
#   geom_line() +
#   labs(title = "Holds") +
#   theme(axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# 
# ggplotly(HD_overall)