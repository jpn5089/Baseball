library(Lahman)
library(rvest)
library(dplyr)

# team_1998_2004 <- c("BOS", "NYY", "TBD", "TOR", "BAL", "CLE", "MIN", "DET", "KCR", "CHW", "HOU",
#                     "OAK", "SEA", "ANA", "TEX", "ATL", "MON", "NYM", "PHI", "FLA", "STL", "CHC",
#                     "MIL", "PIT", "CIN", "SDP", "COL", "LAD", "SFG", "ARI")
# 
# team_2005_2007 <- c("BOS", "NYY", "TBD", "TOR", "BAL", "CLE", "MIN", "DET", "KCR", "CHW", "HOU",
#                     "OAK", "SEA", "LAA", "TEX", "ATL", "MON", "NYM", "PHI", "FLA", "STL", "CHC",
#                     "MIL", "PIT", "CIN", "SDP", "COL", "LAD", "SFG", "ARI")
# 
# team_2008_2011 <- c("BOS", "NYY", "TBR", "TOR", "BAL", "CLE", "MIN", "DET", "KCR", "CHW", "HOU",
#                     "OAK", "SEA", "LAA", "TEX", "ATL", "MON", "NYM", "PHI", "FLA", "STL", "CHC",
#                     "MIL", "PIT", "CIN", "SDP", "COL", "LAD", "SFG", "ARI")
# 
# team_2012_2018 <- c("BOS", "NYY", "TBR", "TOR", "BAL", "CLE", "MIN", "DET", "KCR", "CHW", "HOU",
#                     "OAK", "SEA", "LAA", "TEX", "ATL", "WSN", "NYM", "PHI", "MIA", "STL", "CHC",
#                     "MIL", "PIT", "CIN", "SDP", "COL", "LAD", "SFG", "ARI")
# 
# year <- c(1998:2018)
# 
# season <- list()
# yearly_results <- list()
# 
# for (i in 1:length(year)) {
# 
#   for (j in 1:length(team)) {
# 
#     x <- read_html(paste0("https://www.baseball-reference.com/teams/", team[j], "/",
#                           year[i], "-schedule-scores.shtml")) %>%
#       html_nodes("table") %>% `[[`(1) %>% html_table(fill = TRUE)
# 
#     colnames(x)[3] <- "boxscore"
#     colnames(x)[5] <- "at"
# 
#     y <- x %>%
#       filter(!grepl("Gm#", `Gm#`)) %>%
#       mutate(year = year[i])
# 
#     season[[j]] <- y
# 
#     print(team[j])
#   }
# 
#   all_teams_season <- do.call(rbind, season) %>%
#     mutate(`Gm#` = as.numeric(`Gm#`))
# 
#   yearly_results[[i]] <- all_teams_season
# 
#   print(paste0("Finished with year ", year[i]))
# 
# }
# 
# all_results <- do.call(rbind, yearly_results)
# 
# saveRDS(all_results, "C://Users/johnp/Documents/GitHub/Baseball/team_results.rds")

all_results <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/team_results.rds") %>% 
  select(game_number = `Gm#`, Team = Tm, win_loss = `W/L`, year) %>% 
  mutate(division = case_when(Team %in% c("BOS", "NYY", "TBR", "TOR", "BAL", "TBD") ~ "AL East",
                              Team %in% c("CLE", "MIN", "DET", "KCR", "CHW") ~ "AL Central",
                              Team %in% c("HOU", "OAK", "SEA", "ANA", "TEX", "LAA") ~ "AL West",
                              Team %in% c("ATL", "MON", "NYM", "PHI", "FLA", "WSN", "MIA") ~ "NL East",
                              Team %in% c("STL", "CHC", "MIL", "PIT", "CIN") ~ "NL Central",
                              Team %in% c("SDP", "COL", "LAD", "SFG", "ARI") ~ "NL West")) %>% 
  mutate(win_loss = gsub("-wo", "", win_loss),
         win_loss = gsub(" &H", "", win_loss),
         win_loss = gsub(" &V", "", win_loss),
         win_loss = gsub(" &X", "", win_loss)) %>% 
  mutate(win_count = ifelse(win_loss == "W", 1, 0)) %>% 
  group_by(year, division, game_number) %>%
  summarise(wins = sum(win_count)) %>% 
  ungroup() %>% 
  group_by(division, year) %>% 
  arrange(game_number) %>%
  mutate(cumulative_wins = cumsum(wins)) %>% 
  ungroup() %>% 
  mutate(label = paste0(year, " ", division)) %>% 
  filter(!(division == "AL West" & year < 2013))
# %>% 
  # filter(division %in% c("NL Central")) %>%
  # filter(game_number < 5)

library(ggplot2)    
library(gganimate)

p <- ggplot(all_results, aes(x = year, y = cumulative_wins)) +
  geom_bar(stat = "identity") +
  # geom_text(data=subset(all_results, cumulative_wins > 420),
            # aes(x= 164, label=label))
  # geom_text(aes(x = 164, label = label), hjust = 0) + 
  transition_states(game_number, transition_length = 3, state_length = 1) +
  # coord_cartesian(clip = 'off') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) + 
  facet_wrap(~division)

animate(p, height = 800, width = 1600)
anim_save("test.gif")
