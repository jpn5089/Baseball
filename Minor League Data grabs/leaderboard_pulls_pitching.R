library(rvest)
library(dplyr)
years <- seq(2009,2018,1)

# high_A ------------------------------------------------------------------

output_high_A <- list()

for (i in 1:length(years)) {
  
  california_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=8&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  california_A <- california_A[-c(1,3),]
  names(california_A) <- california_A[1,]
  california_A <- california_A[-1,]
  
  carolina_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=9&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  carolina_A <- carolina_A[-c(1,3),]
  names(carolina_A) <- carolina_A[1,]
  carolina_A <- carolina_A[-1,]
  
  florida_state_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=10&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  florida_state_A <- florida_state_A[-c(1,3),]
  names(florida_state_A) <- florida_state_A[1,]
  florida_state_A <- florida_state_A[-1,]
  
  high_A <- Reduce(rbind, list(california_A, carolina_A, florida_state_A))
  
  print(years[i])
  output_high_A[[i]] <- high_A
  
}

stats_high_A <- do.call(rbind, output_high_A) %>% 
  select(name = Name, Age, BABIP, `K/9`, `BB/9`, `K/BB`, `HR/9`, `GB/FB`, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, 
         `SwStr%`, `K%`, `BB%`, `LOB%`, `HR/FB`, `K-BB%`, AVG, WHIP, ERA, xFIP, FIP, `E-F`) %>% 
  mutate(level = "A+") %>% 
  mutate(`LD%` = as.numeric(gsub("%", "", as.character(`LD%`))),
         `GB%` = as.numeric(gsub("%", "", as.character(`GB%`))),
         `FB%` = as.numeric(gsub("%", "", as.character(`FB%`))),
         `Pull%` = as.numeric(gsub("%", "", as.character(`Pull%`))),
         `Cent%` = as.numeric(gsub("%", "", as.character(`Cent%`))),
         `Oppo%` = as.numeric(gsub("%", "", as.character(`Oppo%`))),
         `SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `BB%` = as.numeric(gsub("%", "", as.character(`BB%`))),
         `K%` = as.numeric(gsub("%", "", as.character(`K%`))),
         `LOB%` = as.numeric(gsub("%", "", as.character(`LOB%`))),
         `K-BB%` = as.numeric(gsub("%", "", as.character(`K-BB%`))),
         `HR/FB` = as.numeric(gsub("%", "", as.character(`HR/FB`))))


saveRDS(stats_high_A, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/highA_pitching.rds")

# A -----------------------------------------------------------------------

output_mid_A <- list()

for (i in 1:length(years)) {
  
  midwest_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=11&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  midwest_A <- midwest_A[-c(1,3),]
  names(midwest_A) <- midwest_A[1,]
  midwest_A <- midwest_A[-1,]
  
  south_atl_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=14&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  south_atl_A <- south_atl_A[-c(1,3),]
  names(south_atl_A) <- south_atl_A[1,]
  south_atl_A <- south_atl_A[-1,]
  
  mid_A <- Reduce(rbind, list(midwest_A, south_atl_A))
  
  print(years[i])
  output_mid_A[[i]] <- mid_A
  
}

stats_A <- do.call(rbind, output_mid_A) %>% 
  select(name = Name, Age, BABIP, `K/9`, `BB/9`, `K/BB`, `HR/9`, `GB/FB`, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, 
         `SwStr%`, `K%`, `BB%`, `LOB%`, `HR/FB`, `K-BB%`, AVG, WHIP, ERA, xFIP, FIP, `E-F`) %>% 
  mutate(level = "A") %>% 
  mutate(`LD%` = as.numeric(gsub("%", "", as.character(`LD%`))),
         `GB%` = as.numeric(gsub("%", "", as.character(`GB%`))),
         `FB%` = as.numeric(gsub("%", "", as.character(`FB%`))),
         `Pull%` = as.numeric(gsub("%", "", as.character(`Pull%`))),
         `Cent%` = as.numeric(gsub("%", "", as.character(`Cent%`))),
         `Oppo%` = as.numeric(gsub("%", "", as.character(`Oppo%`))),
         `SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `BB%` = as.numeric(gsub("%", "", as.character(`BB%`))),
         `K%` = as.numeric(gsub("%", "", as.character(`K%`))),
         `LOB%` = as.numeric(gsub("%", "", as.character(`LOB%`))),
         `K-BB%` = as.numeric(gsub("%", "", as.character(`K-BB%`))),
         `HR/FB` = as.numeric(gsub("%", "", as.character(`HR/FB`))))


saveRDS(stats_A, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/midA_pitching.rds")

# AA ----------------------------------------------------------------------

output_AA <- list()

for (i in 1:length(years)) {
  
  eastern_AA <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=5&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  eastern_AA <- eastern_AA[-c(1,3),]
  names(eastern_AA) <- eastern_AA[1,]
  eastern_AA <- eastern_AA[-1,]
  
  southern_AA <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=6&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  southern_AA <- southern_AA[-c(1,3),]
  names(southern_AA) <- southern_AA[1,]
  southern_AA <- southern_AA[-1,]
  
  texas_AA <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=7&qual=40&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  texas_AA <- texas_AA[-c(1,3),]
  names(texas_AA) <- texas_AA[1,]
  texas_AA <- texas_AA[-1,]
  
  AA <- Reduce(rbind, list(eastern_AA, southern_AA, texas_AA))
  
  print(years[i])
  output_AA[[i]] <- AA
  
}

stats_AA <- do.call(rbind, output_AA) %>% 
  select(name = Name, Age, BABIP, `K/9`, `BB/9`, `K/BB`, `HR/9`, `GB/FB`, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, 
         `SwStr%`, `K%`, `BB%`, `LOB%`, `HR/FB`, `K-BB%`, AVG, WHIP, ERA, xFIP, FIP, `E-F`) %>% 
  mutate(level = "AA") %>% 
  mutate(`LD%` = as.numeric(gsub("%", "", as.character(`LD%`))),
         `GB%` = as.numeric(gsub("%", "", as.character(`GB%`))),
         `FB%` = as.numeric(gsub("%", "", as.character(`FB%`))),
         `Pull%` = as.numeric(gsub("%", "", as.character(`Pull%`))),
         `Cent%` = as.numeric(gsub("%", "", as.character(`Cent%`))),
         `Oppo%` = as.numeric(gsub("%", "", as.character(`Oppo%`))),
         `SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `BB%` = as.numeric(gsub("%", "", as.character(`BB%`))),
         `K%` = as.numeric(gsub("%", "", as.character(`K%`))),
         `LOB%` = as.numeric(gsub("%", "", as.character(`LOB%`))),
         `K-BB%` = as.numeric(gsub("%", "", as.character(`K-BB%`))),
         `HR/FB` = as.numeric(gsub("%", "", as.character(`HR/FB`))))


saveRDS(stats_AA, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/AA_pitching.rds")
