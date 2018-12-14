library(rvest)
library(dplyr)
years <- seq(2009,2018,1)

# high_A ------------------------------------------------------------------

output_high_A <- list()

for (i in 1:length(years)) {
  
  california_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=8&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  california_A <- california_A[-c(1,3),]
  names(california_A) <- california_A[1,]
  california_A <- california_A[-1,]
  
  carolina_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=9&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  carolina_A <- carolina_A[-c(1,3),]
  names(carolina_A) <- carolina_A[1,]
  carolina_A <- carolina_A[-1,]
  
  florida_state_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=10&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  florida_state_A <- florida_state_A[-c(1,3),]
  names(florida_state_A) <- florida_state_A[1,]
  florida_state_A <- florida_state_A[-1,]
  
  high_A <- Reduce(rbind, list(california_A, carolina_A, florida_state_A))
  
  print(years[i])
  output_high_A[[i]] <- high_A
  
}

stats_high_A <- do.call(rbind, output_high_A) %>% 
  select(name = Name, Age, PA, BABIP, `GB/FB`, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, `SwStr%`, `BB%`, `K%`,
         `BB/K`, AVG, OBP, SLG, OPS, ISO, `wRC+`, wOBA) %>% 
  mutate(level = "A+") %>% 
  mutate(`LD%` = as.numeric(gsub("%", "", as.character(`LD%`))),
         `GB%` = as.numeric(gsub("%", "", as.character(`GB%`))),
         `FB%` = as.numeric(gsub("%", "", as.character(`FB%`))),
         `Pull%` = as.numeric(gsub("%", "", as.character(`Pull%`))),
         `Cent%` = as.numeric(gsub("%", "", as.character(`Cent%`))),
         `Oppo%` = as.numeric(gsub("%", "", as.character(`Oppo%`))),
         `SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `BB%` = as.numeric(gsub("%", "", as.character(`BB%`))),
         `K%` = as.numeric(gsub("%", "", as.character(`K%`))))


saveRDS(stats_high_A, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/highA_hitting.rds")

# A -----------------------------------------------------------------------

output_mid_A <- list()

for (i in 1:length(years)) {
  
  midwest_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=11&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  midwest_A <- midwest_A[-c(1,3),]
  names(midwest_A) <- midwest_A[1,]
  midwest_A <- midwest_A[-1,]
  
  south_atl_A <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=14&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  south_atl_A <- south_atl_A[-c(1,3),]
  names(south_atl_A) <- south_atl_A[1,]
  south_atl_A <- south_atl_A[-1,]
  
  mid_A <- Reduce(rbind, list(midwest_A, south_atl_A))
  
  print(years[i])
  output_mid_A[[i]] <- mid_A
  
}

stats_A <- do.call(rbind, output_mid_A) %>% 
  select(name = Name, Age, PA, BABIP, `GB/FB`, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, `SwStr%`, `BB%`, `K%`,
         `BB/K`, AVG, OBP, SLG, OPS, ISO, `wRC+`, wOBA) %>% 
  mutate(level = "A") %>% 
  mutate(`LD%` = as.numeric(gsub("%", "", as.character(`LD%`))),
         `GB%` = as.numeric(gsub("%", "", as.character(`GB%`))),
         `FB%` = as.numeric(gsub("%", "", as.character(`FB%`))),
         `Pull%` = as.numeric(gsub("%", "", as.character(`Pull%`))),
         `Cent%` = as.numeric(gsub("%", "", as.character(`Cent%`))),
         `Oppo%` = as.numeric(gsub("%", "", as.character(`Oppo%`))),
         `SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `BB%` = as.numeric(gsub("%", "", as.character(`BB%`))),
         `K%` = as.numeric(gsub("%", "", as.character(`K%`))))


saveRDS(stats_A, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/midA_hitting.rds")

# AA ----------------------------------------------------------------------

output_AA <- list()

for (i in 1:length(years)) {
  
  eastern_AA <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=5&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  eastern_AA <- eastern_AA[-c(1,3),]
  names(eastern_AA) <- eastern_AA[1,]
  eastern_AA <- eastern_AA[-1,]
  
  southern_AA <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=6&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  southern_AA <- southern_AA[-c(1,3),]
  names(southern_AA) <- southern_AA[1,]
  southern_AA <- southern_AA[-1,]
  
  texas_AA <- read_html(paste0('https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=7&qual=200&type=c,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50&season=', years[i], '&team=&players=&page=1_100000')) %>% 
    html_nodes("table") %>% `[[`(4) %>% html_table(fill=TRUE) 
  
  texas_AA <- texas_AA[-c(1,3),]
  names(texas_AA) <- texas_AA[1,]
  texas_AA <- texas_AA[-1,]
  
  AA <- Reduce(rbind, list(eastern_AA, southern_AA, texas_AA))
  
  print(years[i])
  output_AA[[i]] <- AA
  
}

stats_AA <- do.call(rbind, output_AA) %>% 
  select(name = Name, Age, PA, BABIP, `GB/FB`, `LD%`, `GB%`, `FB%`, `Pull%`, `Cent%`, `Oppo%`, `SwStr%`, `BB%`, `K%`,
         `BB/K`, AVG, OBP, SLG, OPS, ISO, `wRC+`, wOBA) %>% 
  mutate(level = "AA") %>% 
  mutate(`LD%` = as.numeric(gsub("%", "", as.character(`LD%`))),
         `GB%` = as.numeric(gsub("%", "", as.character(`GB%`))),
         `FB%` = as.numeric(gsub("%", "", as.character(`FB%`))),
         `Pull%` = as.numeric(gsub("%", "", as.character(`Pull%`))),
         `Cent%` = as.numeric(gsub("%", "", as.character(`Cent%`))),
         `Oppo%` = as.numeric(gsub("%", "", as.character(`Oppo%`))),
         `SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `BB%` = as.numeric(gsub("%", "", as.character(`BB%`))),
         `K%` = as.numeric(gsub("%", "", as.character(`K%`))))


saveRDS(stats_AA, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/AA_hitting.rds")

  # 
  # if (ind == 0) {
  #   league = 'al'
  #   qual='y'
  #   x = y= 2018
  #   ind = 1
  #   leaders <- read_html(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=", league, "&qual=", qual,
  #                               
  #                               "&type=c,-1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=&rost=&age=&filter=&players=&page=1_100000")) %>% html_nodes("table") %>% `[[`(12) %>% html_table(fill=TRUE)
  #   
  #   leaders <- leaders[-c(1,3),]
  #   leaders <- leaders[-c(1,3),]
  #   
  #   names(leaders) <- leaders[1,]