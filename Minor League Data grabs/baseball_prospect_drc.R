library(rvest)
library(dplyr)
library(tidyr)
years <- seq(2009,2018,1)

# high_A ------------------------------------------------------------------

output_high_A <- list()

for (i in 1:length(years)) {
  
  high_A <- read_html(paste0('https://legacy.baseballprospectus.com/sortable/extras/drc_runs_minors.php?this_lvl=afa&run=default&this_year=', years[i])) %>% 
    html_nodes("table") %>% `[[`(2) %>% html_table(fill=TRUE) %>% 
    filter(PA >= 200) %>% 
    select(Name, Age, `DRC+`, DRAA) %>% 
    separate(Name, c("last", "first"), sep = ",") %>% 
    mutate(first = gsub("\\*|#", "", first), 
           name = paste0(first, " ", last)) %>% 
    select(-c(first, last))
  
  print(years[i])
  output_high_A[[i]] <- high_A
  
}

stats_high_A <- do.call(rbind, output_high_A) 

saveRDS(stats_high_A, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/highA_hitting_BP.rds")

output_A <- list()

for (i in 1:length(years)) {
  
  low_A <- read_html(paste0('https://legacy.baseballprospectus.com/sortable/extras/drc_runs_minors.php?this_lvl=afx&run=default&this_year=', years[i])) %>% 
    html_nodes("table") %>% `[[`(2) %>% html_table(fill=TRUE) %>% 
    filter(PA >= 200) %>% 
    select(Name, Age, `DRC+`, DRAA) %>% 
    separate(Name, c("last", "first"), sep = ",") %>% 
    mutate(first = gsub("\\*|#", "", first), 
           name = paste0(first, " ", last)) %>% 
    select(-c(first, last))
  
  print(years[i])
  output_A[[i]] <- low_A
  
}

stats_low_A <- do.call(rbind, output_A) 

saveRDS(stats_low_A, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/midA_hitting_BP.rds")

output_AA <- list()

for (i in 1:length(years)) {
  
  AA <- read_html(paste0('https://legacy.baseballprospectus.com/sortable/extras/drc_runs_minors.php?this_lvl=aax&run=default&this_year=', years[i])) %>% 
    html_nodes("table") %>% `[[`(2) %>% html_table(fill=TRUE) %>% 
    filter(PA >= 200) %>% 
    select(Name, Age, `DRC+`, DRAA) %>% 
    separate(Name, c("last", "first"), sep = ",") %>% 
    mutate(first = gsub("\\*|#", "", first), 
           name = paste0(first, " ", last)) %>% 
    select(-c(first, last))
  
  print(years[i])
  output_AA[[i]] <- AA
  
}

stats_AA <- do.call(rbind, output_AA) 

saveRDS(stats_AA, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/AA_hitting_BP.rds")
