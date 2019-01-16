library(lubridate)
library(dplyr)
library(rvest)
library(httr)
library(stringr)
library(tidyr)
library(RcppRoll)

# Pitch	           avgSwStr	avgO_Swing
# Changeup (CH)	   12.1	    31.4
# Curveball (CU)	 10.1	    29.3
# Cutter (FC)	     9.7	    29.5
# Fourseam (FA)	   6.7	    22.7
# Knuckleball (KN) 8.8	    25.7
# Screwball (SB)	 6.8	    37.0
# Sinker (SI)	     5.0	    24.7
# Slider (SL)	     14.3	    33.9
# Slow Curve (CS)	 6.9	    27.3
# Splitter (FS)	   15.2	    36.9 

#Fix Age Season Next time (Use July 1)

# data collection -------------------------------------------------------------

url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"

chadwick_player_lu_table <- readr::read_csv(url) %>%
  filter(!is.na(key_fangraphs))

fangraphs_ids <- chadwick_player_lu_table %>%
  filter(birth_year > 1965) %>%
  select(name_last, name_first, key_fangraphs, birth_year, birth_month, birth_day, pro_played_first,
         mlb_played_first, mlb_played_last) %>%
  mutate(name = paste0(name_first, " ", name_last),
         DOB = ymd(paste(birth_year, birth_month, birth_day)),
         minors = mlb_played_first - pro_played_first,
         seasons = mlb_played_last - mlb_played_first)  %>%
  filter(key_fangraphs < 30000) %>% 
  filter(mlb_played_last > 2006) %>% 
  mutate(early_birthday = ifelse(month(DOB) < 7, "yes", "no"))

# pitch_stats <- list()
# 
# # for (i in 1:nrow(fangraphs_ids)) {
# for (i in 3907:nrow(fangraphs_ids)) {
#   
#   url <- paste0("https://www.fangraphs.com/statss.aspx?playerid=", fangraphs_ids$key_fangraphs[i], "&position=C")
#   
#   player_page <- GET(as.character(url))
#   
#   position <- str_sub(player_page$url, -1) 
#   
#   if (position == "P") {
#     
#     url = paste0("https://www.fangraphs.com/pitchfx.aspx?playerid=", fangraphs_ids$key_fangraphs[i], "&position=P&pitch=all")
#     
#     pfx <- read_html(url) %>%
#       html_node(xpath = '//*[@id="PFXOverview1_dgSeason4_ctl00"]') %>%
#       html_table() %>% 
#       mutate(birth_year  = as.numeric(year(fangraphs_ids$DOB[i])),
#              early_birthday = fangraphs_ids$early_birthday[i],
#              age_season = if_else(early_birthday == "yes", as.numeric(Season) - birth_year, 
#                                   as.numeric(Season) - birth_year - 1),
#              name = paste0(fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
#     
#     pitch_stats[[i]] <- pfx
#     
#   } else 
#     next
#   
#   print(paste0(i, " - ", fangraphs_ids$name_first[i], " ", fangraphs_ids$name_last[i]))
# }
# 
# all_pitch_data <- do.call(rbind, pitch_stats)
# 
# saveRDS(all_pitch_data, "C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/historical_pitch_type_data.rds")

# averages ----------------------------------------------------------------

averages <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/historical_pitch_type_data.rds") %>% 
  mutate(`SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `O-Swing%` = as.numeric(gsub("%", "", as.character(`O-Swing%`)))) %>% 
  select(name, everything()) %>% 
  filter(Season != "Total") %>% 
  group_by(Pitch) %>% 
  summarise(avgSwStr = mean(`SwStr%`),
            avgO_Swing = mean(`O-Swing%`, na.rm = T))

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(filtered_data)


# analysis ----------------------------------------------------------------

total_pitches <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/historical_pitch_type_data.rds") %>% 
  filter(Season != "Total") %>% 
  group_by(name, Season) %>%
  summarise(total_pitches = sum(Pitches)) %>% 
  ungroup()
  
pitch_percentages <- readRDS("C://Users/johnp/Documents/GitHub/Baseball/Pitch Data/historical_pitch_type_data.rds") %>% 
  filter(Season != "Total") %>% 
  left_join(., total_pitches) %>%  
  mutate(pitch_pct = round(100*(Pitches/total_pitches),1)) %>% 
  mutate(`SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))),
         `O-Swing%` = as.numeric(gsub("%", "", as.character(`O-Swing%`))),
         `Swing%` = as.numeric(gsub("%", "", as.character(`Swing%`))),
         `Z-Swing%` = as.numeric(gsub("%", "", as.character(`Z-Swing%`))),
         `Z-Contact%` = as.numeric(gsub("%", "", as.character(`Z-Contact%`))),
         `O-Contact%` = as.numeric(gsub("%", "", as.character(`O-Contact%`))),
         `Contact%` = as.numeric(gsub("%", "", as.character(`Contact%`))),
         `Zone%` = as.numeric(gsub("%", "", as.character(`Zone%`)))) %>% 
  select(name, everything())

# new pitch? -------------------------------------------------------------

last_2_years <- pitch_percentages %>% 
  group_by(name) %>% 
  filter(max(Season) > 2016) %>% 
  ungroup() %>% 
  filter(Season > 2016) %>% 
  group_by(name) %>% 
  filter(length(unique(Season)) > 1) %>% 
  ungroup() 

pitches_2017 <- last_2_years %>% 
  filter(Season == 2017) %>% 
  select(name, Pitch, pitches_2017 = Pitches, `O-Swing%_2017` = `O-Swing%`, `SwStr%_2017` = `SwStr%`)

pitches_2018 <- last_2_years %>% 
  filter(Season == 2018) %>% 
  select(name, Pitch, pitches_2018 = Pitches, `O-Swing%_2018` = `O-Swing%`, `SwStr%_2018` = `SwStr%`)

new_pitch <- full_join(pitches_2017, pitches_2018) %>% 
  filter(is.na(pitches_2017)) %>% 
  filter(pitches_2018 > 20)

# biggest improvers -------------------------------------------------------

filtered_data <- pitch_percentages %>% 
  filter(Season > 2015) %>% 
  filter(Pitches > 20) %>% 
  # group_by(name, Season) %>% 
  # filter(max(Pitches) > 200) %>% 
  # ungroup() %>% 
  group_by(name) %>%
  filter(length(unique(Season)) == 3) %>%
  ungroup() 

pitches_thrown_2016 <- filtered_data %>% 
  filter(Season == 2016) %>% 
  select(name, Pitch, Pitches_2016 = Pitches)

pitches_thrown_2017 <- filtered_data %>% 
  filter(Season == 2017) %>% 
  select(name, Pitch, Pitches_2017 = Pitches)

two_year_avg <- filtered_data %>% 
  filter(Season < 2018) %>% 
  group_by(name, Pitch) %>% 
  summarise(`O-Swing%_avg` = round(weighted.mean(`O-Swing%`, Pitches),1),
            `SwStr%_avg` = round(weighted.mean(`SwStr%`, Pitches),1),
            `pVAL_avg` = round(weighted.mean(`pVAL`, Pitches),1)) %>% 
  ungroup()

comparisons_w_2018 <- left_join(two_year_avg, filtered_data %>% filter(Season == 2018) %>% 
                                  select(name, Pitch, Pitches_2018 = Pitches, `O-Swing%`, `SwStr%`, pVAL)) %>% 
  mutate(o_swing_diff = `O-Swing%` - `O-Swing%_avg`,
         swstr_diff = `SwStr%` - `SwStr%_avg`,
         pval_diff = pVAL - pVAL_avg) %>% 
  filter(!is.na(Pitches_2018)) %>% 
  filter(Pitches_2018 > 30) %>%
  left_join(., pitches_thrown_2016) %>% 
  left_join(., pitches_thrown_2017) %>% 
  select(name, Pitch, Pitches_2016, Pitches_2017, Pitches_2018, everything()) %>% 
  group_by(name) %>% 
  filter(sum(Pitches_2018 > 400) > 1) %>% 
  ungroup()
