library(dplyr)

batted_files <- list.files("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/") %>% 
  data.frame(files = .) %>%
  filter(grepl("Batted", files)) %>%
  pull(.)

batted <- list()

for (i in 1:length(batted_files)) {
  
  x <- read.csv(paste0("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/", batted_files[i]))
  
  batted[[i]] <- x
}

batted_data <- do.call(rbind, batted) %>% 
  rename(name = ï..Name) %>% 
  select(name, Age, PA, BABIP, `GB/FB` = GB.FB, `LD%` = LD., `GB%` = GB., `FB%` = FB., `Pull%` = Pull., 
         `Cent%` = Cent., `Oppo%` = Oppo., `SwStr%` = SwStr.) %>% 
  mutate(`LD%` = as.numeric(gsub("%", "", as.character(`LD%`))),
         `GB%` = as.numeric(gsub("%", "", as.character(`GB%`))),
         `FB%` = as.numeric(gsub("%", "", as.character(`FB%`))),
         `Pull%` = as.numeric(gsub("%", "", as.character(`Pull%`))),
         `Cent%` = as.numeric(gsub("%", "", as.character(`Cent%`))),
         `Oppo%` = as.numeric(gsub("%", "", as.character(`Oppo%`))),
         `SwStr%` = as.numeric(gsub("%", "", as.character(`SwStr%`))))

advanced_files <- list.files("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/") %>% 
  data.frame(files = .) %>%
  filter(grepl("Advanced", files)) %>%
  pull(.)

advanced <- list()

for (i in 1:length(advanced_files)) {
  
  x <- read.csv(paste0("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/", advanced_files[i]))
  
  advanced[[i]] <- x
}

advanced_data <- do.call(rbind, advanced) %>% 
  rename(name = ï..Name) %>% 
  select(name, `BB%` = BB., `K%` = K., `BB/K` = BB.K, AVG, OBP, SLG, OPS, ISO, `wRC+` = wRC., wOBA, Age) %>% 
  mutate(`BB%` = as.numeric(gsub("%", "", as.character(`BB%`))),
         `K%` = as.numeric(gsub("%", "", as.character(`K%`))))

standard_files <- list.files("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/") %>% 
  data.frame(files = .) %>%
  filter(grepl("Standard", files)) %>%
  pull(.)

standard <- list()

for (i in 1:length(standard_files)) {
  
  x <- read.csv(paste0("C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/", standard_files[i]))
  
  standard[[i]] <- x
}

standard_data <- do.call(rbind, standard) %>% 
  rename(name = ï..Name) 

all <- Reduce(left_join, list(batted_data, advanced_data))

saveRDS(all, "C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots/data.rds")
