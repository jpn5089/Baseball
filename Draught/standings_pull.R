library(RSelenium)
library(lubridate)

rD <- rsDriver(verbose = FALSE, browser = "firefox")

remDr <- rD$client

cbs_sports_url <- "https://www.cbssports.com/login"

remDr$navigate(cbs_sports_url)

username <- remDr$findElement(using = "id", value = "userid")
username$sendKeysToElement(list("jp19nicola@gmail.com"))

#send password and Enter
passwd <- remDr$findElement(using = "id", value = "password")
passwd$sendKeysToElement(list("liverpool19", "\uE007"))

Sys.sleep(3)

draught_stats_url <- "http://draughtdynastyleague.baseball.cbssports.com/standings/overall"

Sys.sleep(3)

remDr$navigate(draught_stats_url)

Sys.sleep(3)

webElem <- remDr$findElement(using = 'id', value = "btnExport")

Sys.sleep(3)

webElem$clickElement()
standings <- read.csv("C://Users/johnp/Downloads/overall.csv")
file.remove("C://Users/johnp/Downloads/overall.csv")

date <- today() - days(1)

write.csv(standings, paste0("C://Users/johnp/Documents/GitHub/Baseball/Draught/2019/standings_", date, ".csv"),
          row.names = F)

remDr$close()
rD$server$stop()
