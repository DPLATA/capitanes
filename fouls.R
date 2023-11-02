
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(hoopR)

# set headers h/t Ryan Davis
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


View(nbagl_schedule(season = most_recent_nba_season()-1))

reg_season_id <- nbagl_schedule(season = most_recent_nba_season()-1) #%>% slice(1:409) #%>% select(gid)

w <- as.numeric(as.character(reg_season_id$gid))

get_data <- function(w) {
  
  reg_season_id <- nbagl_schedule(season = most_recent_nba_season()-1) %>% slice(1:409)
    %>% select(gid)
  
  reg_season_id <- reg_season_id %>%
    mutate(url_league = paste0("https://stats.gleague.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=", gid, "&RangeType=2&Season=2022-23&SeasonType=Regular+Season&StartPeriod=1&StartRange=0"))
  
  #w <- as.numeric(as.character(reg_season_id$gid))
  
  #url_league <- paste0("https://stats.gleague.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=",w,"&RangeType=2&Season=2022-23&SeasonType=Regular+Season&StartPeriod=1&StartRange=0")
  
  res_league <- GET(url = url_league, add_headers(.headers=headers))
  
  res_league <- GET(url = url_league)
  
  json_resp_game_league <- fromJSON(suppressMessages(content(res_league, "text")))
  
  league_pbp <- data.frame(json_resp_game_league$resultSets$rowSet)
  
  colnames(league_pbp) <- json_resp_game_league$resultSets$headers[[1]]
  
  league_pbp$id <- w
  
  return(league_pbp)
  
}

gl_pbp <- map_df(w, get_data)





gl_pbpOffensive_Fouls <- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 37) %>% count(PERIOD)
colnames(gl_pbpOffensive_Fouls)[2] = "Offensive Foul"

gl_pbpLost_Ball<- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 2 | EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 0) %>% count(PERIOD)
colnames(gl_pbpLost_Ball)[2] = "Lost Ball"

gl_pbpOutOfBounds<- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 40) %>% count(PERIOD)
colnames(gl_pbpOutOfBounds)[2] = "Out of Bonds"

gl_pbpOffCharge<- gl_pbp %>% filter(EVENTMSGTYPE == 6 & EVENTMSGACTIONTYPE == 26) %>% count(PERIOD)
colnames(gl_pbpOffCharge)[2] = "Offensive Charge Foul"

gl_pbpBadPass<- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 1) %>% count(PERIOD)
colnames(gl_pbpBadPass)[2] = "Bad Pass"

gl_pbpTraveling<- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 4) %>% count(PERIOD)
colnames(gl_pbpTraveling)[2] = "Traveling"

gl_pbpCarry<- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 7) %>% count(PERIOD)
colnames(gl_pbpCarry)[2] = "Carry"

gl_pbp3Sec<- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 8) %>% count(PERIOD)
colnames(gl_pbp3Sec)[2] = "OFF 3-Sec Violation"

gl_pbpInbound<- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 12) %>% count(PERIOD)
colnames(gl_pbpInbound)[2] = "Inbound TOV"

# gl_pbpTOV <- gl_pbppbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 0) %>% count(PERIOD)
# colnames(gl_pbpTOV)[2] = "TOV"

gl_pbpOB_BadPass <- gl_pbp %>% filter(EVENTMSGTYPE == 5 & EVENTMSGACTIONTYPE == 45) %>% count(PERIOD)
colnames(gl_pbpOB_BadPass)[2] = "OB Bad Pass"

gl_pbpOFF_Foul <- gl_pbp %>% filter(EVENTMSGTYPE == 6 & EVENTMSGACTIONTYPE == 4) %>% count(PERIOD)
colnames(gl_pbpOFF_Foul)[2] = "OFF Fouls"





league_tov <- print(list(gl_pbpOffensive_Fouls, gl_pbpLost_Ball, gl_pbpOutOfBounds, gl_pbpOffCharge, gl_pbpBadPass, gl_pbpTraveling, gl_pbpCarry, gl_pbp3Sec,gl_pbpInbound, 
                         gl_pbpOB_BadPass,gl_pbpOFF_Foul) %>% reduce(full_join, by = "PERIOD"))





cols.num <- c(2:11)
sapply(league_tov[cols.num],as.numeric)
league_tov[cols.num] <- sapply(league_tov[cols.num],as.numeric)

league_tov <- league_tov %>%
  mutate(Total = select(.,`Offensive Foul`:`OFF Fouls`) %>% rowSums(na.rm = TRUE))

View(league_tov)



















