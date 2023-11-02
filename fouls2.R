library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(hoopR)
library(jsonlite)

reg_season_id <- nbagl_schedule(season = most_recent_nba_season()-1) %>% 
  #slice(1:409) %>%
  select(gid)

reg_season_id <- reg_season_id %>% mutate(url_league = paste0("https://stats.gleague.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=", gid, "&RangeType=2&Season=2022-23&SeasonType=Regular+Season&StartPeriod=1&StartRange=0"))
  
responses <- list()

# Loop through each URL and make a GET request
for (url in reg_season_id$url_league) {
  response <- GET(url)
  responses <- c(responses, list(response))
}

response_content <- list()

for (response in responses) {
  content_text <- content(response, "text")
  response_content <- c(response_content, list(content_text))
}

# response_data_frames <- list()
# 
# # Loop through each response and convert JSON content to a data frame
# for (content_text in response_content) {
#   # Parse the JSON content into a data frame
#   response_data <- fromJSON(content_text)
#   response_data_frames <- c(response_data_frames, list(response_data))
# }
  

response_data_frames <- list()

# Loop through each response and convert JSON content to a data frame
for (content_text in response_content) {
  json_resp_game_league <- fromJSON(content_text)
  
  # Extract the rowSet and headers
  rowSet <- json_resp_game_league$resultSets$rowSet
  headers <- json_resp_game_league$resultSets$headers[[1]]
  
  # Create a data frame from the rowSet
  league_pbp <- data.frame(rowSet)
  
  # Set column names based on the headers
  colnames(league_pbp) <- headers
  
  # Append the data frame to the list
  response_data_frames <- c(response_data_frames, list(league_pbp))
}


# Define a directory where you want to save the CSV files
output_dir <- "22-23_season"

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Loop through the response_data_frames list and save each data frame as a CSV file
for (i in seq_along(response_data_frames)) {
  # Define the file name (adjust as needed)
  file_name <- paste0("data_frame_", i, ".csv")
  
  # Create the full path to the CSV file
  file_path <- file.path(output_dir, file_name)
  
  # Save the data frame as a CSV file
  write.csv(response_data_frames[[i]], file_path, row.names = FALSE)
  
  cat("Saved", file_path, "\n")
}
