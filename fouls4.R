library(dplyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(dplyr)
library(lubridate)


# Set your working directory to the folder containing the CSV files
setwd("22-23_season")

# List CSV files in the directory
csv_files <- list.files(pattern = "\\.csv$")

# Read CSV files and store them in a list
data_frames <- lapply(csv_files, function(file) {
  read.csv(file, header = TRUE)
})

# Combine all data frames into a single dataframe
combined_df <- do.call(rbind, data_frames)

fouls <- combined_df[combined_df$EVENTMSGTYPE == 6, ]

# Define the mapping as a named character vector
mapping <- c(
  "1" = "P.FOUL",
  "2" = "S.FOUL",
  "3" = "L.B.FOUL",
  "4" = "Offensive foul",
  "6" = "away from play foul",
  "9" = "C.P.FOUL",
  "10" = "Double Personal",
  "11" = "T.FOUL",
  "12" = "Non-Unsportsmanlike",
  "13" = "HANGING.TECH.FOUL",
  "14" = "FLAGRANT.FOUL.TYPE1",
  "15" = "FLAGRANT.FOUL.TYPE2",
  "16" = "double technical",
  "17" = "T.Foul Def. 3 Sec",
  "18" = "Delay",
  "25" = "Excess Timeout Technical",
  "26" = "Offensive Charge Foul",
  "28" = "Personal Take Foul",
  "30" = "Too Many Players Tech Foul",
  "31" = "Transition Take Foul"
)

# Map EVENTMSGACTIONTYPE values to the new column
fouls <- fouls %>%
  mutate(MappedEVENTMSGACTIONTYPE = recode(as.character(EVENTMSGACTIONTYPE), !!!mapping))



# Create a bar plot
ggplot(fouls, aes(x = PERIOD, fill = MappedEVENTMSGACTIONTYPE)) +
  geom_bar(position = "dodge") +
  labs(title = "Event Types by Period", x = "Period", y = "Count") +
  theme_minimal()

unique_eventmsgactiontypes <- unique(fouls$EVENTMSGACTIONTYPE)


# Count EVENTMSGACTIONTYPE
event_counts <- count(fouls$MappedEVENTMSGACTIONTYPE)

selected_events <- event_counts %>%
  filter(x %in% c("S.FOUL", "P.FOUL", "Offensive Charge Foul", "Offensive foul"))


# Plot the counts
ggplot(event_counts, aes(x = x, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "EVENTMSGACTIONTYPE Counts", x = "EVENTMSGACTIONTYPE", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


timetest <- fouls %>%
  mutate(PCTIMESTRING = hms(paste0("00:", PCTIMESTRING)))
last_five <- timetest %>%
  filter(PERIOD == 4 & PCTIMESTRING <= hms("00:05:00"))
last_two <- timetest %>%
  filter(PERIOD == 4 & PCTIMESTRING <= hms("00:02:00"))


ggplot(last_five, aes(x = PERIOD, fill = MappedEVENTMSGACTIONTYPE)) +
  geom_bar(position = "dodge") +
  labs(title = "Event Types by Period (Selected Fouls)", x = "Period", y = "Count") +
  theme_minimal()



event_counts_last_five <- count(last_five$MappedEVENTMSGACTIONTYPE)
event_counts_last_two <- count(last_two$MappedEVENTMSGACTIONTYPE)
selected_events_last_five <- event_counts_last_five %>%
  filter(x %in% c("S.FOUL", "P.FOUL", "Offensive Charge Foul", "Offensive foul"))
selected_events_last_two <- event_counts_last_two %>%
  filter(x %in% c("S.FOUL", "P.FOUL", "Offensive Charge Foul", "Offensive foul"))

# Create a bar plot for event type counts
ggplot(event_counts_last_five, aes(x = x, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Event Type Counts (Selected Fouls)", x = "Event Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(event_counts_last_two, aes(x = x, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Event Type Counts (Selected Fouls)", x = "Event Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Filter fouls to include only the desired types
filtered_fouls <- fouls %>%
  filter(MappedEVENTMSGACTIONTYPE %in% c("P.FOUL", "S.FOUL", "Offensive foul", "Offensive Charge Foul"))

# Create a bar plot for the filtered fouls
ggplot(filtered_fouls, aes(x = PERIOD, fill = MappedEVENTMSGACTIONTYPE)) +
  geom_bar(position = "dodge") +
  labs(title = "Event Types by Period (Selected Fouls)", x = "Period", y = "Count") +
  theme_minimal()

# Count the filtered fouls
event_counts_filtered <- count(filtered_fouls$MappedEVENTMSGACTIONTYPE)

# Plot the counts for the filtered fouls
ggplot(event_counts_filtered, aes(x = x, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Event Type Counts (Selected Fouls)", x = "Event Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for event type counts
ggplot(selected_events_last_five, aes(x = x, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Event Type Counts (Selected Fouls)", x = "Event Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(selected_events_last_two, aes(x = x, y = freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Event Type Counts (Selected Fouls)", x = "Event Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

