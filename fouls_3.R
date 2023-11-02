library(dplyr)
library(ggplot2)

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

# Now 'combined_df' contains all your data from the CSV files in a single dataframe.

# Filter rows with "FOUL" in any description column
filtered_df <- subset(combined_df, 
                      grepl("FOUL", HOMEDESCRIPTION) |
                        grepl("FOUL", NEUTRALDESCRIPTION) |
                        grepl("FOUL", VISITORDESCRIPTION))

# Now 'filtered_df' contains only rows with "FOUL" in any description column.

# Load the dplyr package if not already loaded
# install.packages("dplyr")
# library(dplyr)

# Filter rows with "FOUL" in any description column
filtered_df_dplyr <- combined_df %>%
  filter(grepl("FOUL", HOMEDESCRIPTION) |
           grepl("FOUL", NEUTRALDESCRIPTION) |
           grepl("FOUL", VISITORDESCRIPTION))

# Now 'filtered_df' contains only rows with "FOUL" in any description column.

foul_counts <- filtered_df %>%
  group_by(PERIOD) %>%
  summarize(FOUL_COUNT = n())

ggplot(foul_counts, aes(x = PERIOD, y = FOUL_COUNT)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Total Fouls by Period",
       x = "Period",
       y = "Foul Count")


total_plays <- nrow(combined_df)
foul_count <- nrow(filtered_df)
foul_percentage <- (foul_count / total_plays) * 100







# Create a data frame for plotting
data_for_plot <- data.frame(Category = c("Fouls", "Other Plays"),
                            Percentage = c(foul_percentage, 100 - foul_percentage))

# Create a pie chart
pie_chart <- ggplot(data_for_plot, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "bottom")

# Add percentage labels to the pie chart
pie_chart_with_labels <- pie_chart +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))

# Print the pie chart with percentage labels
print(pie_chart_with_labels)








filtered_df <- combined_df %>%
  rowwise() %>%
  filter(any(grepl("P.FOUL|S.FOUL|FLAGRANT.FOUL|L.B.FOUL", 
                   c(HOMEDESCRIPTION, NEUTRALDESCRIPTION, VISITORDESCRIPTION), 
                   ignore.case = TRUE)))

non_foul_df <- combined_df %>%
  anti_join(filtered_df, by = NULL)

# View the non-foul dataframe
View(non_foul_df)

foul_type_mapping <- data.frame(
  EVENTMSGACTIONTYPE = c(15, 14, 11, 9, 3, 2, 1),
  FOUL_TYPE = c("FLAGRANT.FOUL.TYPE2", "FLAGRANT.FOUL.TYPE1", "T.FOUL", "C.P.FOUL", "L.B.FOUL", "S.FOUL", "P.FOUL"),
  stringsAsFactors = FALSE
)

# Filter rows with fouls based on EVENTMSGACTIONTYPE
filtered_df <- filtered_df %>%
  filter(EVENTMSGACTIONTYPE %in% foul_type_mapping$EVENTMSGACTIONTYPE)

# Count fouls by type
foul_counts <- filtered_df %>%
  left_join(foul_type_mapping, by = c("EVENTMSGACTIONTYPE" = "EVENTMSGACTIONTYPE")) %>%
  group_by(FOUL_TYPE) %>%
  summarize(FOUL_COUNT = n())

ggplot(foul_counts, aes(x = reorder(FOUL_TYPE, FOUL_COUNT), y = FOUL_COUNT)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = FOUL_COUNT), vjust = -0.5, size = 3) +  # Add labels to the bars
  labs(title = "Total Fouls by Type",
       x = "Foul Type",
       y = "Foul Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Count fouls by type and period
foul_counts_period <- filtered_df %>%
  left_join(foul_type_mapping, by = c("EVENTMSGACTIONTYPE" = "EVENTMSGACTIONTYPE")) %>%
  group_by(FOUL_TYPE, PERIOD) %>%
  summarize(FOUL_COUNT = n())

# Create a grouped bar chart of fouls by type for each period
ggplot(foul_counts_period, aes(x = reorder(FOUL_TYPE, FOUL_COUNT), y = FOUL_COUNT, fill = factor(PERIOD))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fouls by Type per Period",
       x = "Foul Type",
       y = "Foul Count",
       fill = "Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Period")

ggplot(foul_counts_period, aes(x = reorder(FOUL_TYPE, FOUL_COUNT), y = FOUL_COUNT, fill = factor(PERIOD))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = FOUL_COUNT), vjust = -0.5, size = 3) +  # Add labels to the bars
  labs(title = "Fouls by Type per Period",
       x = "Foul Type",
       y = "Foul Count",
       fill = "Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Period")

