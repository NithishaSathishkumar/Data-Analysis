getwd()
setwd("/Users/nithishasathishkumar/Desktop/Data_Analysis/R/Project")

# Install and load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
install.packages("viridis")

# What factors, such as release year, duration, genre, tags, screenwriter influence the success of a K-drama in terms of ratings?
# Read the dataset (replace 'kdrama.csv' with your actual dataset file)
kdrama <- read.csv("kdrama.csv")
names(kdrama)

# Number of Kdrama based on released year
kdrama %>% ggplot(aes(x = Year.of.release)) + 
  geom_bar() +
  labs(
    title = "Number of Korean drama released based on Years",
    x = "Year",
    y = "Number of Korean Drama",
  ) + 
  theme(
    plot.title = element_text(
      hjust = 0.5, #center
      size = 12, 
      color = "blue",
      face = "bold" # bold title
    )
  ) 

table(kdrama$Year.of.release)

# Function to convert duration strings into total minutes
convert_to_minutes <- function(time) {
  hours <- as.numeric(str_extract(time, "\\d+(?= hr)")) # Extract hours
  minutes <- as.numeric(str_extract(time, "\\d+(?= min)")) # Extract minutes
  
  hours[is.na(hours)] <- 0  # Replace NA when no hours present
  minutes[is.na(minutes)] <- 0 # Replace NA when no minutes present
  
  return(hours * 60 + minutes) # Total minutes
}

# Convert the Duration column
kdrama <- kdrama %>%
  mutate(Duration_Minutes = sapply(Duration, convert_to_minutes))

# Define 30-minute interval bins correctly
breaks_seq <- seq(0, max(kdrama$Duration_Minutes, na.rm = TRUE) + 30, by = 30) 

# Adjust labels dynamically based on the number of breaks
labels_seq <- paste0(head(breaks_seq, -1), "-", tail(breaks_seq, -1) - 1, " min")

# Assign bins
kdrama <- kdrama %>%
  mutate(Time_Group = cut(Duration_Minutes, 
                          breaks = breaks_seq, 
                          labels = labels_seq, 
                          right = FALSE)) 

# Check if grouping worked
table(kdrama$Time_Group)

# Calculate the average rating for each duration interval
duration_summary <- kdrama %>%
  group_by(Time_Group) %>%
  summarise(Average_Rating = mean(Rating, na.rm = TRUE))  # Compute mean rating

# Plot the influence of duration on ratings
ggplot(duration_summary, aes(x = Time_Group, y = Average_Rating, fill = Time_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Influence of Duration on K-drama Ratings",
       x = "Duration Interval",
       y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ratings by Original Network
# Function to convert duration strings into total minutes
convert_to_minutes <- function(time) {
  hours <- as.numeric(str_extract(time, "\\d+(?= hr)")) # Extract hours
  minutes <- as.numeric(str_extract(time, "\\d+(?= min)")) # Extract minutes
  
  hours[is.na(hours)] <- 0  # Replace NA when no hours present
  minutes[is.na(minutes)] <- 0 # Replace NA when no minutes present
  
  return(hours * 60 + minutes) # Total minutes
}

# Convert the Duration column
kdrama <- kdrama %>%
  mutate(Duration_Minutes = sapply(Duration, convert_to_minutes))

# Define 30-minute interval bins correctly
breaks_seq <- seq(0, max(kdrama$Duration_Minutes, na.rm = TRUE) + 30, by = 30) 

# Adjust labels dynamically based on the number of breaks
labels_seq <- paste0(head(breaks_seq, -1), "-", tail(breaks_seq, -1) - 1, " min")

# Assign bins
kdrama <- kdrama %>%
  mutate(Time_Group = cut(Duration_Minutes, 
                          breaks = breaks_seq, 
                          labels = labels_seq, 
                          right = FALSE)) 

# Check if grouping worked
# table(kdrama$Time_Group)

# Calculate the average rating for each duration interval
duration_summary <- kdrama %>%
  group_by(Time_Group) %>%
  summarise(Average_Rating = mean(Rating, na.rm = TRUE))  # Compute mean rating

# Plot the influence of duration on ratings
ggplot(duration_summary, aes(x = Time_Group, y = Average_Rating, fill = Time_Group)) +
  geom_bar(stat = "identity") +
  labs(
    # title = "Influence of Duration on K-drama Ratings",
    x = "Duration Interval",
    y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



