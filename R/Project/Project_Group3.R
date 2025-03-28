getwd()
setwd("/Users/nithishasathishkumar/Desktop/Data_Analysis/R/Project")

# Install and load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the dataset (replace 'kdrama.csv' with your actual dataset file)
kdrama_data <- kdrama %>% filter(!is.na(Rating))

# Display the structure of the dataset
names(kdrama_data)



# Example 1: Influence of Release Year on Ratings
ggplot(kdrama_data, aes(x = Year.of.release, y = Rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Influence of Release Year on K-drama Ratings",
       x = "Year of Release", y = "Rating")

# Example 2: Influence of Genre on Ratings
# First, we need to split the Genre column into individual genres (if it's a comma-separated list)
kdrama_data <- kdrama_data %>%
  mutate(Genre = strsplit(as.character(Genre), ",")) %>%
  unnest(Genre)

# Plot
ggplot(kdrama_data, aes(x = Genre, y = Rating)) +
  geom_boxplot() +
  labs(title = "Influence of Genre on K-drama Ratings",
       x = "Genre", y = "Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Example 3: Influence of Duration on Ratings
ggplot(kdrama_data, aes(x = Duration, y = Rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Influence of Duration on K-drama Ratings",
       x = "Duration (minutes)", y = "Rating") +
  theme()

# Example 4: Influence of Screenwriter on Ratings
ggplot(kdrama_data, aes(x = Screenwriter, y = Rating)) +
  geom_boxplot() +
  labs(title = "Influence of Screenwriter on K-drama Ratings",
       x = "Screenwriter", y = "Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

