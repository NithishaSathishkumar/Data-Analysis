library(tidyverse)

# get and set the directory
getwd()
setwd("/Users/nithishasathishkumar/Desktop/Data_Analysis/R/Class_exercise")

titanic_df <- read.csv("TitanicData.csv")
names(titanic_df)

ggplot(titanic_df, aes(x = factor(pclass), fill = factor(survived))) +
  geom_bar(position = "dodge") +
  facet_wrap(~ sex) +
  labs(
    title = "Titanic Survival Rate by Class and Gender",
    x = "Passenger Class",
    y = "Count",
    fill = "Survived"
  ) +
  theme_minimal()
