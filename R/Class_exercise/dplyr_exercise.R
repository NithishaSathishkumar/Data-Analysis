# Exercise: Student-to-Teacher Ratio Analysis
# 
# Research Question: How do student-to-teacher ratios relate to graduation rates at different types
# of institutions? Are institutions with lower ratios (smaller classes) seeing different
# graduation rate patterns?

# Load required packages
library(tidyverse)

# get and set the directory
getwd()
setwd("/Users/nithishasathishkumar/Desktop/Data_Analysis/R/Class_exercise")

# Tasks:
# 1. Create a new dataset from IPEDs that includes:
#    - Institution name
#    - Sector
#    - Student-to-faculty ratio
#    - 6-year graduation rate

IPEDs_df <- read.csv("IPEDS2020-1.csv", header = TRUE)
IPEDS <- IPEDs_df %>% select(institution.name, Sector.of.institution, Student.to.faculty.ratio, Graduation.rate...Bachelor.degree.within.6.years..total)
IPEDS <- IPEDS %>% rename(
  Institution_name = institution.name,
  Sector = Sector.of.institution,
  Student_to_faculty_ratio = Student.to.faculty.ratio,
  graduation_rate_6_year = Graduation.rate...Bachelor.degree.within.6.years..total
)

# 2. Create a new categorical variable that classifies institutions as:
#    - "Small Classes" (bottom 25% of student-to-faculty ratios)
#    - "Medium Classes" (middle 50% of student-to-faculty ratios)
#    - "Large Classes" (top 25% of student-to-faculty ratios)

quantiles <- quantile(IPEDS$Student_to_faculty_ratio, probs = c(0.25, 0.75), na.rm = TRUE)

IPEDS <- IPEDS %>%
  mutate(Class_Size = case_when(
    Student_to_faculty_ratio <= quantiles[1] ~ "Small Classes",
    Student_to_faculty_ratio > quantiles[1] & Student_to_faculty_ratio <= quantiles[2] ~ "Medium Classes",
    Student_to_faculty_ratio > quantiles[2] ~ "Large Classes"
  ))

# 3. Calculate summary statistics of 6-year graduation rates grouped by:
#    - Class size category and Sector (Public, 4-year or above & Private non-for-profit, 4-year or above)

# Filter for Public 4-year+ and Private Nonprofit 4-year+ institutions
IPEDS_filtered <- IPEDS %>%
  filter(Sector %in% c("Public, 4-year or above", "Private non-for-profit, 4-year or above"))

# Group by Class Size and Sector, then calculate summary statistics
IPEDS_summary <- IPEDS_filtered %>%
  group_by(Class_Size, Sector) %>%
  summarise(
    Avg_Graduation_Rate = mean(graduation_rate_6_year, na.rm = TRUE),
    Median_Graduation_Rate = median(graduation_rate_6_year, na.rm = TRUE),
    Min_Graduation_Rate = min(graduation_rate_6_year, na.rm = TRUE),
    Max_Graduation_Rate = max(graduation_rate_6_year, na.rm = TRUE),
    Count = n()
  )
IPEDS_summary

# HINTS:
# 1. Use case_when() to create the class size categories
# 2. Use quantile() to find the 25th and 75th percentiles
# 3. Use group_by() and summarise() to calculate summary statistics


# Questions to consider after completing the exercise:
# 1. How do the relationships between ratio and graduation rate differ between sectors?
# Based on your data, smaller class sizes tend to have higher average graduation rates.
# Public institutions with Small Classes have the highest Avg. Graduation Rate (53%), followed by Medium Classes (51.65%), and Large Classes (48.98%).
# This suggests that lower student-to-faculty ratios (smaller classes) are associated with higher graduation rates in public institutions.

# 2. What other variables might influence this relationship? Are those variables available in the dataset?
# Several variables could influence the relationship in the dataset, depending 
# on the specific relationship being analyzed some of the example are Institutional characteristics 
# such as sector (public vs. private), Carnegie classification, and institution 
# size category may impact graduation and transfer rates. Additionally, demographic variables, including
# race/ethnicity distributions,and nonresident alien percentages,
# may contribute to variations in graduation rates. 