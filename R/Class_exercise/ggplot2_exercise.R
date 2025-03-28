# Challenge Exercise: Adapting Our Visualization for Tuition Analysis
# Your task is to modify our previous admission rates visualization to explore 
# the relationship between tuition and graduation rates across sectors.
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

IPEDs <- read.csv("IPEDS2020-1.csv", header = TRUE)
# STEP 1: Create correlation dataset
# Hint: You'll need to change the correlation calculation to use Tuition.and.fees..2020.21
# instead of Percent.admitted...total
IPEDs_with_cor <- IPEDs %>%
  filter(Sector.of.institution %in% 
           c("Private not-for-profit, 4-year or above", "Public, 4-year or above")) %>%
  group_by(Sector.of.institution) %>%
  summarise(
    # Modify this line to calculate correlation with tuition instead of admission rate
    r = cor(Tuition.and.fees..2020.21, 
            Graduation.rate...Bachelor.degree.within.6.years..total,
            use = "complete.obs"),
    # Modify these lines to position the text based on tuition values
    x_pos = quantile(Tuition.and.fees..2020.21, 0.9, na.rm = TRUE),
    y_pos = quantile(Graduation.rate...Bachelor.degree.within.6.years..total, 0.9, na.rm = TRUE)
  ) %>%
  mutate(
    r_label = sprintf("r = %.2f", r),
    # This part can stay the same
    Sector.of.institution = case_when(
      Sector.of.institution == "Private not-for-profit, 4-year or above" ~ "Private 4-Year",
      Sector.of.institution == "Public, 4-year or above" ~ "Public 4-Year"
    )
  )

# STEP 2: Create main visualization
# Hint: You'll need to modify the categories and variable names
IPEDs %>%
  filter(Sector.of.institution %in% 
           c("Private not-for-profit, 4-year or above", "Public, 4-year or above")) %>%
  mutate(
    # This part can stay the same
    Sector.of.institution = case_when(
      Sector.of.institution == "Private not-for-profit, 4-year or above" ~ "Private 4-Year",
      Sector.of.institution == "Public, 4-year or above" ~ "Public 4-Year"
    ),
    # Keep performance categories the same
    performance_category = case_when(
      Graduation.rate...Bachelor.degree.within.6.years..total >= 75 ~ "High Performing",
      Graduation.rate...Bachelor.degree.within.6.years..total >= 50 ~ "Medium Performing",
      TRUE ~ "Low Performing"
    ),
    # Modify this to create cost categories based on tuition quartiles
    # Hint: Replace Percent.admitted...total with Tuition.and.fees..2020.21
    # and change the category labels appropriately
    cost_category = cut(
      Tuition.and.fees..2020.21,
      breaks = quantile(Tuition.and.fees..2020.21, 
                        probs = c(0, 0.25, 0.5, 0.75, 1), 
                        na.rm = TRUE),
      labels = c("Low Cost", "Moderate Cost", "High Cost", "Very High Cost"),
      include.lowest = TRUE
    ),
    # Keep the factor conversions but modify the cost_category levels
    performance_category = factor(
      performance_category, 
      levels = c("Low Performing", "Medium Performing", "High Performing")
    ),
    cost_category = factor(
      cost_category,
      levels = c("Low Cost", "Moderate Cost", "High Cost", "Very High Cost")  # Add your cost levels here
    )
  ) %>%
  # Modify this to use tuition instead of admission rate
  ggplot(aes(x = Tuition.and.fees..2020.21, 
             y = Graduation.rate...Bachelor.degree.within.6.years..total)) +
  # These geoms can stay mostly the same
  geom_point(aes(color = performance_category, shape = cost_category), 
             alpha = 0.6) +
  geom_smooth(aes(linetype = Sector.of.institution),
              method = "lm", se = TRUE, size = 1) +
  geom_text(data = IPEDs_with_cor,
            aes(x = x_pos, y = y_pos, label = r_label),
            color = "gray30",
            hjust = 1, vjust = 1, size = 4) +
  # Add this scale to format tuition values
  scale_x_continuous(labels = scales::dollar_format(scale = 1/1000, suffix = "K")) +  # Hint: Use scales::dollar_format()
  # Color scale can stay the same
  scale_color_manual(
    values = c(
      "Low Performing" = "#E41A1C",
      "Medium Performing" = "#377EB8",
      "High Performing" = "#4DAF4A"
    )
  ) +
  # Modify the labels appropriately
  labs(title = "Tuition vs. Graduation Rates by Institution Type",
       subtitle = "Exploring the correlation between tuition costs and 6-year graduation rates",
       x = "Tuition and Fees (2020-21)",
       y = "6-Year Graduation Rate (%)",
       color = "Performance Level",
       shape = "Cost Category",
       linetype = "Institution Type") +
  # Theme can stay the same
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  # Guides can stay the same
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    linetype = guide_legend(
      override.aes = list(
        color = "black"
      ),
      order = 3
    )
  )

# HINTS:
# 1. For tuition formatting, try: scales::dollar_format(scale = 1/1000, suffix = "K")
# 2. Suggested cost category labels: "Low Cost", "Moderate Cost", "High Cost", "Very High Cost"
# 3. Remember to update the plot title and axis labels to reflect the change from admission rates to tuition
# 4. You might want to experiment with different breaks for the cost categories