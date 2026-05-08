### Accessing the Crosson & Kaslovsky (2024) Staff Data
### https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KEWY2U


### Libraries
library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)

## Setting working directory
setwd("~/Dropbox/correspondence_data/data/Staff")

## Opening staff data
staff<-read_dta("Dataset_ForMainAnalysis.dta")

### staff variable of interest is: pct_constituencyservice
mean(staff$pct_constituencystaff, na.rm=TRUE) 
#38.47696

## Calculating means per year

# 1) Create the summary table: mean per year
staff_year_means <- staff %>%
  group_by(year) %>%
  summarize(
    mean_pct = mean(pct_constituencystaff, na.rm = TRUE),  # ignore NAs
    n = sum(!is.na(pct_constituencystaff))                 # optional: count non-missing
  ) %>%
  arrange(year)

# Inspect the table
print(staff_year_means)
print(staff_year_means, n=30)

##plot Showing change in constituency service staff % over time


ggplot(staff_year_means, aes(x = year, y = mean_pct)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  labs(
    title = "Mean Constituency Staff by Year",
    x = "Year",
    y = "Mean % Constituency Staff"
  ) +
  theme_minimal()



