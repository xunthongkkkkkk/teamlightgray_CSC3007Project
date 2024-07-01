## -----------------------------------------------------------------------------
#| label: library
#| message: false

library(tidyverse)
library(dplyr)
library(knitr)
library(readr)


## -----------------------------------------------------------------------------
#| label: fig-databreach
#| echo: false
#| fig.cap: "Visualization of World's Biggest Data Breaches & Hacks from
#|   2004 to 2024 by David McCandless, Tom Evans, and Paul Barton."

include_graphics("images/previous_visualization.png")


## -----------------------------------------------------------------------------
#| label: input-data
#| message: false

data <- read_csv("IIB Data Breaches - LATEST - breaches.csv")

# Clean the data
data <- data %>%
  mutate(
    records.lost = as.numeric(gsub("[^0-9]", "", `records lost`)),
    year = as.numeric(year)
  ) %>%
  drop_na(records.lost, year, sector)
data



## -----------------------------------------------------------------------------
#| label: group-sectors

# Group sectors into broader categories
data <- data %>%
  mutate(
    sector_group = case_when(
      grepl("web", sector, ignore.case = TRUE) ~ "Web",
      grepl("healthcare|health", sector, ignore.case = TRUE) ~ "Healthcare",
      grepl("app", sector, ignore.case = TRUE) ~ "App",
      grepl("retail", sector, ignore.case = TRUE) ~ "Retail",
      grepl("gaming", sector, ignore.case = TRUE) ~ "Gaming",
      grepl("transport", sector, ignore.case = TRUE) ~ "Transport",
      grepl("financial|finance", sector, ignore.case = TRUE) ~ "Financial",
      grepl("tech", sector, ignore.case = TRUE) ~ "Tech",
      grepl("government", sector, ignore.case = TRUE) ~ "Government",
      grepl("telecoms", sector, ignore.case = TRUE) ~ "Telecoms",
      grepl("legal", sector, ignore.case = TRUE) ~ "Legal",
      grepl("media", sector, ignore.case = TRUE) ~ "Media",
      grepl("academic", sector, ignore.case = TRUE) ~ "Academic",
      grepl("energy", sector, ignore.case = TRUE) ~ "Energy",
      grepl("military", sector, ignore.case = TRUE) ~ "Military",
      TRUE ~ "Miscellaneous"
    )
  ) %>%
  drop_na(records.lost, year, sector_group)
data



## -----------------------------------------------------------------------------
#| label: filter-data

# Define the year range
start_year <- 2010
end_year <- 2024

# Filter the data based on the specified year range and minimum records lost
data <- data %>%
  filter(year >= start_year & year <= end_year)

# Calculate total records lost for each sector group and select top 10 sectors
top_sectors <- data %>%
  group_by(sector_group) %>%
  summarize(total_records_lost = sum(records.lost)) %>%
  arrange(desc(total_records_lost)) %>%
  top_n(10, wt = total_records_lost) %>%
  pull(sector_group)

# Filter the data to include only top 10 sectors
data <- data %>%
  filter(sector_group %in% top_sectors)
data



## -----------------------------------------------------------------------------
#| label: prepare-data

# Convert the year to a date format for plotting
data <- data %>%
  mutate(
    StartDate = as.Date(paste(year, "-01-01", sep = "")),
    EndDate = as.Date(paste(year, "-12-31", sep = ""))
  )

# Sort the sectors by total records lost
sector_levels <- data %>%
  group_by(sector_group) %>%
  summarize(total_records_lost = sum(records.lost)) %>%
  arrange(desc(total_records_lost)) %>%
  pull(sector_group)

data <- data %>%
  mutate(sector_group = factor(sector_group, levels = rev(sector_levels)))

# Define important sectors to highlight
highlight_sectors <- c("Web", "Financial", "App", "Transport")

# Add a new column to specify color based on sector importance
data <- data %>%
  mutate(
    highlight = ifelse(as.character(sector_group) %in% highlight_sectors, as.character(sector_group), "Other")
  )

# Convert sector_group to a factor and then to numeric for plotting
data$sector_group_num <- as.numeric(factor(data$sector_group))

data

# Calculate the total number of data breaches each year
yearly_data <- data %>%
  group_by(year) %>%
  summarize(total_records_lost = sum(records.lost))
yearly_data <- yearly_data %>%
  filter(year < 2024)
data <- data %>%
  filter(year < 2024)

yearly_data


