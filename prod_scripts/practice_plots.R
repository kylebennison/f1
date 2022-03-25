# Boxplot of laptimes by team for practice sessions


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
library(lubridate)
library(gt)


# Themes ------------------------------------------------------------------

source("prod_helpers/colors_and_themes.r")


# Data --------------------------------------------------------------------

year <- 2021
round <- 22
session <- 'FP1'

laps <- fread(paste0("timing_data/", year, "_", round, "_", session, "_laps.csv"))


# Clean Data --------------------------------------------------------------

# Apply changes to all time columns
# Can revisit if I need other columns later
# laps %>% 
#   tibble() %>% 
#   mutate(across(contains("time"), 
#                 .fns = ~ if_else(str_detect(.x, "\\."), .x, paste0(.x, ".000000")))) %>% 
#   mutate(across(contains("time"),
#                 .fns = ~ str_extract_all(.x, "\\.[0-9]+$"),
#                 .names = "ms_{.col}"))
    
laps_clean <- laps %>% 
  tibble() %>% 
  mutate(LapTime = if_else(str_detect(LapTime, "\\."), LapTime, 
                           if_else(LapTime == "", NA_character_,
                                   paste0(LapTime, ".000000")))) %>% 
  mutate(ms = str_extract_all(LapTime, "\\.[0-9]+$"),
         sec = str_extract_all(LapTime, "[0-9]+\\."),
         sec = str_replace(sec, "\\.", ""),
         min = str_extract_all(LapTime, "\\:[0-9]+\\:"),
         min = str_replace_all(min, "\\:", ""),
         across(c(ms, sec, min), as.double),
         seconds = min*60 + sec + ms)


# Plot --------------------------------------------------------------------

# Boxplot of times by team
laps_clean %>% 
  ggplot(aes(x = seconds, y = Team, fill = Team)) +
  geom_boxplot() +
  scale_fill_manual(name = "Team", values = team_colors) +
  theme_fivethirtyeight()

# Boxplot of only hot laps
med_fast_lap <- laps_clean %>% 
  slice_min(seconds, n = 20L) %>% 
  pull(seconds) %>% 
  median()

laps_clean %>% 
  filter(seconds < med_fast_lap*1.1) %>% 
  ggplot(aes(x = seconds, y = reorder(Team, desc(seconds)), fill = Team)) +
  geom_boxplot() +
  scale_fill_manual(name = "Team", values = team_colors) +
  theme_fivethirtyeight() +
  labs(title = paste0("Lap Times (Race Pace)"),
       subtitle = paste0("Jeddah - ", session, " - ", year))


# WIP ---------------------------------------------------------------------

# Race pace stints
laps_clean %>% 
  filter(Driver %in% c("HAM", "VER", "LEC")) %>% 
  filter(seconds <= med_fast_lap * 1.1) %>% 
  ggplot(aes(x = LapNumber, y = seconds, color = Driver)) +
  geom_line()
