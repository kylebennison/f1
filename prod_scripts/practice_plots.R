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

year <- readline(prompt = "Year: ")
year <- as.integer(year)
round <- readline(prompt = "Round: ")
round <- as.integer(round)
session <- readline(prompt = "Session: ")

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
       subtitle = paste0(session, " - ", "Round ", round, ", ",  year))

ggsave(filename = paste0("Plots/team_box_", year, "_", round, "_", session, ".jpg"),
              plot = last_plot(),
              units = "mm",
              width = 200,
              height = 200)

# Laps run
laps_clean %>% 
  filter(!is.na(seconds)) %>% 
  group_by(Team) %>% 
  summarise(laps_run = n()) %>% 
  ggplot(aes(x = laps_run, y = reorder(Team, laps_run), fill = Team)) +
  geom_col() +
  scale_fill_manual(name = "Team", values = team_colors) +
  theme_fivethirtyeight() +
  labs(title = paste0("Laps Run"),
       subtitle = paste0(session, " - ", "Round ", round, ", ",  year))

ggsave(filename = paste0("Plots/team_laps_run_", year, "_", round, "_", session, ".jpg"),
       plot = last_plot(),
       units = "mm",
       width = 200,
       height = 200)

# Race pace stints
laps_clean %>% 
  filter(Driver %in% c("HAM", "VER", "LEC", "SAI", "RUS", "PER")) %>% 
  filter(seconds <= med_fast_lap * 1.1) %>% 
  ggplot(aes(x = LapNumber, y = seconds, color = Driver)) +
  geom_point(size = 3) +
  geom_line(size = 2, alpha = .7) +
  scale_color_manual(name = "Driver", values = driver_colors) +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom") +
  labs(title = "Lap Times (Race Pace)",
       subtitle = paste0(session, " - ", "Round ", round, ", ",  year))

ggsave(filename = paste0("Plots/driver_laps_", year, "_", round, "_", session, ".jpg"),
       plot = last_plot(),
       units = "mm",
       width = 200,
       height = 200)
