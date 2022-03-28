library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
library(lubridate)
library(gt)


# Themes and Colors -------------------------------------------------------
source("prod_helpers/colors_and_themes.r")

# Get Data ----------------------------------------------------------------

round <- 2
year <- lubridate::year(today())
session <- 'Q'

# Telem
path <- "C:/Users/Kyle/Documents/Projects/Data Projects/f1/timing_data/"

telem <- fread(paste0(path, year, "_", round, "_", session, "_telem_fastest.csv"))

# Build Plots -------------------------------------------------------------

# Full laps speed over distance
telem %>% 
  filter(Driver %in% c('VER', "LEC", "HAM")) %>% 
  ggplot(aes(x = Distance, y = Speed, color = Driver)) +
  geom_line() +
  labs(x = "Distance (m)",
       y = "Speed (kph)") +
  scale_x_continuous(n.breaks = 10)

# Use this to calculate turn locations
telem %>% filter(Distance > 4700 &
                   Distance < 5000) %>%
  group_by(Driver) %>% 
  slice_min(Speed) %>% pull(Distance) %>% median()

# Calculate turns automatically by finding where slope goes from negative to positive
corners <- telem %>% group_by(Driver) %>% 
  mutate(slope = (Speed - lag(Speed, 1L))/(Distance - lag(Distance, 1L))) %>% 
  mutate(apex = if_else(slope >= 0 & lag(slope, 1L) < 0, TRUE, FALSE)) %>% 
  filter(apex == TRUE) %>% 
  mutate(turn_start = Distance - 100L,
         turn_end = Distance + 100L) %>% 
  mutate(turn_start = round(turn_start, -1),
         turn_end = round(turn_end, -1)) %>% 
  group_by(turn_start, turn_end) %>% 
  mutate(n = n()) %>% 
  select(turn_start, turn_end, n) %>% 
  slice_max(1) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  filter(n > 10) %>%  # get only corners that more than half the grid agree on
  arrange(turn_start) %>% 
  mutate(turn_num = row_number())

## TODO Current corner calculation only finds corners where you decelerate and then re-accelarate
# This method gets the corners based on if the driver lifts off the throttle and then throttles down again, using median throttle for all drivers on their fastest lap
# In Bahrain, identified 10/15 turns vs. 8/15 using speed alone. 5 turns require 0 lifting, apparently.
telem %>% 
  mutate(dist_rounded = (Distance %/% 20) * 20) %>% 
  group_by(dist_rounded) %>% 
  select(dist_rounded, Throttle, Brake, Speed) %>% 
  summarise(med_throttle = median(Throttle),
            med_brake = median(Brake),
            med_Speed = median(Speed)) %>% 
  mutate(lifting = (med_throttle - lag(med_throttle, 1L))) %>% 
  mutate(local_min = if_else(lead(med_throttle) > med_throttle & lag(med_throttle) >= med_throttle,
                             TRUE,
                             FALSE)) %>% 
  filter(local_min == TRUE) %>% 
  mutate(turn_start = dist_rounded - 100L,
         turn_end = dist_rounded + 100L) %>% 
  group_by(turn_start, turn_end) %>% 
  mutate(n = n())

#Try to mimic corner slope method more closely
telem %>% 
  group_by(Driver) %>% 
  mutate(dist_rounded = (Distance %/% 20) * 20) %>% 
  group_by(Driver, dist_rounded) %>% 
  select(Driver, dist_rounded, Throttle, Brake, Speed) %>% 
  mutate(local_min = if_else(lead(Throttle) > Throttle & lag(Throttle) >= Throttle,
                             TRUE,
                             FALSE)) %>% 
  filter(local_min == TRUE) %>% 
  mutate(turn_start = dist_rounded - 100L,
         turn_end = dist_rounded + 100L) %>% 
  group_by(turn_start, turn_end) %>% 
  mutate(n = n())
  
# v3 WIP using throttle instead of Speed
telem %>% group_by(Driver) %>% 
  mutate(slope = (Throttle - lag(Throttle, 1L))/(Distance - lag(Distance, 1L))) %>% 
  mutate(apex = if_else(slope >= 0 & lag(slope, 1L) < 0, TRUE, FALSE)) %>% 
  filter(apex == TRUE) %>% 
  mutate(turn_start = Distance - 100L,
         turn_end = Distance + 100L) %>% 
  mutate(turn_start = round(turn_start, -1),
         turn_end = round(turn_end, -1)) %>% 
  group_by(turn_start, turn_end) %>% 
  mutate(n = n()) %>% 
  select(turn_start, turn_end, n) %>% 
  slice_max(1) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice_max(order_by = n, n = 8) %>% 
  arrange(turn_start) %>% 
  mutate(turn_num = row_number())

# Filtering using the corners df to get telem data of corners only
# Corners
library(sqldf)

corners_only <- sqldf("select * from telem a
      join corners b
      where a.Distance between b.turn_start and b.turn_end")

analyze_corner_minimums <- function(driver1, ...){
  
  data <- corners_only %>% 
    filter(Driver %in% c(driver1, ...),
           !is.na(turn_num)) %>% 
    group_by(turn_num) %>% 
    mutate(Distance = normalize_column(Distance, -1, 1) * 100)
  
  min_speeds <- data %>% 
    mutate(turn_num = paste0("Turn ", turn_num)) %>% 
    group_by(turn_num, Driver) %>% 
    summarise(min = min(Speed)) %>% 
    ungroup() %>% 
    group_by(turn_num) %>% 
    gt(groupname_col = "turn_num", rowname_col = "Driver") %>% 
    gt::cols_label(min = "Speed (kph)") %>% 
    tab_header(title = "Minimum Speed by Corner",
               subtitle = "Speed (kph)") %>% 
    gt::data_color(columns = min,
                   colors = scales::col_numeric(
                     palette = c(
                       "red", "white", "blue"
                     ),
                     domain = NULL
                   )
                   ) %>% 
    tab_options(column_labels.hidden = TRUE)
  
  return(min_speeds)
}

analyze_corners <- function(driver1, ...){
  
  # Filter for drivers provided
  data <- corners_only %>% 
    filter(Driver %in% c(driver1, ...),
           !is.na(turn_num)) %>% 
    group_by(turn_num) %>% 
    mutate(Distance = normalize_column(Distance, -1, 1) * 100)
  
  ## Calculate time lost in the corner vs. teammate (calculate delta from 100m before to 100m after apex)
  corner_speed <- data %>% 
    mutate(Time = if_else(str_detect(Time, "\\."), Time, paste0(Time, ".000000"))) %>% 
    mutate(ms = str_extract_all(Time, "\\.[0-9]+$"),
           sec = str_extract_all(Time, "[0-9]+\\."),
           sec = str_replace(sec, "\\.", ""),
           min = str_extract_all(Time, "\\:[0-9]+\\:"),
           min = str_replace_all(min, "\\:", ""),
           across(c(ms, sec, min), as.double),
           seconds = min*60 + sec + ms) %>% 
    group_by(turn_num, Driver) %>% 
    summarise(turn_start_time = min(seconds),
              turn_end_time = max(seconds),
              turn_time = turn_end_time - turn_start_time,
              turn_distance = max(Distance) - min(Distance),
              turn_kph = (turn_distance/turn_time)/1000*3600,
              plot_y = min(Speed))
  
  corner_speed_text <- corner_speed %>% 
    group_by(turn_num) %>% 
    mutate(text_comparison = if_else(turn_kph == max(turn_kph),
                                     paste0(Driver,
                                            " ",
                                            round(turn_kph - min(turn_kph), 3),
                                            " kph faster"),
                                     ""))
  
  # Plot each corner
  data %>% 
    left_join(corner_speed_text, by = c("Driver", "turn_num")) %>% 
    arrange(turn_num) %>% 
    ggplot(aes(x = Distance, y = Speed, color = Driver)) +
    geom_line(size = 2) +
    facet_wrap(vars(turn_num),
               ncol = 4) +
    scale_color_manual(name = "Driver",
                       values = driver_colors) +
    #scale_alpha_identity() +
    theme_fivethirtyeight() +
    theme(legend.position = 'bottom') +
    labs(title = "Corner Analysis",
         x = "Meters from Apex",
         y = "Speed (kph)") +
    geom_text(aes(label = text_comparison, x = 0, y = plot_y + 50), color = "#000000")
}

# Teammate corner analysis
for(i in unique(telem$Team)){
  teammates <- telem %>% 
    filter(Team == i) %>% 
    pull(Driver) %>% 
    unique() %>% 
    c()
  
  p <- analyze_corners(teammates) + 
    labs(subtitle = paste0(teammates[1], " vs. ", teammates[2], "\n",
                           "Speed (kph) and Meters from Apex (braking corners only)"))
  
  ggsave(filename = paste0("Plots/corner_analysis_", teammates[1], "_", teammates[2], ".jpg"),
         plot = p,
         units = "mm",
         width = 200,
         height = 400)
}

# TODO get the median speed for each corner
corners_only %>% 
  group_by(turn_num, Driver) %>% 
  slice_min(Speed) %>% 
  group_by(turn_num) %>% 
  summarise(med_speed = median(Speed))
  

# TODO analyze sectors (break track into 400m sectors)
sector_data <- telem %>% 
  mutate(sector = Distance%/%400) %>% 
  group_by(sector) %>% 
  mutate(meters = normalize_column(Distance, 0, 400)) %>% 
  ungroup()

analyze_sectors <- function(driver1, ...){
  
  sector_data %>% 
    filter(Driver %in% c(driver1, ...)) %>% 
    ggplot(aes(x = meters, y = Speed, color = Driver)) +
    geom_line(size = 2) +
    facet_wrap(vars(sector)) +
    theme_fivethirtyeight()
  
}

# Get speed distribution to indicate if this is a fast or slow track
speed_dist_plot <- sector_data %>% 
  ggplot(aes(x = Speed)) +
  geom_density(fill = team_colors["AlphaTauri"]) +
  theme_fivethirtyeight() +
  labs(title = "Speed Distribution (km/h)",
       subtitle = paste0("Fastest qualifying lap for each driver\nBahrain - 2022"))+
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = .05),
        plot.subtitle = element_text(hjust = .05))

ggsave(filename = paste0("Plots/speed_dist_", year, "_", round, ".jpg"),
       plot = speed_dist_plot,
       units = "mm",
       width = 200,
       height = 200)

# WIP ---------------------------------------------------------------------


# Slowest corner speeds
telem %>% 
  mutate(turn_num = case_when(
    Distance > 621 & Distance < 821 ~ 1,
    Distance > 1414 & Distance < 1614 ~ 2,
    Distance > 1774 & Distance < 1974 ~ 3,
    Distance > 2124 & Distance < 2324 ~ 4,
    Distance > 2588 & Distance < 2788 ~ 5,
    Distance > 3319 & Distance < 3519 ~ 6,
    Distance > 3973 & Distance < 4173 ~ 7,
    Distance > 4774 & Distance < 4974 ~ 8,
    TRUE ~ NA_real_
  )) %>% 
  filter(!is.na(turn_num)) %>% 
  group_by(turn_num) %>% 
  mutate(Distance = scale(Distance) * 100) %>% 
  group_by(Driver, turn_num) %>% 
  slice_min(order_by = Speed, n = 1L) %>% 
  filter(turn_num == 1) %>% 
  arrange(Speed) %>% 
  ungroup() %>% 
  select(Driver, Speed) %>% 
  distinct() %>% 
  gt() %>% 
  data_color(
    columns = Speed,
    colors = scales::col_numeric(
      palette = c(
        "red", "white", "blue"
      ),
      domain = NULL
    )
  ) %>% 
  gt::tab_header(title = md("Minimum Speeds - **Turn 1**"),
                 subtitle = "On best qualifying lap")

# Top speed per car
telem %>% 
  group_by(Driver) %>% 
  slice_max(Speed) %>% 
  select(Driver, Speed) %>% 
  distinct() %>% 
  arrange(desc(Speed)) %>% 
  ungroup() %>% 
  gt() %>% 
  data_color(
    columns = Speed,
    colors = scales::col_numeric(
      palette = c(
        "red", "white", "blue"
      ),
      domain = NULL
    )
  )

telem %>% 
  group_by(Driver) %>% 
  slice_max(Speed) %>% 
  select(Driver, Speed) %>% 
  distinct() %>% 
  arrange(desc(Speed)) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(Driver, desc(Speed)), y = Speed)) +
  geom_col() +
  geom_text(aes(label = Speed, y = Speed * .95))
