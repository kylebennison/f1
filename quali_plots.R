library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
library(lubridate)
library(gt)


# Themes and Colors -------------------------------------------------------
driver_colors <- c(
  "HAM" = "#000000", "VER" = "#0600EF", "LEC" = "#DC0000",
  "SAI" = "#ffeb00", "RUS" = "#00d2be", "LAT" = "#00A3E0",
  "STR" = "#cedc00", "RIC" = "#47C7FC", "HUL" = "#006f62",
  "TSU" = "#636363", "ZHO" = "#909090", "ALB" = "#041e42",
  "NOR" = "#ff8000", "MSC" = "#b3b3b3", "OCO" = "#0064bf",
  "GAS" = "#2b4562", "ALO" = "#d60812", "MAG" = "#E6002D",
  "BOT" = "#a51d2f", "VET" = "#00594f", "PER" = "#db0a40"
)

team_colors <- c(
  'Alfa Romeo'= '#900000', 
  'AlphaTauri'= '#2b4562', 
  'Alpine'= '#0090ff', 
  'Aston Martin'= '#006f62', 
  'Ferrari'= '#dc0000', 
  'Haas F1 Team'= '#ffffff', 
  'McLaren'= '#ff8700', 
  'Mercedes'= '#00d2be', 
  'Red Bull Racing'= '#0600ef', 
  'Williams'= '#005aff'
)

source("theme_fivethirtyeight.r")

normalize_column <- function(column, min, max){
  ((max-min)*(column - min(column)) / (max(column) - min(column))) + min
}

# Get Data ----------------------------------------------------------------

# TODO
laps <- fread("C:/Users/Kyle/Documents/Projects/Data Projects/f1/timing_data/2022_1_fp2_laps.csv")

glimpse(laps)


laps %>%
  mutate(across(
    c(LapTime, PitOutTime, PitInTime),
    .fns = ~ str_replace_all(.x, "0 days ", "")
  ),
  across(c(LapTime),
         .fns = ~ str_replace(.x, "00:0", "")),
  laps = as_datetime(LapTime, format = "m:ss"))
##

# Telem
path <- "C:/Users/Kyle/Documents/Projects/Data Projects/f1/timing_data/"

telem <- fread(paste0(path, "2022_1_q_telem_fastest.csv"))

glimpse(telem)


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
  slice_max(order_by = n, n = 8) %>% 
  arrange(turn_start) %>% 
  mutate(turn_num = row_number())

# TODO Replace long casewhen with something filtering using the corner data, or maybe left join
# Corners
corners_only <- telem %>% 
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
  ))

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
  
  # Plot each corner
  data %>% 
    ggplot(aes(x = Distance, y = Speed, color = Driver)) +
    geom_line(size = 2) +
    facet_wrap(vars(paste0("Turn ", turn_num)),
               nrow = 4) +
    scale_color_manual(name = "Driver",
                       values = driver_colors) +
    #scale_alpha_identity() +
    theme_fivethirtyeight() +
    theme(legend.position = 'bottom') +
    labs(title = "Corner Analysis",
         x = "Meters from Apex",
         y = "Speed (kph)")
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
