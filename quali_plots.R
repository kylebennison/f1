library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
library(lubridate)
library(gt)


# Themes and Colors -------------------------------------------------------
driver_colors <- c(
  "HAM" = "#000000",
  "VER" = "#0600EF",
  "LEC" = "#DC0000"
)

# Team	Color	RGB
# Mercedes	#00D2BE	0,210,90
# Ferrari	#DC0000	220,0,0
# Red Bull Racing	#0600EF	6,0,239
# Alpine	#0090FF	0,144,255
# Haas	#FFFFFF	255,255,255
# Aston Martin	#006F62	0,111,98
# AlphaTauri	#2B4562	43,69,98
# McLaren	#FF8700	255,135,0
# Alfa Romeo Racing	#900000	144,0,0
# Williams	#005AFF	0,90,255

# Get Data ----------------------------------------------------------------

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
  filter(Driver %in% c('VER', 'LEC', 'HAM'),
         !is.na(turn_num)) %>% 
  group_by(turn_num) %>% 
  mutate(Distance = scale(Distance) * 100) %>% 
  ggplot(aes(x = Distance, y = Speed, color = Driver)) +
  geom_line(size = 2, alpha = .5) +
  facet_wrap(vars(turn_num)) +
  scale_color_manual(name = "Driver",
                     values = driver_colors) +
  scale_alpha_identity()

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
