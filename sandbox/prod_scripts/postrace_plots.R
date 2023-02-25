# Telem Gap Analysis

#' Review gap to Leclerc before and after VER 2nd pit stop, and see what he 
#' would need to have lapped to be ahead of Leclerc, and if it was feasible based on
#' other out laps on that tyre, and his best lap.


# Libraries ---------------------------------------------------------------

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
session <- 'R'

# Telem
path <- "C:/Users/Kyle/Documents/Projects/Data Projects/f1/timing_data/"

laps <- fread(paste0(path, year, "_", round, "_", session, "_laps.csv"))

telem <- fread(paste0(path, year, "_", round, "_", session, "_telem_all.csv"))


# Plots -------------------------------------------------------------------

laps_clean <- laps %>% 
  add_seconds("LapTime")

laptime_chart <- function(driver1, ...){
  
  laps_clean %>% 
    filter(Driver %in% c(driver1, ...)) %>% 
    filter(seconds <= min(laps_clean$seconds, na.rm = TRUE) * 1.2) %>% 
    ggplot(aes(x = LapNumber, y = seconds, color = Driver, linetype = Driver)) +
    geom_line(size = 2) +
    scale_color_manual(name = "Driver", values = driver_colors) +
    geom_text(aes(x = .5, y = seconds, label = if_else(LapNumber == 2, Driver, "")))
  
}

laptime_chart("PER", "LEC")

# Tyre life
laps_clean %>% 
  ggplot(aes(x = TyreLife, y = seconds, color = Compound)) +
  geom_smooth() +
  facet_wrap(vars(Stint),
             ncol = 1)

# Lap-to-lap time improvement/dropoff from previous

laptime_vs_previous <- function(driver1, ...){
  
  laps_clean %>% 
    filter(Driver %in% c(driver1, ...)) %>% 
    filter(seconds <= min(laps_clean$seconds, na.rm = TRUE) * 1.2) %>% 
    group_by(Driver) %>% 
    mutate(delta = seconds - lag(seconds)) %>% 
    ggplot(aes(x = LapNumber, y = delta, color = Driver, linetype = Driver)) +
    geom_line(size = 2) +
    scale_color_manual(name = "Driver", values = driver_colors) +
    geom_text(aes(x = .5, y = delta, label = if_else(LapNumber == 2, Driver, ""))) +
    geom_hline(yintercept = 0)
  
}

laptime_vs_previous("PER", "LEC")

telem %>% 
  filter(Driver == "LAT", 
         Lap == max(telem[telem$Driver == "LAT"]$Lap)) %>% 
  ggplot(aes(x = Distance, y = Throttle)) +
  geom_line()
