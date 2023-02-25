rm(list = ls())
library(tidyverse)
library(htmltools)
library(rvest)
library(lubridate)
library(jsonlite)
library(rtweet)

session_info <- "https://livetiming.formula1.com/static/SessionInfo.json"
url <- "https://livetiming.formula1.com/static/2021/2021-10-10_Turkish_Grand_Prix/2021-10-10_Race/SPFeed.json"
url <- URLencode(url)


# Get latest race path based on session info ------------------------------

sesh_data <- jsonlite::fromJSON(session_info)
path <- sesh_data$Path
live_timing_url <- paste0("https://livetiming.formula1.com/static/", 
                          path,
                          "SPFeed.json")

library(httr)

data <- httr::GET(url = live_timing_url,
                  content_type_json())

data_tbl <- content(data, as="text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE) %>% 
  tibble()


# Previous Code -----------------------------------------------------------

data <- fromJSON(live_timing_url)
dt2 <- data$LapPos$graph$data
dt3 <- dt2 %>% enframe %>% unnest(cols = "value")
dt4 <- dt3 %>% mutate(def = rep(c("Lap", "Pos"), nrow(dt3)/2)) # Identify which row is lap and which is position

# Confirm Lap and Position mapped correctly
dt4 %>% filter(value > 20 & def == "Pos")

# Group lap and positions together for pivoting
dt5 <- dt4 %>% 
  mutate(group = round(as.integer(rownames(dt4)) %% nrow(dt4)/2 +.1)) %>% 
  pivot_wider(names_from = def, values_from = value)

# Finally get all drivers position and lap matrix
dt6 <- dt5 %>% 
  select(-group) %>% 
  pivot_wider(names_from = Pos, values_from = name)

# Order positions correctly
dt7 <- dt6 %>% select(`Lap`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, 
               `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`, `20`, `NA`)


# Weather -----------------------------------------------------------------

w1 <- data$Weather$graph$data %>% enframe() %>% unnest(cols = "value")

# Every other datapoint is time in seconds from race start.
w1 %>% group_by(name) %>% summarise(max_v = max(value))

# Try to label rows again
w2 <- w1 %>% mutate(col_def = rep(c("time_in_seconds", "value"), nrow(w1)/2))

# Pivot so each data collection type is its own column
w3 <- w2 %>% 
  mutate(group = round(as.integer(rownames(w2)) %% nrow(w2)/2 +.1)) %>% 
  pivot_wider(names_from = col_def, values_from = value) %>% 
  select(-group) %>% # remove group since no longer needed
  pivot_wider(names_from = name, values_from = value)

# Driving data --------------------------------------------------------------------

# Not clear about these - only two values per driver, and one of them is always 0
steering1 <- data$Scores$graph$Steering %>% enframe() %>% unnest(cols = "value")

gforcelat1 <- data$Scores$graph$GforceLat %>% enframe() %>% unnest(cols = "value")

gforcelong1 <- data$Scores$graph$GforceLong %>% enframe() %>% unnest(cols = "value")

brake1 <- data$Scores$graph$Brake %>% enframe() %>% unnest(cols = "value")

# This one is by lap by driver
performance1 <- data$Scores$graph$Performance %>% enframe() %>% unnest(cols = "value")

throttle1 <- data$Scores$graph$Throttle %>% enframe() %>% unnest(cols = "value")


# Driver Info -------------------------------------------------------------
# Thank god this one was stored normally

driver_tbl <- data$init$data$Drivers %>% tibble()


# Best --------------------------------------------------------------------
# I think each name is a driver in finishing (running) order, but no clue what each datapoint is. I'm assuming the time is their fastest lap?
# Shorter times look like sector times. I'm guessing there might be n_pitstops in there, laps led, etc.

best1 <- data$best$data$DR$B %>% enframe() %>% unnest(cols = "value")
# name is position 1-20


# Free -------------------------------------------------------------------
# I think this is info about the state of the race

data$free$data[1:10]

results1 <- data$free$data[11]$DR

# Can't seem to get this into a neat table for some reason
# separate(results1, c("driver", "best", "team", "pos", "interval", "DNF"), ",")
# str_split_fixed(results1, ",", 6)
