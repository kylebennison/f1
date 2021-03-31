rm(list = ls())
library(tidyverse)
library(htmltools)
library(rvest)
library(lubridate)
library(jsonlite)
library(rtweet)

session_info <- "https://livetiming.formula1.com/static/SessionInfo.json"
url <- "https://livetiming.formula1.com/static/2021/2021-03-28_Bahrain_Grand_Prix/2021-03-28_Race/SPFeed.json"
url <- URLencode(url)

data <- fromJSON(url)
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
