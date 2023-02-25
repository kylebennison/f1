rm(list = ls())

library(data.table)
library(tidyverse)

data <- fread("has_beef_with.csv")

nodes <- data %>% 
  select(driver, has_beef_with)

n2 <- bind_rows(
nodes[,1],
nodes[,2]
) %>% 
  mutate(driver = if_else(is.na(driver), has_beef_with, driver)) %>% 
  select(1) %>% 
  unique() %>% 
  rename(label = driver) %>% 
  rowid_to_column("id")

edges <- data %>% 
  select(driver, has_beef_with, because, beef_level) %>% 
  mutate(weight = case_when(beef_level == "low" ~ 1L,
                            beef_level == "medium" ~ 2L,
                            beef_level == "high" ~ 3L,
                            TRUE ~ 3L))

edges <- edges %>% left_join(n2, by = c("driver" = "label")) %>% 
  rename(from = "id")

edges <- edges %>% left_join(n2, by = c("has_beef_with" = "label")) %>% 
  rename(to = "id")

#edges <- edges %>% ungroup() %>% select(from, to, weight)

library(visNetwork)
library(networkD3)

edges <- edges %>% rename(label = because)

edges <- edges %>% mutate(width = weight)

edges <- edges %>% mutate(length = 600, font.size = 10)

n2 <- n2 %>% mutate(font.size = 10)

visNetwork(nodes = n2, edges = edges, main = "F1 2021 Beef Network") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle", length = 1000) %>% 
  visLayout(randomSeed = 12)
