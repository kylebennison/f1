library(httr)
library(tidyverse)
library(xml2)

master <- tibble()
for(lap in 1:60){
  message("Doing Lap ", lap)
  
  # URL is series, year, round #, endpoint, lap number
  data <- httr::GET(url = paste0("http://ergast.com/api/f1/2021/16/laps/",
                               as.character(lap)))
  
  
  d3 <- content(data, encoding = "UTF-8")
  
  d4 <- d3 %>% xml_ns_strip() %>% xml_find_all("//Timing")
  
  attrs <- d4 %>% xml_attrs()
  
  df <-
    data.frame(matrix(unlist(attrs), nrow = length(attrs), byrow = TRUE)) %>%
    tibble()
  
  colnames(df) <- (c("name", "lap", "position", "time"))
  
  master <- rbind(master, df)
  
}

# Turn time into an actual time
laptimes <- master %>% 
  mutate(lap_time_in_seconds = as.integer(str_sub(time, 1L, 1L)) * 60 +
           as.integer(sub(":", "", str_extract(time, ":[0-9]+"))) +
           as.double(str_extract(time, "\\.[0-9]+")),
         lap = as.integer(lap),
         postion = as.integer(postion)
         )

avg_laptime <- laptimes %>% 
  group_by(lap) %>% 
  summarise(lap_time_in_seconds = mean(lap_time_in_seconds)) %>% 
  mutate(name = "field", postion = 0L)

# Add gap to car in front, behind, and leader
gaps <- laptimes %>% 
  group_by(name) %>% 
  mutate(total_time = cumsum(lap_time_in_seconds)) %>% # sum all laptimes so far
  group_by(lap) %>% 
  mutate(leader_time = min(total_time)) %>% # get lowest total time per lap as leader
  group_by(name, lap) %>% 
  mutate(gap_to_front = leader_time - total_time) %>% # subtract your time from leader's time
  group_by(lap) %>% 
  mutate(gap_to_car_infront = lag(total_time, n = 1L, order_by = total_time) - total_time,
         gap_to_car_behind = lead(total_time, n = 1L, order_by = total_time) - total_time
  )

# Graph Laptimes

laptimes %>%
  select(-time) %>%
  rbind(avg_laptime) %>%
  filter(postion <= 10,
         lap > 30) %>%
  filter(name %in% c("bottas", "leclerc", "hamilton", "perez", "field")) %>%
  ggplot(aes(x = lap, y = lap_time_in_seconds)) +
  geom_line(aes(color = name,
                linetype = if_else(name != "field", "dashed", "solid"),
                alpha = if_else(name == "hamilton", 1, 
                                if_else(name == "field", .5, .2))), 
            size = 2) +
  scale_color_manual(values = c("#ed7784", "#000000", "#1fed7c", "#827af0", "#eb07f7")) +
  guides(linetype = FALSE,
         alpha = FALSE) +
  annotate(geom = "curve",
           x = 52, 
           y = 115, 
           xend = 50,
           yend = 95,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = 52, y = 115,
           label = "HAM called in to pit again",
           hjust = "left") +
  annotate(geom = "segment",
           x = 42, 
           y = 105, 
           xend = 42,
           yend = 95,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = 42, y = 105,
           label = "HAM called in to pit for\nthe first time",
           vjust = "bottom") +
  annotate(geom = "segment",
           x = 42, y = 112,
           xend = 38, yend = 112,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = 42, y = 112,
           label = "Perez and Bottas\npit for fresh inters",
           hjust = "left") +
  coord_cartesian(clip = "off") +
  labs(x = "Lap",
       y = "Lap Time",
       color = "Driver",
       title = "HAM Lap Time vs. The Field",
       caption = "@kylebeni012 for @staturdays | Data: ergast") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = "lightgrey"),
        plot.title = element_text(size = 25)) +
  scale_alpha_continuous(range = c(.2, 1))

ggsave(filename = paste0(lubridate::today(),
                         "_",
                         "HAM_lap_times.jpg"),
       path = "C:/Users/Kyle/Downloads",
       plot = last_plot(),
       height = 200, width = 400,
       units = "mm",
       dpi = 300)


# Gap

gaps %>% 
  filter(lap > 30, postion < 10) %>% 
  ggplot(aes(x = lap, y = gap_to_car_behind)) + 
  geom_line(aes(color = name,
                alpha = if_else(name == "hamilton", 1, .3))) +
  annotate(geom = "curve",
           x = 52, 
           y = 30, 
           xend = 50,
           yend = 11,
           arrow = arrow(length = unit(2, "mm")),
           curvature = -.3) +
  annotate(geom = "text",
           x = 52, y = 30,
           label = "HAM asked to box again",
           hjust = "left",
           vjust = "bottom") +
  annotate(geom = "segment",
           x = 42, 
           y = 30, 
           xend = 42,
           yend = 15,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = 42, y = 30,
           label = "HAM is asked to box for\nthe first time",
           vjust = "bottom") +
  annotate(geom = "segment",
           x = 45, 
           y = 20, 
           xend = 45,
           yend = 15,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = 47, y = 20,
           label = "HAM starts losing the gap",
           vjust = "bottom") +
  coord_cartesian(clip = "off") +
  guides(alpha = FALSE,
         color = FALSE) +
  scale_alpha_continuous(range = c(.2, 1)) +
  labs(x = "Lap",
       y = "Gap to Car Behind",
       color = "Driver",
       title = "HAM Gap to Car Behind",
       caption = "@kylebeni012 for @staturdays | Data: ergast") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = "lightgrey"),
        plot.title = element_text(size = 25))

ggsave(filename = paste0(lubridate::today(),
                         "_",
                         "HAM_gap_car_behind.jpg"),
       path = "C:/Users/Kyle/Downloads",
       plot = last_plot(),
       height = 200, width = 400,
       units = "mm",
       dpi = 300)


gaps %>% 
  filter(lap > 30, postion < 10) %>% 
  ggplot(aes(x = lap, y = gap_to_front)) + 
  geom_line(aes(alpha = if_else(name == "hamilton", 1, .9), # not respecting my .9 wishes
                color = name)) +
  guides(alpha = FALSE) +
  scale_alpha_continuous(range = c(.3, 1)) +
  annotate(geom = "curve",
           x = 52, 
           y = -70, 
           xend = 50,
           yend = -20,
           arrow = arrow(length = unit(2, "mm")),
           curvature = -.3) +
  annotate(geom = "text",
           x = 52, y = -70,
           label = "HAM asked to box again",
           hjust = "left") +
  annotate(geom = "segment",
           x = 42, 
           y = -70, 
           xend = 42,
           yend = -20,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = 42, y = -70,
           label = "HAM is asked to box for\nthe first time",
           vjust = "top") +
  annotate(geom = "segment",
           x = 46, 
           y = -80, 
           xend = 46,
           yend = -20,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = 46, y = -80,
           label = "HAM starts dropping back",
           vjust = "top") +
  coord_cartesian(clip = "off") +
  labs(x = "Lap",
       y = "Gap to Leader",
       color = "Driver",
       title = "HAM Gap to Leader",
       caption = "@kylebeni012 for @staturdays | Data: ergast") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = "lightgrey"),
        plot.title = element_text(size = 20))

ggsave(filename = paste0(lubridate::today(),
                         "_",
                         "HAM_gap_to_leader.jpg"),
       path = "C:/Users/Kyle/Downloads",
       plot = last_plot(),
       height = 200, width = 400,
       units = "mm",
       dpi = 300)

