
# pre_weekend_start -------------------------------------------------------

# Last years race data
year <- lubridate::year(today())-1L
round <- readline(prompt = "Round: ")
round <- as.integer(round)
session <- 'R'

laps <- fread(paste0("timing_data/", year, "_", round, "_", session, "_laps.csv"))

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

fast_lap <- laps_clean %>% 
  slice_min(order_by = seconds) %>% 
  pull(seconds)

laps_clean %>% 
  filter(seconds <= 1.5 * fast_lap) %>% 
  ggplot(aes(x = TyreLife, y = seconds, color = Compound)) +
  geom_smooth(size = 2) +
  facet_wrap(vars(Stint),
             nrow = 3) +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom") +
  labs(title = "Tire Dropoff",
       subtitle = paste0("Laptime over tire life\nSaudi Arabian GP - Round ", round, ", ", year),
       caption = "@kylebeni012 for @thesingleseater via FastF1")

ggsave(filename = paste0("Plots/race_tire_dropoff_", year, "_", round, "_", session, ".jpg"),
       plot = last_plot(),
       units = "mm",
       width = 200,
       height = 400)
