
# Themes, Colors, Functions ----------------------------------------------------

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
