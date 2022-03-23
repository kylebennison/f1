
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


# Functions ---------------------------------------------------------------

normalize_column <- function(column, min, max){
  ((max-min)*(column - min(column)) / (max(column) - min(column))) + min
}

convert_to_seconds <- function(string){
  
  if(str_detect(string, "[0-9]+\\:[0-9]+\\:[0-9]+\\.")){
    
    ms = str_extract_all(string, "\\.[0-9]+$")
    
    sec = str_extract_all(string, "[0-9]+\\.")
    
    sec = str_replace(sec, "\\.", "")
    
    min = str_extract_all(string, "\\:[0-9]+\\:")
    
    min = str_replace_all(min, "\\:", "")
    
    ms = as.double(ms)
    sec = as.double(sec)
    min = as.double(min)
    seconds = 60*min + sec + ms
    return(seconds)
    
  } else if(str_detect(string, "[0-9]+\\:[0-9]+\\:[0-9]+")){
    
    string = paste0(string, ".000000")
      
    # do rest normally
    
    ms = str_extract_all(string, "\\.[0-9]+$")
    
    sec = str_extract_all(string, "[0-9]+\\.")
    
    sec = str_replace(sec, "\\.", "")
    
    min = str_extract_all(string, "\\:[0-9]+\\:")
    
    min = str_replace_all(min, "\\:", "")
    
    ms = as.double(ms)
    sec = as.double(sec)
    min = as.double(min)
    seconds = 60*min + sec + ms
    return(seconds)
    
  } else {
    message("Could not parse string. Expecting format:\nhh:mm:ss.ssssss")
    return(string)
  }
  
}

add_seconds <- function(dataframe, column_to_convert){
  
  df <- dataframe %>% 
    mutate(
      clean_column = case_when(
        str_detect(!!sym(column_to_convert), "[0-9]+\\:[0-9]+\\:[0-9]+\\.") ~ !!sym(column_to_convert),
        !!sym(column_to_convert) == "" ~ NA_character_,
        str_detect(!!sym(column_to_convert), "[0-9]+\\:[0-9]+\\:[0-9]+") ~ paste0(!!sym(column_to_convert), ".000000"),
        TRUE ~ "parsing_failed"),
      row = row_number())
  
  rows <- df %>% filter(clean_column == "parsing_failed") %>% 
    pull(row)
  
  if(length(rows) > 0){
    message("Warning: failed to parse time at rows ", rows)
  }
  
    df <- df %>% 
      mutate(
      ms = str_extract_all(clean_column, "\\.[0-9]+$"),
      sec = str_extract_all(clean_column, "[0-9]+\\."),
      sec = str_replace(sec, "\\.", ""),
      min = str_extract_all(clean_column, "\\:[0-9]+\\:"),
      min = str_replace_all(min, "\\:", ""),
      ms = as.double(ms),
      sec = as.double(sec),
      min = as.double(min),
      seconds = 60*min + sec + ms
    )
  
  return(df)
  
}
