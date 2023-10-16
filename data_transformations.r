library(tidyverse)

transform_metadata_to_df <- function(stations_metadata) {

  
  # Convert the data to a tibble and perform transformations
  df <- stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    mutate(location = map(location, unlist)) %>% 
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>%
    select(-location) # remove the nested 'location' column
  
  return(df)
}
transform_metadata_to_df

to_iso8601 <- function(datetime_str, offset){
  datetime <- as_datetime(datetime_str)
  adj_datetime <- datetime + days(offset)
  iso_format <- iso8601(adj_datetime)
  return(paste0(iso_format, "Z"))
  }

transform_volumes <- function(x){
  
  # Extract the edges from the input
  edges <- x$trafficData$volume$byHour$edges
  
  # Use sapply to traverse through the list
  from <- sapply(edges, function(edge) edge$node$from)
  to <- sapply(edges, function(edge) edge$node$to)
  volume <- sapply(edges, function(edge) edge$node$total$volumeNumbers$volume)
  
  # Convert the extracted data to a data frame
  df <- data.frame(from = as.POSIXct(from, format="%Y-%m-%dT%H:%M:%S", tz = "UTC"),
                   to = as.POSIXct(to, format="%Y-%m-%dT%H:%M:%S", tz = "UTC"),
                   volume = volume)
  
  return(df)
}



