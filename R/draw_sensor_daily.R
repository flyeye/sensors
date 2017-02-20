draw_sensor <- function (filename, sensor)
{
  grower_data <- read.csv(filename, header = TRUE)
  grower_data$Date.Time <- as.POSIXct(as.character(grower_data$Date.Time))
  plot(grower_data$Date.Time, grower_data[[sensor]], type = "l", yaxp = c(min(floor(grower_data[[sensor]]-2)), floor(max(grower_data[[sensor]]+2)), 20))
  
}