draw_sensor_ggplot <- function (filename, sensor, ylabel)
{
  grower_data <- read.csv(filename, header = TRUE)
  grower_data$Date.Time <- as.POSIXct(as.character(grower_data$Date.Time))
#  plot(grower_data$Date.Time, grower_data[[sensor]], type = "l", yaxp = c(min(floor(grower_data[[sensor]]-2)), floor(max(grower_data[[sensor]]+2)), 20))
  #ggplot2::ggplot(grower_data, aes(x=c(rep(grower_data[[1]], 2)), y = c(grower_data[[2]], grower_data[[4]]))) + geom_point() + geom_line() + geom_smooth() + xlab("Date/Time") + ylab(ylabel)
  #ggplot2::ggplot(grower_data, aes(x=c(rep(grower_data[[1]], 2)), y = c(grower_data[[4]], grower_data[[6]]))) + geom_point() + xlab("Date/Time") + ylab(ylabel)
  qplot(grower_data[[1]], grower_data[[4]])  
  
}