#'  Draw Lighter relay states
#'
#'  test
#'
#'  This is a function that draw Ligther relays states dutring the day.
#'
#'
#'
#' @param filename A character string giving path and filename where input data has been stored.
#'
#' @return A ggplot2 object
#' @export
draw_Lighter <- function (filename)
{
  grower_data <<- read.csv(filename, header = TRUE)
  pic_filename <- paste0(filename, "LT")
  grower_data$Date.Time <- as.POSIXct(as.character(grower_data$Date.Time), tz="GMT")
  grower_data$Relay.1.1. <- (-1*as.numeric(grower_data$Relay.1.1.) + 3)*5 + 5
  grower_data$Relay.2.2. <- (-1*as.numeric(grower_data$Relay.2.2.) + 3)*5 + 15
  grower_data$Relay.3.3. <- (-1*as.numeric(grower_data$Relay.3.3.) + 3)*5 + 25
  grower_data$Relay.4.4. <- (-1*as.numeric(grower_data$Relay.4.4.) + 3)*5 + 35
  grower_data$Relay.5.5. <- (-1*as.numeric(grower_data$Relay.5.5.) + 3)*5 + 45
  grower_data$Relay.6.6. <- (-1*as.numeric(grower_data$Relay.6.6.) + 3)*5 + 55
  grower_data$Reserved.7. <- (-1*as.numeric(grower_data$Reserved.7.) + 3)*5 + 65
  grower_data$Fan.8. <- (-1*as.numeric(grower_data$Fan.8.) + 3)*5 + 75
  g <- ggplot2::ggplot(grower_data, aes(x=grower_data[[1]])) + xlab("Time")
  g <- g + scale_x_datetime( breaks=date_breaks("2 hours"),
                             minor_breaks=date_breaks("1 hour"),
                             labels=date_format("%H:%M"))
  g <- g + scale_y_continuous(breaks=seq(10,90,by=5), minor_breaks=seq(10,90,by=2.5), limits = c(10, 90))
  g <- g + ylab("Temperature") +
    geom_line(aes(y = grower_data$X.9., colour = "Lighter T"), size = 1.5) +
    geom_line(aes(y = grower_data$Relay.1.1., colour = "Shelf 1")) +
    geom_line(aes(y = grower_data$Relay.2.2., colour = "Shelf 2")) +
    geom_line(aes(y = grower_data$Relay.3.3., colour = "Shelf 3")) +
    geom_line(aes(y = grower_data$Relay.4.4., colour = "Shelf 4")) +
    geom_line(aes(y = grower_data$Relay.5.5., colour = "Shelf 5")) +
    geom_line(aes(y = grower_data$Relay.6.6., colour = "Shelf 6")) +
    geom_line(aes(y = grower_data$Reserved.7., colour = "Reserved")) +
    geom_line(aes(y = grower_data$Fan.8., colour = "Fan"), size = 1.5)
  g <- g + ggtitle( paste("Lighter Data, ", as.Date(grower_data$Date.Time[1])))
  g <- g + scale_colour_manual("Lighter",
                               #breaks = c("Lighter T", "Shelf 1", "Shelf 2","Shelf 3", "Shelf 4", "Shelf 5", "Shelf 6", "Reserved", "Fan"),
                               #values = c("green", "blue", "red", "brown", "orange", "grey", "black", "purple", "#FFCC00"))
                               breaks = c("Lighter T", "Fan",  "Reserved", "Shelf 6", "Shelf 5","Shelf 4", "Shelf 3", "Shelf 2", "Shelf 1"),
                               values = c("blue", "green", "red", "#FFCC00", "purple", "black", "grey", "orange", "brown"))
  ggsave(paste0(pic_filename,".png"), width = 10, height = 5)
  g
}
