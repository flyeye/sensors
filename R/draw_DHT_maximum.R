draw_DHT_maximum <- function(start, end, path, parameter = "T")
{
    currentDate <- as.Date(start)
    endDate <- as.Date(end)
    filename <- paste0(path, as.character(currentDate), ".csv")
    pic_filename <- paste0(path, as.character(currentDate)," - ", as.character(endDate), " MAX ")
    
    dht_max <<- data.frame(Date = as.Date(as.character()), 
                          T1 = numeric(0), H1 = numeric(0), 
                          T2 = numeric(0), H2 = numeric(0),
                          T3 = numeric(0), H3 = numeric(0),
                          T4 = numeric(0), H4 = numeric(0),
                          T5 = numeric(0), H5 = numeric(0),
                          T6 = numeric(0), H6 = numeric(0))
    
    
    while (as.POSIXct(currentDate)<=as.POSIXct(endDate))
    {
        filename <- paste0(path, as.character(currentDate), ".csv")
        message(filename)
        if (file.exists(filename))
        {
            grower_data <- read.csv(filename, header = TRUE)
            d <- data.frame(Date = as.Date(currentDate), 
                            T1 = max(grower_data$Shelf.1.T.23.), H1 = max(grower_data$Shelf.1.H.23.), 
                            T2 = max(grower_data$Shelf.2.T.24.), H2 = max(grower_data$Shelf.2.H.24.),
                            T3 = max(grower_data$Shelf.3.T.25.), H3 = max(grower_data$Shelf.3.H.25.),
                            T4 = max(grower_data$Shelf.4.T.26.), H4 = max(grower_data$Shelf.4.H.26.),
                            T5 = max(grower_data$Shelf.5.T.27.), H5 = max(grower_data$Shelf.5.H.27.),
                            T6 = max(grower_data$Shelf.6.T.28.), H6 = max(grower_data$Shelf.6.H.28.))
            dht_max <<- rbind(dht_max, d)
        }
        currentDate <- as.Date(currentDate) + 1;
    }
    
    dht_max$Date <<- as.POSIXct(as.character(dht_max$Date), tz="GMT")
    
    message("Graph plotting...")
    g <- ggplot2::ggplot(dht_max, aes(x=dht_max[[1]])) + xlab("Time")
    g <- g + scale_x_datetime( breaks=date_breaks("2 days"), 
                               #minor_breaks=date_breaks("day"), 
                               labels=date_format("%m/%d"))
    
    if (length(grep("T", parameter, ignore.case = TRUE))>0 ) {
      g <- g + scale_y_continuous(breaks=seq(20,32,by=1), minor_breaks=seq(20,32,by=0.5), limits = c(20,32))  
      g <- g + ylab("Temperature") + 
        geom_line(aes(y = dht_max$T1, colour = "T1")) + 
        geom_line(aes(y = dht_max$T2, colour = "T2")) + 
        geom_line(aes(y = dht_max$T3, colour = "T3")) + 
        geom_line(aes(y = dht_max$T4, colour = "T4")) + 
        geom_line(aes(y = dht_max$T5, colour = "T5")) + 
        geom_line(aes(y = dht_max$T6, colour = "T6"))
      g <- g + ggtitle( paste("Air MAX temperature, ", as.Date(start), "-", as.Date(end)))  
      g <- g + scale_colour_manual("Sensors",  
                                   breaks = c("T1", "T2", "T3","T4", "T5", "T6"), 
                                   values = c("green", "blue", "red", "brown", "orange", "black")) 
      pic_filename <- paste0(pic_filename, "T")
    } 
    else 
      if (length(grep("H", parameter, ignore.case = TRUE))>0 ) {
        g <- g + scale_y_continuous(breaks=seq(0,100,by=10), minor_breaks=seq(0,100,by=5))  
        g <- g + ylab("Humidity") + 
          geom_line(aes(y = dht_max$H1, colour = "H1")) + 
          geom_line(aes(y = dht_max$H2, colour = "H2")) + 
          geom_line(aes(y = dht_max$H3, colour = "H3")) + 
          geom_line(aes(y = dht_max$H4, colour = "H4")) + 
          geom_line(aes(y = dht_max$H5, colour = "H5")) + 
          geom_line(aes(y = dht_max$H6, colour = "H6"))
        g <- g + ggtitle( paste("Air MAX humidity, ", as.Date(start), "-", as.Date(end)))  
        g <- g + scale_colour_manual("Sensors",  
                                     breaks = c("H1", "H2", "H3","H4", "H5", "H6"), 
                                     values = c("green", "blue", "red", "brown", "orange", "black")) 
        pic_filename <- paste0(pic_filename, "H")
      }
        ggsave(paste0(pic_filename,".png"), width = 10, height = 5)
    g
}