draw_DHT_continius <- function(start, end, path, parameter = "T")
{
    currentDate <- as.Date(start)
    endDate <- as.Date(end)
    filename <- paste0(path, as.character(currentDate), ".csv")
    pic_filename <- paste0(path, as.character(currentDate)," - ", as.character(endDate))
    
    first <- TRUE;
    
    while (as.POSIXct(currentDate)<=as.POSIXct(endDate))
    {
        filename <- paste0(path, as.character(currentDate), ".csv")
        message(filename)
        if (file.exists(filename))
        {
            grower_data_raw <- read.csv(filename, header = TRUE)
            if (first){
                message("first data loading...")
                grower_data <<- grower_data_raw
                first <- FALSE
            } else {
                message("rbinding...")  
                grower_data <<- rbind(grower_data, grower_data_raw)
            }
        }
        
        currentDate <- as.Date(currentDate) + 1;
    }
    message("Time converting...")
    grower_data$Date.Time <<- as.POSIXct(as.character(grower_data$Date.Time), tz="GMT")
    
    
    message("Graph plotting...")
    g <- ggplot2::ggplot(grower_data, aes(x=grower_data[[1]])) + xlab("Time")
    g <- g + scale_x_datetime( breaks=date_breaks("2 days"), 
                               #minor_breaks=date_breaks("day"), 
                               labels=date_format("%m/%d"))
    
    if (length(grep("T", parameter, ignore.case = TRUE))>0 ) {
      g <- g + scale_y_continuous(breaks=seq(20,32,by=1), minor_breaks=seq(20,32,by=0.5), limits = c(20,32))  
      g <- g + ylab("Temperature") + 
        geom_smooth(aes(y = grower_data$Shelf.1.T.23., colour = "T1")) + 
        geom_smooth(aes(y = grower_data$Shelf.2.T.24., colour = "T2")) + 
        geom_smooth(aes(y = grower_data$Shelf.3.T.25., colour = "T3")) + 
        geom_smooth(aes(y = grower_data$Shelf.4.T.26., colour = "T4")) + 
        geom_smooth(aes(y = grower_data$Shelf.5.T.27., colour = "T5")) + 
        geom_smooth(aes(y = grower_data$Shelf.6.T.28., colour = "T6"))
      g <- g + ggtitle( paste("Air temperature, ", as.Date(start), "-", as.Date(end)))  
      g <- g + scale_colour_manual("Sensors",  
                                   breaks = c("T1", "T2", "T3","T4", "T5", "T6"), 
                                   values = c("green", "blue", "red", "brown", "orange", "black")) 
      pic_filename <- paste0(pic_filename, "T")
    } 
    else 
      if (length(grep("H", parameter, ignore.case = TRUE))>0 ) {
        g <- g + scale_y_continuous(breaks=seq(0,100,by=10), minor_breaks=seq(0,100,by=5))  
        g <- g + ylab("Humidity") + 
          geom_smooth(aes(y = grower_data$Shelf.1.H.23., colour = "H1")) + 
          geom_smooth(aes(y = grower_data$Shelf.2.H.24., colour = "H2")) + 
          geom_smooth(aes(y = grower_data$Shelf.3.H.25., colour = "H3")) + 
          geom_smooth(aes(y = grower_data$Shelf.4.H.26., colour = "H4")) + 
          geom_smooth(aes(y = grower_data$Shelf.5.H.27., colour = "H5")) + 
          geom_smooth(aes(y = grower_data$Shelf.6.H.28., colour = "H6"))
        g <- g + ggtitle( paste("Air humidity, ", as.Date(start), "-", as.Date(end)))  
        g <- g + scale_colour_manual("Sensors",  
                                     breaks = c("H1", "H2", "H3","H4", "H5", "H6"), 
                                     values = c("green", "blue", "red", "brown", "orange", "black")) 
        pic_filename <- paste0(pic_filename, "H")
      }
        ggsave(paste0(pic_filename,".png"), width = 10, height = 5)
    g
}