draw_SoilMoisture <- function(start, end, path)
{
    currentDate <- as.Date(start)
    endDate <- as.Date(end)
    filename <- paste0(path, as.character(currentDate), ".csv")
    pic_filename <- paste0(path, as.character(currentDate)," - ", as.character(endDate), "SM.png")
    
    grower_data <<- read.csv(filename, header = TRUE)
    grower_data <<- grower_data[-1,]
    
    while (as.POSIXct(currentDate)<=as.POSIXct(endDate))
    {
      filename <- paste0(path, as.character(currentDate), ".csv")
      message(filename)
      grower_data_raw <- read.csv(filename, header = TRUE)
      message("rbinding...")
      grower_data <<- rbind(grower_data, grower_data_raw)
      currentDate <- as.Date(currentDate) + 1;
    }
    message("Time converting...")
    grower_data$Date.Time <<- as.POSIXct(as.character(grower_data$Date.Time), tz="GMT")
    
    
    message("Graph plotting...")
    g <- ggplot2::ggplot(grower_data, aes(x=grower_data[[1]])) + xlab("Time")
    g <- g + scale_x_datetime( breaks=date_breaks("day"), 
                               #minor_breaks=date_breaks("12 hours"), 
                               labels=date_format("%m/%d"))
    g <- g + scale_y_continuous(breaks=seq(0,5,by=1), minor_breaks=seq(0,5,by=0.5), limits = c(0, 5))  
    g <- g + ylab("Moisture") +
      geom_smooth(aes(y = grower_data$X11.10., colour = "SM11")) +
      geom_smooth(aes(y = grower_data$X12.11., colour = "SM12")) +
      geom_smooth(aes(y = grower_data$X21.12., colour = "SM21")) +
      geom_smooth(aes(y = grower_data$X22.13., colour = "SM22")) +
      geom_smooth(aes(y = grower_data$X31.14., colour = "SM31")) +
      geom_smooth(aes(y = grower_data$X32.15., colour = "SM32")) +
      geom_smooth(aes(y = grower_data$X41.16., colour = "SM41")) +
      geom_smooth(aes(y = grower_data$X42.17., colour = "SM42"))
    g <- g + ggtitle( paste("Soil Moisure, ", as.Date(start), "-", as.Date(end)))   
    g <- g + scale_colour_manual("Sensors",  
                                 #breaks = c("Lighter T", "Shelf 1", "Shelf 2","Shelf 3", "Shelf 4", "Shelf 5", "Shelf 6", "Reserved", "Fan"), 
                                 #values = c("green", "blue", "red", "brown", "orange", "grey", "black", "purple", "#FFCC00"))
                                 breaks = c("SM11", "SM12",  "SM21", "SM22", "SM31","SM32", "SM41", "SM42"), 
                                 values = c("blue", "green", "red", "purple", "black", "grey", "orange", "brown")) 
    ggsave(pic_filename, width = 10, height = 5)
    g
    
}