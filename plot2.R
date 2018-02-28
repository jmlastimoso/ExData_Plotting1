# Code for Plot 2

# Preliminary codes to start the process of creating the plot
# Read the dataset and assign to <household_power_consumption>
household_power_consumption <- read.csv("~/household_power_consumption.txt", 
                                        sep=";", stringsAsFactors=FALSE)
# Create a Date/Time column and assign to <When>,this will be useful later on
household_power_consumption$When = paste(household_power_consumption$Date, 
                                         household_power_consumption$Time,
                                         sep = " ")
# Convert <When> into a Time object using the strptime() function
household_power_consumption$When = strptime(household_power_consumption$When, 
                                            "%d/%m/%Y %H:%M:%S")
# Convert <When> as POSIXct in order to be filtered later on using the
# filter() function of the dplyr package
household_power_consumption$When = as.POSIXct(household_power_consumption$When)
# Convert the <Date> column as a Date object this will make the filtering process
# easier
household_power_consumption$Date = as.Date(household_power_consumption$Date,
                                           "%d/%m/%Y")

# Load the dplyr package
library(dplyr)
# Filter the data only from February 1-2, 2007 using the filter() function of the
# dplyr package and then assign to <power>
power = filter(household_power_consumption, 
               Date == "2007-02-01" | Date == "2007-02-02")

# Creating Plot 2 using the Base plotting system
plot2 = plot(power$When, power$Global_active_power, 
             type = "l", 
             xlab = "", 
             ylab = "Global Active Power (kilowatts)")
dev.copy(png, file = "plot2.png", 
         height = 480, width = 480)
dev.off()

# Creating Plot 2 using the ggplot2 package
library(ggplot2)
gplot2 = ggplot(power,
                aes(x = When, y = as.numeric(Global_active_power))) +
        geom_line() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black")) +
        ylab("Global Active Power (kilowatts") +
        xlab("") +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%a")
gplot2
ggsave("gplot2.png", 
       height = 4.8, width = 4.8, units = "in", dpi = 100)
