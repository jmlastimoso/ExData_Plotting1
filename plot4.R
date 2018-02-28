# Code for Plot 3

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

# Creating Plot 4 using the Base plotting system
# Create a blank canvass with 4 divisions
par(mfrow = c(2,2),
    mar = c(4.1,4.1,2,2.1))
# Plot Top Left
hist(as.numeric(power$Global_active_power), 
     col = "red", 
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)")
# Plot Top Right
plot(x = power$When, y = power$Voltage,
     type = "l",
     xlab = "datetime",
     ylab = "Voltage")
# Plot Bottom Left
plot(x = power$When, y = power$Sub_metering_1, 
     type = "n",
     xlab = "",
     ylab = "Energy sub metering") 
        lines(x = power$When, y = power$Sub_metering_1, 
              type = "l") 
        lines(x = power$When, y = power$Sub_metering_2, 
              type = "l", 
              col = "Red")
        lines(x = power$When, y = power$Sub_metering_3, 
              type = "l", 
              col = "Blue")
                legend("topright",
                       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                       lty = 1,
                       col = c("black", "red", "blue"),
                       cex = 0.8,
                       bty = "n",
                       y.intersp = 0.5,
                       x.intersp = 0.1)
# Plot Bottom Right
plot(x = power$When, y = power$Global_reactive_power,
     type = "l",
     ylab = "Global Reactive Power",
     xlab = "datetime")
dev.copy(png, file = "plot4.png", 
         height = 480, width = 480)
dev.off()

# Creating Plot 4 using the ggplot2 package
library(ggplot2)
# Plot Top Left
plotTL = ggplot(power, 
                aes(as.numeric(Global_active_power))) +
         geom_histogram(bins = 23,
                        fill = "red",
                        col = "black") +
         ggtitle("Global Active Power") +
         theme(plot.title = element_text(hjust = 0.5),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
         xlab("Global Active Power (kilowatts") +
         ylab("Frequency")
# Plot Top Right
plotTR = ggplot(power,
                aes(x = When, y = as.numeric(Voltage))) +
         geom_line() +
         theme(plot.title = element_text(hjust = 0.5),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
         ylab("Voltage") +
         xlab("datetime") +
         scale_x_datetime(date_breaks = "1 day", date_labels = "%a")
# Plot Bottom Left
plotBL = ggplot() + 
         geom_line(data = power, 
                   aes(x = When, y = as.numeric(Sub_metering_1), 
                       color = "Sub_metering_1")) +
         geom_line(data = power, 
                   aes(x = When, y = as.numeric(Sub_metering_2), 
                       color = "Sub_metering_2")) +
         geom_line(data = power, 
                   aes(x = When, y = as.numeric(Sub_metering_3), 
                       color = "Sub_metering_3")) +
         theme(plot.title = element_text(hjust = 0.5),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.title = element_blank(),
               legend.key = element_rect(fill = "white",
                                         color = "white"),
               legend.justification = "top",
               legend.position = c(.75, .95),
               legend.key.size = unit(0.3, "cm"),
               legend.text = element_text(size = 7),
               legend.margin = margin(0,0,0,0)) +
         scale_color_manual(values = c("#000000", 
                                       "#f45342", 
                                       "#41bbf4"),
                            labels = c("Sub_metering_1", 
                                       "Sub_metering_2", 
                                       "Sub_metering_3")) +
         ylab("Energy sub metering") +
         xlab("") +
         scale_x_datetime(date_breaks = "1 day", date_labels = "%a")
# Plot Bottom Right
plotBR = ggplot(power,
                aes(x = When, y = as.numeric(Global_reactive_power))) +
         geom_line() +
         theme(plot.title = element_text(hjust = 0.5),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
         ylab("Global Reactive Power") +
         xlab("datetime") +
         scale_x_datetime(date_breaks = "1 day", date_labels = "%a")
# Use the plot_grid() function in the cowplot package to arrange them in a grid
library(ggpubr)
gplot4 = ggarrange(plotTL, plotTR, plotBL, plotBR, ncol = 2, nrow = 2)
gplot4
ggsave("plot4(using_ggplot2).png", 
       height = 4.8, width = 4.8, units = "in", dpi = 100)

















