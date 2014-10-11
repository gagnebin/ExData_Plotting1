###
# Created on SAT Oct 11 12:19:45 2014
# Coursera:Exploratory Data Analysis, Peer Assessments, Project1
# Dataset: "https://d396qusza40orc.cloudfront.net/
# exdata%2Fdata%2Fhousehold_power_consumption.zip", downloaded on local computer
# @author: Gagnebin
###

# Loading the data

# We will only be using data from the dates 2007-02-01 and 2007-02-02. We read 
# the data from just those dates rather than reading in the entire dataset.

# Read data line by line and don't keep it in memory
# ** Please change the path to your own direct where the data are stored.**
f = file("./data/household_power_consumption.txt", "r")

# Read first the headers
line = readLines(f, n=1)
# Define the date we are looking for
d1 = "1/2/2007" # start date
d2 = "2/2/2007" # stop date
d3 = "3/2/2007" # date where we can stop reading the data

counter = 0 # count how many rows between d1 and d2
counter2 = 1 # index of rows (will not be used)

memo = TRUE # "flag" for index of d1
counter_memo=0 # index of d1 (will nto be used)

# Loop of the file to find how many rows are between d1 and d2
# Stop at the end of d2 or at the end of the file (if d2 is not find)
while(TRUE){
    line = readLines(f, n=1)
    counter2 = counter2 + 1
    if (substr(line, 1, 8) == d1 || substr(line, 1, 8) == d2){
        if (memo){
            counter_memo = counter2
            memo = FALSE
        }
        counter = counter + 1
    }
    if (substr(line, 1, 8) ==  d3 || length(line) == 0){
        break
    }
}

# Close the file
close(f)

# Fast reading command fread from data.table (load library)
library(data.table)

# First read the headers
tab2rows <- read.table("./data/household_power_consumption.txt", 
                       nrows = 2, sep=';', header= TRUE)

# Determine the classes of each columns
# Goal: speed up fread for large data sets
classes <- sapply(tab2rows, class)
# Save the headers
headers <- names(tab2rows)

# Load the data from d1 for counter+1 rows in a data table format
DT <- fread("./data/household_power_consumption.txt",
            sep=';',
            na.strings="?",
            skip = d1,
            nrows = counter+1
            )
# Add previously saved headers
setnames(DT, names(DT),headers)  

# Remove all data which are incomplete
good <- complete.cases(DT)
cleanDT <- DT[good, ][, ]

# Convert the Date variables to Date/Time classes
cleanDT$Date <- as.Date(cleanDT$Date, format='%d/%m/%Y', tz="EST")
# For convinience create a new column with Date and Time combined
cleanDT$DateTime <- paste(cleanDT$Date, cleanDT$Time)
z <- strptime(cleanDT$DateTime, "%Y-%m-%d %H:%M:%S", tz = "EST")

# Load library for base plotting
library(datasets)

################################################################################
################################# plot4 ########################################
################################################################################

# Note: The default Date/Time system on my computer is setup in French region.
# For this reason, when I plot days on x-axis the labels apprear in French
# (e.g. "jeu.", "ven.", "sam."). To avaoid to change the setup of my computer, 
# and to have the proper days label I will plot the graph without axis and 
# personalize them after.

# Open a plotting device png format at a specific size
png("plot4.png",  width = 480, height = 480, units = "px")

# Define plot matrix and margins
par(mfrow = c(2,2))
par(mar=c(5,4,4,2))

# Plot 1st plot (top left) (see Note above) 
plot(z, cleanDT$Global_active_power, type = "l",  xlab = "", 
     ylab = "Global Active Power", axes=F)
# Plot y axis
axis(2, cex.axis=1.0) # plot the y axis
# Define my x-axis labels
days <- c("Thu", "Fri", "Sat")
# Define the ticks
ticks <- seq(z[1], z[length(z)], by = "days")  
# Plot x axis
axis(1, at=ticks, labels=days, cex.axis=1.0)
# Draw the box around the graph
box()

# Plot 2nd plot (top right) (see Note above)
plot(z, cleanDT$Voltage, type = "l",  xlab = "datetime", ylab = "Voltage",
     axes=F)
# Plot y axis
axis(2, cex.axis=1.0) # plot the y axis
# Define my x-axis labels
days <- c("Thu", "Fri", "Sat")
# Define the ticks
ticks <- seq(z[1], z[length(z)], by = "days")  
# Plot x axis
axis(1, at=ticks, labels=days, cex.axis=1.0)
# Draw the box around the graph
box()

# Plot 3rd plot (bottom left) (see Note)
# Define y-axis limits
ylim1 <- max(cleanDT$Sub_metering_1)
ylim2 <- min(cleanDT$Sub_metering_1)
# Plot the main plot without axes
plot(z, cleanDT$Sub_metering_1, ylim=range(c(ylim1,ylim2)), axes=F, type = "l",  
     xlab = "", ylab = "Energy sub metering")
# Plot y axis
axis(2, cex.axis=1.0) # plot the y axis
# Define my x-axis labels
days <- c("Thu", "Fri", "Sat")
# Define the ticks
ticks <- seq(z[1], z[length(z)], by = "days")  
# Plot x axis
axis(1, at=ticks, labels=days, cex.axis=1.0)
# Draw the box around the graph
box()
# To plot the 2nd graph on the same windows
par(new=TRUE)
plot(z, cleanDT$Sub_metering_2, ylim=range(c(ylim1,ylim2)), axes=F, 
     type = "l",  col="red", xlab = "", ylab = "")
# To plot the 3rd graph on the same windows
par(new=TRUE)
plot(z, cleanDT$Sub_metering_3, ylim=range(c(ylim1,ylim2)), axes=F, 
     type = "l",  col="blue", xlab = "", ylab = "")
# Add legends
par(new=TRUE)
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", 
                              "Sub_metering_3"), lty=1, bty="n",
       col=c("black", "red", "blue"), cex=1.0)

# Plot 4rd plot (bottom right) (see Note)
plot(z, cleanDT$Global_reactive_power, type = "l",  xlab = "datetime", 
     ylab="Global_reactive_power", axes=F)
# Plot y axis
axis(2, cex.axis=1.0) # plot the y axis
# Define my x-axis labels
days <- c("Thu", "Fri", "Sat")
# Define the ticks
ticks <- seq(z[1], z[length(z)], by = "days")  
# Plot x axis
axis(1, at=ticks, labels=days, cex.axis=1.0)
# Draw the box around the graph
box()

# Close the PNG file device
dev.off() 