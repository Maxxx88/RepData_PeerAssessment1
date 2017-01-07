## -------------------------------------------
##
## Created by: Max
## Created date: 07/01/2016
## Description: Create report
##
##
## Modification date:
## Modify by: 
##
## -------------------------------------------

# Specific library to load
library(ggplot2)
library(plyr)

# Variables
fileName <- "./activity.csv"
zipFile  <- "./activity.zip"

# Extract data file 
unzip(zipFile)


# Read the file
mydata <- read.csv(fileName)
mydata$date = as.POSIXct(mydata$date)

# What is mean total number of steps taken per day?
step.day <- aggregate(mydata$steps, by = list(mydata$date), sum, na.rm=TRUE) 
names(step.day) <- c("Date", "steps")
# Make a histogram of the total number of steps taken each day
qplot(step.day$steps, geom = "histogram", binwidth = 400, 
      xlab = "Daily # steps", ylab = "Count", main = "Total # steps taken each day")
step.day.mean <- mean(step.day$steps) # 10766.19
step.day.median <- median(step.day$steps) # 10765

# What is the average daily activity pattern?
step.act <- aggregate(mydata$steps, by = list(mydata$interval), mean, na.rm=TRUE)
step.act.med <- aggregate(mydata$steps, by = list(mydata$interval), median, na.rm=TRUE)
step.act <- cbind(step.act[], step.act.med$x)
names(step.act) = c("interval","steps.mean", "steps.median")

step.act$steps.mean <- round(step.act$steps.mean)
step.act$steps.median <- round(step.act$steps.median)

ggplot(step.act, aes(x = interval, y = steps.mean)) + geom_line()

step.act.max <- step.act$interval[step.act$steps.mean == max(step.act$steps.mean) ] # 835

# Imputing missing values
num.NA <- sum(is.na(mydata$steps)) # 2304
# Replace NAs by the median of the period
step <- data.frame(date = mydata$date[is.na(mydata$steps)], 
                    interval = mydata$interval[is.na(mydata$steps)],
                    steps = step.act[match(step.act$interval, mydata$interval[is.na(mydata$steps)]), 3])
mydata2 <- subset(mydata, !is.na(steps))
mydata2 <- rbind(mydata2, step)

step.day2 <- aggregate(mydata2$steps, by = list(mydata2$date), sum, na.rm=TRUE)
names(step.day2) <- c("Date", "steps")

qplot(steps, data = step.day2, geom="histogram", xlab = "Daily Number of Steps", binwidth = 400)

step2.mean <- mean(step.day2$steps) # 9503.869
step2.median <- median(step.day2$steps) # 10395

# Create the Weekday/Weekend in new column
mydata2$week <- ifelse(weekdays(mydata2$date) == "Saturday" | weekdays(mydata2$date) == "Sunday" ,"weekend","weekday")

# Recalculate the mean and median for the new colum in step3
step3.mean <- aggregate(mydata2$steps, by = list(mydata2$week, mydata2$interval), mean, na.rm=TRUE)
step3.median <- aggregate(mydata2$steps, by = list(mydata2$week, mydata2$interval), median, na.rm=TRUE)
step3 <- cbind(step3.mean[], step3.median$x)
names(step3) = c("weekday", "interval","step.mean", "step.median")

# Create the graph
ggplot(step3, aes(x = interval, y = step.mean)) + ylab("# steps") + geom_line() + facet_grid(weekday~.)


# Clean all variables before end  
remove(list = ls())

## End