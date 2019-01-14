## this is the code for all the questions
library(dplyr)

##first let's get the data
myData <- read.table(unz("activity.zip","activity.csv"), header = TRUE, sep = ",", na.strings = "NA", colClasses = c("integer", "Date", "integer"))
transf <- function(x) {paste(x %/% 100, x %% 100, sep = ":")}
myData$timeinterval <- transf(myData$interval)

##Q1: What is mean total number of steps taken per day?

##1: Make a histogram of the total number of steps taken each day
myDays <- group_by(myData, date)
myTotals <- summarise(myDays, steps = sum(steps, na.rm = TRUE))
hist(myTotals$steps, col = "green", xlab = "Total number of steps", main = "Histogram of total number of daily steps")

##2: Calculate and report the mean and median total number of steps taken per day
mean(myTotals$steps)
median(myTotals$steps)

##Q2: What is the average daily activity pattern?

##1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
myInterval <- group_by(myData, interval)
myAvg <- summarise(myInterval, steps = mean(steps, na.rm = TRUE))
plot(myAvg$interval, myAvg$steps, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per 5 minute interval", col = "red")

##2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
transf(subset(myAvg, steps == max(myAvg$steps))[1,1])

##Q3: Imputing missing values

##1: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(myData$steps))

##2: Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use the 
## mean/median for that day, or the mean for that 5-minute interval, etc.

myInterval <- group_by(myData, interval)
myInterval <- mutate(myInterval, average = mean(steps, na.rm = TRUE))

##3: Create a new dataset that is equal to the original dataset but with the
## missing data filled in.
newData <- myInterval
newData$steps[is.na(newData$steps)] <- newData$average[is.na(newData$steps)]


##4: Make a histogram of the total number of steps taken each day and Calculate
## and report the mean and median total number of steps taken per day. Do
## these values differ from the estimates from the first part of the assignment?
## What is the impact of imputing missing data on the estimates of the total
## daily number of steps?
newDays <- group_by(newData, date)
newTotals <- summarise(newDays, steps = sum(steps))
hist(newTotals$steps, col = "green", xlab = "Total number of steps", main = "Histogram of total number of daily steps")
mean(newTotals$steps)
median(newTotals$steps)

##Q4: Are there differences in activity patterns between weekdays and weekends?

##1: Create a new factor variable in the dataset with two levels – “weekday”
## and “weekend” indicating whether a given date is a weekday or weekend
## day.

wkdays <- function(x) {
        val <- weekdays(x)
        if (val %in% c("Saturday", "Sunday")) {
                res <- "weekend"
        } else {
                res <- "weekday"
        }
        res
}
newData$wkdays <- as.factor(sapply(newData$date, wkdays))

##2: Make a panel plot containing a time series plot (i.e. type = "l") of the
## 5-minute interval (x-axis) and the average number of steps taken, averaged
## across all weekday days or weekend days (y-axis).

myWkInterval <- group_by(newData, interval, wkdays)
myWkAvg <- summarize(myWkInterval, steps = mean(steps))

## par(mfrow = c(2, 1), mar = c(5, 4, 2, 1))
## with(subset(myWkAvg, wkdays == "Weekday"), plot(interval, steps, main = "Weekday", type = "l", col = "red"))
## with(subset(myWkAvg, wkdays == "Weekend"), plot(interval, steps, main = "Weekend", type = "l", col = "green"))

library(ggplot2)
g <- ggplot(myWkAvg, aes(interval, steps))
g + geom_line(color = "steelblue") + facet_grid(wkdays ~ .)







