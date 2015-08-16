 

Reproducible Research: Peer Assessment 1

Sathish V

August 16, 2015


Loading and preprocessing data
 activity <- read.csv("activity.csv" )
head(activity)
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
library(lubridate)
activity$date <- ymd(activity$date)


What is mean total number of steps taken per day?

Calculate the total number of steps taken per day
stepsTotal <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

Make a histogram of the total number of steps taken each day
hist(stepsTotal$steps, main = "Total steps by day", xlab = "day", col = "blue")

 Calculate and report the mean and median of the total number of steps taken per day
mean(stepsTotal$steps)
## [1] 10766.19
median(stepsTotal$steps)
## [1] 10765


What is the average daily activity pattern?

Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

The plot is
plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "green")



Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- which.max(time_series)
names(max_interval)
## [1] "835"


Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
activity_NA <- sum(is.na(activity))
activity_NA
## [1] 2304

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval,etc
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}

Create a new dataset that is equal to the original dataset but with the missing data filled in.
new_activity <- activity
new_activity$steps <- fillNA

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
stepsTotal2 <-  aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)

The Histogram is
hist(stepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "green")

 And the mean and median is
mean(stepsTotal2$steps)
## [1] 10766.19
median(stepsTotal2$steps)
## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels weekday and weekend indicating whether a given date is a weekday or weekend day.
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")

Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
library(lattice)
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")


   
