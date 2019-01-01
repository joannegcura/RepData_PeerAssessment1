---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
setwd("G:/Documents/q+_+p/data science 2018/Data Science/Module 5 Reproducible Data Science/proj 1")
activity <- read.csv("activity.csv", sep = ",")
```

Variable names and  structure


```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity[which(!is.na(activity$steps)), ])
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## What is mean total number of steps taken per day?


```r
library(reshape2)
activityMelt <- melt(activity[which(!is.na(activity$steps)), ], id.vars = c("date", "interval"))
head(activityMelt)
```

```
##         date interval variable value
## 1 2012-10-02        0    steps     0
## 2 2012-10-02        5    steps     0
## 3 2012-10-02       10    steps     0
## 4 2012-10-02       15    steps     0
## 5 2012-10-02       20    steps     0
## 6 2012-10-02       25    steps     0
```

```r
stepsSum <- dcast(activityMelt, date ~ variable, sum)
head(stepsSum)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Total number of steps per day


```r
summary(stepsSum$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

Histogram of the total number of steps taken each day showing mean and median


```r
hist(stepsSum$steps, main = "Histogram of Total Steps Taken Per Day",
     xlab = "Total Steps Per Day", ylab = "Number of Days", ylim = c(0,20),
     breaks = seq(0,25000, by=2500), col = "pink")
abline(v = mean(stepsSum$steps), lty = 1, lwd = 2, col = "yellow")
abline(v = median(stepsSum$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("yellow", "black"), lty = c(1, 2), lwd = c(2, 2))
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->

## What is the average daily activity pattern?

Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsMeanInterval <- dcast(activityMelt, interval ~ variable, mean, na.rm = TRUE)
head(stepsMeanInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(stepsMeanInterval$interval, stepsMeanInterval$steps, ty = "l", 
     xlab = "Time Interval", ylab = "Average Steps", main = "Ave. Steps Over All Days vs. Time Interval")
```

![](PA1_template_files/figure-html/time series plot-1.png)<!-- -->

The maximum number of steps


```r
maxStepsInterval <- stepsMeanInterval$interval[which.max(stepsMeanInterval$steps)]
maxStepsInterval
```

```
## [1] 835
```

## Imputing missing values
Total number of missing values in the dataset. 

- get a sense for the missing values
- replace the missing data for a day by the time average over all other days
- assume that the time intervals form a disjoint partitioning of 24 hours is erroneous
- the time interval for each day is approximately 40 hours, this refutes the intervals being disjoint


```r
activity2 <- split(activity, activity$interval)
activity2 <- lapply(activity2, function(x) {
        x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
        return(x)
})
activity2 <- do.call("rbind", activity2)
row.names(activity2) <- NULL

activity2 <- split(activity2, activity2$date)
df <- lapply(activity2, function(x) {
        x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
        return(x)
})
activity2 <- do.call("rbind", activity2)
row.names(activity2) <- NULL
head(activity2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
library(reshape2)
activityMelt2 <- melt(activity2, id.vars = c("date", "interval"))
stepsSum <- dcast(activityMelt2, date ~ variable, sum, na.rm = TRUE)
head(stepsSum)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

Histogram of the total number of steps taken each day with the imputed missing values


```r
hist(stepsSum$steps, main = "Histogram of Total Steps Taken Per Day", 
     xlab = "Total Steps Per Day", ylim = c(0,30), ylab = "Number of Days", 
     breaks = seq(0,25000, by=2500), col = "pink")
abline(v = mean(stepsSum$steps), lty = 1, lwd = 2, col = "yellow")
abline(v = median(stepsSum$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("yellow", "black"), lty = c(2, 1), lwd = c(2, 2))
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

Number of rows with NA values


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity$steps))*100/nrow(activity)
```

```
## [1] 13.11475
```

## Are there differences in activity patterns between weekdays and weekends?
Column describing if the date is a weekday or weekend


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
weekdays <- which(weekdays(as.Date(activity2$date)) != "Saturday" &
              weekdays(as.Date(activity2$date)) != "Sunday")
weekends <- which(weekdays(as.Date(activity2$date)) == "Saturday" |
              weekdays(as.Date(activity2$date)) == "Sunday")

temp <- c(rep("a", length(activity2)))
temp[weekdays] <- "weekday"
temp[weekends] <- "weekend"

length(temp)
```

```
## [1] 17568
```

```r
names(temp) <- "day"
activity2 <- cbind(activity2, temp)
names(activity2)[4] <- "day"
```

Steps taken over each interval averaged across weekday days and weekend days


```r
activity2split <- split(activity2, activity2$day)
stepsMeanInterval <- lapply(activity2split, function(x) {
        temp <- aggregate(x$steps, list(x$interval), mean)
        names(temp) <- c("interval", "steps")
        return(temp)
})

stepsMeanInterval <- do.call("rbind", stepsMeanInterval)

weekdays <- grep("weekday" ,row.names(stepsMeanInterval))
weekends <- grep("weekend" ,row.names(stepsMeanInterval))

temp <- c(rep("a", length(stepsMeanInterval$steps)))
temp[weekdays] <- "weekdays"
temp[weekends] <- "weekends"

stepsMeanInterval <- cbind(stepsMeanInterval, temp)
row.names(stepsMeanInterval) <- NULL
names(stepsMeanInterval)[3] <- "day"
head(stepsMeanInterval)
```

```
##   interval      steps      day
## 1        0 2.25115304 weekdays
## 2        5 0.44528302 weekdays
## 3       10 0.17316562 weekdays
## 4       15 0.19790356 weekdays
## 5       20 0.09895178 weekdays
## 6       25 1.59035639 weekdays
```

```r
tail(stepsMeanInterval)
```

```
##     interval       steps      day
## 571     2330  1.38797170 weekends
## 572     2335 11.58726415 weekends
## 573     2340  6.28773585 weekends
## 574     2345  1.70518868 weekends
## 575     2350  0.02830189 weekends
## 576     2355  0.13443396 weekends
```

```r
library(ggplot2)
ggplot(stepsMeanInterval, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) 
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Mean of steps taken over the weekdays and weekends


```r
stepsdatamelt <- melt(stepsMeanInterval, id.vars = c("interval", "day"))
dcast(stepsdatamelt, day ~ variable, mean)
```

```
##        day    steps
## 1 weekdays 35.61058
## 2 weekends 42.36640
```
