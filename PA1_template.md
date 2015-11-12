---
title: "Reproducible Research - Peer Assessment 1"
date: "November 12, 2015"
output: html_document
---
##Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Loading and Preprocessing the Data

```r
activity<-read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
str(activity)
```

```
## Error in str(activity): object 'activity' not found
```

```r
activity["date"]<-as.Date(activity$date,"%Y-%m-%d")
```

```
## Error in as.Date(activity$date, "%Y-%m-%d"): object 'activity' not found
```
##What is mean total number of steps taken per day?
Calculating the total number of steps taken per day

```r
total_steps<-aggregate(steps~date, data = activity, sum, na.rm=TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```
Histogram of the total number of steps taken each day

```r
hist(total_steps$steps, xlab="Number of Steps", ylab = "Number of Days",
main = "Steps per Day")
```

```
## Error in hist(total_steps$steps, xlab = "Number of Steps", ylab = "Number of Days", : object 'total_steps' not found
```





The mean and median of the total number of steps taken per day are

```r
mean(total_steps$steps)
```

```
## Error in mean(total_steps$steps): object 'total_steps' not found
```

```r
median(total_steps$steps)
```

```
## Error in median(total_steps$steps): object 'total_steps' not found
```

##What is the average daily activity pattern?
A time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
average_steps<-aggregate(steps~interval, data = activity, mean)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
str(average_steps)
```

```
## Error in str(average_steps): object 'average_steps' not found
```

```r
plot(average_steps$interval, average_steps$steps,type="l", xlab="Interval",
ylab = "Daily Average", main = "Average Number of Steps by Interval")
```

```
## Error in plot(average_steps$interval, average_steps$steps, type = "l", : object 'average_steps' not found
```



5-minute interval that contains the maximum number of steps (on average across all the days in the dataset) is 835

```r
average_steps[which.max(average_steps$steps),"interval"]
```

```
## Error in eval(expr, envir, enclos): object 'average_steps' not found
```

##Imputing missing values
The total number of missing values in the dataset is

```r
sum(is.na(activity))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

We are going to substitute missing values by average for that interval

```r
activity_fixed<-transform(activity, steps = ifelse(is.na(activity$steps),
average_steps$steps, activity$steps))
```

```
## Error in transform(activity, steps = ifelse(is.na(activity$steps), average_steps$steps, : object 'activity' not found
```
Histogram of the total number of steps taken each day 

```r
total_steps_fixed<-aggregate(steps~date, data = activity_fixed, sum, na.rm=TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'activity_fixed' not found
```

```r
hist(total_steps_fixed$steps, xlab="Number of Steps", ylab = "Number of Days",
main = "Steps per Day")
```

```
## Error in hist(total_steps_fixed$steps, xlab = "Number of Steps", ylab = "Number of Days", : object 'total_steps_fixed' not found
```


The mean total number of steps taken per day is:

```r
mean(total_steps_fixed$steps)
```

```
## Error in mean(total_steps_fixed$steps): object 'total_steps_fixed' not found
```
The median total number of steps taken per day is:

```r
median(total_steps_fixed$steps)
```

```
## Error in median(total_steps_fixed$steps): object 'total_steps_fixed' not found
```
Notice, the mean stays the same and the median increases after imputing missing values. Also, there was a significant increase in the number of days that fall into 10K-15K steps range (around the mean)

##Are there differences in activity patterns between weekdays and weekends?
Creating a new variable that indicates weekday/weekend

```r
activity_fixed$day<-weekdays(activity_fixed$date)
```

```
## Error in weekdays(activity_fixed$date): object 'activity_fixed' not found
```

```r
activity_fixed$weekend<- ifelse(activity_fixed$day=="Saturday" |
activity_fixed$day=="Sunday",  "weekend", "weekday")
```

```
## Error in ifelse(activity_fixed$day == "Saturday" | activity_fixed$day == : object 'activity_fixed' not found
```
Time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days

```r
g<-aggregate(steps~interval + weekend, data = activity_fixed, mean)
```

```
## Error in eval(expr, envir, enclos): object 'activity_fixed' not found
```

```r
library("lattice")
xyplot(steps ~ interval | weekend, g, type = "l")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'g' not found
```



