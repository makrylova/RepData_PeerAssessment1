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
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity["date"]<-as.Date(activity$date,"%Y-%m-%d")
```
##What is mean total number of steps taken per day?
Calculating the total number of steps taken per day

```r
total_steps<-aggregate(steps~date, data = activity, sum, na.rm=TRUE)
```
Histogram of the total number of steps taken each day

```r
hist(total_steps$steps, xlab="Number of Steps", ylab = "Number of Days",
main = "Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 





The mean and median of the total number of steps taken per day are

```r
mean(total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
A time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
average_steps<-aggregate(steps~interval, data = activity, mean)
str(average_steps)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
plot(average_steps$interval, average_steps$steps,type="l", xlab="Interval",
ylab = "Daily Average", main = "Average Number of Steps by Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 



5-minute interval that contains the maximum number of steps (on average across all the days in the dataset) is 835

```r
average_steps[which.max(average_steps$steps),"interval"]
```

```
## [1] 835
```

##Imputing missing values
The total number of missing values in the dataset is

```r
sum(is.na(activity))
```

```
## [1] 2304
```

We are going to substitute missing values by average for that interval

```r
activity_fixed<-transform(activity, steps = ifelse(is.na(activity$steps),
average_steps$steps, activity$steps))
```
Histogram of the total number of steps taken each day 

```r
total_steps_fixed<-aggregate(steps~date, data = activity_fixed, sum, na.rm=TRUE)
hist(total_steps_fixed$steps, xlab="Number of Steps", ylab = "Number of Days",
main = "Steps per Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 


The mean total number of steps taken per day is:

```r
mean(total_steps_fixed$steps)
```

```
## [1] 10766.19
```
The median total number of steps taken per day is:

```r
median(total_steps_fixed$steps)
```

```
## [1] 10766.19
```
Notice, the mean stays the same and the median increases after imputing missing values. Also, there was a significant increase in the number of days that fall into 10K-15K steps range (around the mean)

##Are there differences in activity patterns between weekdays and weekends?
Creating a new variable that indicates weekday/weekend

```r
activity_fixed$day<-weekdays(activity_fixed$date)
activity_fixed$weekend<- ifelse(activity_fixed$day=="Saturday" |
activity_fixed$day=="Sunday",  "weekend", "weekday")
```
Time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days

```r
g<-aggregate(steps~interval + weekend, data = activity_fixed, mean)
library("lattice")
xyplot(steps ~ interval | weekend, g, type = "l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 



