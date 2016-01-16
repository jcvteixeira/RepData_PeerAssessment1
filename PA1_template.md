---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data=read.csv("data/activity.csv")
data$date=as.Date(data$date)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
totStepsDay = tapply(data$steps,data$date,sum,na.rm=T)
totStepsDay = as.data.frame.table(totStepsDay,stringsAsFactors = F)
colnames(totStepsDay) = c("date","steps")
```

2. Make a histogram of the total number of steps taken each day

```r
with(totStepsDay, hist(steps))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totStepsDay$steps,na.rm=T)
```

```
## [1] 9354.23
```

```r
median(totStepsDay$steps,na.rm=T)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgStepsInt = tapply(data$steps,data$interval,mean,na.rm=T)
avgStepsInt = as.data.frame.table(avgStepsInt,stringsAsFactors = F)
colnames(avgStepsInt) = c("interval","steps")
with(avgStepsInt, plot(steps ~ interval, type="l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
with(avgStepsInt, interval[which.max(steps)])
```

```
## [1] "835"
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# Adopted strategy for filling missing values of "steps" in data frame "data":
# replace NAs by the mean per 5-minute interval computed days with non-missing values
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data2=data
data2$steps[is.na(data$steps)]=rep(avgStepsInt$steps,nrow(totStepsDay))[is.na(data$steps)]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totStepsDay2 = tapply(data2$steps,data2$date,sum,na.rm=T)
totStepsDay2 = as.data.frame.table(totStepsDay2,stringsAsFactors = F)
colnames(totStepsDay2) = c("date","steps")
with(totStepsDay2, hist(steps))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
compareMeans=data.frame(row.names=c("mean","median"))
compareMeans$totStepsDay=with(totStepsDay, c(mean(steps),median(steps)))
compareMeans$totStepsDay2=with(totStepsDay2, c(mean(steps),median(steps)))
compareMeans
```

```
##        totStepsDay totStepsDay2
## mean       9354.23     10766.19
## median    10395.00     10766.19
```

**Observations:**  

* The mean and median of the total number of steps per day increased in the second dataset. Explanation: in the first dataset, days with missing values had zero steps, whereas in the second they have a positive number of steps.
* The mean and median are equal in the second dataset. This is a coincidence given the original data and the strategy for filling missing values. It was not to be expected since the distribution of the variable is not symmetric (which can be seen in the histogram above of totStepsDay2$steps).

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
Sys.setlocale("LC_TIME","English")
```

```
## [1] "English_United States.1252"
```

```r
w=weekdays(data2$date, abbreviate=T)
w2=w
w2[w=="Sat" | w=="Sun"]="weekend"
w2[w2!="weekend"]="weekday"
data2$weekday=factor(w2)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
# Compute mean number of steps per Interval and weekday type

# Alternative 1: ddply
#library(plyr)
#avgStepsInt2=ddply(data2,.(interval,weekday),summarize, steps=mean(steps))

# Alternative 2: aggregate
avgStepsInt2=aggregate(steps ~ weekday + interval, data=data2, mean)

# Make panel plot
par(mfrow = c(2, 1))
with(subset(avgStepsInt2, weekday=="weekday"), plot(steps ~ interval, col="blue", type="l", main="weekday"))
with(subset(avgStepsInt2, weekday=="weekend"), plot(steps ~ interval, col="blue", type="l", main="weekend"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
