---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    fig_caption: yes
---



###Loading and preprocessing the data




```r
library(ggplot2)
data <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```



###What is mean total number of steps taken per day?###


```r
data1 <- aggregate(x = list(totsteps = data$steps), by = list(data$date), FUN = sum)
```



1. Histogram of the total number of steps taken each day

```r
qplot(totsteps, data = data1)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 



2. Calculate and report the mean and median total number of steps taken per day.

```r
mean(data1$totsteps, na.rm = TRUE)
```

```
## [1] 10766.19
```
Mean is 10766.19



```r
median(data1$totsteps, na.rm = TRUE)
```

```
## [1] 10765
```
And median is 10765


###What is the average daily activity pattern?

1. Time series plot of the 5-minute interval and the average number of steps taken.

```r
data2 <- aggregate(x = list(avsteps = data$steps), by = list(interval = data$interval),
                   FUN = mean, na.rm = TRUE)
qplot(x = interval, y = avsteps, data = data2, geom = "line", xlab = "5-minute interval",     
      ylab = "average number of steps") 
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
data2$interval[which.max(data2$avsteps)]
```

```
## [1] 835
```


###Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(subset(data, is.na(steps) | is.na(interval)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.

I'm going to fill in missing values by average number of steps taken for that 5-minute interval across all days.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#data2 contains average number of steps taken.
data3 <- data
for (i in 1:nrow(data)){
        if (is.na(data3$steps[i])){
            data3$steps[i] <- rep(data2$avsteps, 61)[i]
        }        
} 
```

4. Make a histogram of the total number of steps taken each day.


```r
data4 <- aggregate(x = list(totsteps = data3$steps), by = list(data3$date), FUN = sum)

qplot(totsteps, data = data4)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Calculate and report the mean and median total number of steps taken per day. 


```r
mean(data4$totsteps)
```

```
## [1] 10766.19
```

```r
median(data4$totsteps)
```

```
## [1] 10766.19
```

New mean is 10766.19
Median is also 10766.19

Do these values differ from the estimates from the first part of the assignment? 

Mean is the same, but median is slightly higher.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

There is no impact of imputing missing values on the estimates of daily number of steps.


###Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
data3$date <- strptime(data3$date, format = "%Y-%m-%d")
for (i in 1:nrow(data3)){
     if (weekdays(data3$date[i], abbreviate = T) == "Sut" | weekdays(data3$date[i], abbreviate = T) == "Sun"){
        data3$day[i] <- "weekend"
     } else {
        data3$day[i] <- "weekday"
       }
}

data3 <- transform(data3, day = factor(day))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
data5 <- aggregate(steps ~ interval + day, data = data3, FUN = mean)
qplot(interval, steps, data = data5, geom = "line", facets = day ~ .)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
