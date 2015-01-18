#Reproducible Research: Peer Assessment 1
Masha Pitiranggon

##Loading and preprocessing the data


```r
data <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

##What is mean total number of steps taken per day?

Aggregate the steps data by date:

```r
aggByDate <- aggregate(steps ~ date, data = data, FUN = sum)
```

Histogram of the total steps taken per day:

```r
hist(aggByDate$steps, main = "Histogram of Total Daily Steps", xlab = "Number of Steps" )
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Mean daily steps:

```r
mean(aggByDate$steps)
```

```
## [1] 10766.19
```

Median daily steps:

```r
median(aggByDate$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

Aggregate the steps data by interval:

```r
aggByInt <- aggregate(steps ~ interval, data = data, FUN = mean)
```

Time series plot of average steps by interval:

```r
plot(aggByInt$interval, aggByInt$steps, main = "Average Daily Activity Pattern",
     type = "l", xlab = "5-min Interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Which 5-min interval, on average across all days in the dataset contains the maximum number of steps?

```r
aggByInt[aggByInt$steps == max(aggByInt$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values

Total number of missing values in the dataset (total number of rows with NAs):

```r
sum(is.na(data))
```

```
## [1] 2304
```

Fill in missing steps data with average steps for that 5-minute interval:

```r
newdata <- data
for (i in 1:nrow(newdata)) {
    if (is.na(newdata$steps[i])) {
        newdata$steps[i] <- aggByInt[which(newdata$interval[i] == aggByInt$interval), ]$steps
    }
}
```

Histogram of total daily steps using new dataset:

```r
newdataAgg <- aggregate(steps ~ date, data = newdata, FUN = sum)
hist(newdataAgg$steps, main = "Histogram of Total Daily Steps", xlab = "Number of Steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

New mean:

```r
mean(newdataAgg$steps)
```

```
## [1] 10766.19
```

New median:

```r
median(newdataAgg$steps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  *The mean of the total daily number of steps is exactly the same after filling in missing values, however the median value is slightly higher after filling in missing values.*

##Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable with two levels - "weekday" and "weekend":

```r
newdata$weekday <- weekdays(newdata$date)
newdata$weekday <- ifelse(newdata$weekday == "Sunday" | newdata$weekday == "Saturday", "weekend", "weekday")
newdata$weekday <- factor(newdata$weekday)
```

Time series panel plot of average steps by interval (weekdays vs. weekends):

```r
weekdayAgg <- aggregate(steps ~ interval + weekday, data = newdata, FUN = mean)
library(lattice)
xyplot(steps ~ interval | weekday, data = weekdayAgg, layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

