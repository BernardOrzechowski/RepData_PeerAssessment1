# Reproducible Research: Peer Assessment 1

I will use the ggplot2 package throughout this assignment.

## Loading and preprocessing the data

**Step 1. Load the data**

First we load the data 

```r
data <- read.csv(file = "activity.csv", colClasses = c("numeric", "character", 
    "numeric"), header = TRUE, sep = ",", row.names = NULL, na.strings = "NA", 
    stringsAsFactors = TRUE)
```


**Step 2. Process/transform the data (if necessary) into a format suitable for your analysis**

I create a data frame with the sums of steps taken per day

```r
dayAggregates <- aggregate(data[, c("steps")], by = list(data$date), "sum")
names(dayAggregates) <- c("day", "sumOfSteps")
head(dayAggregates)
```

```
##          day sumOfSteps
## 1 2012-10-01         NA
## 2 2012-10-02        126
## 3 2012-10-03      11352
## 4 2012-10-04      12116
## 5 2012-10-05      13294
## 6 2012-10-06      15420
```


Also we will nedd a 5-minute interval and the average number of steps taken, averaged across all days 


```r
intervalAverages <- aggregate(data[, c("steps")], by = list(data$interval), 
    FUN = function(x) mean(x, na.rm = TRUE))
names(intervalAverages) <- c("interval", "averageNumberSteps")
head(intervalAverages)
```

```
##   interval averageNumberSteps
## 1        0            1.71698
## 2        5            0.33962
## 3       10            0.13208
## 4       15            0.15094
## 5       20            0.07547
## 6       25            2.09434
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

**Step 1. Make a histogram of the total number of steps taken each day**
   


```r
library(ggplot2)

qplot(sumOfSteps, data = dayAggregates, xlab = "number of steps taken each day", 
    ylab = "count", main = "Histogram of the total number of steps taken each day ")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk makeHistogram](figure/makeHistogram.png) 

   
**Step 2. Calculate and report the mean and median total number of steps taken per day**
   
   Let us first compute the mean of steps taken per day (NA values ae removed before the calculation is done)

```r
mean(dayAggregates$sumOfSteps, na.rm = TRUE)
```

```
## [1] 10766
```

   Then median of steps taken per day (NA values ae removed before the calculation is done)

```r
median(dayAggregates$sumOfSteps, na.rm = TRUE)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

   **1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**
  
  We will use the dataFrame intervalAverages("interval","averageNumberSteps") that contains the computed data.
  

```r
library(ggplot2)

g <- ggplot(intervalAverages, aes(x = interval, y = averageNumberSteps)) + geom_line()
g <- g + labs(title = "5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)")
g <- g + labs(x = "intervals")
g <- g + labs(y = "average number of steps across all days")
print(g)
```

![plot of chunk timeSeries](figure/timeSeries.png) 

   
   **2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**
   

```r
subset(intervalAverages, intervalAverages$averageNumberSteps == max(intervalAverages$averageNumberStep, 
    na.rm = TRUE))
```

```
##     interval averageNumberSteps
## 104      835              206.2
```





## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

