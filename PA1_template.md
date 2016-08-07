---
Reproducible Research: Peer Assessment 1
======================================================
This assignment makes use of data from a personal activity monitoring device. This device (Fitbit, Nike Fuelband, or 
Jawbone Up) collects data at 5 minute intervals through out the day. The data consists of two months of data from 
an anonymous individual collected during the months of October and November, 2012 and include the number of steps 
taken in 5 minute intervals each day.

The variables included in this dataset are:
-steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
-date: The date on which the measurement was taken in YYYY-MM-DD format
-interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

```r
knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path="figure\\")
```
# Load the data from activity.csv file and prepare for processing. 

```r
my_activity <- read.csv(".\\activity.csv")
```

#Summary for the data set


```r
summary(my_activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day with the NA values


```r
my_activity$steps <- as.numeric(my_activity$steps)
#my_activity[is.na(my_activity)] <- 0
steps_daily <- aggregate(my_activity$steps, by=list(my_activity$date), sum)
names(steps_daily)[1] <- "date"
names(steps_daily)[2] <- "steps"

steps_daily
```

```
##          date steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

##Histogram of daily steps
2. A histogram of the total number of steps taken each day


```r
hist(steps_daily$steps, main = "Total Number of Steps Per Day", 
    xlab = "Total Number of Steps Per Day", ylab = "Frequency", 
    col = "blue")
```

![](figure\Total_Steps-1.png)<!-- -->

##Mean and Median of the total number of steps taken per day


```r
#Mean calculation
mean_steps <- mean(steps_daily$steps, na.rm = TRUE)
mean_steps
```

```
## [1] 10766.19
```

```r
#Median calculation
median_steps <- median(steps_daily$steps, na.rm = TRUE)
median_steps
```

```
## [1] 10765
```

##What is the average daily activity pattern?

1. A time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
averaged across all days (y-axis).


```r
time_interval_data <- aggregate(steps ~ interval, data = my_activity, FUN = function(x) {
    mean(x, na.rm = TRUE)
})

plot(time_interval_data, type = "l", xlab = "5 minute interval", ylab = "Average number of steps taken across all days", 
     main = "Average Daily Activity Pattern")
```

![](figure\Daily_Average-1.png)<!-- -->

2.The 5-minute interval, containing the maximum number of steps on average across all the days in the dataset.


```r
#The 5-minute interval with the most steps accross all days
max_interval <- time_interval_data$interval[which.max(time_interval_data$steps)]
max_interval
```

```
## [1] 835
```

```r
#The masimum number of steps on average across all the days
max_steps <- max(time_interval_data$steps)
max_steps
```

```
## [1] 206.1698
```
## Imputing missing values
1. The total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing_values <- sum(is.na(my_activity))
missing_values
```

```
## [1] 2304
```
2. Replace missing values with the average of steps per 5 minute interval

```r
my_temp_activity <- cbind(my_activity, time_interval_data[,2])
names(my_temp_activity)[4] <- "mean"
head(my_temp_activity)
```

```
##   steps       date interval      mean
## 1    NA 2012-10-01        0 1.7169811
## 2    NA 2012-10-01        5 0.3396226
## 3    NA 2012-10-01       10 0.1320755
## 4    NA 2012-10-01       15 0.1509434
## 5    NA 2012-10-01       20 0.0754717
## 6    NA 2012-10-01       25 2.0943396
```

```r
my_temp_activity$steps <- ifelse(is.na(my_temp_activity$steps), my_temp_activity$mean, my_temp_activity$steps)
head(my_temp_activity)
```

```
##       steps       date interval      mean
## 1 1.7169811 2012-10-01        0 1.7169811
## 2 0.3396226 2012-10-01        5 0.3396226
## 3 0.1320755 2012-10-01       10 0.1320755
## 4 0.1509434 2012-10-01       15 0.1509434
## 5 0.0754717 2012-10-01       20 0.0754717
## 6 2.0943396 2012-10-01       25 2.0943396
```

```r
tail(my_temp_activity)
```

```
##           steps       date interval      mean
## 17563 2.6037736 2012-11-30     2330 2.6037736
## 17564 4.6981132 2012-11-30     2335 4.6981132
## 17565 3.3018868 2012-11-30     2340 3.3018868
## 17566 0.6415094 2012-11-30     2345 0.6415094
## 17567 0.2264151 2012-11-30     2350 0.2264151
## 17568 1.0754717 2012-11-30     2355 1.0754717
```

3. New activity data set without the NAs and the mean value column

```r
my_new_activity <- my_temp_activity[,1:3]
head(my_new_activity)
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
4. Mean and median total number of steps taken per day calculated for the new data set

```r
#new_steps_daily <- aggregate(steps ~ date, data = my_new_activity, sum)
new_steps_daily <- aggregate(my_new_activity$steps, by=list(my_new_activity$date), FUN=sum)
names(new_steps_daily)[1] <- "date"
names(new_steps_daily)[2] <- "steps"
new_steps_daily
```

```
##          date    steps
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01 10766.19
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 10766.19
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 10766.19
## 41 2012-11-10 10766.19
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 10766.19
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 10766.19
```

```r
#Mean calculation
new_mean_steps <- mean(new_steps_daily$steps)
new_mean_steps
```

```
## [1] 10766.19
```

```r
#Median calculation
new_median_steps <- median(new_steps_daily$steps)
new_median_steps
```

```
## [1] 10766.19
```
Histogram of the total number of steps taken each day


```r
hist(new_steps_daily$steps, main = "Total Number of Steps Per Day", 
    xlab = "Total Number of Steps Per Day", ylab = "Frequency", 
    col = "gray")
```

![](figure\Total Steps 2-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
1. New data set with weekday and weekend information


```r
day_type <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}

#Add a column to the new data set
my_new_activity$day_type <- as.factor(sapply(my_new_activity$date, day_type))
head(my_new_activity)
```

```
##       steps       date interval day_type
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```
2. A panel plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
new_time_interval_data <- aggregate(steps ~ interval + day_type, data = my_new_activity, mean)

ggplot(new_time_interval_data, aes(interval, steps)) + geom_line() + facet_grid(. ~ day_type ) + 
    xlab("5-minute interval") + ylab("Average number of steps taken") + ggtitle("Average Daily Activity Pattern")
```

![](figure\Weekend-Weekday-1.png)<!-- -->
