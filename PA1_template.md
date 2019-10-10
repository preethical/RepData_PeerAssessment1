---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
##Introduction


##Load all required packages

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.6.1
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

## Loading and preprocessing the data
- First the working directory is set.
-The file is then loaded and read

```r
setwd ("~/R/datascience_coursera_main/Reproducible Research/RepData_PeerAssessment1/RepData_PeerAssessment1/activity")

activity <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
```

-The date is then converted into the right format

```r
activity$date <- ymd(activity$date)
```

-next we inspect the dataset

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

- To calculate the mean total number of steps per day, we first use the aggregate function and remove missing values (NA's) as mentioned in the question. 


```r
steps_per_day <- aggregate(steps~date, data = activity, FUN = sum, na.rm=TRUE)

mean_steps <- as.integer(mean(steps_per_day$steps))

median_steps <- as.integer(median(steps_per_day$steps))

hist(steps_per_day$steps, xlab = "Steps per day", ylab = "frequency", 
     main = "Histogram of mean total steps/day", col= "blue")
abline(v=c(mean_steps,median_steps), col=c("black", "red"), lty=c(1,3), lwd=c(1, 3))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## What is the average daily activity pattern?

-To calculate the average daily pattern, we will first create a time-series graph with the 5 minute intervals on the x-axis and the average number of steps per day averaged across all days in the y-axis. 



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
