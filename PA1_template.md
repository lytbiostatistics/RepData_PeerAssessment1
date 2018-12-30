---
title: 'Reproducible Research: Peer Assessment 1'
author: "Yuntian Liu"
date: "2018/12/28"
output: 
  html_document: 
    keep_md: yes
---



##1. get and clean the data
###1. prepare and load the data

```r
library('ggplot2')
library('plyr')
library('timeDate')
library('reshape2')
zipFile <- "repdata-data-activity.zip"
if (!file.exists("Data/activity.csv")) {
    dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(dataURL, zipFile, mode = "wb")
    unzip(zipFile, files = NULL, exdir = "Data",  unzip = "internal")
    file.remove(zipFile)
}
dirName <- 'Data'
fileName = "activity.csv"
fileNameActivity <- file.path(dirName, fileName)
data <- read.csv(file = fileNameActivity, header = TRUE, colClasses = c("numeric", "Date", "numeric"))
head(data)
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

###2. Preprocessing the data

```r
# add the weekday to the dataset
data$weekday <- weekdays(data$date)
# create a copy of data set with NA rows
data.WithNA <- data
# remove all the rows with 'NA'
data.withoutNA <- data[complete.cases(data),]
head(data.WithNA)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0  星期一
## 2    NA 2012-10-01        5  星期一
## 3    NA 2012-10-01       10  星期一
## 4    NA 2012-10-01       15  星期一
## 5    NA 2012-10-01       20  星期一
## 6    NA 2012-10-01       25  星期一
```

##2 What is mean toal number of steps taken per day?  

###1. Calculate the daily steps

```r
sum.steps.day <- ddply(data.withoutNA, .(date), summarise, steps = sum(steps,na.rm = TRUE))
head(sum.steps.day)
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

###2. Plotting histogram of the daily steps

```r
hist(sum.steps.day$steps, main="Histogram of daily steps", 
    xlab="Number of steps", 
    border="black", 
    col="grey",
    breaks=10)
    
    abline(v=median(sum.steps.day$steps), col = "green", lwd=2)
    abline(v=mean(sum.steps.day$steps), col = "red",lwd=2)
```

![](PA1_template_files/figure-html/plotting histogram-1.png)<!-- -->

###3.Calculate the mean and median of the daily steps

```r
paste("Mean steps per Day =", round(mean(sum.steps.day$steps, na.rm = TRUE), 0))
```

```
## [1] "Mean steps per Day = 10766"
```

```r
paste("Median steps per Day =", round(median(sum.steps.day$steps, na.rm = TRUE), 0))
```

```
## [1] "Median steps per Day = 10765"
```

##3. What is the average daily activity pattern?
###1. Calculate the number of steps per interval

```r
mean.steps.interval <- ddply(data.withoutNA, .(interval), summarise, steps = mean(steps, na.rm = TRUE))
head(mean.steps.interval)
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

###2. Plotting the average daily activity pattern by interval

```r
ggplot(mean.steps.interval, aes(interval, steps)) + geom_line() + xlab("Interval") + ylab("Average steps per interval") + ggtitle("Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/plotting the average daily pattern by interval-1.png)<!-- -->

###3. Maximum number of steps on 5-minute interval on average among all the days

```r
paste("Maximum number of steps in interval =", mean.steps.interval$interval[which.max(mean.steps.interval$steps)])
```

```
## [1] "Maximum number of steps in interval = 835"
```

##4. Imputing missing values
###1. Calculate the total number of missing values(NA)

```r
sum(is.na(data.WithNA$steps))
```

```
## [1] 2304
```
###2. Devise a strategy for filling in all of the missing values
Ideally, we calculate the mean number of steps per 5-minute interval

```r
# Calculate the mean value per day and interval.
mean.weekday <- ddply(data.WithNA, .(interval, weekday), summarise, steps = round(mean(steps, na.rm = TRUE), 2))
# Get list of indices where steps value = NA
naIndex = which(is.na(data.WithNA$steps))
# Merge dataset 'data.WithNA' with dataset mean.steps.interval 
merged.NA = merge(data.WithNA, mean.steps.interval, by = "interval", suffixes = c(".actual", ".stepsInt"))
data.Complete <- data.WithNA
# Replace NA values with value by steps
data.Complete[naIndex, "steps"] <- merged.NA[naIndex, 'steps.stepsInt']
# Ascertain if all the NA values have been replaced
paste("Missing values in new dataset =", sum(is.na(data.Complete)))
```

```
## [1] "Missing values in new dataset = 0"
```

```r
# Calculate total number of steps per day  
steps.day <- ddply(data.Complete, .(date), summarise, steps = round(sum(steps, na.rm = TRUE),0))
head(steps.day)
```

```
##         date steps
## 1 2012-10-01   146
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

###3 create a new dataset equals to the original dataset but with the missing data filled in.  

```r
hist(steps.day$steps, main="Histogram of daily steps (NAs repaced)", 
    xlab="Number of steps", 
    border="black", 
    col="grey",
    breaks=10)
    
    abline(v=median(steps.day$steps), col = "green", lwd=2)
    abline(v=mean(steps.day$steps), col = "red",lwd=2)
```

![](PA1_template_files/figure-html/plotting histogram for new dataset-1.png)<!-- -->

###4. Calculate the mean and the median of new dataset  

```r
# Calculate total number of steps per day  
sum.steps.day <- ddply(data.Complete, .(date), summarise, steps = sum(steps))
paste("Mean steps per Day =", round(mean(sum.steps.day$steps), 0)) 
```

```
## [1] "Mean steps per Day = 10890"
```

```r
paste("Median steps per Day =", round(median(sum.steps.day$steps), 0))
```

```
## [1] "Median steps per Day = 11015"
```

##5. Differences in activity patterns between weekdays and weekends
###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# Evaluate wether date is weekday or weekend
data.Complete$daytype <- lapply(data.Complete$date, function(x) ifelse(isWeekday(x, wday = 1:5), 'weekday', 'weekend'))
# flatten list to vector
data.Complete$daytype <- unlist(data.Complete$daytype, use.names = TRUE)
# Create Factor variable
data.Complete$daytype <- as.factor(data.Complete$daytype)
str(data.Complete)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  1.72 1.72 1.72 1.72 1.72 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekday : chr  "星期一" "星期一" "星期一" "星期一" ...
##  $ daytype : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
head(data.Complete)
```

```
##      steps       date interval weekday daytype
## 1 1.716981 2012-10-01        0  星期一 weekday
## 2 1.716981 2012-10-01        5  星期一 weekday
## 3 1.716981 2012-10-01       10  星期一 weekday
## 4 1.716981 2012-10-01       15  星期一 weekday
## 5 1.716981 2012-10-01       20  星期一 weekday
## 6 1.716981 2012-10-01       25  星期一 weekday
```
###2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days

```r
# Calculate the 5 - minute interval and the average number of steps taken on weekdays and weekends
day.interval.Steps <- ddply(data.Complete, .(interval, daytype), summarise, steps = mean(steps, na.rm = TRUE))
# plotting the timw series panel plot
ggplot(day.interval.Steps, aes(x = interval, y = steps)) +
    geom_line(aes(col=daytype))+
    ylab('Number of steps') + xlab("Interval") +
    ggtitle("Number of Steps per Interval by daytype (weekend/weekend)") +
    facet_grid(daytype ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
