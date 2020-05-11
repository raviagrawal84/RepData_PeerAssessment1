---
title: 'Reproducible Reserch : Week 2 Assignment'
author: "Ravi Agrawal"
date: "5/10/2020"
output: html_document
         keep_md: true
---


#Introduction 

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken

## Data Setup

Use the read.csv to load the activity file in data frame


```r
df_activity <- read.csv("activity.csv")

print(paste0("Total Number of Observations are ",nrow(df_activity)," and Total Number of columns are ",ncol(df_activity)))
```

```
## [1] "Total Number of Observations are 17568 and Total Number of columns are 3"
```

## Analysis 1 : What is mean total number of steps taken per day?

  - Calculate the total steps per day


```r
#Applying tapply to calculate the sum of the steps by date
total_steps_per_day<-with(df_activity,tapply(steps,date,sum,na.rm=TRUE))

#Pivot down the value to show date as column
total_steps_per_day_pivot<-data.frame(date=names(total_steps_per_day),steps=total_steps_per_day)
```

  - Calculate the total steps per day

```r
par(mfrow=c(1,2),mar=c(4,4,4,4))

hist(total_steps_per_day_pivot$steps,xlab="Steps",main="Steps Frequency")

barplot(total_steps_per_day_pivot$steps,xlab="days",ylab="Steps",main="Steps per Day Bar Plot")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

  - Calculate the mean and median

```r
df_report<-c(Mean=mean(total_steps_per_day_pivot$steps),Median=median(total_steps_per_day_pivot$steps))

print(df_report)
```

```
##     Mean   Median 
##  9354.23 10395.00
```

## Analysis 2 : What is the average daily activity pattern?

  - Make Time Series Plot for Interval vs Avg Steps


```r
#Applying tapply to calculate the mean of the steps by interval
avg_steps_per_interval<-with(df_activity,tapply(steps,interval,mean,na.rm=TRUE))

#Pivot down the value to show date as column
avg_steps_per_interval_pivot<-data.frame(interval=names(avg_steps_per_interval),avg_steps=avg_steps_per_interval)

with(avg_steps_per_interval_pivot,plot(as.vector(interval),as.vector(avg_steps),type='l',main="Average Steps Per Inteval",xlab="Time Interval",ylab="avg steps"))
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

  - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  

```r
subset(avg_steps_per_interval_pivot,avg_steps==max(avg_steps))
```

```
##     interval avg_steps
## 835      835  206.1698
```

## Analysis 3 : Imputing missing values

  -Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(df_activity$steps))
```

```
## [1] 2304
```
  -Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated & Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(dplyr)
df_actvity_new<-df_activity%>%mutate(steps=ifelse(is.na(steps),mean(steps,na.rm=TRUE),steps))%>%select(steps,date,interval)

sum(is.na(df_actvity_new$steps))
```

```
## [1] 0
```
  
-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps


```r
total_steps_per_day_new<-with(df_actvity_new,tapply(steps,date,sum,na.rm=TRUE))

total_steps_per_day_pivot_new<-data.frame(date=names(total_steps_per_day_new),steps=total_steps_per_day_new)

par(mfrow=c(1,2),mar=c(4,4,4,4))

hist(total_steps_per_day_pivot_new$steps,xlab="Steps",main="Steps Frequency (New)")
hist(total_steps_per_day_pivot$steps,xlab="Steps",main="Steps Frequency(Original)")
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)



```r
c(Mean_orignal=mean(total_steps_per_day_pivot$steps),Median_orignal=median(total_steps_per_day_pivot$steps),Mean_new=mean(total_steps_per_day_pivot_new$steps),Median_new=median(total_steps_per_day_pivot_new$steps))
```

```
##   Mean_orignal Median_orignal       Mean_new     Median_new 
##        9354.23       10395.00       10766.19       10766.19
```
## Analysis 4 : Are there differences in activity patterns between weekdays and weekends?

  -Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day


```r
df_actvity_new_weekday<-df_actvity_new%>%mutate(day=as.factor(ifelse(weekdays(as.Date(date)) == "Sunday"|weekdays(as.Date(date)) =="Saturday",'Weekend','Weekday')))

summary(df_actvity_new_weekday)
```

```
##      steps                date          interval           day       
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Weekday:12960  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   Weekend: 4608  
##  Median :  0.00   2012-10-03:  288   Median :1177.5                  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5                  
##  3rd Qu.: 37.38   2012-10-05:  288   3rd Qu.:1766.2                  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0                  
##                   (Other)   :15840
```

  -Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
library(lattice)
xyplot(steps~interval|day,data=df_actvity_new_weekday,type="l",scales=list(y=list(relation="free")),layout=c(1,2))
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)



