---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This is my analysis of activity monitoring data.

## Load the data

Fist, load the data and see the head of it.

```r
activity_data <- read.csv("activity.csv")
head(activity_data)
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


## What is mean total number of steps taken per day?

To analyse the data per day, calculate the sum of data on the basis of one day. 

```r
group_by_date <- aggregate(activity_data$steps, 
                           by=activity_data["date"],
                           sum)
group_by_date$date <- as.Date(group_by_date$date)
names(group_by_date) <- c("date","steps")
head(group_by_date)
```

```
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```


To analyse this question, plot the historgram between steps and date.

```r
hist(group_by_date$steps,
     col = rgb(0.8,0.1,0.1,0.6),
     xlab = "Numbers per day ",
     ylab = "Frequency",
     main = "Histogram of Total number of stpes taken each day",
     breaks =30)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
summary(group_by_date$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

According to the data shown above, the mean and median of the total number of steps taken per day is 10766 and 10765, respectively.

## What is the average daily activity pattern?

To analyse this problem, we make a time plot series of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis).


```r
group_by_interval <- aggregate(activity_data$steps,
                               by = activity_data["interval"],
                               sum, 
                               na.rm=TRUE)
names(group_by_interval) <- c("interval","steps")
group_by_interval$average_steps <- group_by_interval$steps / 61
with(group_by_interval, 
     plot(x = interval,
          y = average_steps, 
          type="l",
          col=3,
          xlab = "Time interval",
          ylab = "Average steps",
          main = "Averge number of steps across all day in 5-minute interval",
          xaxt="n")
     )
axis(1,
     at = seq(0,2360,by=100),
     las=2)
abline(v=group_by_interval$interval[which.max(group_by_interval$average_steps)],
       col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maximum <- group_by_interval$interval[which.max(group_by_interval$average_steps)]
maximum
```

```
## [1] 835
```

From the analysis above, we can conclude that the interval (830,835) contains the maximum number of steps.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA.). The presence of missing days may introduce bias into some calculations or summaries of the data.

Therefore, we would like to eliminate the bias caused by missing values.

First, calculate the total number of missing values in the dataset

```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```

From the code above, we can conclude that there are 2304 NA values in the dataset. 

Now we want to fill in all of the missing values in the dataset. 

We would like to replace NA by the mean . 


```r
new_group <- activity_data
new_group$steps[is.na(new_group$steps)] <- mean(new_group$steps, na.rm=TRUE)
head(new_group)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

```r
newgroup_by_date <- aggregate(new_group$steps, 
                             by=new_group["date"],
                             sum)
head(newgroup_by_date)
```

```
##         date        x
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
newgroup_by_date$date <- as.Date(newgroup_by_date$date)
names(newgroup_by_date) <- c("date","steps")
```

Now we get the dataset without missing values, let's plot the histogram and make the summary.

```r
hist(newgroup_by_date$steps,
     col = rgb(0.8,0.1,0.1,0.6),
     xlab = "Total number of steps per day",
     ylab = "Frequencu",
     main = "Histogram of Total number of stpes taken each day",
     breaks = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
summary(newgroup_by_date$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

From the data above, we can conclude that the mean and median are both 10766.

## Are there differences in activiy patterns between weekdays and weekends?

First, create a new factor variable in the dataset with two levels -- “weekday”and"weekend".


```r
library(ggplot2)
new_group$day <- weekdays(as.Date(new_group$date))
new_group$day[new_group$day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")] <- "weekday"
new_group$day[new_group$day %in% c("Saturday","Sunday")] <- "weekend"
mean_steps <- aggregate(new_group$steps, by=list(new_group$interval,new_group$day),mean)
head(new_group)
```

```
##     steps       date interval     day
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```

```r
head(mean_steps)
```

```
##   Group.1 Group.2        x
## 1       0 weekday 7.006569
## 2       5 weekday 5.384347
## 3      10 weekday 5.139902
## 4      15 weekday 5.162124
## 5      20 weekday 5.073235
## 6      25 weekday 6.295458
```

```r
names(mean_steps) <- c("interval","day","steps")
g <- ggplot(mean_steps, aes(x=interval, y=steps))
g +  geom_line(col="firebrick")+
     facet_grid(day~.)+
     labs(title="Time series plot of total number of steps between weekday and weekends")+
     labs(x="Interval",y="Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Above all, the analysis of activity monitoring data has finished. 


```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.6.3
```
