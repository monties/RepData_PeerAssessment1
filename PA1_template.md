# Reproducible Research: Peer Assessment 1

===============================================================================  

# Introduction
The data is from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The dataset is downloaded from the [course web site](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

# Loading and preprocessing the data
- Load the data and see its structure.  

```r
act = read.csv("activity.csv")
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(act)
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
- We find there are many NAs. So we take out NAs for this stage and make a sub dataset withou NAs.  

```r
act.clean = act[which(!is.na(act$steps)),]
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(act)
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

# What is mean total number of steps taken per day?
- We first calculate the total number of steps taken per day.  

```r
ttl.perday = aggregate(steps ~ date, data = act.clean, sum)
```
- Then we make a histogram showing the total number of steps take each day. Most of the total steps taking are between 10,000 and 15,000.  

```r
hist(ttl.perday$steps, xlab = "total steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

- Last we calculate the mean and mdian of the total number of steps take per day.  

```r
mean(ttl.perday$steps)
```

```
## [1] 10766.19
```

```r
median(ttl.perday$steps)
```

```
## [1] 10765
```

# What is the average daily activity pattern?
- We make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg.perday = aggregate(steps ~ interval, data = act.clean, mean)
library(ggplot2)
g <- ggplot(avg.perday, aes(interval, steps)) + geom_line() + ylab("average steps taken")
g
```

![](PA1_template_files/figure-html/calculate average steps within each interval across all days-1.png) 

- On the chart above, we can see there is a peak interval for steps taken. Let's find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.  


```r
library(ggplot2)
g <- ggplot(avg.perday, aes(interval, steps)) + geom_line() + ylab("average steps taken")
g + geom_point(data = subset(avg.perday, steps == max(steps)), aes(interval, steps),
               shape = "O", col = "red", size = 10) + 
    geom_text(data = subset(avg.perday, steps == max(steps)),
              aes(interval, steps, label = paste("(",interval,", ",round(steps,2),")")),
              vjust = -0.1, hjust = -0.1, col = "red")
```

![](PA1_template_files/figure-html/find out the peak interval-1.png) 

# Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

So we first calculate total number of missing values in the dataset (i.e. the total number of rows with NAs)  


```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

Then we create a new dataset that is equal to the original dataset but with the mean for that 5-minute interval filling in the missing data.


```r
act.fill = act
act.fill$temp <- avg.perday$steps
for (i in 1:nrow(act.fill)) {
    if (is.na(act.fill[i,"steps"])) {
        act.fill[i,"steps"] = act.fill[i,"temp"]
        }
}
```

Now we can make a histogram of the total number of steps taken each day and calculate its mean and median values.  

- To see if these values differ from the estimates from the first part above.


```r
ttl.perday_fill = aggregate(steps ~ date, data = act.fill, sum)
hist(ttl.perday_fill$steps, xlab = "total steps taken per day")
```

![](PA1_template_files/figure-html/mean and median of new dataset-1.png) 

```r
mean(ttl.perday_fill$steps)
```

```
## [1] 10766.19
```

```r
median(ttl.perday_fill$steps)
```

```
## [1] 10766.19
```

- To find the impact of imputing missing data on the estimates of the total daily number of steps.


```r
mean(ttl.perday_fill$steps) / mean(ttl.perday$steps)
```

```
## [1] 1
```

```r
median(ttl.perday_fill$steps) / median(ttl.perday$steps)
```

```
## [1] 1.00011
```

# Are there differences in activity patterns between weekdays and weekends?
Now we use the dataset with the filled-in missing values for this part.  
First step is to create a new factor variable in the dataset with two levels ("weekday" and "weekend") indicating whether a given date is a weekday or weekend.  


```r
act.fill$date <- as.POSIXct(strptime(act.fill$date, "%Y-%m-%d"))
act.fill$wkday <- weekdays(act.fill$date)
act.fill$isweekend <- ifelse(act.fill$wkday %in% c("星期六", "星期日"), "weekend", "weekday")
act.fill$isweekend <- as.factor(act.fill$isweekend)
```

Then we make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avg.perday_fill = aggregate(steps ~ interval + isweekend, data = act.fill, mean)
library(ggplot2)
g <- ggplot(avg.perday_fill, aes(interval, steps)) + geom_line() + ylab("average steps taken")
g + facet_grid(isweekend ~ .)
```

![](PA1_template_files/figure-html/make panel plot-1.png) 
