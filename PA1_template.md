# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data

```r
unzip("repdata-data-activity.zip")
df <- read.csv("activity.csv", sep = ",")
df <- transform(df, date = as.Date(as.character(date), "%Y-%m-%d"))
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
steppd <- tapply(df$steps, df$date, sum, na.rm = TRUE)
print(steppd)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

```r
hist(steppd/1000, main = paste("Histogram of" , "total number of steps taken per day"), xlab = "total steps (1000 steps)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day.

```r
stepm <- mean(steppd)
stepmd <- median(steppd)
```
The mean of the total number of steps taken per day is **9354.2295082**, and the median of the total number of steps taken per day is **10395**.

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
library(ggplot2)
# qplot(interval, steps, data = df, color = date, geom = "path")
df2 <- data.frame(averagestep = tapply(df$steps, df$interval, mean, na.rm = TRUE), interval = df[df$date == "2012-10-01","interval"])
qplot(interval, averagestep, data = df2, geom = "path")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intv <- df2[max(df2$averagestep),]
```
At the **1705** interval contains the maximum number of steps.

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
#summary(df$steps)
#summary(df$interval)
#summary(as.factor(df$date))
#table(df$steps %in% NA)
table(is.na(df))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

```r
colSums(is.na(df))
```

```
##    steps     date interval 
##     2304        0        0
```
There is **2304** missing values in the dataset.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*The missing value will be filled by the mean of that 5-minute interval across the days.*

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# tapply(is.na(df$steps), df$date, sum)  #check the distribution of NAs
# df2$averagestep is the mean of that 5-minute interval across the days
dfnona <- df
for (i in seq_along(df2$interval)){
      dfnona[which(is.na(dfnona$steps) & dfnona$interval == df2[i, "interval"]), "steps"] <- df2[i, "averagestep"]  
}
```
The new generated data frame without missing value is *dfnona*.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steppdnona <- tapply(dfnona$steps, dfnona$date, sum, na.rm = TRUE)
hist(steppdnona, main = paste("Histogram of" , "total number of steps taken per day"), xlab = "total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
stepmnona <- mean(steppdnona)
stepmdnona <- median(steppdnona) # floor() take the integer part
```
The new mean of the total number of steps taken per day is **1.0766189\times 10^{4}**, and the new median of the total number of steps taken per day is 1.0766189\times 10^{4}.   
The value get slightly bigger.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
dfnona$weekdays <- weekdays(dfnona$date)
dfnona[dfnona$weekdays %in% weekday, "weekdays"] <- "weekday"
dfnona[dfnona$weekdays %in% weekend, "weekdays"] <- "weekend"
dfnona$weekdays <- factor(dfnona$weekdays, levels = c("weekday", "weekend"))
# dfnona$weekday <- as.factor(dfnona$weekday)
```
The new factor variable is **weekdays**.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(dplyr)
df3 <- aggregate(dfnona, by = list(dfnona$weekdays, dfnona$interval), FUN = "mean")
# df3 <- rename(df3, weekdayfac = Group.1)
# df3  %>% select(weekdayfac = Group.1, steps, interval)
# dfnona %>%  aggregate(by = list(dfnona$weekdays, dfnona$interval), FUN = "mean") 
#    %>% select(weekdayfac = Group.1, steps, interval) 
df4 <- select(df3, weekdayfac = Group.1, steps, interval)
qplot(interval, steps, data = df4, facets = weekdayfac ~ ., geom = "path", xlab = "interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

