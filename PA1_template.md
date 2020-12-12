---
title: "Reproducible Research: Peer Assessment 1"
author: "Ahmed Hassan"
date: "12/12/2020"
output: 
  html_document:
    keep_md: true
---


## Setting Global Options

```r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, fig.height = 5, fig.keep = 'all' ,fig.path = 'figures\ ', dev = 'png')
```
## Loading and preprocessing the data


```r
# Loading packages
library(ggplot2)

# Unzipping the file and reading it
unzip("activity.zip") 
activity <- read.csv("activity.csv")

# Setting date format to help get the weekdays of the dates
activity$date <- as.POSIXct(activity$date, "%Y%m%d")

# Getting the days of all the dates on the dataset
activity$day <- weekdays(activity$date)

# Viewing the processed data
summary(activity)
```

```
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
```

```r
head(activity)
```

```
##   steps       date interval    day
## 1    NA 2012-10-01        0 Monday
## 2    NA 2012-10-01        5 Monday
## 3    NA 2012-10-01       10 Monday
## 4    NA 2012-10-01       15 Monday
## 5    NA 2012-10-01       20 Monday
## 6    NA 2012-10-01       25 Monday
```


## What is mean total number of steps taken per day?

```r
# Calculating total steps taken at each day as data frame 
totalStepsPerDay <- as.data.frame(aggregate(activity$steps, by = list(activity$date), sum, na.rm = TRUE))

# Changing col names
names(totalStepsPerDay) <- c("Date", "Steps")

# Plotting a histogram using ggplot2
totalStepsPlot <- ggplot(totalStepsPerDay, aes(x = Steps)) + 
        geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "lightskyblue1", col = "black") + 
        ylim(0, 30) + 
        xlab("Total Steps Taken Per Day") + 
        ylab("Frequency") + 
        ggtitle("Total Number of Steps Taken at each Day")
  
totalStepsPlot
```

![](figures unnamed-chunk-3-1.png)<!-- -->

```r
## The mean of the total number of steps taken per day is:
mean(totalStepsPerDay$Steps)
```

```
## [1] 9354.23
```
## The median of the total number of steps taken per day is: 

```r
median(totalStepsPerDay$Steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
# Calculating the average number of steps taken, averaged across all days by 5-min intervals. 
averageDailyActivity <- as.data.frame(aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE))

names(averageDailyActivity) <- c("Interval", "Mean") 

# Plotting on ggplot2 
daPlot <- ggplot(averageDailyActivity, mapping = aes(Interval, Mean)) + 
  geom_line(col = "magenta") +
  xlab("Interval") +
  ylab("Average Number of Steps") +
  ggtitle("Average Number of Steps Per Interval")

daPlot
```

![](figures unnamed-chunk-5-1.png)<!-- -->
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averageDailyActivity[which.max(averageDailyActivity$Mean), ]$Interval
```

```
## [1] 835
```

## Imputing missing values


```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs.

sum(is.na(activity$steps))
```

```
## [1] 2304
```
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Matching the mean of daily activity with the missing values
imputedSteps <- averageDailyActivity$Mean[match(activity$interval, averageDailyActivity$Interval)]
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# Transforming steps in activity if they were missing values with the filled values from above.
activityImputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputedSteps, no = activity$steps))

# Forming the new dataset with the imputed missing values.
totalActivityImputed <- aggregate(steps ~ date, activityImputed, sum)

# Changing col names
names(totalActivityImputed) <- c("date", "dailySteps")
```
### Testing the new dataset to check if it still has any missing values -  

```r
sum(is.na(totalActivityImputed$dailySteps))
```

```
## [1] 0
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Converting the data set into a data frame to be able to use ggplot2
totalImputedStepsdf <- data.frame(totalActivityImputed)

# Plotting a histogram using ggplot2
p <- ggplot(totalImputedStepsdf, aes(x = dailySteps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#83CAFF", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day")

print(p)
```

![](figures unnamed-chunk-11-1.png)<!-- -->
### The mean of the total number of steps taken per day is:  

```r
mean(totalActivityImputed$dailySteps)
```

```
## [1] 10766.19
```
### The median of the total number of steps taken per day is:  

```r
median(totalActivityImputed$dailySteps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# Updating format of the dates
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

# Creating a function that distinguises weekdays from weekends
activity$dayType <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"}
  else {y <- "Weekday"}
  y
})
```
### Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
# Creating the data set that will be plotted
activityByDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)

# Plotting using ggplot2
dayPlot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day Type") 

print(dayPlot) 
```

![](figures unnamed-chunk-15-1.png)<!-- -->



