---
title: "PA1_template"
output: html_document
---

###Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

###Code for reading in the dataset and/or processing the data
Code which is necessary for initial loading of the dataset

```r
   #Reads the data
   DataActivity <- read.csv("./activity/activity.csv", stringsAsFactors = FALSE)
   summary(DataActivity)
   #Splits the data per date
   DataActivity$date <- as.Date(DataActivity$date, format = "%Y-%m-%d")
   str(DataActivity
   #Checks dimensions and few rows of New Data Frame
   dim(DataActivity)
   #Shows number of observations and variables mentioned in the assignment and we can see missing     values at the first    day of data collection
   head(DataActivity)
```

###What is mean total number of steps taken per day?
  
```r 
   #Calculating the total steps per day
   stepsperday <- with(DataActivity, tapply(steps,as.factor(DataActivity$date), sum, na.rm = T))
   #Histogram of  total number of steps taken per day, plotted with appropriate bin interval.
   hist(stepsperday, main = "Histogram of total number of steps taken per day", xlab = "Total        number of    steps")
```

![Total number of steps taken per day](M:\Reproducible Research\RepData_PeerAssessment1\figures\stepsperday.png) 


##Mean and median number of steps taken each day 
  
```r
   #Mean and median number of steps taken each day
   summary(stepsperday)
```

###What is the average daily activity pattern?

##Time series plot of the average number of steps taken

```r
   #Average Steps Aggregrate
   averagesteps <- aggregate(steps ~ interval, data = DataActivity, mean, na.rm = TRUE)
   #Plots the avgsteps vs time interval
   plot(steps ~ interval, data = averagesteps, type = "l", xlab = "Time Intervals (5-minute)", ylab =      "Mean       number of steps taken (all Days)", main = "Average number of steps Taken at 5 minute           Intervals",  col    = "blue")
```

![Time series plot of the average number of steps taken](M:\Reproducible Research\RepData_PeerAssessment1\figures\Avgnoofstepstaken.png)

###The 5-minute interval that, on average, contains the maximum number of steps

```r
   averagesteps$interval[which.max(averagesteps$steps)]
```   
###Imputing missing values

##Code to describe and show a strategy for imputing missing data

```r
   #Number of rows that contain an NA
   sum(is.na(DataActivity))
   #Mean of 5 minute interval to fill in the values of the missing values
   mean(is.na(DataActivity$steps))
   #New Data based on mean to cover missing values
   ND <- DataActivity
   #Using for loop
   for (i in averagesteps$interval) {
   ND[ND$interval == i & is.na(ND$steps), ]$steps <-
   averagesteps[averagesteps$interval == i]
   }
   for (i in averagesteps$interval) {
   ND[ND$interval == i & is.na(ND$steps), ]$steps <-
   averagesteps$steps[averagesteps$interval == i]
   }
   head(ND)
   sum(is.na(ND))
``` 

##Histogram of the total number of steps taken each day after missing values are imputed
  
```r
   #total number of steps aggregrate
   totalsteps <- aggregate(steps ~ date, data = ND, sum, na.rm = TRUE)
   #Plots histogram of the number of total steps per day
   hist(totalsteps$steps, breaks = 20,
   main = "Total Number of Steps Taken per day (Imputed)",
   col = "blue", border = "black", xlab = "Step", axes = FALSE)
   axis(1)
   axis(2, las = 1)
   #mean total number of steps taken per day
   mean(totalsteps$steps)
   #median total number of steps taken per day
   median(totalsteps$steps)
``` 


![Histogram of the total number of steps taken each day after missing values are imputed](M:\Reproducible Research\RepData_PeerAssessment1\figures\totalstepsperdayimputed.png)


###Are there differences in activity patterns between weekdays and weekends?


##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r  
   #Create a new factor variable in the dataset with 2 levels "weekday" and "weekend" indicating whether a given date    is a weekly or weekend day
   ND$day <- weekdays(ND$date)
   ND$week <- ""
   ND[ND$day == "Saturday" | ND$day == "Sunday", ]$week <- "weekend"
   ND[!(ND$day == "Saturday" | ND$day == "Sunday"),]$week <- "weekday"
   ND$week <- factor(ND$week)
   #Panel Plot containing timeseries plot (type="1") of the 5 min interval (X-axis) and average number of steps taken,    averaged across all weekday and weekend days (y-axis)
   avgstepsND <- aggregate(steps ~ interval + week, data = ND, mean)
   library(lattice)
   xyplot(steps ~ interval | week, data =         
   avgstepsND, type = "l", lwd = 2,
   layout = c(1, 2),
   xlab = "5-minute interval",
   ylab = "Average number of steps",
   main = "Average Number of Steps Taken (across all weekday days or weekend days)")
``` 

![Comparing the average number of steps taken per 5-minute interval across weekdays and weekends](M:\Reproducible Research\RepData_PeerAssessment1\figures\avg steps takenweekdayandweekend.png)