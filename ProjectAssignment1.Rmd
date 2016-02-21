---
title: "Project1"
author: "WJ"
date: "February 21, 2016"
output: html_document
---

#Loading and preprocessing the data

What is mean total number of steps taken per day?

1. Read the csv file. Hint : Use the read.csv() function
```{r}
library(ggplot2)
library(knitr)
opts_knit$set(base.dir = "C:/Users/WW/Documents/RepDataProject/figure")
file<-("C:/Users/WW/Documents/RepDataProject/activity.csv")
MyData<-read.csv(file,header = TRUE, sep = ",")
```
For this part of the assignment, you can ignore the missing values in the dataset.

2. The total number of steps taken per day
```{r}
steps.perday <- aggregate(steps ~ date, MyData, FUN = sum)
steps.perday
```

3. Make a histogram of the total number of steps taken each day
```{r}
barplot(steps.perday$steps, names.arg = steps.perday$date, xlab = "Date", ylab = "Steps")
```

4. The mean and median of the total number of steps taken per day
```{r}
mean(steps.perday$steps)
median(steps.perday$steps)
```

#What is the average daily activity pattern?

5. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
steps.interval<- aggregate(steps ~ interval, MyData, FUN = mean)
plot(steps.interval, type = "l")
```

6. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
steps.interval$interval[which.max(steps.interval$steps)]
#835
```

#Missing values

7. Note that there are a number of days/intervals where there are missing values (coded as NA).Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
sum(is.na(MyData))
#2304
```

8. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*I prefer to use the means for the 5-minute intervals as fillers for missing values in the datasets.

9. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
MyData <- merge(MyData, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(MyData$steps)
MyData$steps[nas] <- MyData$steps.y[nas]
MyData <- MyData[, c(1:3)]
```

10. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
```{r}
steps.perday <- aggregate(steps ~ date, MyData, FUN = sum)
barplot(steps.perday$steps, names.arg = steps.perday$date, xlab = "Date", ylab = "Steps")
mean(steps.perday$steps)
median(steps.perday$steps)
```

11. Do these values differ from the estimates from the first part of the assignment?
The value just a little different in decimal only. 

12. What is the impact of imputing missing data on the estimates of the total daily number of steps?
The impact is so minimal.

#Differences in activity patterns between weekdays and weekends?

13. For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
MyData$daytype <- as.factor(sapply(MyData$date, daytype))
```

14. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
par(mfrow = c(2, 1))
for (type in c("weekday", "weekend")) {
    steps.type <- aggregate(steps ~ interval, MyData, subset = MyData$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

From both graph we can see that the activity on the weekday has the highest peak from all steps intervals. For the weekends activities, it has many peaks that passed 100 than weekday. This is due to we tend to have routine activity during weekdays but there a some changes in weekend as there are many things that can be done. 
