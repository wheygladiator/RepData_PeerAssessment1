# Reproducible Research: 

## Loading and preprocessing the data

```r
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip",method="curl")

unzip(zipfile="./data/activity.zip",exdir="./data")
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

```r
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="gray", xlab="Steps", ylim = c(0,30))
```
 
![]Pic%201.png

```r
print(paste("The mean is: ", meanPreNA))
```

```
## [1] "The mean is:  9354.23"
```

```r
print(paste("The median is: ", medianPreNA))
```

```
## [1] "The median is:  10395"
```


## What is the average daily activity pattern?

```r
plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col="black", type="l", xlab = "Intervals (5 Minute)", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")
```

![image-2](image-2.png) 


On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?

```r
print(paste("Interval containing the most steps on average: ",stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))
```
```r
print(paste("Average steps for that interval: ",round(max(stepsPerInterval$meansteps),digits=2)))
```

```
## [1] "Interval containing the most steps on average:  835"
```
```
## [1] "Average steps for that interval:  206.17"
```

## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps))))
```


The NA strategy
```r
activityNoNA <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activityNoNA$steps[i]<- stepsPerInterval$meansteps[activityNoNA$interval[i] == stepsPerInterval$interval]
        }
}
head(activityNoNA,10)
```

All of the missing values are filled in with mean value for that 5-minute
interval.

```r
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="gray", xlab="Steps")
```

![image-4](image-4.png) 

```r
NACompare <- data.frame(mean = c(meanPreNA,meanPostNA),median = c(medianPreNA,medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)
```

```
##                           mean median
## Pre NA Transformation  9354.23  10395
## Post NA Transformation 9354.23  10395
```

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```r
wactivityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)
```
```r
activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)
```

```r
wactivityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)
```

```r
g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16)) + 
        labs(y = "Number of Steps") + labs(x = "Interval") + 
        ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
        theme(plot.title = element_text(hjust = 0.5))
```

![image-6](image-6.png) 
