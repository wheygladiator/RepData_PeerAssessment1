# Reproducible Research Week 2



## PreProcess
1. Read data into memory via read.csv
2. Change Date from String to Date


```r
df.activity.data <- read.csv("activity.csv")
df.activity.data$date <- as.Date(df.activity.data$date, format = "%Y-%m-%d")
```

## Tasks

### 1. Histogram of the total number of steps taken each day


```r
df.activity.steps_per_day <- aggregate(steps ~ date, data = df.activity.data, sum)
hist(df.activity.steps_per_day$steps, xlab="Steps per day", main="Histogram of the total number of steps taken each day")
```

![](Pic%201.png)

### 2. Mean and median number of steps taken each day

```r
df.activity.steps_per_day.mean <- mean(df.activity.steps_per_day$steps)
df.activity.steps_per_day.median <- median(df.activity.steps_per_day$steps)
df.activity.steps_per_day.mean 
```

```
## [1] 10766.19
```

```r
df.activity.steps_per_day.median
```

```
## [1] 10765
```


### 3. Time series plot of the average number of steps taken

```r
df.average_steps_per_interval <- aggregate(steps ~ interval, data = df.activity.data, mean)
plot(df.average_steps_per_interval, type="l", main="Time series plot of the average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 5.The 5-minute interval that, on average, contains the maximum number of steps

```r
df.average_steps_per_interval$interval[which.max(df.average_steps_per_interval$steps)]
```

```
## [1] 835
```

### 6. Code to describe and show a strategy for imputing missing data

#### Impute using Linear Interpolation of closest Non-NA rows.

```r
sum(apply(df.activity.data, 1, function(row) any(is.na(row))))
```

```
## [1] 2304
```

```r
df.activity.imputed <- df.activity.data
df.activity.imputed$steps <- approxfun(seq_along(df.activity.data$steps), df.activity.data$steps, method="linear", rule=2)(seq_along(df.activity.data$steps))
if(any(is.na(df.activity.imputed))) {print("WARN: NA values in df.activity.imputed")}
```

### 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
df.activity.imputed.steps_per_day <- aggregate(steps ~ date, data = df.activity.imputed, sum)
hist(df.activity.imputed.steps_per_day$steps, xlab="Steps per day", , main="The total number of steps taken each day (Imputed Missing Values")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### 8

```r
df.activity.imputed.steps_per_day.mean <- mean(df.activity.imputed.steps_per_day$steps)
df.activity.imputed.steps_per_day.median <- median(df.activity.imputed.steps_per_day$steps)
```


```
## [1] 9354.23
```

```
## [1] 10395
```

```
## [1] 10766.19
```

```
## [1] 10765
```


### 9. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
weekorweekend <- function(date) {
    if (weekdays(date) %in% c("Saturday", "Sunday")) {"weekend"} else {"weekday"}
}
df.activity.imputed$weekorweekend <- as.factor(sapply(df.activity.imputed$date, weekorweekend))
df.activity.imputed.weekorweekend <- aggregate(steps ~ interval + weekorweekend, data = df.activity.imputed, mean)
suppressMessages(library(ggplot2))
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
ggplot(data = df.activity.imputed.weekorweekend, aes(x=interval, y=steps)) + facet_grid(weekorweekend ~ .) + geom_line() + ggtitle("Average number of steps taken per 5-minute interval across weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


