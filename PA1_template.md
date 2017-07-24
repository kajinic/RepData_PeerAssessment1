Introduction
============

This report makes use of data from a personal activity monitoring
device, which collects data at 5 minute intervals through out the day.
Data were collected from an anonymous individual during October and
November of 2012 and include the number of steps taken in 5 minute
intervals each day.

Data
----

The dataset can be downloaded from the following website:

<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

It is stored in a comma-separated value (CSV) file.

There are 17568 observations of 3 variables:

steps : Number of steps taking in a 5-min interval ; missing values are
noted as NA date : The date on which the measurement was taken in
YYYY-MM-DD format  
interval : Identifier for the 5-minute interval in which measurement was
taken

### 1. Code for reading in the dataset.

    data <- read.csv("C:/Users/Owner/Documents/Coursera/repdata_project1/activity.csv")

### 2. Histogram of the total number of steps taken each day (missing data ignored).

    library(ggplot2)
    total.steps <- tapply(data$steps, data$date,
                          FUN = sum, na.rm =TRUE, simplify = TRUE)

    total.steps <- total.steps[!is.na(total.steps)]

    g1 <- qplot(total.steps, binwidth = 1000, xlab = "Daily Total Steps",
                main = "Distribution of daily total steps (missing data ignored)")

    plot(g1)  

### 3. Mean and median number of steps taken each day.

    mean.total.steps <- mean(total.steps, na.rm=TRUE)  

The mean number of steps taken each day is 9,354.

    median.total.steps <- median(total.steps, na.rm=TRUE)  

The median number of steps taken each day is 10,395.

### 4. Time series plot of the average number of steps taken.

    library(ggplot2)
    averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                          FUN=mean, na.rm=TRUE)
                          
    g2 <- ggplot(data=averages, aes(x=interval, y=steps)) +
          geom_line() +
          xlab("5-minute interval") +
          ylab("average steps per interval across all days")

    plot(g2)

### 5. The 5-minute interval that, on average, contains the maximum number of steps.

    max.steps <- averages[which.max(averages$steps),]

The interval 835 contains, on average, the maximum number of steps.

The number of steps in 835 interval is 206.

### 6. Code to describe and show a strategy for imputing missing data.

Let us first calculate and report the total number of missing values in
the dataset by counting the total number of rows with NAs.

    missing.data <- sum(is.na(data$steps))

The number of missing values in the dataset is 2,304.

A strategy that is chosen for imputing a missing value is to use the
mean value for that 5-minute interval in the dataset.

A new data frame `data.impute` is created and is equal to the original
dataset but with the missing data filled in by using the mean value for
that interval.

    data.impute <- data
    data.ign <- subset(data, !is.na(data$steps))
    ndx <- is.na(data.impute$steps)
    int.avg <- tapply(data.ign$steps, data.ign$interval, mean, na.rm = TRUE)
    data.impute$steps[ndx] <- int.avg[as.character(data.impute$interval[ndx])]

### 7. Histogram of the total number of steps taken each day after missing values are imputed.

    library(ggplot2)

    new.dailysum <- tapply(data.impute$steps, data.impute$date, sum, 
                           na.rm = TRUE, simplify = TRUE)

    g3 <- qplot(new.dailysum, binwidth = 1000, xlab = "Daily Total Steps",
                main = "Distribution of daily total steps (missing data imputed)")

    plot(g3)  

Mean and median number of steps taken each day, after the missing values
are imputed.

    mean.new.dailysum <- mean(new.dailysum, na.rm=TRUE)  

After the missing values are imputed, the mean number of steps taken
each day is 10,766.

    median.new.dailysum <- median(new.dailysum, na.rm = TRUE)  

After the missing values are imputed, the median number of steps taken
each day is 10,766.

Both the mean and median values are higher after imputing missing data.
The reason is that in the original data, there are some days with
`steps` values `NA` for any `interval`. The total number of steps taken
in such days are set to zeros by default, which explains the lower
values for the mean and median in the original dataset.

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

Let us create a new factor variable called `wk` in the dataset with two
levels: `weekday` and `weekend` indicating whether a given date is a
weekday or weekend.

    is.weekday <- function(d) {
        wd <- weekdays(d)
        ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
    }

    data.impute$date <- as.Date(data.impute$date, format = "%m/%d/%Y")

    wx <- sapply(data.impute$date, is.weekday)
    data.impute$wk <- as.factor(wx)

Now we are ready to create a panel plot showing a time series plot using
the 5-minute intervals as the x-axis and the average number of steps
taken (averaged across all weekday days or weekend days) as the y-axis.

    wk.data <- aggregate(steps ~ wk+interval, data=data.impute, FUN=mean)

    library(lattice)

    xyplot(steps ~ interval | factor(wk),
           layout = c(1, 2),
           xlab = "5-minute interval",
           ylab = "Number of steps per interval",
           type = "l",
           lty = 1,
           data = wk.data)
