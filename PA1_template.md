# Analysis of Activity Data from a Personal Activity Monitoring Device in Peer 1 Assessment for 'Reproducible Research'

## This is the R markdown file to describe the results of the Peer 1 Assessment.
---
Title: RepData_CP1.Rmd.
Author: Mark A. Jack.
Date: July 6, 2015.
Output: html_document.

The data of a personal activity monitoring device as used by companies such as Fitbit, Nike Fuelband, or Jawbone Up that collects data such as steps taken by a person at 5 minute intervals through out a day is analyzed and the results are illustrated below. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012, and include the number of steps taken in 5 minute intervals each day.

The initial libraries are being loaded and the data is stored in a file named 'activity.csv' with a data table then created named activity_data and missing data subsequently omitted in steps_data as preparation for the first histogram plot. We load the data from our working directory and store it as a data frame named 'activity_data':



The sum of total steps taken each day is calculated from the data set 'activity_data' and plotted. A histogram is shown below that shows the total number of steps and their occuring frequency for all days in the data set. Missing values (NA) have been omitted from the data set. Data from 53 remaining data points are used.

```r
steps_data_melt <- melt(activity_data, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
steps_data_select <- group_by(steps_data_melt, date, variable)                       
steps_data_sum <- ddply(steps_data_select, c("date"), summarise, sum = sum(value, na.rm = TRUE))
dim(steps_data_sum)
```

```
## [1] 53  2
```

```r
head(steps_data_sum, 10)
```

```
##          date   sum
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
```


```r
par(mfrow = c(1,3))
par(mar = c(4,4,2,2))
histogram(steps_data_sum$sum, nint = 10, col = "red", 
          ylab = "frequency", xlab = "Total number of steps taken, N", 
          main = "Total number of steps taken each day", xlim=range(0:22000))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
#dev.copy(png, file = "/Users/markjack/data/plot_repdata_steps_sum.png")
#dev.off()
```

The mean and median of the number of steps taken per day are then calculated for all 5-minute time intervals averaged across all days in the data set with NAs removed:

```r
ac_data_melt <- melt(activity_data, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
ac_data_select <- group_by(ac_data_melt, date, variable)                       
ac_data_mean <- ddply(ac_data_select, c("date"), summarise, mean = mean(value, na.rm = TRUE))
head(ac_data_mean, 10)
```

```
##          date     mean
## 1  2012-10-02  0.43750
## 2  2012-10-03 39.41667
## 3  2012-10-04 42.06944
## 4  2012-10-05 46.15972
## 5  2012-10-06 53.54167
## 6  2012-10-07 38.24653
## 7  2012-10-09 44.48264
## 8  2012-10-10 34.37500
## 9  2012-10-11 35.77778
## 10 2012-10-12 60.35417
```

```r
ac_data_med <- ddply(ac_data_select, c("date"), summarise, med = median(value, na.rm = TRUE))
head(ac_data_med, 10)
```

```
##          date med
## 1  2012-10-02   0
## 2  2012-10-03   0
## 3  2012-10-04   0
## 4  2012-10-05   0
## 5  2012-10-06   0
## 6  2012-10-07   0
## 7  2012-10-09   0
## 8  2012-10-10   0
## 9  2012-10-11   0
## 10 2012-10-12   0
```

Interestingly, the median is consistently calculated to be zero which means that for each recorded day more than half of the times no steps are taken in each 5-minute interval. To determine in which 5-minute time interval the largest average number of steps taken per day is recorded, following R code is proposed:

```r
ac_data_int_melt <- melt(activity_data, id.vars = "interval", measure.vars = "steps", na.rm = TRUE)
ac_data_int_select <- group_by(ac_data_int_melt, interval, variable)                         
ac_data_int_mean <- ddply(ac_data_int_select, c("interval"), summarise, mean = mean(value, na.rm = TRUE))
```

Below the average number of steps taken per 5-min time interval in a day (y-axis) is plotted versus all 5-min time intervals in a day (x-axis) (averaged over all days recorded) using a line chart (type="l"). In a second panel, we zoom in on the 5-min time interval with largest mean number of steps taken.


```r
par(mfrow = c(1,2))
par(mar = c(4,4,1.5,2.5))
plot(ac_data_int_mean$interval, ac_data_int_mean$mean, type="l", col="red", cex.lab=0.8, lwd=2, 
     xlab = "5-min time interval T [min]", ylab = "Mean number N of steps taken per day", 
     main = "Mean no. of steps taken per day", xaxp  = c(0, 2400, 24))
plot(ac_data_int_mean$interval, ac_data_int_mean$mean, type="l", col="red", cex.lab=0.8, lwd=2, 
     xlab = "5-min time interval T [min]", ylab = "Mean number N of steps taken per day", 
     main = "Zoom: N max. at T = 835 min", xaxp  = c(0, 2400, 480), xlim = c(800,900))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
#dev.copy(png, file = "/Users/markjack/data/plot_repdata_int_means.png")
#dev.off()
```

Thus, the largest average number of steps taken in a 5-min time interval can be found in the 5-minute time interval [835, 840] min. The number of steps taken in that interval is roughly 206:

```r
ac_data_int_mean$mean[99:108]
```

```
##  [1] 129.4340 157.5283 171.1509 155.3962 177.3019 206.1698 195.9245
##  [8] 179.5660 183.3962 167.0189
```

To modify the data with its missing values (NA), we first calculate the total number of missing values in the data to be 2304:

```r
activity_data_imp <- activity_data
x <- activity_data_imp$steps
length(x[is.na(x)])
```

```
## [1] 2304
```

In order to 'impute', i.e. replace missing values in the data set, each NA-value is replaced with the calculated total average number of steps taken per day averaged over all days. The new imputed data set is named 'activity_data_imp':

```r
activity_data_imp$steps <- as.numeric(impute(activity_data_imp$steps, mean))
head(activity_data_imp, 10)
```

```
##      steps       date interval
## 1  37.3826 2012-10-01        0
## 2  37.3826 2012-10-01        5
## 3  37.3826 2012-10-01       10
## 4  37.3826 2012-10-01       15
## 5  37.3826 2012-10-01       20
## 6  37.3826 2012-10-01       25
## 7  37.3826 2012-10-01       30
## 8  37.3826 2012-10-01       35
## 9  37.3826 2012-10-01       40
## 10 37.3826 2012-10-01       45
```

Anologous to above, we calculate the total number of steps taken on each day for the new imputed data set 'activity_data_imp'. The total number of days including imputed data in 'activity_data_imp' is now 61 compared to 53 in the original data set 'activity_data' with NA-values omitted.

```r
steps_data_melt_imp <- melt(activity_data_imp, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
steps_data_select_imp <- group_by(steps_data_melt_imp, date, variable)                       
steps_data_sum_imp <- ddply(steps_data_select_imp, c("date"), summarise, sum = sum(value, na.rm = TRUE))
dim(steps_data_sum_imp)
```

```
## [1] 61  2
```

```r
head(steps_data_sum_imp, 10)
```

```
##          date      sum
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
```

The total number of steps per day is plotted in a histogram below now for the imputed data and shows as the earlier histogram where missing values had been neglected that about 10,000 to 12,000 steps taken per day occurs most frequently over all the days recorded:

```r
par(mfrow = c(1,1))
par(mar = c(4,4,2,2))
histogram(steps_data_sum_imp$sum, nint = 10, col = "red", ylab = "frequency", xlab = "Total number of steps taken, N", main = "Total number of steps taken each day", xlim=range(0:22000))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
#dev.copy(png, file = "/Users/markjack/data/plot_repdata_steps_sum_imp.png")
#dev.off()
```

We calculate and print the mean and median number of steps taken each day for the imputed data set: 

```r
ac_data_melt_imp <- melt(activity_data_imp, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
ac_data_select_imp <- group_by(ac_data_melt_imp, date, variable)                       
ac_data_mean_imp <- ddply(ac_data_select_imp, c("date"), summarise, mean = mean(value, na.rm = TRUE))
head(ac_data_mean_imp, 10)
```

```
##          date     mean
## 1  2012-10-01 37.38260
## 2  2012-10-02  0.43750
## 3  2012-10-03 39.41667
## 4  2012-10-04 42.06944
## 5  2012-10-05 46.15972
## 6  2012-10-06 53.54167
## 7  2012-10-07 38.24653
## 8  2012-10-08 37.38260
## 9  2012-10-09 44.48264
## 10 2012-10-10 34.37500
```

```r
ac_data_med_imp <- ddply(ac_data_select_imp, c("date"), summarise, med = median(value, na.rm = TRUE))
head(ac_data_med_imp, 10)
```

```
##          date     med
## 1  2012-10-01 37.3826
## 2  2012-10-02  0.0000
## 3  2012-10-03  0.0000
## 4  2012-10-04  0.0000
## 5  2012-10-05  0.0000
## 6  2012-10-06  0.0000
## 7  2012-10-07  0.0000
## 8  2012-10-08 37.3826
## 9  2012-10-09  0.0000
## 10 2012-10-10  0.0000
```
The most notable difference is that the median values now include non-zero contributions to the included, imputed data (being the total average of number of steps taken over all days). There are no drastic differences compared to the mean values calculated earlier for the 'cleaned' data set with NA-values omitted.

Finally, we compare the activity levels on weekdays and weekend days: We calculate the average number of steps taken per 5-min time interval in a day, averaged either over all weekdays or over all weekend days, respectively:

```r
activity_data_imp$date <- as.character(weekdays(as.Date(activity_data_imp$date)))
activity_data_imp$weekday = activity_data_imp$date %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")
ac_data_int_melt_wdays <- melt(activity_data_imp, id.vars = c("interval", "weekday"), measure.vars = "steps", na.rm = TRUE)
ac_data_int_select_wdays <- group_by(ac_data_int_melt_wdays, weekday, interval, variable)   
ac_data_int_wdays <- ddply(ac_data_int_select_wdays, c("weekday", "interval"), summarise, mean = mean(value, na.rm = TRUE))
head(ac_data_int_wdays, 10)
```

```
##    weekday interval     mean
## 1    FALSE        0 4.672825
## 2    FALSE        5 4.672825
## 3    FALSE       10 4.672825
## 4    FALSE       15 4.672825
## 5    FALSE       20 4.672825
## 6    FALSE       25 7.922825
## 7    FALSE       30 4.672825
## 8    FALSE       35 4.672825
## 9    FALSE       40 4.672825
## 10   FALSE       45 5.047825
```

When we plot the average number of steps taken per 5-min time interval in a day (y-axis) versus 5-min time intervals in one day (x-axis) in a panel plot using a line chart (type="l"), we see some noticeable differences between weekend days and weekday activity levels:

```r
par(mfrow = c(1,2))
par(mar = c(4,4,2,2))
facet_names <- list("TRUE" = "Weekday (Mon-Fri)", "FALSE" = "Weekend Day (Sat, Sun)")
facet_labeller <- function(weekday,value){
  value <- as.character(value)
  return(facet_names[value])
}
g <- ggplot(ac_data_int_wdays, aes(interval, mean))
g1 <- g + geom_point(color = "blue") + geom_line(color = "blue") + facet_grid(. ~ weekday, labeller = facet_labeller) 
g2 <- g1 + geom_smooth(color = "red", method = "lm") 
g3 <- g2 + labs(title = "Average number of steps taken on weekend days and weekdays") 
g4 <- g3 + labs(x = "5-min time interval T [min]", y = "Mean number of steps N per 5-min interval")
g4
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

```r
#dev.copy(png, file = "/Users/markjack/data/plot_repdata_int_weekday.png")
#dev.off()
```

Week days show a clear peak in average number of steps taken in the 835- to 840-minute time interval which is significantly higher than any peak in activity on weekdays. On the other hand, when including and highlighting a linear regression fit (in red) for both plots the activity levels on weekdays remain comparatively flat over all 5-min time intervals while on the two weekend days there is a slight increase in activity levels on average to later times in the day.  
