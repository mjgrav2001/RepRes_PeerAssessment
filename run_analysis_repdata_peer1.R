#-------------------------------------------------------------------------
# This R script called 'run_analysis_repdata_peer1.R' analyzes the activity data
# from the website https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
#
# Note: The data linked to the course website can be found here:
# https://github.com/mjgrav2001/RepData_PeerAssessment1
#
#-------------------------------------------------------------------------
# Necessary library calls in R:
library(plyr)
library(dplyr)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(lubridate)
#
#-------------------------------------------------------------------------
# Load data from working directory and store as data frame 'activity_data':
activity_data <- read.csv("/Users/markjack/data/activity.csv")
#
#-------------------------------------------------------------------------
# Calculate and print sum of total steps taken each day from data set 'activity_data':
steps_data_melt <- melt(activity_data, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
steps_data_select <- group_by(steps_data_melt, date, variable)                       
steps_data_sum <- ddply(steps_data_select, c("date"), summarise, sum = sum(value, na.rm = TRUE))
head(steps_data_sum, 20)
#
#-------------------------------------------------------------------------
# Plot total number of steps taken each day as a histogram:
par(mfrow = c(1,3))
par(mar = c(4,4,2,2))
histogram(steps_data_sum$sum, nint = 22, col = "red", 
          ylab = "frequency", xlab = "Total number of steps taken, N", 
          main = "Total number of steps taken each day", xlim=range(0:22000))
dev.copy(png, file = "/Users/markjack/data/plot_repdata_steps_sum.png")
dev.off()
#
#-------------------------------------------------------------------------
# Calculate and print mean and median numbes of steps taken each day from data set 'activity_data':
ac_data_melt <- melt(activity_data, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
ac_data_select <- group_by(ac_data_melt, date, variable)                       
ac_data_mean <- ddply(ac_data_select, c("date"), summarise, mean = mean(value, na.rm = TRUE))
head(ac_data_mean, 20)
ac_data_med <- ddply(ac_data_select, c("date"), summarise, med = median(value, na.rm = TRUE))
head(ac_data_med, 20)
#
#-------------------------------------------------------------------------
# Calculate average number of steps taken per 5-min time interval in a day, 
# averaged over all days recorded in data set 'activity_data':
ac_data_int_melt <- melt(activity_data, id.vars = "interval", measure.vars = "steps", na.rm = TRUE)
ac_data_int_select <- group_by(ac_data_int_melt, interval, variable)                         
ac_data_int_mean <- ddply(ac_data_int_select, c("interval"), summarise, mean = mean(value, na.rm = TRUE))
ac_data_int_mean$mean[99:108]
#
#-------------------------------------------------------------------------
# Plot average number of steps taken per 5-min time interval in a day (y-axis) versus 5-min time intervals 
# in one day (x-axis) (averaged over all days recorded) using a line chart (type="l"):
par(mfrow = c(1,2))
par(mar = c(4,4,1.5,2.5))
plot(ac_data_int_mean$interval, ac_data_int_mean$mean, type="l", col="red", cex.lab=0.8, lwd=2, 
     xlab = "5-min time interval T [min]", ylab = "Mean number N of steps taken per day", 
     main = "Mean no. of steps taken per day", xaxp  = c(0, 2400, 24))
plot(ac_data_int_mean$interval, ac_data_int_mean$mean, type="l", col="red", cex.lab=0.8, lwd=2, 
     xlab = "5-min time interval T [min]", ylab = "Mean number N of steps taken per day", 
     main = "Zoom: N max. at T = 835 min", xaxp  = c(0, 2400, 480), xlim = c(800,900))
dev.copy(png, file = "/Users/markjack/data/plot_repdata_int_means.png")
dev.off()
#
#-------------------------------------------------------------------------
# Store data set 'activity_data' in new data set 'activity_data_imp' and calculate total number of missing values (NA)
# in the data set:
activity_data_imp <- activity_data
x <- activity_data_imp$steps
length(x[is.na(x)])
#
#-------------------------------------------------------------------------
# 'Impute' i.e. replace missing values (NA) in data set 'activity_data_imp' with calculated 
# total average number steps taken over all recorded days and re-store in data set 'activity_data_imp':
activity_data_imp$steps <- as.numeric(impute(activity_data_imp$steps, mean))
head(activity_data_imp, 10)
#
#-------------------------------------------------------------------------
# Calculate sum of total steps taken each day in imputed data set 'activity_data_imp':
steps_data_melt_imp <- melt(activity_data_imp, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
steps_data_select_imp <- group_by(steps_data_melt_imp, date, variable)                       
steps_data_sum_imp <- ddply(steps_data_select_imp, c("date"), summarise, sum = sum(value, na.rm = TRUE))
head(steps_data_sum_imp, 20)
#
#-------------------------------------------------------------------------
# Plot total number of steps taken each day as a histogram for imputed data set stored as 'steps_data_sum_imp':
par(mfrow = c(1,1))
par(mar = c(4,4,2,2))
histogram(steps_data_sum_imp$sum, nint = 22, col = "red", ylab = "frequency", xlab = "Total number of steps taken, N", 
          main = "Total number of steps taken each day", xlim=range(0:22000))
dev.copy(png, file = "/Users/markjack/data/plot_repdata_steps_sum_imp.png")
dev.off()
#
#-------------------------------------------------------------------------
# Calculate and print mean and median number of steps taken each day for imputed data set 'activity_data_imp':
ac_data_melt_imp <- melt(activity_data_imp, id.vars = "date", measure.vars = "steps", na.rm = TRUE)
ac_data_select_imp <- group_by(ac_data_melt_imp, date, variable)                       
ac_data_mean_imp <- ddply(ac_data_select_imp, c("date"), summarise, mean = mean(value, na.rm = TRUE))
head(ac_data_mean_imp, 10)
ac_data_med_imp <- ddply(ac_data_select_imp, c("date"), summarise, med = median(value, na.rm = TRUE))
head(ac_data_med_imp, 10)
#
#-------------------------------------------------------------------------
# Comparing of activities on weekdays and weekend days: Calculate average number of steps taken 
# per 5-min time interval in a day, averaged either over all weekdays or over all weekend days, respectively:
activity_data_imp$date <- as.character(weekdays(as.Date(activity_data_imp$date)))
activity_data_imp$weekday = activity_data_imp$date %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")
ac_data_int_melt_wdays <- melt(activity_data_imp, id.vars = c("interval", "weekday"), measure.vars = "steps", na.rm = TRUE)
ac_data_int_select_wdays <- group_by(ac_data_int_melt_wdays, weekday, interval, variable)   
ac_data_int_wdays <- ddply(ac_data_int_select_wdays, c("weekday", "interval"), summarise, mean = mean(value, na.rm = TRUE))
head(ac_data_int_wdays, 10)
#
#-------------------------------------------------------------------------
# Comparing of activities on weekdays and weekend days: Plot average number of steps taken per 5-min time interval 
# in a day (y-axis) versus 5-min time intervals in one day (x-axis) (averaged either over all weekdays 
# or over all weekend days, respectively) in a panel plot using a line chart (type="l"):
#
par(mfrow = c(1,2))
par(mar = c(4,4,2,2))
#png(filename = "plot_repdata_int_wdays.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white")
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
dev.copy(png, file = "/Users/markjack/data/plot_repdata_int_weekday.png")
dev.off()
#-------------------------------------------------------------------------
##write.table(ac_data_int_wdays, file = "./data/ac_data_impute.txt", sep =" ", eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)
