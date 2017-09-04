### **Summary**

This is the R markdown document for the Course 5 - Reproducible Research
Course Project 1.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals throughout the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November 2012 and
includes the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

      steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
      date: The date on which the measurement was taken in YYYY-MM-DD format
      interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (csv) file and there
are a total of 17,568 observations in this dataset.

This script does the following:

    - Reads the activity.csv file and loads the data set.  
    - Creates a histogram of the total number of steps taken each day.  
    - Calculates and reports the mean and median total number of steps taken per day.
    - Creates a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
      averaged across all days (y-axis).
    - Calculates which 5-minute interval, on average across all the days in the dataset, contains the maximum
      number of steps.
    - Calculates and reports the total number of missing values in the dataset.
    - Creates a new dataset that is equal to the original dataset but with the missing data filled in (imputed).
    - Creates a histogram of the total number of steps taken each day with the imputed dataset.
    - Calculates and reports the mean and median total number of steps taken per day with the imputed dataset.
    - Creates a time series panel plot of the 5-minute interval (x-axis) and the average number of steps taken,
      averaged across all weekday days or weekend days (y-axis).

### **Loading and Pre-processing the Data**

This section contains the code for reading in the dataset and processing
the data.

    ##  Load the dplyr and Hmisc (ddply) packages to manipulate data as needed.

        library(dplyr)
        library(plyr)
        library(date)
        library(Hmisc)

    ##  Assign data frame variable for the activity.csv dataset.
    ##  Convert the "Date" column format from "chr"" class to "Date" class using the standard YYYY-MM-DD format.

        unzip("activity.zip")
        activity_df <- read.csv("activity.csv")  
        activity_df$date <- as.Date(activity_df$date) 

### **Mean total number of steps taken per day?**

What is the mean total number of steps taken per day?  
Calculate the total number of steps taken per day:

    ##  Create a data frame that returns the total number of steps by date across all days.
        
        sum_steps_by_day_df <- aggregate(steps~date,data=activity_df,sum,na.rm=TRUE)

Calculate and report the mean and median number of steps per day:

    ##  Calculate the mean and median number of steps taken per day.
        
        mean(sum_steps_by_day_df$steps)
        median((sum_steps_by_day_df$steps))

*Mean = 10766*  
*Median = 10765*

    ##  Create a histogram plot showing the distribution of the total number of steps taken each day.
        
        hist(sum_steps_by_day_df$steps,main="Number of Steps per Day",breaks=100,ylim=c(0,10),xlab = "# of Steps",col="blue")

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

### **Average daily activity pattern?**

What is the average daily activity pattern?  
Calculate the 5-min interval, on average across all the days that
contains the maximum number of steps:

    ##  Create a data frame that returns the average number of steps by interval across all days.
    ##  Create a data frame that returns the max number of steps by interval across all days.
    ##  Calculate the maximum steps (max_steps) by taking the max of the max_steps_df "steps" column.
    ##  Calculate the interval (max_interval) that contains the maximum steps of the max_steps_df.
        
        mean_steps_by_interval_df <- aggregate(steps~interval,data=activity_df,mean,na.rm=TRUE)
        max_steps_df <- aggregate(steps~interval,data=mean_steps_by_interval_df,max,na.rm=TRUE)
        max_steps <- max(max_steps_df$steps)
        max_interval <- max_steps_df[max_steps_df$steps==max_steps,1]

*Maximum number of steps in a 5-min interval = 206.17*  
*5-min interval with maximum number of steps = 835*

    ##  Create a time series plot (type="l") for average steps taken per interval for all days.

        plot(mean_steps_by_interval_df$interval,mean_steps_by_interval_df$steps,type="l",
             main="Average # of Steps by Interval",xlab = "Interval",
             ylab = "Average # of Steps", col="blue")

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

### **Imputing Values**

Calculate the total number of missing values in the original
activity.csv dataset:

    ##  Calculate and report the total number of missing values in the dataset (rows = NA)
        
        rows_NA <- sum(is.na(activity_df))

*Total number of missing values (NAs) = 2304*

Create a new imputed dataset using the average steps per interval. A
sample of imputed data is shown below.

    ##  Create a new data frame that imputes missing data using the mean of the interval across all of the dataset.

        imputed_df <- ddply(activity_df,"interval",mutate,imputed.value=impute(steps,mean))
        head(imputed_df)

    ##   steps       date interval imputed.value
    ## 1    NA 2012-10-01        0      1.716981
    ## 2     0 2012-10-02        0      0.000000
    ## 3     0 2012-10-03        0      0.000000
    ## 4    47 2012-10-04        0     47.000000
    ## 5     0 2012-10-05        0      0.000000
    ## 6     0 2012-10-06        0      0.000000

Calculate and report the mean and median number of steps per day for the
imputed dataset:

    ##  Create a data frame that returns the total number of steps by date across all days for imputed dataset.
        
        sum_steps_by_day_imputed_df <- aggregate(imputed.value~date,data=imputed_df,sum,na.rm=TRUE)

Calculate and report the mean and median number of steps per day for the
imputed dataset:

    ##  Calculate the mean and median number of steps taken per day for the imputed dataset.
        mean(sum_steps_by_day_imputed_df$imputed.value)
        median((sum_steps_by_day_imputed_df$imputed.value))

*Mean = 10766*  
*Median = 10766*

The median of the imputed data is slightly higher but very similar to
the original dataset since the imputed data was based upon the mean over
the intervals.

    ##  Create a histogram plot showing the distribution of the total number of steps taken each day for
    ##  the imputed dataset.
        
        hist(sum_steps_by_day_imputed_df$imputed.value,main="Number of Steps per Day (Imputed Data)",
             breaks=100,ylim=c(0,10),xlab = "# of Steps (Imputed)",col="blue")

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

### **Weekday vs Weekend Activity Patterns**

Are there differences in activity patterns between weekdays and
weekends?

    ##  Create a variable (day.of.week) to determine the day of the week for each data sample
    ##  and add the new variable to the imputed dataset using the cbind function and store in a 
    ##  new dataset called combined_df.
    ##
    ##  Create weekday and weekend variables to be used to determine the type of day for each data sample.
    ##
    ##  Create a factor variable (type.of.day) with two levels ("Weekday" and "Weekend") indicating whether a
    ##  given date is a weekday or weekend day.
    ##
    ##  Add the new variable (type.of.day) to the combined_df dataset using the cbind function.
    ##
    ##  Create a data frame (mean_steps_by_interval_daytype_df) that uses the aggregate function to return the average
    ##  imputed number of steps by interval and type of day.
    ##
    ##  Subset the "Weekday" and "Weekend" datasets by using the type.of.day variable and store in the
    ##  mean_steps_by_interval_weekday_df and mean_steps_by_interval_weekend_df variables.
        
        day.of.week <- weekdays(imputed_df$date)
        combined_df <- cbind(imputed_df,day.of.week)
        weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
        weekend <- c("Saturday","Sunday")
        type.of.day <- ifelse(day.of.week %in% weekday==TRUE,"Weekday","Weekend")
        combined_df <- cbind(combined_df,type.of.day)
        mean_steps_by_interval_daytype_df <-
            aggregate(imputed.value~interval + type.of.day,data=combined_df,mean,na.rm=TRUE)
        mean_steps_by_interval_weekday_df <-
            mean_steps_by_interval_daytype_df[mean_steps_by_interval_daytype_df$type.of.day=="Weekday",]
        mean_steps_by_interval_weekend_df<-
            mean_steps_by_interval_daytype_df[mean_steps_by_interval_daytype_df$type.of.day=="Weekend",]

    ##  Create a time series panel plot of the 5-minute interval (x-axis) and the average number of steps taken, 
    ##  averaged across all weekday days or weekend days (y-axis) using the imputed data (combined_df).
        
        par(mfrow=c(2,1))
        plot(mean_steps_by_interval_weekday_df$interval,mean_steps_by_interval_weekday_df$imputed.value,type="l",
             main="Average # of Steps by Interval - Weekday",ylim=c(0,210),xlab = "Interval",
             ylab = "Average # of Steps",col="blue")
        plot(mean_steps_by_interval_weekday_df$interval,mean_steps_by_interval_weekend_df$imputed.value,type="l",
             main="Average # of Steps by Interval - Weekend",ylim=c(0,210),xlab = "Interval",
             ylab = "Average # of Steps",col="blue")

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />
*Maximum number of steps in a 5-min interval (Weekday) = 230.38*  
*Maximum number of steps in a 5-min interval (Weekend) = 166.64*
