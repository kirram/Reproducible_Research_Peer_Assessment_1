---
  title: "Assignment1"
author: "Kirram"
date: "October 17, 2015"
output: html_document
  ---
    
    This report was produced to represent answers to the questions in the Peer Assessment 1 of Reproducible Research Class.
    
    Libraries required:
  
  ```{r, echo=TRUE}
library(ggplot2)
  library(plyr)
  library(Hmisc)
  ```

1.  Loading and preprocessing the data
  
  Show any code that is needed to
  
  Load the data (i.e. read.csv())
  
  ```{r, echo=TRUE}
data1<-read.csv("activity.csv")
  ```


Process/transform the data (if necessary) into a format suitable for your analysis
  
  For question 1, we need to sum up numbers of steps for each day:
    
    ```{r, echo=TRUE}
  data_q1<-ddply(data1, .(date),summarise, SumSteps=sum(steps))
    
    ```
  
  For question 2, we need to average numbers of steps for each interval:
    
    ```{r, echo=TRUE}
  data_q2<-ddply(data1, .(interval), summarise, Mean=mean(steps, na.rm = TRUE)) 
    
    ```
  
  For question 3, we need to replace the NAs with the means:
    
    ```{r echo=TRUE}
  data_q3<-data1
    data_q3$steps<-impute(data_q3$steps, fun=mean)
    data_q3$steps<-as.numeric(data_q3$steps)
    ```
  
  For question 4:
    
    ```{r, echo=TRUE}
  data_q4<-data_q3
    data_q4$dateType<- ifelse(as.POSIXlt(data_q4$date)$wday %in% c(0,6), "weekend", "weekday")
    ```
  
  
  Q1. What is mean total number of steps taken per day?
  
  For this part of the assignment, you can ignore the missing values in the dataset.
  
  Make a histogram of the total number of steps taken each day
    
    
    ```{r, echo=TRUE}
  q1<-ggplot(data_q1, aes(x=date, y=SumSteps))+geom_histogram(stat = "identity")
    q1
    ```
  
  Calculate and report the mean and median total number of steps taken per day
    Mean: 
    
    ```{r, echo=TRUE}
  mean_q1<-mean(data_q1$SumSteps, na.rm = TRUE)
    mean_q1
    ```
  
  Median:
    ```{r,echo=TRUE}
  median_q1<-median(data_q1$SumSteps, na.rm=TRUE)
    median_q1
    ```
  
  
  
  Q2. What is the average daily activity pattern?
  
  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average      number of steps taken, averaged across all days (y-axis)
    ```{r, echo=TRUE}
  q2<-ggplot(data_q2)+geom_freqpoly(aes(x=interval,y=Mean), stat="identity")  
    q2
    ```
  
  Which 5-minute interval, on average across all the days in the dataset, contains the maximum     number of steps?
  
  ```{r, echo=TRUE}
  
  data_q2_sorted<-arrange(data_q2, by=Mean, decreasing=TRUE)
    data_q2_max<-data_q2_sorted[1:1,]
  data_q2_max
    ```
  
  
  Q3. 
  
  1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    
    ```{r, echo=TRUE}
  na_sum<-sum(is.na(data1$steps))
    na_sum
    ```
  
  2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  
  For this task we'll compute averages of steps and replace the NAs with the means
    
    3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
  
  ```{r echo=TRUE}
  data_q3<-data1
    data_q3$steps<-impute(data_q3$steps, fun=mean)
    data_q3$steps<-as.numeric(data_q3$steps)
    ```
  
  4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
  ```{r, echo=TRUE}
  q3<-ggplot(data_q3, aes(x=date, y=steps))+geom_histogram(stat = "identity")
    q3
    ```
  ```{r, echo=TRUE}
  data_q3_4<-ddply(data_q3, .(date),summarise, SumSteps=sum(steps))
    mean_q3_4<-mean(data_q3_4$SumSteps, na.rm=TRUE)
    mean_q3_4
    ```
  ```{r,echo=TRUE}
  median_q3_4<-median(data_q3_4$SumSteps, na.rm=TRUE)
    median_q3_4
    ```
  
  As can be seen, the median value is different to what it was in q1 and now equals the mean. To conclude, replacing the NAs with the means has a smoothing effect and decreases/eliminates the difference between the mean and median.
  
  Q5. 
  
  Are there differences in activity patterns between weekdays and weekends?
  
  For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
  
  1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
  
  ```{r, echo=TRUE}
  data_q4<-data_q3
    data_q4$dateType<- ifelse(as.POSIXlt(data_q4$date)$wday %in% c(0,6), "weekend", "weekday")
    ```
  
  2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
  
  ```{r,echo=TRUE}
  data_q4<- aggregate(steps ~ interval + dateType, data=data_q4, mean)
    ggplot(data_q4, aes(interval, steps)) + 
      geom_line() + 
      facet_grid(dateType ~ .) +
      xlab("5-minute interval") + 
      ylab("avarage number of steps")
    ```
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  