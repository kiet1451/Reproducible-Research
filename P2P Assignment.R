#Import the data
report <- read.csv('activity.csv')
x4 <- aggregate(steps~date,report,sum,na.rm=T)
#historam number of steps each day
hist(x4$steps,,main='Total Number of steps each day',xlab = 'Steps each day')
#Mean and Median 
mean(x4$steps)
median(x4$steps)
#The average daily activity pattern
x3 <- aggregate(steps~interval,report,mean,na.rm=T)
plot(x3$interval,x3$steps, type='l',col =1, main='The average daily activity pattern',xlab='5-mins Time Interval',ylab='Average number of steps')
#The interval with the most step
x3$interval[which.max((x3$steps))]

#Total NA value 
sum(is.na(report$steps))

#Convert NA to mean
report$steps[is.na(report$steps)==T] <- mean(report$steps, na.rm=TRUE)
#historam number of steps each day computed
x2 <- aggregate(steps~date,report,sum)
hist(x2$steps,main='Historam number of steps each day computed',xlab = 'Steps each day')
#Mean and Median computed
mean(x2$steps)
median(x2$steps)

#Differences in activity patterns between weekdays and weekends
#Create a function to convert date to weekend and weekday
wkday <- function(dat_val) {
  wd <- weekdays(as.Date(dat_val, '%Y-%m-%d'))
  if  (!(wd == 'Saturday' || wd == 'Sunday')) {
    x <- 'Weekday'
  } 
  else {
    x <- 'Weekend'
  }
  x
}

#Factor Weekday vs Weekend
report$Day <- as.factor(sapply(report$date, wkday))
#Graph using ggplot
library(ggplot2)
#Aggregate mean of steps on Interval and Day
report_activity <- aggregate(steps~interval+Day,report,mean)
#Make the plot
g<- ggplot(report_activity, aes(interval,steps))
g <-g+ geom_line(stat = 'identity', aes(color='Day')) + facet_grid(Day~.)
g+ labs(x= '5 mins Interval', y = "Average of Steps") + ggtitle("The dataset with two levels – “weekday” and “weekend”")
#The activities during the weekend is later and less than the weekday but it is more consistent.

