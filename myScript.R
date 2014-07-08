library(plyr)
library(lattice)
setwd('/Users/patteg/RepData_PeerAssessment1/')

data <-read.csv('./activity.csv')

# What is the mean total number of steps taken per day?

# sum daily steps taken
daily_data<-ddply(data,'date',summarise, steps=sum(steps,na.rm=TRUE))
hist(daily_data$steps, xlab='Number of Steps', 
     main='Total Number of Steps Taken Each Day')

#calc mean and median
mean(daily_data$steps, na.rm=TRUE)
median(daily_data$steps, na.rm=TRUE)


#What is the average daily activity pattern?
interval_data<-ddply(data,'interval', summarise, steps=mean(steps, na.rm=TRUE))
with(interval_data, plot(interval, steps, type='l', xlab='Time Interval', 
     ylab='Steps Taken', main='Average Steps Taken per Time Interval'))

# calc interval with highest number of steps
interval_data[which.max(interval_data$steps),]

#Imputing missing values

# number of missing values
sum(!is.na(data$steps))

imp_data<-merge(data, interval_data, by.x='interval', by.y='interval')
imp_data$steps.x<-ifelse(is.na(imp_data$steps.x), 
                         imp_data$steps.y, 
                         imp_data$steps.x)
imp_data<-imp_data[,c(2,3,1)]
colnames(imp_data)<-c('steps','date','interval')
imp_data<-imp_data[order(imp_data$date, imp_data$interval),]
row.names(imp_data)<-NULL

# 
# sum daily steps taken
daily_imp_data<-ddply(imp_data,'date',summarise, steps=sum(steps,na.rm=TRUE))
hist(daily_imp_data$steps, 
     xlab='Number of Steps', 
     main='Total Number of Steps Taken Each Day')

#calc mean and median
mean(daily_imp_data$steps, na.rm=TRUE)
median(daily_imp_data$steps, na.rm=TRUE)

# Are there differences in activity patterns between weekdays and weekends?
imp_data$dayofweek<-weekdays(as.Date(imp_data$date))

imp_data$weektype<-ifelse((imp_data$dayofweek=='Saturday' | 
     imp_data$dayofweek== 'Sunday'), 'weekend', 'weekday')

imp_data$weektype<-as.factor(imp_data$weektype)

interval_data<-ddply(imp_data,c('weektype','interval'), summarise, steps=mean(steps, na.rm=TRUE))
xyplot(steps~interval|weektype,interval_data, 
                           type='l', 
                           xlab='Time Interval', 
                           ylab='Steps Taken', 
                           main='Average Steps Taken per Time Interval',
                           layout=c(1,2))

