getwd()
setwd('C:/Users/Amanda/Documents/data')
data<- read.csv("activity.csv")
data$date<- as.Date(data$date)
stepsbyday<- tapply(data$steps, data$date, sum, na.rm=TRUE)
library(ggplot2)
qplot(stepsbyday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
medianbyday<- median(stepsbyday)
meanbyday<- mean(stepsbyday)
medianbyday
meanbyday
avg<- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(names(avg), avg, xlab="5-min interval", type="l", ylab="Average no. of steps")
maxavg<- max(avg)
maxinterval<- as.numeric(names(avg)[which(avg==max(avg))])
maxavg
maxinterval
totalna<- sum(is.na(data$steps))
totalna
imputedata<- data
imputedata$steps[which(is.na(data$steps))]<- as.vector(avg[as.character(data[which(is.na(data$steps)),3])])
stepseachday<- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
medianEachDayImputed<- median(stepseachday)
meanEachDayImputed<- mean(stepseachday)
medianEachDayImputed

