#Loading and preprocessing the data
Activity_data<-read.csv("activity.csv",stringsAsFactors = FALSE)
Activity_data$date<-as.Date(Activity_data$date, "%Y-%m-%d")

#What is mean total number of steps taken per day?
Activity_data_clean<-Activity_data[!is.na(Activity_data$steps),]
step_per_day<-tapply(Activity_data_clean$steps,Activity_data_clean$date,sum)

#step<-data.frame(sum=tapply(Activity_data_clean$steps,Activity_data_clean$date,sum))


dates_data<-as.Date(levels(factor(Activity_data_clean$date)), "%Y-%m-%d")

png("plot1.png", width=480, height=480)
plot(dates_data,step_per_day, ylab = "Total number of steps")

barplot(step_per_day, ylab = "Total number of steps")
hist(step_per_day, breaks = 10 , xlab = "Total number of steps per day",main="Histogram: steps per day")
dev.off()
summary(step_per_day)

# What is the average daily activity pattern? 
step_mean_5min<-tapply(Activity_data_clean$steps,Activity_data_clean$interval,mean)
time_5min<-levels(factor(Activity_data_clean$interval))
plot.ts(time_5min,step_mean_5min,type = "l",ylab="average steps per 5 min", xlab="5-min hrmin")

# To show the 5-min that has the highest average steps
max_step_mean_5min<-max(step_mean_5min)
Time_step_5min<-names(step_mean_5min[step_mean_5min==max(step_mean_5min)])
abline(v=as.numeric(Time_step_5min), col="blue")

#Imputing missing values
summary(Activity_data)
mean(is.na(Activity_data))
sum((is.na(Activity_data))==TRUE)

step_mean_day<-tapply(Activity_data_clean$steps,Activity_data_clean$date,mean)
step_mean_day_w_NA<-tapply(Activity_data$steps,Activity_data$date,mean)
## There are some days are completely missing so maybe it's better to replace them with average over all
step_mean_day_w_NA[is.na(step_mean_day_w_NA)]<-mean(step_mean_day)
## Now we replace the missing data with the average day which is in step_mean_day_w_NA 
Activity_data_W_NA<-Activity_data
Activity_data_W_NA$steps[is.na(Activity_data_W_NA$steps)]<-step_mean_day_w_NA[as.character(Activity_data_W_NA$date[is.na(Activity_data_W_NA$steps)])]

dates_data_W_NA<-as.Date(levels(factor(Activity_data_W_NA$date)), "%Y-%m-%d")

step_per_day_W_NA<-tapply(Activity_data_W_NA$steps,Activity_data_W_NA$date,sum)

png("plot4.png", width=480, height=480)
plot(dates_data_W_NA,step_per_day_W_NA, ylab = "Total number of steps")

barplot(step_per_day_W_NA, ylab = "Total number of steps")
hist(step_per_day_W_NA, breaks = 10 , xlab = "Total number of steps per day",main="Histogram: steps per day")
dev.off()

summary(step_per_day_W_NA)

## Imputing missing values deosn't change the min, mean, and max but it changes the median


#Are there differences in activity patterns between weekdays and weekends?
dates_data_W_NA_weekD<-Activity_data_W_NA[weekdays(Activity_data_W_NA$date)!="Saturday"|weekdays(Activity_data_W_NA$date)!="Sunday",]
dates_data_W_NA_weekD1<-subset(Activity_data_W_NA,weekdays(Activity_data_W_NA$date)!="Saturday"|weekdays(Activity_data_W_NA$date)!="Sunday")
Activity_data_W_NA$day<-as.factor(weekdays(Activity_data_W_NA$date))

Activity_data_weekday<-Activity_data_W_NA[(Activity_data_W_NA$day!="Saturday"&Activity_data_W_NA$day!="Sunday"),]

step_mean_5min_weekday<-tapply(Activity_data_weekday$steps,Activity_data_weekday$interval,mean)
time_5min_weekday<-levels(factor(Activity_data_weekday$interval))

Activity_data_weeked<-Activity_data_W_NA[(Activity_data_W_NA$day=="Saturday"|Activity_data_W_NA$day=="Sunday"),]

step_mean_5min_weeked<-tapply(Activity_data_weeked$steps,Activity_data_weeked$interval,mean)
time_5min_weeked<-levels(factor(Activity_data_weeked$interval))

par(mfrow=c(2,1),mar=c(4,4,1,1))
plot.ts(time_5min_weeked,step_mean_5min_weeked,type = "l",ylab="Weekend", xlab="5-min hrmin")
plot.ts(time_5min_weekday,step_mean_5min_weekday,type = "l",ylab="Weekday", xlab="5-min hrmin")

