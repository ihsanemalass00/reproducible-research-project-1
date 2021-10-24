#download file from theweb
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
#unzip data and read 
unzip("activity.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)

### I. 
#### -1st.Calculate total number of steps taken each day- ########
library(magrittr)
library(dplyr)
databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
#2nd. Histogram of total nbr of steps taking per day#
hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)

#### -3rd- Calculate and report the mean and median of the total number of steps taken per day- ######
mean(databydate$tsteps)
median(databydate$tsteps)

##### -4th- ime series plot
library(ggplot2)
databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()

##### -5th.The 5-minute interval that, on average, contains the maximum number of steps- ###########
databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]

#### II. Imputing missing values
##### 6th Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)#####
# generate the list of NA's 
missingVals <- sum(is.na(data))
missingVals

#####7th. .Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### will use the mean for the 5 -minute interval to replace all the missing values in the dataset. At the end,  we will check if all the NAs have been replaced

library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)

##### 8th.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.#####

FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)

####III. Finally: Summary of new data : mean & median#####
summary(FullSummedDataByDay)

#histogram of Full summary
hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)

#compare the mean and median of old and new data
# a) Old mean and New mean
oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
oldmean
newmean

# b)  old median and new median
oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)
oldmedian
newmedian

###IV. Answer question: are there differences in activity patterns between weekdays and weekends?###

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")

