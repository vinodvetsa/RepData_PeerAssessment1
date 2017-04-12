#Read 
activity <- read.csv("activity.csv")
cleanActivity <- na.omit(activity)
#activity$steps <- sapply(activity$steps, FUN = function(x){if(is.na(x)){0}else{x}})
#activity$date <- as.POSIXlt(as.character(cleanActity$date))
#filterActivity <- imputeActivity[ imputeActivity$steps!=0, ]
#stepsActivity <- filterActivity[, 1:2]
library(plyr)
summaryActivity <- ddply(cleanActivity, .(date), function(x)c(sum(x[,1]), median(x[,1]), mean(x[,1])))
colnames(summaryActivity)[2:4] <- c("sum", "median", "mean")
hist(summaryActivity$sum, main = "Histogram of the total number of steps taken each day", xlab = "Total Number Of Steps")

meanOfSteps <- mean(summaryActivity$sum)
medianOfSteps <- median(summaryActivity$sum)

stepsPerDay <- ddply(cleanActivity, .(interval), function(x){mean(x[,1])})
colnames(stepsPerDay)[2]<-"Average.Steps"
plot(stepsPerDay$interval, stepsPerDay$Average.Steps, type="l", xlab = "Interval", ylab = "Average Steps")

boolSteps <- is.na(activity$steps)
rowIdswithNASteps <- which(boolSteps)

imputeFunction <- function(stepsPerDay, activity){
        refActivity <- activity
        refStepsPerDay <- stepsPerDay
        
        getImputeValue<-function(interval){
                refStepsPerDay[refStepsPerDay$interval == interval, ][[2]]
        }
        getValue <- function(index){
                refActivity[index,1]
        }
        
        impute<-function(index){
                
                returnValue = imputeValue$getValue(index)
                
                if(is.na(returnValue)){
                        imputeValue$getImputeValue(refActivity[index, 3])
                }
                else {returnValue}
        }
        
        list( getImputeValue = getImputeValue, getValue=getValue , impute = impute)
}
imputeValue <- imputeFunction(stepsPerDay = stepsPerDay, activity = activity)



newSteps <- sapply(rownames(activity) , imputeValue$impute)
newActivity <- cbind(activity, newSteps)

#nonNAActivity <- activity[ (is.na(activity$steps) == FALSE),]
#nonZeroNAActivity <- nonNAActivity[ nonNAActivity$steps != 0 , ]
#naActivity <- activity[is.na(activity$steps), ]



summaryImputedActivity <- ddply(newActivity, .(date), function(x)c(sum(x[,4]), median(x[,4]), mean(x[,4])))
colnames(summaryImputedActivity)[2:4] <- c("sum", "median", "mean")
hist(summaryImputedActivity$sum, main = "Post Impute Histogram of the total number of steps taken each day", xlab = "Total Number Of Steps")
meanOfImputedSteps <- mean(summaryImputedActivity$sum)
medianOfImputedSteps <- median(summaryImputedActivity$sum)


newActivity['DayOfWeek']<- weekdays(as.Date(newActivity$date))
newActivity['TypeOfDay'] <- "TypeOfDay"
newActivity$TypeOfDay[newActivity$DayOfWeek %in% c('Saturday', 'Sunday')]<- "Weekend"
newActivity$TypeOfDay[newActivity$TypeOfDay != "Weekend"]<- "Weekday"
newActivity$TypeOfDay <- as.factor(newActivity$TypeOfDay)
newActivity<- newActivity[, c("date", "interval", "newSteps","TypeOfDay", "DayOfWeek")]
library(ggplot2)
perDayWithWeekendEffect<-ddply(newActivity, .( interval, TypeOfDay), summarize, Mean=mean(newSteps))
g<- ggplot(perDayWithWeekendEffect, aes(x=interval, y=Mean.of.Steps))+geom_line()+facet_wrap(~TypeOfDay, ncol = 1)
print(g)