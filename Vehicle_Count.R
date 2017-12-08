setwd("C:/Users/sebastian.pathrose/Desktop/Analytics/vehicle_count")

train = read.csv("train.csv")


summary(train)

train_new = train

str(train_new)

train_new$datetime = as.POSIXct(train_new$datetime, format="%Y-%m-%d %H:%M:%S")
train_new$season = as.factor(train_new$season)
train_new$holiday = as.factor(train_new$holiday)
train_new$workingday = as.factor(train_new$workingday)
train_new$weather = as.factor(train_new$weather)



?strftime
##day
train_new$day =  strftime(train_new$datetime, '%u')
train_new$day <- as.factor(train_new$day)

##month

train_new$hour =  strftime(train_new$datetime, '%H')
train_new$hour <- as.factor(train_new$hour)

str(train_new)
train_new = train_new[,-c(1,10,11)]
str(train_new)

########exploratory
install.packages("sqldf")
library(sqldf)
?sqldf
library(ggplot2)
table(mean(train_new$count),train_new$season)
mean_season = with(train_new,tapply(train_new$count,train_new$season,mean))
mean_hour = with(train_new,tapply(train_new$count,train_new$hour,mean))
plot(mean_hour,type = "h")

season_summary_by_hour <- sqldf('select weather, hour, avg(count) as count from train_new group by weather, hour')

ggplot(train_new, aes(x=hour, y=count, color=weather))+
  geom_point(data = season_summary_by_hour, aes(group = weather))+
  geom_line(data = season_summary_by_hour, aes(group = weather))+
  ggtitle("Bikes Rent By weather")

#########Splitting the Train dataset

?set.seed
?subset
library(caTools)
?caTools
split <- sample.split(train_new$count, SplitRatio = 0.75)
training_set <- subset(train_new, split == TRUE)
validation_set <- subset(train_new, split == FALSE)

summary(training_set)
summary(validation_set)

####model creation

lmBikeRent <- lm(count~., data = training_set)
summary(lmBikeRent)
plot(lmBikeRent)
par(mfrow = c(2, 2))

library(MASS)
?stepAIC
lmBikeRentAIC<-stepAIC(lmBikeRent, direction="both")

summary(lmBikeRentAIC)


#######prediction

install.packages("Metrics")
library(Metrics)
lm_predict_validation = predict(lmBikeRentAIC, newdata = training_set)


Output2Mod = lm_predict_validation
Output2Mod[lm_predict_validation<=0] <-1

summary(Output2Mod)

validaion_rmse = rmse(training_set$count,lm_predict_validation)
validaion_rmse  = rmse(training_set$count,Output2Mod)
validaion_rmsl = rmsle(training_set$count,Output2Mod)
print(validaion_rmse)
print(validaion_rmsl)

lmBikeRentLog =  lm(log(count)~., data = training_set)  
lmBikeRentLogAIC = stepAIC(lmBikeRentLog, direction="both")

lm_predict_validation_log = predict(lmBikeRentLogAIC,newdata=training_set)
lm_predict_validation_nonlog = exp(lm_predict_validation_log)

validaion_rmse = rmsle(training_set$count,lm_predict_validation_nonlog)
print(validaion_rmse)

lm_predict_test_log = predict(lmBikeRentLogAIC,newdata=training_set)
lm_predict_test_nonlog = exp(lm_predict_test_log)

