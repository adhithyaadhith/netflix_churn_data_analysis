par(mfrow=c(2,2))
library(readr)
library(caret)
library(ggplot2)
library(tidyr)

netflix=read.csv('netflix_customer_churn.csv')

names(netflix)
str(netflix)
summary(netflix)
head(netflix)
tail(netflix)

is.na(netflix)
netflix=na.omit(netflix)
netflix=distinct(netflix)
netflix

table(netflix$churned)
factor(netflix$churned)

prop.table(table(netflix$churned))*100

table(netflix$churned,netflix$subscription_type)
ggplot(netflix,aes(subscription_type,fill = as.factor(churned))) + geom_bar(position = 'dodge') + ggtitle('CHURNED_RANGE BASED ON SUBSCRIPTION_TYPE')

table(netflix$gender,netflix$churned)
ggplot(netflix,aes(gender,fill = as.factor(churned))) + geom_bar(position = 'dodge') + ggtitle('CHURNED_RANGE ACCORDING TO GENDER')

table(netflix$churned , netflix$watch_hours)
ggplot(netflix,aes(x=factor(churned),y=watch_hours))+ geom_boxplot() + labs(title = 'watch_hours-churned')

table(netflix$churned, netflix$last_login_days)
ggplot(netflix,aes(x=factor(churned),y=last_login_days)) + geom_boxplot() + labs(title = 'Churn-last_login_days')

table(netflix$monthly_fee,netflix$churned)    
ggplot(netflix,aes(x=factor(churned),y=monthly_fee)) + geom_boxplot() + ggtitle('Churned-Monthly_Fees')

table(netflix$favorite_genre , netflix$churned)
ggplot(netflix,aes(favorite_genre,fill = as.factor(churned))) + geom_bar(position = 'dodge') + ggtitle('Favorite_Genre based on Churn')
ggplot(netflix,aes(x=factor(churned),y=favorite_genre)) + geom_boxplot() + ggtitle('Favorite_Genre based on Churn')

netflix_correlation=select(netflix,age,watch_hours,last_login_days,monthly_fee,number_of_profiles,avg_watch_time_per_day)
cor(netflix_correlation)
names(netflix)

#prediction_process
netflix$churned=as.factor(netflix$churned)
netflix$churned

set.seed(100)
train_index=createDataPartition(netflix$churned,p=0.8,list = FALSE)
train=netflix[train_index,]
test=netflix[-train_index,]

model=glm(churned ~ age + watch_hours + monthly_fee + number_of_profiles + avg_watch_time_per_day + subscription_type ,
          data = train,family = "binomial"  )
pred=predict(model , newdata = test , type='response')
predict_class = ifelse(pred>0.5,1,0)
confusionMatrix(as.factor(predict_class),as.factor(test$churned))
