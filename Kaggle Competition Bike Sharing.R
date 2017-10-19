
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)  # to split the data in train and test

bike.train=
    read.csv('/Users/pawanshivhare/Desktop/R Bootcamp/R Scripts/linear regression/train.csv')

bike.test=
  read.csv('/Users/pawanshivhare/Desktop/R Bootcamp/R Scripts/linear regression/test.csv')

summary(bike.train)
str(bike.train)

#feature Engineering on train and test dataset

bike.train=bike.train %>% mutate(Season.factor=factor(season),
                  Hol.factor=factor(holiday),
                  Workday.factor=factor(workingday),
                  Weather.factor=factor(weather),
                  date2=as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),
                  hour=factor(format(as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),'%H')),
                  Mon=factor(format(as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),'%m')),
                  yymon=format(as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),'%Y%m')) %>% 
                  select(-season,-holiday,-workingday,-weather,-casual,-registered)


bike.test=bike.test %>% mutate(Season.factor=factor(season),
                                 Hol.factor=factor(holiday),
                                 Workday.factor=factor(workingday),
                                 Weather.factor=factor(weather),
                                 date2=as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),
                                 hour=factor(format(as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),'%H')),
                                 Mon=factor(format(as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),'%m')),
                                 yymon=format(as.POSIXct(datetime,format='%Y-%m-%d %H:%M:%S'),'%Y%m')) %>% 
  select(-season,-holiday,-workingday,-weather)

any(is.na(bike.train))

as.date

# Exploratory data Analysis

str(bike.train)

boxplot(bike.train$count,main='Distribution of Price',col = "darkgreen")
ggplot(bike.train,aes(x=count)) +geom_histogram()

round(cor(bike.train[,sapply(bike.train,is.numeric)]),2)
corrplot(cor(bike.train[,sapply(bike.train,is.numeric)]))
corrgram(bike.train,order = T,lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

ggplot(bike.train,aes(y=count,x=Weather.factor)) + stat_summary(fun.y=mean,geom = 'bar')
ggplot(bike.train,aes(y=count,x=Hol.factor)) + stat_summary(fun.y=mean,geom = 'bar')
ggplot(bike.train,aes(y=count,x=Workday.factor)) + stat_summary(fun.y=mean,geom = 'bar')
ggplot(bike.train,aes(y=count,x=Season.factor)) + stat_summary(fun.y=mean,geom = 'bar')

ggplot(bike.train,aes(x=temp,y=count)) +geom_point(alpha=0.4,aes(color=temp)) +
  scale_color_gradient(low = 'blue',high = 'red')

ggplot(bike.train,aes(x=datetime,y=count)) +geom_point(alpha=0.4,aes(color=temp))
round(cor(bike.train[,c('count','temp')]),2)


ggplot(filter(bike.train,Workday.factor==1),aes(x=hour,y=count)) +
  geom_point(alpha=0.4,aes(color=temp),position=position_jitter(w=1, h=0)) +
  scale_color_gradientn(colours=c('red','green','yellow','blue'))


ggplot(filter(bike.train,Workday.factor==0),aes(x=hour,y=count)) +
  geom_point(alpha=0.4,aes(color=temp),position=position_jitter(w=1, h=0)) +
  scale_color_gradientn(colours=c('red','green','yellow','blue'))

ggplot(bike.train,aes(x=Season.factor,y=count)) +geom_boxplot(aes(color=Season.factor))


temp.model <- lm(count~temp,bike.train)
summary(temp.model)

temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

bike.train$hour <- sapply(bike.train$hour,as.numeric)
bike.test$hour <- sapply(bike.test$hour,as.numeric)

model <- lm(count ~ . -windspeed -Hol.factor -casual - registered -datetime -atemp -yymon -Mon -date2,bike.train )
summary(model)

bike.train$predict.count=predict(model,bike.train)
plot(model)
