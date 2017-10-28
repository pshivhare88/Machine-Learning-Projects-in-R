
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
library(e1071)


loans <- read.csv('loan_data.csv')
head(loans)
str(loans)

# check Missing Data
any(is.na(loans))

# Data Cleaning

loans= loans %>% mutate(credit.policy=factor(credit.policy),
                        inq.last.6mths=factor(inq.last.6mths),
                        delinq.2yrs=factor(delinq.2yrs),
                        pub.rec=factor(pub.rec),
                        not.fully.paid=factor(not.fully.paid))

#EDA
ggplot(loans,aes(not.fully.paid)) + geom_bar(color='black',fill='blue') + theme_classic()

ggplot(loans,aes(x=delinq.2yrs,y=not.fully.paid))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue')

ggplot(loans,aes(x=pub.rec,y=not.fully.paid))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') + theme_classic()

ggplot(loans,aes(x=inq.last.6mths,y=not.fully.paid))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') + theme_classic()

ggplot(loans,aes(x=not.fully.paid,y=fico))+
  geom_boxplot(aes(fill=not.fully.paid)) + theme_classic()

ggplot(loans,aes(x=not.fully.paid,y=dti))+
  geom_boxplot(aes(fill=not.fully.paid)) + theme_classic()

ggplot(loans,aes(x=not.fully.paid,y=int.rate))+
  geom_boxplot(aes(fill=not.fully.paid)) + theme_classic()

ggplot(loans,aes(x=fico))+
  geom_histogram(aes(fill=not.fully.paid),color='black') + theme_classic()

ggplot(loans,aes(x=purpose))+
  geom_bar(aes(fill=not.fully.paid),color='black',position = 'dodge') + theme_classic() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(loans,aes(x=int.rate,y=fico)) +geom_point()

ggplot(loans,aes(x=int.rate,y=fico)) +geom_point(aes(color=not.fully.paid))

#building svm model

sample=sample.split(loans$not.fully.paid,SplitRatio = 0.7)
train.loans=loans[sample==T,]
test.loans=loans[sample!=T,]

svm.model=svm(not.fully.paid ~ .,train.loans)
summary(svm.model)

# prediction Stats

train.predict=predict(svm.model,train.loans)
mean(train.predict!=train.loans$not.fully.paid)
table(train.loans$not.fully.paid,train.predict)

# bad results. We need to fix the model using tune

tune.svm=tune(svm,train.x = not.fully.paid ~ ., data = train.loans, 
              kernel='radial',ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.1,0.2,0.3)))

summary(tune.svm)

svm.model=svm(not.fully.paid ~ .,train.loans,cost=100,gamma=0.1)
summary(svm.model)

train.predict=predict(svm.model,train.loans)
mean(train.predict!=train.loans$not.fully.paid)
table(train.loans$not.fully.paid,train.predict)

test.predict=predict(svm.model,test.loans)
mean(test.predict!=test.loans$not.fully.paid)
table(test.loans$not.fully.paid,test.predict)


