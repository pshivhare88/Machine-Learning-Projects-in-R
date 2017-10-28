
library(ISLR)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
library(rpart.plot) 
library(class)

df= College
head(df)
str(df)

# EDA
any(is.na(df))

corrplot(round(cor(df[,sapply(df,is.numeric)]),2))

ggplot(df, aes(x=Private)) + geom_bar()

ggplot(df, aes(x=Private,y=Apps)) + geom_boxplot(aes(fill=Private)) +theme_classic()
ggplot(df, aes(x=Private,y=Accept)) + geom_boxplot(aes(fill=Private)) +theme_classic()
ggplot(df, aes(x=Private,y=Enroll)) + geom_boxplot(aes(fill=Private)) +theme_classic()

ggplot(df, aes(x=Private,y=Grad.Rate)) + geom_boxplot(aes(fill=Private)) +theme_classic()


ggplot(df, aes(x=Private,y=Room.Board)) + geom_boxplot(aes(fill=Private)) +theme_classic()

ggplot(df, aes(x=Private,y=Outstate)) + geom_boxplot(aes(fill=Private)) +theme_classic()
ggplot(df, aes(x=Private,y=S.F.Ratio)) + geom_boxplot(aes(fill=Private)) +theme_classic()

ggplot(df, aes(x=Room.Board,y=Grad.Rate)) + 
  geom_point(aes(color=Private)) +theme_classic()

ggplot(df, aes(Grad.Rate)) + 
  geom_histogram(aes(fill=Private),color='black', bins = 50) +theme_classic()

ggplot(df, aes(F.Undergrad)) + 
  geom_histogram(aes(fill=Private),color='black', bins = 50) +theme_classic()

ggplot(df, aes(Enroll)) + 
  geom_histogram(aes(fill=Private),color='black', bins = 50) +theme_classic()

df$Grad.Rate=ifelse(df$Grad.Rate>100,100,df$Grad.Rate)

# split data into test and train

sample=sample.split(df$Private,SplitRatio = 0.7)
train.df=df[sample==T,]
test.df=df[sample==F,]

# Building Decision tree Model

dt.model=rpart(Private ~ . ,method = 'class', data = train.df)
summary(dt.model)
printcp(dt.model)
plot(dt.model)
text(dt.model)

prp(dt.model)

#Prediction Stats - Train

fit.prob = predict(dt.model,train.df)
Private.predict.train=factor(ifelse(fit.prob[,2]>0.5,'Yes','No'))

error.train=(mean(Private.predict.train != train.df$Private))
accuracy.train=(mean(Private.predict.train == train.df$Private))

table(train.df$Private,Private.predict.train)


#Prediction Stats - Test

fit.prob = predict(dt.model,test.df)
Private.predict.test=factor(ifelse(fit.prob[,2]>0.5,'Yes','No'))

error.test=(mean(Private.predict.test != test.df$Private))
accuracy.test=(mean(Private.predict.test == test.df$Private))

table(test.df$Private,Private.predict.test)


# Building Random forest model

rf.model=randomForest(Private ~ ., data = train.df,importance = TRUE)
summary(rf.model)

# model Stats - train

rf.model$confusion
rf.model$importance

rf.train.error=mean(rf.model$predicted!=train.df$Private)
rf.train.accuracy=1-rf.train.error

# model Stats - test

rf.test.predictions=predict(rf.model,test.df)

rf.test.error=mean(rf.test.predictions!=test.df$Private)
rf.test.accuracy=1-rf.test.error

table(test.df$Private,rf.test.predictions)

# writing a loop to choose ntree value for the model

error.train=NULL
error.test=NULL
nt=NULL
    
  for (i in 100:150){
    m1=randomForest(Private ~ .,data=train.df,ntree=i)
    error.train[i]=mean(m1$predicted!=train.df$Private)
    p1 = predict(m1,test.df)
    error.test[i]=mean(p1!=test.df$Private)
    nt[i]=i
    m1=NULL
  }
  
out=cbind.data.frame(nt=nt[100:150],error.train=error.train[100:150],error.test=error.test[100:150])

ggplot(out,aes(x=as.numeric(nt))) + geom_line(aes(y=error.train),color='blue')+ 
  geom_line(aes(y=error.test),color='green') +scale_y_continuous(limits = c(0.05,0.15))


#Building a KNN model

test.label=test.df$Private
train.label=train.df$Private

train.knn=select(train.df,-Private)
test.knn=select(test.df,-Private)

test.knn=as.data.frame(scale(test.knn))
train.knn=as.data.frame(scale(train.knn))


fix_k=function(train,test,train.class,test.class){
  prediction.k=NULL
  error=NULL
  predictions=NULL
  for(i in 1:20){
    predictions = knn(train,test,train.class,k=i)
    error[i]=as.numeric(mean(test.class != predictions))
    prediction.k[i]=i
  }
  
  out=cbind.data.frame(prediction.k,error)
  return(out)
}

y=fix_k(train.knn,test.knn,train.label,test.label)

ggplot(y,aes(prediction.k,error)) + geom_point(size=2) +
  geom_line(lty="dotted",color='red')+theme_classic()


#  final models

#Knn with k=7

predictions = knn(train.knn,test.knn,train.label,k=7)
as.numeric(mean(test.label != predictions))
table(test.label,predictions)

#random forest with ntree=120

rf.model=randomForest(Private ~ ., data = train.df ,ntree=120)
rf.test.predictions=predict(rf.model,test.df)
mean(rf.test.predictions!=test.df$Private)
table(test.df$Private,rf.test.predictions)

# Decision Tree

fit.prob = predict(dt.model,test.df)
Private.predict.test=factor(ifelse(fit.prob[,2]>0.5,'Yes','No'))

mean(Private.predict.test != test.df$Private)
table(test.df$Private,Private.predict.test)

#the best model is random forest with tree size 120
