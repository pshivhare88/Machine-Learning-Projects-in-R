
library(ISLR)
library(class)
library(ggplot2)
library(ggthemes)
library(dplyr)

df<-Caravan
str(df)

table(df$Purchase)

any(is.na(df))

# for Knn the scales of numeric variables matters a lot since we will be calculating the distance
# of the new data point. Hence standardization of scales is very important
# for eg. if var of one column is very different from variance of the other column

var(df[,1])
var(df[,2])
purchase=df[,'Purchase']

standard.df=as.data.frame(scale(select(df,-Purchase)))

var(standard.df[,1])
var(standard.df[,2])

# First 1000 rows for test set
test.index <- 1:1000
test.data <- standard.df[test.index,]
test.purchase <- purchase[test.index]

# Rest of data for training
train.data <- standard.df[-test.index,]
train.purchase <- purchase[-test.index]

######################

#Builiding KN Models

######################

set.seed(101)

predicted.purchase <- knn(train.data,test.data,train.purchase,k=1)
misclass=mean(test.purchase != predicted.purchase)

predicted.purchase <- knn(train.data,test.data,train.purchase,k=3)
mean(test.purchase != predicted.purchase)

predicted.purchase <- knn(train.data,test.data,train.purchase,k=6)
mean(test.purchase != predicted.purchase)

#Fixing a K value

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

y=fix_k(train.data,test.data,train.purchase,test.purchase)

ggplot(y,aes(prediction.k,error)) + geom_point(size=2) +
  geom_line(lty="dotted",color='red')+theme_classic()





