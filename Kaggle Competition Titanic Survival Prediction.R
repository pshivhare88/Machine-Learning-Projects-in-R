


library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)  # to split the data in train and test
library(mice)
library(VIM)
library(Amelia)
library(e1071)
library(rpart.plot) 
library(rpart) 
library(class)
library(neuralnet)
library(nnet)
library(xgboost)
library(Matrix)

df.train <- 
read.csv('/Users/pawanshivhare/Desktop/R Bootcamp/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_train.csv')


df.test <- 
  read.csv('/Users/pawanshivhare/Desktop/R Bootcamp/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_test.csv')

head(df.train)
str(df.train)

# check missing values

md.pattern(df.train)
aggr_plot = aggr(df.train, col=c('navyblue','red'), 
                 numbers=TRUE, sortVars=TRUE, labels=names(data), 
                 cex.axis=1, gap=3, ylab=c("Histogram of missing data","Pattern"))

md.pattern(df.test)
aggr_plot = aggr(df.test, col=c('navyblue','red'), 
                 numbers=TRUE, sortVars=TRUE, labels=names(data), 
                 cex.axis=1, gap=3, ylab=c("Histogram of missing data","Pattern"))

# 20% of age data is missing in train dataset and Age and Fare is missing in test data


# Exploratory data Analysis

ggplot(df.train,aes(x=Survived)) + geom_bar()
ggplot(df.train,aes(x=Pclass)) + geom_bar(aes(fill=factor(Pclass))) + theme_classic()
ggplot(df.train,aes(x=Sex)) + geom_bar(aes(fill=Sex)) + theme_classic()

ggplot(df.train,aes(x=Age))+ 
  geom_histogram(alpha=0.5,fill='blue',color='black',binwidth = 2) + theme_classic()

ggplot(df.train,aes(x=Fare))+ 
  geom_histogram(alpha=0.5,fill='blue',color='black',binwidth = 10) + theme_classic()

ggplot(df.train,aes(x=Survived,y=Age)) + geom_boxplot(aes(fill=factor(Survived))) + theme_classic()

ggplot(df.train,aes(x=Survived,y=Fare)) + geom_boxplot(aes(fill=factor(Survived))) + theme_classic()

ggplot(df.train,aes(y=Survived,x=Pclass)) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Survived,x=Embarked)) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Survived,x=Sex)) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Survived,x=substr(df.train$Cabin,1,1))) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Survived,x=Parch)) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Survived,x=SibSp)) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Age,x=Pclass)) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Age,x=Sex)) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

ggplot(df.train,aes(y=Age,x=substr(df.train$Cabin,1,1))) + 
  stat_summary(fun.y=mean,geom = 'bar', color='black', fill='blue') + theme_classic()

# Data Cleaning

# Impute missing age values in train

df.train$Cabin_let=ifelse(substr(df.train$Cabin,1,1)=="","U",substr(df.train$Cabin,1,1))
df.test$Cabin_let=ifelse(substr(df.test$Cabin,1,1)=="","U",substr(df.test$Cabin,1,1))

miss_age=function(df){

means=summarize(group_by(df,Pclass,Sex),m=mean(Age,na.rm = T))
out=df$Age

for (i in 1:length(df[,'Age'])){
 if (is.na(df[i,'Age'])){
   out[i]=as.numeric(means[(means$Pclass==df[i,'Pclass']) & (means$Sex==df[i,'Sex']),'m'])
 }
}
return(out)
}

df.train$fixed.age=miss_age(df.train)
df.test$fixed.age=miss_age(df.test)

df.train=df.train %>% mutate(Survived=factor(Survived),
                             Pclass=factor(Pclass),
                             Cabin_let=factor(Cabin_let),
                             SibSp=factor(SibSp),
                             Parch=factor(Parch)) %>%
                      select(-PassengerId,-Name,-Age,-Ticket,-Cabin,-Cabin_let,-Fare)


df.test=df.test %>% mutate(Pclass=factor(Pclass),
                             Cabin_let=factor(Cabin_let),
                             SibSp=factor(SibSp),
                             Parch=factor(Parch)) %>%
                      select(-Name,-Ticket,-Age,-Cabin,-Cabin_let,-Fare)


str(df.test)
str(df.train)

# Building Model

m1 <- glm(formula=Survived ~ ., family = binomial(link='logit'),data = df.train)
summary(m1)

fit.probs=predict(m1,newdata = df.train,type='response')
Survived.predict=ifelse(fit.probs>0.5,1,0)

misClasificError <- mean(Survived.predict != df.train$Survived)
print(paste('Accuracy',1-misClasificError))

table(df.train$Survived,Survived.predict)

# Building a SVM, Random forest and NMN model

#SVM

svm.titanic=svm(Survived ~ .,data = df.train,kernel="poly",gamma=3)
summary(svm.titanic)

Survived.predict=predict(svm.titanic,df.train)
misClasificError <- mean(Survived.predict != df.train$Survived)
print(paste('Accuracy',1-misClasificError))
table(df.train$Survived,Survived.predict)

tune.svm=tune(svm,Survived ~ ., data = df.train, kernel='poly',
              ranges = list(cost=c(1,2,3),gamma=c(2.5,3,3.5)))
summary(tune.svm)

# random forest

rf.model=randomForest(Survived ~ ., data= df.train,Importance=T,ntree=100)
misClasificError <- mean(rf.model$predicted != df.train$Survived)
print(paste('Accuracy',1-misClasificError))
rf.model$confusion

rf.model$importance
summary(rf.model)

error.train=NULL
nt=NULL

for (i in 200:300){
  m1=randomForest(Survived ~ .,data=df.train,ntree=i)
  error.train[i]=mean(m1$predicted!=df.train$Survived)
  nt[i]=i
  m1=NULL
}

out=cbind.data.frame(nt=nt[200:300],error.train=error.train[200:300])


# NN model
n.data=model.matrix(~ Survived + Pclass + Sex + SibSp + fixed.age 
      + Parch + Embarked,data=df.train)
n.data=as.data.frame(n.data)

n <- names(n.data)
f <- as.formula(paste("Survived1 ~", paste(n[!n %in% c("Survived1","(Intercept)")], collapse = " + ")))
f

nn.model=neuralnet(f,data = n.data, hidden = 10,linear.output = FALSE,stepmax =1e6)
predicted.nn.values = neuralnet::compute(nn.model,n.data[,3:21])

predictions <- sapply(predicted.nn.values$net.result,round)

misClasificError <- mean(predictions != df.train$Survived)
print(paste('Accuracy',1-misClasificError))
table(df.train$Survived,predictions)

# XGBoost model

n.data=sparse.model.matrix(~ Pclass + Sex + SibSp + fixed.age 
                           + Parch + Embarked,data=df.train)
n.label=as.numeric(levels(df.train$Survived))[df.train$Survived]

dtrain <- xgb.DMatrix(data = n.data, label = n.label)

bst <- xgboost(data = dtrain, max_depth = 15, eta = 1, 
                      nthread = 2, nrounds = 30, objective = "binary:logistic")

predicted.xg.values = predict(bst,dtrain)

predictions <- ifelse(predicted.xg.values>0.5,1,0)

misClasificError <- mean(predictions != df.train$Survived)
print(paste('Accuracy',1-misClasificError))
table(df.train$Survived,predictions)


n.data=sparse.model.matrix(~ Pclass + Sex + SibSp + fixed.age 
                           + Parch + Embarked,data=df.test)

predicted.xg.values = predict(bst,n.data)

predictions <- ifelse(predicted.xg.values>0.5,1,0)
fin=as.data.frame(cbind(as.data.frame(as.matrix(n.data)),predictions,df.test$PassengerId))

#running model on test

df.test$fit.probs=predict(m1,newdata = df.test,type='response')
df.test$Survived=ifelse(df.test$fit.probs>0.5,1,0)

write.csv(fin,'/Users/pawanshivhare/Desktop/R Bootcamp/R Scripts/logistic/titanic_prediction.csv')


