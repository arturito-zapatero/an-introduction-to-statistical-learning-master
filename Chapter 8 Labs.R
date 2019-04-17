# Chapter 8 Lab: Decision Trees

# Fitting Classification Trees

library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)


plot(tree.carseats)
text(tree.carseats,pretty=0)

tree.carseats
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)

tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200


set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200

prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

# Fitting Regression Trees

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Bagging and Random Forests

library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

# Boosting

library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

#CHapter 8 conceptual

#3
gini=function(m1){
  return(2*(m1*(1-m1)))
}

ent=function(m1){
  m2=1-m1
  return(-((m1*log(m1))+(m2*log(m2))))
}

classerr=function(m1){
  m2=1-m1
  return(1-max(m1,m2))
  #return(min((1-m1),m1))
  #return(m1)
}

err=seq(0,1,by=0.01)
c.err=sapply(err,classerr)
g=sapply(err,gini)
e=sapply(err,ent)
d=data.frame(Gini.Index=g,Cross.Entropy=e)
plot(err,c.err,type='l',col="red",xlab="m1",ylim=c(0,0.8),ylab="value")
matlines(err,d,col=c("green","blue"))





















#exercises
#Problem 7

#In the lab, we applied random forests to the Boston data using mtry=6 and using ntree=25 and ntree=500. Create a plot displaying
#the test error resulting from random forests on this data set for a more comprehensive range of values for mtry and ntree. You 
#can model your plot after Figure 8.10. Describe the results obtained.
#mtry is the number of variables randomly sampled as candidates for each split. There are 13 variables to look at in the boston
#dataset. This defaults to r sqrt(13) for a dataset of this size.

library(ISLR)
library(MASS)
library(randomForest)
library(tree)

mtry=c(3,4,6)
ntree=c(10,30,50,75,100,500)
x = matrix(rep(NA,length(mtry)*length(ntree)),length(ntree),length(mtry))
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston[-train,'medv']

for(i in 1:length(ntree)){
  for(j in 1:length(mtry)){
    rf.boston = randomForest(medv~.,data=Boston,
                           subset=train,mtry=mtry[j],ntree=ntree[i],
                           importance=TRUE)
    yhat.rf = predict(rf.boston,newdata=Boston[-train,])
    err = sqrt(mean((yhat.rf-boston.test)^2))
    x[i,j] = err
  }
}

cols = c("red","green","blue","orange")

plot(ntree,x[,1],xlab="Number of trees",ylim=c(3,5),ylab="Test RMSE",col=cols[1],type='l')
for(j in 2:length(mtry)){
  lines(ntree,x[,j],col=cols[j])
}
legend("topright",sprintf("mtry=%g",mtry),lty = 1,col=cols)
#Larger trees definitely had a slight advantage. The default choice of 4 did pretty well, and perhaps bumping up that 
#a bit helps even more, especially at larger numbers of trees. The default value of 4 maximixed its performance at fewer 
#numbers of trees. Overall 6 looks like a good choice for mtry, and 500 a good choice for ntree on this dataset and train/test split.

#Problem 8

#In the lab, a classification tree was applied to the Carseats data set after converting Sales into a qualitative response
#variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable.
#Part a

#Split the data set into a training set and a test set.
set.seed(1)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
library(tree)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]
#Part b

#Fit a regression tree to the training set. Plot the tree, and interpret the results. What test error rate do you obtain?
tree.carseats=tree(Sales~.,Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
sales.est=predict(tree.carseats,Carseats.test)
test.R2=1-(sum((sales.est-Carseats.test$Sales)^2)/sum((Carseats.test$Sales-mean(Carseats.test$Sales))^2))
test.R2
#Part c

#Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test error rate?
cv.carseats=cv.tree(tree.carseats, K=10)
plot(cv.carseats$size,cv.carseats$dev,type="b")
min.carseats=which.min(cv.carseats$dev)
#8 is min
prune.carseats=prune.tree(tree.carseats,best=min.carseats)
plot(prune.carseats)
text(prune.carseats ,pretty=0)
sales.est=predict(prune.carseats,Carseats.test)
test.R2=1-(sum((sales.est-Carseats.test$Sales)^2)/sum((Carseats.test$Sales-mean(Carseats.test$Sales))^2))
test.R2
#The error rate is actually not better with the pruned tree.. interesting.

#Part d

#Use the bagging approach in order to analyze this data. What test error rate do you obtain? Use the
#importance() function to determine which variables are most important.
library(randomForest)
set.seed(1)
bag.carseats=randomForest(Sales~.,data=Carseats,subset=train,
                          mtry=ncol(Carseats)-1,importance =TRUE)
importance(bag.carseats)
varImpPlot(bag.carseats)
sales.est=predict(bag.carseats,Carseats.test)
test.R2=1-(sum((sales.est-Carseats.test$Sales)^2)/sum((Carseats.test$Sales-mean(Carseats.test$Sales))^2))
test.R2
#Part e

#Use random forests to analyze this data. What test error rate do you obtain? Use the importance() function
#to determine which variables are most important. Describe the effect of m, the number of variables considered at each 
#split, on the error rate obtained.

rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,importance=T)
importance(rf.carseats)

mtotry=2:6
errs=rep(NA,length(mtotry))
for(i in 1:length(mtotry)){
  m=mtotry[i]
  rf.carseats=randomForest(Sales~.,data=Carseats,
                           subset=train,mtry=mtotry[i],
                           importance=T)
  sales.est=predict(rf.carseats,Carseats.test)
  test.R2=1-(sum((sales.est-Carseats.test$Sales)^2)/
               sum((Carseats.test$Sales-
                      mean(Carseats.test$Sales))^2))
  errs[i]=test.R2
}
errs
#Problem 9

#This problem involves the OJ data set which is part of the ISLR package.
#Part a

#Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.
set.seed(10)
train=sample(1:nrow(OJ),800)
train.OJ=OJ[train,]
test.OJ=OJ[-train,]
#Part b

#Fit a tree to the training data, with Purchase as the response and the other variables except for Buy as predictors. 
#Use the summary() function to produce summary statistics about the tree, and describe the results obtained. What is the 
#training error rate? How many terminal nodes does the tree have?

tree.oj=tree(Purchase~.,data=train.OJ)
summary(tree.oj)
#The training error rate is 0.1625, and there are 7 terminal nodes.

#Part c

#Type in the name of the tree object in order to get a detailed text output. Pick one of the terminal nodes, 
#and interpret the information displayed.
tree.oj
#Node 4 shows the split which occures of LoyalCH is less first less than 0.45956 and then less than 0.276142. 
#The predicted outcome is MM. There is a deviance of 100. Smaller values of deviance ar indicative of how pure 
#this node is. Finally there is the probability confidence bound on this prediction.

#Part d

#Create a plot of the tree, and interpret the results.
plot(tree.oj)
text(tree.oj,pretty=0)
#Part e

#Predict the response on the test data, and produce a confusion matrix comparing the test labels to the predicted
#test labels. What is the test error rate?
preds=predict(tree.oj,test.OJ,type="class")
table(test.OJ$Purchase,preds)
test.err=(155+66)/(155+22+27+66)
test.err
#Part f

#Apply the cv.tree() function to the training set in order to determine the optimal tree size.
cv.oj=cv.tree(tree.oj,FUN=prune.misclass)
#Part g

#Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.
plot(cv.oj$size ,cv.oj$dev ,type="b")
#Part h

#Which tree size corresponds to the lowest cross-validated classification error rate?
msize=cv.oj$size[which.min(cv.oj$dev)]
msize
#Part i

#Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation.
#If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes.
prune.oj=prune.misclass(tree.oj,best=msize)
#Part j

#Compare the training error rates between the pruned and un- pruned trees. Which is higher?
prune.pred=predict(prune.oj,test.OJ,type="class")
table(prune.pred,test.OJ$Purchase)
#Part k

#Compare the test error rates between the pruned and unpruned trees. Which is higher?
prune.test.err=(151+68)/(151+68+26+25)
1-prune.test.err
1-test.err
#The classification accurazy is slightly worse in the pruned tree.

#Problem 10

#We now use boosting to predict Salary in the Hitters data set.
#Part a

#Remove the observations for whom the salary information is unknown, and then log-transform the salaries.
H = Hitters[!is.na(Hitters$Salary),,drop=F]
H$Salary = log(H$Salary)
#Part b

#Create a training set consisting of the first 200 observations, and a test set consisting of the remaining observations.
H.train=H[1:200,]
H.test=H[201:nrow(H),]
#Part c

#Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter $\lambda$. 
#Produce a plot with different shrinkage values on the x-axis and the corresponding training set MSE on the y-axis.
library(gbm)
set.seed(1)
shrinkage=c(0.00001,0.0001,0.001,0.01,0.1,1)
errs=rep(NA,length(shrinkage))
for (i in 1:length(shrinkage)){
  s=shrinkage[i]
  boost.H=gbm(Salary~., data=H.train, 
              distribution="gaussian", 
              n.trees=1000,
              shrinkage = s,
              interaction.depth=1,
              n.cores=10)
  yhat.boost=predict(boost.H,newdata=H.train, n.trees=1000)
  errs[i]=mean((yhat.boost-H.train$Salary)^2)
}
plot(log(shrinkage),errs)
#Part d

#Produce a plot with different shrinkage values on the x-axis and the corresponding test set MSE on the y-axis.
library(gbm)
set.seed(1)
errs.test=rep(NA,length(shrinkage))
for (i in 1:length(shrinkage)){
  s=shrinkage[i]
  boost.H=gbm(Salary~., data=H.train, 
              distribution="gaussian", 
              n.trees=1000,
              shrinkage = s,
              interaction.depth=1,
              n.cores=10)
  yhat.boost=predict(boost.H,newdata=H.test, n.trees=1000)
  errs.test[i]=mean((yhat.boost-H.test$Salary)^2)
}
plot(log(shrinkage),errs.test)
#Part e

#Compare the test MSE of boosting to the test MSE that results from applying two of the regression approaches seen in Chapters 3 and 6.
boost.H=gbm(Salary~., data=H.train, 
            distribution="gaussian", 
            n.trees=1000,
            shrinkage = shrinkage[which.min(errs)],
            interaction.depth=1,
            n.cores=10)

boost.mse=errs[which.min(errs)]
library(leaps)
fit=regsubsets(Salary~.,data=H.train,nvmax=19)
fit.summ=summary(fit)
to.inc=fit.summ$which[which.min(fit.summ$cp),][2:20]
features=c(features,"Division","Salary")
fit.lm=lm(Salary~.,data=H.train[,colnames(H.train)%in%features])
yhat=predict(fit.lm,H.test[,colnames(H.train)%in%features])
best.sub=mean((yhat-H.test$Salary)^2)

cols.bad=c("League","Division","NewLeague")
n.H=model.matrix(~.,H)[,-1]
n.H.train=n.H[1:200,]
n.H.test=n.H[201:nrow(n.H),]

library(glmnet)
fit=cv.glmnet(n.H.train[,colnames(n.H)!="Sallary"],n.H.train[,"Salary"])
fit=glmnet(n.H.train[,colnames(n.H)!="Sallary"],n.H.train[,"Salary"],lambda=fit$lambda.1se)
pred=predict(fit,n.H.test[,colnames(n.H)!="Sallary"])
best.lasso=mean((pred[,1]-H.test$Salary)^2)


#boost
boost.mse

#Best subset lm:
best.sub

#best lasso:
best.lasso
#the lasso is the best by a really little bit on the test data, but boosting came in close.

#Part f

#Which variables appear to be the most important predictors in the boosted model?
summary(boost.H)
#CAtBat and PutOuts were the top two predictors by a lot. Next at about half of the importance was RBI and Walks.

#Part g

#Now apply bagging to the training set. What is the test set MSE for this approach?
library(randomForest)
set.seed(1)
bag.H=randomForest(Salary~.,data=H.train,
                   mtry=ncol(H.train)-1,
                   importance=TRUE)
preds=predict(bag.H,newdata=H.test)
mean((preds-H.test$Salary)^2)
#Problem 11

#This question uses the Caravan data set.
#Part a

#Create a training set consisting of the first 1,000 observations, and a test set consisting of the remaining observations.
train.C=Caravan[1:1000,]
test.C=Caravan[1001:nrow(Caravan),]
#Part b

#Fit a boosting model to the training set with Purchase as the response and the other variables as predictors.
#Use 1,000 trees, and a shrinkage value of 0.01. Which predictors appear to be the most important?
boost.C=gbm(I(Purchase=="Yes")~., data=train.C, 
            distribution="bernoulli", 
            n.trees=1000,
            shrinkage = 0.01,
            interaction.depth=1,
            n.cores=10)

summary(boost.C)

#The most important predictors are PPEARSAUT, MOPLHOOG and MKOOPKLA, followed pretty closely by a group of others.
#Part c

#Use the boosting model to predict the response on the test data. Predict that a person will make a purchase if 
#the estimated probability of purchase is greater than 20 %. Form a confusion matrix. What fraction of the people
#predicted to make a purchase do in fact make one? How does this compare with the results obtained from applying 
#KNN or logistic regression to this data set?

preds=predict(boost.C,test.C,type="response",n.trees=1000)
yhat=ifelse(preds>.2,"Yes","No")
table(yhat,test.C$Purchase)
#the following is the fraction of people predicted to make
# a purchase who actually do
34/154
#Problem 12

#Apply boosting, bagging, and random forests to a data set of your choice. Be sure to fit the models on a training
#set and to evaluate their performance on a test set. How accurate are the results compared to simple methods like 
#linear or logistic regression? Which of these approaches yields the best performance?
