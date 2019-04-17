# Chaper 5 Lab: Cross-Validation and the Bootstrap
x=seq(1,100000)
y=sapply(x,function(n){1-((1-(1/n))^n)})
plot(x,y,xlab="n",ylab="Probability jth observation is in the bootstrap sample",log="x")

store=rep(NA, 10000)
for(i in 1:10000){
  store[i] = sum(sample(1:100, rep=TRUE)==4)>0 
}
mean(store)

# The Validation Set Approach
library(ISLR)
set.seed(1)
#select random 196 from vector 1:392 (length of Auto set)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

attach(Auto)

mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)

mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)

mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
library(boot)

glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# k-Fold Cross-Validation

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

# The Bootstrap
attach(Portfolio)

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

#APPLIED
#1
library(ISLR)
set.seed(1)
glm.fit=glm(default~income+balance,data=Default, family="binomial")
#Part b)

#Using the validation set approach, estimate the test error of this model. In order to do this, you must perform the following steps: i. Split the sample set into a training set and a validation set. ii. Fit a multiple logistic regression model using only the train- ing observations. iii. Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default for that individual, and classifying the individual to the default category if the posterior probability equals 0.5. iv. Compute the validation set error, which is the fraction of the observations in the validation set that are misclassified.
set.seed(1)
train=sample(nrow(Default),nrow(Default)-nrow(Default)/4)
Default.train=Default[train,]
Default.test=Default[-train,]
glm.fit=glm(default~income+balance,data=Default.train, family="binomial")
glm.probs=predict(glm.fit,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
table(glm.pred)
mean(glm.pred==Default.test$default)
#Part c)

#Repeat the process in (b) three times, using three different splits of the observations into a training set and a validation set. Com- ment on the results obtained.
set.seed(15)
train=sample(nrow(Default),nrow(Default)-nrow(Default)/4)
Default.train=Default[train,]
Default.test=Default[-train,]
glm.fit=glm(default~income+balance,data=Default.train, family="binomial")
glm.probs=predict(glm.fit,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
mean(glm.pred!=Default.test$default)

set.seed(5)
train=sample(nrow(Default),nrow(Default)-nrow(Default)/4)
Default.train=Default[train,]
Default.test=Default[-train,]
glm.fit=glm(default~income+balance,data=Default.train, family="binomial")
glm.probs=predict(glm.fit,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
mean(glm.pred!=Default.test$default)

set.seed(31)
train=sample(nrow(Default),nrow(Default)-nrow(Default)/4)
Default.train=Default[train,]
Default.test=Default[-train,]
glm.fit=glm(default~income+balance,data=Default.train, family="binomial")
glm.probs=predict(glm.fit,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
mean(glm.pred!=Default.test$default)
#Part d)

#Now consider a logistic regression model that predicts the prob- ability of default using income, balance, and a dummy variable for student. Estimate the test error for this model using the val- idation set approach. Comment on whether or not including a dummy variable for student leads to a reduction in the test error rate.
set.seed(15)
train=sample(nrow(Default),nrow(Default)-nrow(Default)/4)
Default.train=Default[train,]
Default.test=Default[-train,]
glm.fit=glm(default~income+balance+student,data=Default.train, family="binomial")
glm.probs=predict(glm.fit,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
mean(glm.pred!=Default.test$default)

set.seed(5)
train=sample(nrow(Default),nrow(Default)-nrow(Default)/4)
Default.train=Default[train,]
Default.test=Default[-train,]
glm.fit=glm(default~income+balance+student,data=Default.train, family="binomial")
glm.probs=predict(glm.fit,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
mean(glm.pred!=Default.test$default)

set.seed(31)
train=sample(nrow(Default),nrow(Default)-nrow(Default)/4)
Default.train=Default[train,]
Default.test=Default[-train,]
glm.fit=glm(default~income+balance+student,data=Default.train, family="binomial")
glm.probs=predict(glm.fit,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
mean(glm.pred!=Default.test$default)

#Problem 6.

#We continue to consider the use of a logistic regression model to predict the probability of default using income and balance on the Default data set. In particular, we will now compute estimates for the standard errors of the income and balance logistic regression co- efficients in two different ways: (1) using the bootstrap, and (2) using the standard formula for computing the standard errors in the glm() function. Do not forget to set a random seed before beginning your analysis.
#Part a)

#Using the summary() and glm() functions, determine the estimated standard errors for the coefficients associated with income and balance in a multiple logistic regression model that uses both predictors.
set.seed(1)
glm.fit=glm(default~income+balance,data=Default, family="binomial")
summary(glm.fit)$coef


#Part b)

#Write a function, boot.fn(), that takes as input the Default data set as well as an index of the observations, and that outputs the coefficient estimates for income and balance in the multiple logistic regression model.
boot.fn=function(data,index){
  coefficients(glm(default~income+balance, data=data, subset=index, family="binomial"))
}

boot.fn(Default,1:nrow(Default))

#Part c)

#Use the boot() function together with your boot.fn() function to estimate the standard errors of the logistic regression coefficients for income and balance.
library(boot)
boot(Default,boot.fn,1000)
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
 boot(data = Default, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##       original     bias    std. error
## t1* -1.154e+01 -8.008e-03   4.239e-01
## t2*  2.081e-05  5.871e-08   4.583e-06
## t3*  5.647e-03  2.300e-06   2.268e-04
#Part d)

#Comment on the estimated standard errors obtained using the glm() function and using your bootstrap function.
#These bootstrap estimates actually match up with the glm summary estimates. That is a really good sign.

#Problem 7.

#In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be used in order to compute the LOOCV test error estimate. Alterna- tively, one could compute those quantities using just the glm() and predict.glm() functions, and a for loop. You will now take this ap- proach in order to compute the LOOCV error for a simple logistic regression model on the Default data set. Recall that in the context of classification problems, the LOOCV error is given in (5.4).
#Part a)

#Fit a logistic regression model that predicts the probability of default using balance.
glm.fit=glm(default~balance,data=Default,family="binomial")
#Part b)

#Fit a logistic regression model that predicts the probability of default using balance using all but the first observation.
glm.fit2=update(glm.fit,subset=-1)
#Part c)

#Use the model from (b) to predict the default status of the first observation. You can do this by predicting that the first observation will default if P (default|balance) > 0.5. Was this observation correctly classified?
Default.test=Default[1,,drop=F]
glm.probs=predict(glm.fit2,Default.test,type="response")
glm.pred=ifelse(glm.probs>.5,"Yes","No")
mean(glm.pred==Default.test$default)
#This observation was correctly calssified.

#Part d)

#Write a for loop from i=1 to i=n, where n is the number of observations in the data set, that performs each of the following steps: i. Fit a logistic regression model using all but the ith observation to predict probability of default using balance. ii. Compute the posterior probability of default for the ith observation. iii. Use the posterior probability of default for the ith observation in order to predict whether or not the observation defaults. iv. Determine whether or not an error was made in predicting the default status for the ith observation. If an error was made, then indicate this as a 1, and otherwise indicate it as a 0.
library(multicore)
# predictions=unlist(mclapply(seq(nrow(Default)), function(i){
#   glm.fit2=update(glm.fit,subset=-i)
#   Default.test=Default[i,,drop=F]
#   glm.probs=predict(glm.fit2,Default.test,type="response")
#   glm.pred=ifelse(glm.probs>.5,"Yes","No")
#   mean(glm.pred==Default.test$default)
# },mc.cores=8))
#Part e)

#Take the average of the n numbers obtained in (d)iv in order to obtain the LOOCV estimate for the test error. Comment on the results.
# 1 - mean(predictions)
## [1] 0.0275
#Problem 8.

#We will now perform cross-validation on a simulated data set.
#Part a) Generate a simulated data set as follows:
  
  set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
#?????????In this data set, what is n and what is p? Write out the model used to generate the data in equation form.
#In this dataset, n is 100 and p is 2.

#Part b)

#Create a scatterplot of X against Y . Comment on what you find.
plot(x,y)
#x is quadratic in terms of y.

#Part c)

#Set a random seed, and then compute the LOOCV errors that result from fitting the following four models using least squares: i. Y = ??0 + ??1X + o ii. Y = ??0 + ??1X + ??2X2 + o iii. Y = ??0 +??1X +??2X2 +??3X3 +o iv. Y = ??0 +??1X +??2X2 +??3X3 +??4X4 +o.
dat=data.frame(x=x,y=y)
fit.errors = unlist(mclapply(seq(4),function(i){ 
  glm.fit.i=glm(y~poly(x,i),data=dat)
  cv.err=cv.glm(dat,glm.fit.i)
  cv.err$delta[1]
}))
names(fit.errors)<-sprintf("poly_%d",seq(4))
fit.errors
#Part d)

#Repeat c) using another random seed, and report your results. Are your results the same as what you got in c)? Why?
set.seed(131)
fit.errors = unlist(mclapply(seq(4),function(i){ 
  glm.fit.i=glm(y~poly(x,i),data=dat)
  cv.err=cv.glm(dat,glm.fit.i)
  cv.err$delta[1]
}))
names(fit.errors)<-sprintf("poly_%d",seq(4))
fit.errors
#The results are the same because LOOCV does not have a randomness factor involved, it is the same with any iteration 
#given the same undelrying data and model.

#Part e)

#Which of the models in c) had the smallest LOOCV error? Is this what you expected? Explain your answer.
#The poly(x,2) model had the smallest LOOCV error which is encouraging becuase this is what was used to generate the data!
  
#  Part f)

#Comment on the statistical significance of the coefficient estimates that results from fitting each of the models in c) 
#using least squares. Do these results agree with the conclusions drawn based on the cross-validation results?
glm.fit.i=glm(y~poly(x,4),data=dat)
summary(glm.fit.i)
#Yes when we do a poly(x,4) we see that the x and x**2 terms are the two that end up statistically significant.

#Problem 9.

#We will now consider the Boston housing data set, from the MASS library.
#Part a)

#Based on this data set, provide an estimate for the population mean of medv. Call this estimate ??^.
  library(MASS)
mu=mean(Boston$medv)
mu
#Part b)
plot(Boston$medv)
sd(Boston$medv)
#Provide an estimate of the standard error of ??^. Interpret this result. 
#Hint: We can compute the standard error of the sample mean by dividing the sample standard deviation by the square root of the number of observations.
sd(Boston$medv)/sqrt(length(Boston$medv))
#Part c)

#Now estimate the standard error of ??^ using the bootstrap. How does this compare to your answer from (b)?
boot.fn <- function(data,index){
  mean(data[index])
}
boot(Boston$medv,boot.fn,1000,parallel ="multicore")
#Part d)

#Based on your bootstrap estimate from (c), provide a 95% confidence interval for the mean of medv. 
#Compare it to the results obtained using t.test(Boston$medv). Hint: You can approximate a 95% confidence 
#interval using the formula [??^ ??? 2SE(??^), ??^ + 2SE(??^)].

t.test(Boston$medv)
mu=22.53
se=0.4016
mu-2*se
mu+2*se
#They are very similar, the bootstrap estimate is slightly tighter than the one we just calculated with 
#the mean and std error from bootstrap. (23.33 vs 23.34) the lower bound is the same. They are probably basically the same.

#Part e)

#Based on this data set, provide an estimate, $\hat\mu_{med}$, for the median value of medv in the population.
median(Boston$medv)

#Part f)

#Wenowwouldliketoestimatethestandarderrorof??^med.Unfor- tunately, there is no simple formula for computing the standard error of the median. Instead, estimate the standard error of the median using the bootstrap. Comment on your findings.
boot.fn<-function(data,index){
  median(data[index])
}
boot(Boston$medv,boot.fn,1000,parallel ="multicore")
#Interestingly the std error of the median is lower than that of the mean! Cool.

#Part g)

#Based on this data set, provide an estimate for the tenth per- centile of medv in Boston suburbs. Call this quantity ??^0.1. (You can use the quantile() function.)
quantile(Boston$medv,p=0.1)

#Part h)

#Use the bootstrap to estimate the standard error of ??^0.1. Com- ment on your findings.
boot.fn<-function(data,index){
  quantile(data[index],p=0.1)
}
boot(Boston$medv,boot.fn,1000,parallel ="multicore")
#The lower 10% of the data has a higher std error than the mean and the median, that is interesting. 
#Apparently these outliers must be more sensitive to which subset is chosen than the mean and median.