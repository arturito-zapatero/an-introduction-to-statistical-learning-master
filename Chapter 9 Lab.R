# Chapter 9 Lab: Support Vector Machines

# Support Vector Classifier

set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))
dat=data.frame(x=x, y=as.factor(y))

library(e1071)
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)

svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat)

svmfit$index
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)

svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)

# Support Vector Machine

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train=sample(200,100)

svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)

svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
set.seed(1)

tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))

# ROC Curves

library(ROCR)
rocplot = function(pred, truth, ...){
   predob = prediction(pred, truth)
   perf = performance(predob, "tpr", "fpr")
   plot(perf,...)}

svmfit.opt = svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)

fitted = attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")

svmfit.flex = svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")

fitted = attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")

fitted = attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# SVM with Multiple Classes

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Application to Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)

summary(out)
table(out$fitted, dat$y)
dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)


























#Applied
#4


#We create a random initial dataset which lies along the parabola $y = 3*x^2 + 4$. 
#We then separate the two classes by translating them along Y-axis.

#```{r 4a}
set.seed(131)
x = rnorm(100)
y = 3 * x^2 + 4 + rnorm(100)
train = sample(100, 50)
y[train] = y[train] + 3
y[-train] = y[-train] - 3
# Plot using different colors
plot(x[train], y[train], pch="+", lwd=4, col="red", ylim=c(-4, 20), xlab="X", ylab="Y")
points(x[-train], y[-train], pch="o", lwd=4, col="blue")

#The plot clearly shows non-linear separation. We now create both train and test dataframes by taking half 
#of positive and negative classes and creating a new `z` vector of 0 and 1 for classes. 
#r 4b}
set.seed(315)
z = rep(0, 100)
z[train] = 1
# Take 25 observations each from train and -train
final.train = c(sample(train, 25), sample(setdiff(1:100, train), 25))
data.train = data.frame(x=x[final.train], y=y[final.train], z=as.factor(z[final.train]))
data.test = data.frame(x=x[-final.train], y=y[-final.train], z=as.factor(z[-final.train]))
library(e1071)
svm.linear = svm(z~., data=data.train, kernel="linear", cost=10)
plot(svm.linear, data.train)
table(z[final.train], predict(svm.linear, data.train))
#The plot shows the linear boundary. The classifier makes $10$ classification errors on train data.

#Next, we train an SVM with polynomial kernel
#+```{r 4c}
set.seed(32545)
svm.poly = svm(z~., data=data.train, kernel="polynomial", cost=10)
plot(svm.poly, data.train)
table(z[final.train], predict(svm.poly, data.train))

#This is a default polynomial kernel with degree 3. It makes $15$ errors on train data.

#Finally, we train an SVM with radial basis kernel with gamma of 1.
#{r 4d}
set.seed(996)
svm.radial = svm(z~., data=data.train, kernel="radial", gamma=1, cost=10)
plot(svm.radial, data.train)
table(z[final.train], predict(svm.radial, data.train))

#This classifier perfectly classifies train data!.

#Here are how the test errors look like.
#+```{r 4e}
plot(svm.linear, data.test)
plot(svm.poly, data.test)
plot(svm.radial, data.test)
table(z[-final.train], predict(svm.linear, data.test))
table(z[-final.train], predict(svm.poly, data.test))
table(z[-final.train], predict(svm.radial, data.test))

#The tables show that linear, polynomial and radial basis kernels classify 6, 14, and 0 test points 
#incorrectly respectively. Radial basis kernel is the best and has a zero test misclassification error. 

#Chapter 9: Exercise 6

a

#We randomly generate 1000 points and scatter them across line $x = y$ with wide margin. We also create noisy 
#points along the line $5x -4y - 50 = 0$. These points make the classes barely separable and also shift the maximum margin classifier.

set.seed(3154)
# Class one 
x.one = runif(500, 0, 90)
y.one = runif(500, x.one + 10, 100)
x.one.noise = runif(50, 20, 80)
y.one.noise = 5 / 4 * (x.one.noise - 10) + 0.1

# Class zero
x.zero = runif(500, 10, 100)
y.zero = runif(500, 0, x.zero - 10)
x.zero.noise = runif(50, 20, 80)
y.zero.noise = 5 / 4 * (x.zero.noise - 10) - 0.1

# Combine all
class.one = seq(1, 550)
x = c(x.one, x.one.noise, x.zero, x.zero.noise)
y = c(y.one, y.one.noise, y.zero, y.zero.noise)

plot(x[class.one], y[class.one], col="blue", pch="+", ylim=c(0, 100))
points(x[-class.one], y[-class.one], col="red", pch=4)
#The plot shows that classes are barely separable. The noisy points create a fictitious boundary $5x - 4y - 50 = 0$.

#b

#We create a z variable according to classes.

library(e1071)
set.seed(555)
z = rep(0, 1100)
z[class.one] = 1
data = data.frame(x=x, y=y, z=z)
tune.out = tune(svm, as.factor(z)~., data=data, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)
data.frame(cost=tune.out$performances$cost, misclass=tune.out$performances$error * 1100)

#The table above shows train-misclassification error for all costs. A cost of 10000 seems to classify all points correctly.
#This also corresponds to a cross-validation error of 0.

#c

#We now generate a random test-set of same size. This test-set satisfies the true decision boundary $x = y$.

set.seed(1111)
x.test = runif(1000, 0, 100)
class.one = sample(1000, 500)
y.test = rep(NA, 1000)
# Set y > x for class.one
for(i in class.one) {
  y.test[i] = runif(1, x.test[i], 100)
}
# set y < x for class.zero
for (i in setdiff(1:1000, class.one)) {
  y.test[i] = runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col="blue", pch="+")
points(x.test[-class.one], y.test[-class.one], col="red", pch=4)
#We now make same predictions using all linear svms with all costs used in previous part.

set.seed(30012)
z.test = rep(0, 1000)
z.test[class.one] = 1
all.costs = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.errors = rep(NA, 8)
data.test = data.frame(x=x.test, y=y.test, z=z.test)
for (i in 1:length(all.costs)) {
  svm.fit = svm(as.factor(z)~., data=data, kernel="linear", cost=all.costs[i])
  svm.predict = predict(svm.fit, data.test)
  test.errors[i] = sum(svm.predict != data.test$z)
}

data.frame(cost=all.costs, "test misclass"=test.errors)
#$\tt{cost} = 10$ seems to be performing better on test data, making the least number of classification errors.
#This is much smaller than optimal value of 10000 for training data.

#d

#We again see an overfitting phenomenon for linear kernel. A large cost tries to fit correctly classify noisy-points 
#and hence overfits the train data. A small cost, however, makes a few errors on the noisy test points and performs better on test data.


#Chapter 9: Exercise 7

#a

library(ISLR)
gas.med = median(Auto$mpg)
new.var = ifelse(Auto$mpg > gas.med, 1, 0)
Auto$mpglevel = as.factor(new.var)
#b

library(e1071)
## Loading required package: class
set.seed(3255)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 
                                                                                          0.1, 1, 5, 10, 100)))
summary(tune.out)
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##     1
## 
## - best performance: 0.01269 
## 
## - Detailed performance results:
##    cost   error dispersion
## 1 1e-02 0.07404    0.04272
## 2 1e-01 0.04083    0.03009
## 3 1e+00 0.01269    0.02154
## 4 5e+00 0.01526    0.02457
## 5 1e+01 0.02038    0.02338
## 6 1e+02 0.03051    0.02332
#We see that cross-validation error is minimized for $\tt{cost}=1$.

#c

set.seed(21)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.1, 
                                                                                              1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost degree
##    10      2
## 
## - best performance: 0.5539 
## 
## - Detailed performance results:
##    cost degree  error dispersion
## 1   0.1      2 0.5871    0.06765
## 2   1.0      2 0.5871    0.06765
## 3   5.0      2 0.5871    0.06765
## 4  10.0      2 0.5539    0.09384
## 5   0.1      3 0.5871    0.06765
## 6   1.0      3 0.5871    0.06765
## 7   5.0      3 0.5871    0.06765
## 8  10.0      3 0.5871    0.06765
## 9   0.1      4 0.5871    0.06765
## 10  1.0      4 0.5871    0.06765
## 11  5.0      4 0.5871    0.06765
## 12 10.0      4 0.5871    0.06765
#The lowest cross-validation error is obtained for $\tt{cost} = 10$ and $\tt{degree} = 2$.

set.seed(463)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.1, 
                                                                                          1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost gamma
##    10  0.01
## 
## - best performance: 0.02301 
## 
## - Detailed performance results:
##    cost gamma   error dispersion
## 1   0.1 1e-02 0.08929    0.04546
## 2   1.0 1e-02 0.07667    0.04539
## 3   5.0 1e-02 0.04853    0.03303
## 4  10.0 1e-02 0.02301    0.03299
## 5   0.1 1e-01 0.07923    0.04768
## 6   1.0 1e-01 0.05628    0.03974
## 7   5.0 1e-01 0.02801    0.02232
## 8  10.0 1e-01 0.02545    0.02077
## 9   0.1 1e+00 0.50859    0.15577
## 10  1.0 1e+00 0.06397    0.04235
## 11  5.0 1e+00 0.06141    0.04395
## 12 10.0 1e+00 0.06141    0.04395
## 13  0.1 5e+00 0.55109    0.03711
## 14  1.0 5e+00 0.49481    0.04403
## 15  5.0 5e+00 0.49481    0.04566
## 16 10.0 5e+00 0.49481    0.04566
## 17  0.1 1e+01 0.55109    0.03711
## 18  1.0 1e+01 0.50763    0.04193
## 19  5.0 1e+01 0.50250    0.04261
## 20 10.0 1e+01 0.50250    0.04261
## 21  0.1 1e+02 0.55109    0.03711
## 22  1.0 1e+02 0.55109    0.03711
## 23  5.0 1e+02 0.55109    0.03711
## 24 10.0 1e+02 0.55109    0.03711
#Finally, for radial basis kernel, $\tt{cost} = 10$ and $\tt{gamma} = 0.01$.

#d

svm.linear = svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, 
               degree = 2)
svm.radial = svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)

#plot(svmfit, Auto, mpglevel~mpg)

#Chapter 9: Exercise 8

#a

library(ISLR)
set.seed(9004)
train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]
#b

library(e1071)
svm.linear = svm(Purchase~., kernel="linear", data=OJ.train, cost=0.01)
summary(svm.linear)
#Support vector classifier creates 432 support vectors out of 800 training points. Out of these,
#217 belong to level $\tt{CH}$ and remaining 215 belong to level $\tt{MM}$.

#c

train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
(82 + 53) / (439 + 53 + 82 + 226)
test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
(19 + 29) / (142 + 19 + 29 + 80)
#The training error rate is $16.9$% and test error rate is about $17.8$%.

#d

set.seed(1554)
tune.out = tune(svm, Purchase~., data=OJ.train, kernel="linear", ranges=list(cost=10^seq(-2, 1, by=0.25)))
summary(tune.out)
#Tuning shows that optimal cost is 0.3162

#e

svm.linear = svm(Purchase~., kernel="linear", data=OJ.train, cost=tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
(57 + 71) / (435 + 57 + 71 + 237)
test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
(29 + 20) / (141 + 20 + 29 + 80)
#The training error decreases to $16$% but test error slightly increases to $18.1$% by using best cost.

#f

set.seed(410)
svm.radial = svm(Purchase~., data=OJ.train, kernel="radial")
summary(svm.radial)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
(40 + 78) / (452 + 40 + 78 + 230)
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
(27 + 15) / (146 + 15 + 27 + 82)
#The radial basis kernel with default gamma creates 367 support vectors, out of which, 184
#belong to level $\tt{CH}$ and remaining 183 belong to level $\tt{MM}$. The classifier has a training error of $14.7$%
#and a test error of $15.6$% which is a slight improvement over linear kernel. We now use cross validation to find optimal gamma.

set.seed(755)
tune.out = tune(svm, Purchase~., data=OJ.train, kernel="radial", ranges=list(cost=10^seq(-2, 1, by=0.25)))
summary(tune.out)
svm.radial = svm(Purchase~., data=OJ.train, kernel="radial", cost=tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
(77 + 40) / (452 + 40 + 77 + 231)
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
(28 + 15) / (146 + 15 + 28 + 81)
#Tuning slightly decreases training error to $14.6$% and slightly increases test error to $16$% which is still better than linear kernel.

#g

set.seed(8112)
svm.poly = svm(Purchase~., data=OJ.train, kernel="poly", degree=2)
summary(svm.poly)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
(32 + 105) / (460 + 32 + 105 + 203)
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
(12 + 37) / (149 + 12 + 37 + 72)
#Summary shows that polynomial kernel produces 452 support vectors, out of which, 232 belong to 
#level $\tt{CH}$ and remaining 220 belong to level $\tt{MM}$. This kernel produces a train error of 
#$17.1$% and a test error of $18.1$% which are slightly higher than the errors produces by radial kernel
#but lower than the errors produced by linear kernel.

#h

set.seed(322)
tune.out = tune(svm, Purchase~., data=OJ.train, kernel="poly", degree=2, ranges=list(cost=10^seq(-2, 1, by=0.25)))
summary(tune.out)
svm.poly = svm(Purchase~., data=OJ.train, kernel="poly", degree=2, cost=tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
(37 + 84) / (455 + 37 + 84 + 224)
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
(13 + 34) / (148 + 13 + 34 + 75)
#Tuning reduces the training error to $15.12$% and test error to $17.4$% which is worse than radial
#kernel but slightly better than linear kernel.

#g

#Overall, radial basis kernel seems to be producing minimum misclassification error on both train and test data.  