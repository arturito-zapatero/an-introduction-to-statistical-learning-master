library(ISLR)
library(class)
names(Smarket)
head(Smarket)
summary(Smarket)
library(MASS)

cor(Smarket[,-9])
plot(Smarket$Lag1, Smarket$Lag2)
plot(Smarket$Today)

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef

glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]

attach(Smarket)
contrasts(Direction)

glm.pred = rep("down",1250)
glm.pred[glm.probs > 0.5] = "up"

table(glm.pred, Direction)

(507+145)/1250
mean(glm.pred == Direction)
#Direction = as.character(Direction)

train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)

Direction.2005 = Direction[!train]

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("down",252)
glm.pred[glm.probs > 0.5] = "up"
table(glm.pred, Direction.2005)

mean(glm.pred != Direction.2005)

glm.fit = glm(Direction ~ Lag1 + Lag2 , data = Smarket, family = binomial, subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("down",252)
glm.pred[glm.probs > 0.5] = "up"
table(glm.pred, Direction.2005)

mean(glm.pred != Direction.2005)

predict(glm.fit, newdata = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.1,-0.8)), type = "response")


lda.fit = lda(Direction ~ Lag1 + Lag2 , data = Smarket, subset = train)
summary(lda.fit)
lda.fit

plot(lda.fit)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)

lda.pred$posterior

#1

library(MASS)
library(ISLR)
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-ncol(Weekly)])

logit.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=Weekly)
contrasts(Weekly$Direction)
summary(logit.fit)

#checkon training set, confusion matrix
glm.probs=predict(logit.fit,Weekly,type="response")
glm.pred=rep("Down",nrow(Weekly))
glm.pred[glm.probs > 0.50]="Up"
table(glm.pred,Weekly$Direction)
mean(glm.pred==Weekly$Direction)

train=Weekly$Year <= 2008
Weekly.test=Weekly[!train,]
logit.fit = glm(Direction ~ Lag2, family=binomial, data=Weekly, subset=train)
contrasts(Weekly$Direction)
summary(logit.fit)
glm.probs=predict(logit.fit,Weekly.test,type="response")
glm.pred=rep("Down",nrow(Weekly.test))
glm.pred[glm.probs > 0.50]="Up"
table(glm.pred,Weekly.test$Direction)
mean(glm.pred==Weekly.test$Direction)

lda.fit = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.fit
lda.class=predict(lda.fit,Weekly.test)$class
table(lda.class,Weekly.test$Direction)
mean(lda.class==Weekly.test$Direction)
#This one performed identicaly to logistic regression.

#Part f

#Repeat (d) using QDA.
qda.fit = qda(Direction ~ Lag2, data=Weekly, subset=train)
qda.fit
qda.class=predict(qda.fit,Weekly.test)$class
table(qda.class,Weekly.test$Direction)
mean(qda.class==Weekly.test$Direction)
#Interestingly it seems that QDA overfit this variable. LDA/logistic regression performs better on the test data.

#Part g

#Repeat (d) using KNN with K = 1.
library(class)
train.X=Weekly[train,"Lag2",drop=F]
test.X=Weekly[!train,"Lag2",drop=F]
train.Direction=Weekly[train,"Direction",drop=T]
test.Direction=Weekly[!train,"Direction",drop=T]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)
#Part h

#Which of these methods appears to provide the best results on this data?
#KNN is totally random looking. QDA appears to overfit the data slightly more than LDA and Logistic Regression, 
#which perform equally well on the test data.

#Part i

#Experiment with different combinations of predictors, including possible transformations and interactions,
#for each of the methods. Report the variables, method, and associated confusion matrix that appears to provide 
#the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier.
#KNN with K=4 performs pretty well at 0.57.

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=4)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)

#QDA appears to perform worse as we add in more variables, with Lag1, Lag2 and Volume it goes down to 0.46.
#With Lag1 and Lag2 it is a little better, at 0.55, but still Lag2 by itself is pretty good.

qda.fit = qda(Direction ~ Lag2, data=Weekly, subset=train)
qda.fit
qda.class=predict(qda.fit,Weekly.test)$class
table(qda.class,Weekly.test$Direction)
mean(qda.class==Weekly.test$Direction)
#Logistic regression also seems to perform worse with more variables thrown in. Lag2 seems to be a pretty good fit.

train=Weekly$Year <= 2008
Weekly.test=Weekly[!train,]
logit.fit = glm(Direction ~ Lag1+Lag2+Volume, family=binomial, data=Weekly, subset=train)
contrasts(Weekly$Direction)
summary(logit.fit)
glm.probs=predict(logit.fit,Weekly.test,type="response")
glm.pred=rep("Down",nrow(Weekly.test))
glm.pred[glm.probs > 0.50]="Up"
table(glm.pred,Weekly.test$Direction)
mean(glm.pred==Weekly.test$Direction)

#11
library(MASS)
library(ISLR)
Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)
pairs(Auto[,-9])

set.seed(1)
rands <- rnorm(nrow(Auto))
test <- rands > quantile(rands,0.75)
train <- !test
Auto.train <- Auto[train,]
Auto.test <- Auto[test,]
#Part d)

#Perform LDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in 
#b). What is the test error of the model obtained?
lda.fit = lda(mpg01 ~ horsepower+weight+acceleration, data=Auto.train)
lda.fit
lda.class=predict(lda.fit,Auto.test)$class
table(lda.class,Auto.test$mpg01)
mean(lda.class==Auto.test$mpg01)
#LDA achieved 88.8% test accuracy.

#Part e)

#Perform QDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 
#in (b). What is the test error of the model obtained?

qda.fit = qda(mpg01 ~ horsepower+weight+acceleration, data=Auto.train)
qda.fit
qda.class=predict(qda.fit,Auto.test)$class
table(qda.class,Auto.test$mpg01)
mean(qda.class==Auto.test$mpg01)
#QDA performed a little better, and achieved 92.9% accuracy on the test set.

#Part f)

#Perform logistic regression on the training data in order to pre- dict mpg01 using the variables that 
#seemed most associated with mpg01 in (b). What is the test error of the model obtained?
logit.fit = glm(mpg01 ~ horsepower+weight+acceleration, family=binomial, data=Auto.train)
summary(logit.fit)
glm.probs=predict(logit.fit,Auto.test,type="response")
glm.pred=rep(0,nrow(Auto.test))
glm.pred[glm.probs > 0.50]=1
table(glm.pred,Auto.test$mpg01)
mean(glm.pred==Auto.test$mpg01)
#Recompiling this a few times, I see that the accuracy and everything fluctuates a bit. Sometimes LDA 
#and Logistic Regression do the same, sometimes Logistic Regression does a little worse, sometimes LDA a
#little better. QDA seems to fairly consistently perform the best.

#Part g)

#Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables
#that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to 
#perform the best on this data set?

set.seed(1)
train.Auto = Auto.train[,c("horsepower","weight","acceleration")]
test.Auto =  Auto.test[,c("horsepower","weight","acceleration")]
knn.pred=knn(train.Auto,test.Auto,Auto.train$mpg01,k=1)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01)

knn.pred=knn(train.Auto,test.Auto,Auto.train$mpg01,k=2)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01)

knn.pred=knn(train.Auto,test.Auto,Auto.train$mpg01,k=3)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01)

knn.pred=knn(train.Auto,test.Auto,Auto.train$mpg01,k=4)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01)

knn.pred=knn(train.Auto,test.Auto,Auto.train$mpg01,k=5)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01)

knn.pred=knn(train.Auto,test.Auto,Auto.train$mpg01,k=11)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01)
#Interestingly at least in one case, KNN with K=3 outperforms all other models. K=4 and K=5 perform similarly well.

#12
Power = function(){
  print(2^3)
}
Power()

Power2 <- function(x,a){
  print(x^a)
}

Power2(3,8)

Power3 <- function(x,a){
  return(x^a)
}
Power3(2,155)

sapply(seq(1,10), function(x) Power3(x,2))

plot(seq(1,10),
     sapply(seq(1,10), function(x) Power3(x,2)))


PlotPower <- function(x,a){
  plot(x,
       sapply(x, function(z) Power3(z,a)),
       log="y",
       main=sprintf("Plotting x vs x**%d",a),
       xlab="x",
       ylab=sprintf("x**%d",a))
}

PlotPower(1:10,3)

#13

#Using the Boston data set, fit classification models in order to predict whether a given suburb has a crime rate 
#above or below the median. Explore logistic regression, LDA, and KNN models using various subsets of the predictors. Describe your findings.

attach(Boston)
Boston$crim01 <- as.numeric(Boston$crim > median(Boston$crim))
# as.numeric converts FALSE to 0 and TRUE to 1

set.seed(1)
rands <- rnorm(nrow(Boston))
test <- rands > quantile(rands,0.75)
train <- !test
Boston.train <- Boston[train,]
Boston.test <- Boston[test,]

Boston.train.fact <- Boston.train
Boston.train.fact$crim01 <- factor(Boston.train.fact$crim01)
library(GGally)
ggpairs(Boston.train.fact, colour='crim01')
#pairs(Boston.train)

#We should explore "black"
# "ptratio" "rad" "dis" "nox, and "zn" "lstat" "rm"

########################
# Logistic Regression
glm.fit=glm(crim01~lstat+rm+zn+nox+dis+rad+ptratio+black+medv+age+chas+indus+tax, data=Boston.train)
summary(glm.fit)
#NOX,RAD,MEDV,AGE,TAX look good
glm.probs=predict(glm.fit,Boston.test,type="response")
glm.pred=rep(0,nrow(Boston.test))
glm.pred[glm.probs > 0.50]=1
table(glm.pred,Boston.test$crim01)
mean(glm.pred==Boston.test$crim01)

glm.fit=glm(crim01~nox+rad+medv+age+tax, data=Boston.train)
summary(glm.fit)

glm.probs = predict(glm.fit,Boston.test,type="response")
glm.pred = rep(0,nrow(Boston.test))
glm.pred[glm.probs > 0.50] = 1
table(glm.pred, Boston.test$crim01)
mean(glm.pred == Boston.test$crim01)

#ptratio helps a bit, but the nox*dis helps quite a bit
glm.fit=glm(crim01~nox*dis+medv*tax+rad+age, data=Boston.train)
summary(glm.fit)
glm.probs=predict(glm.fit,Boston.test,type="response")
glm.pred=rep(0,nrow(Boston.test))
glm.pred[glm.probs > 0.50]=1
table(glm.pred,Boston.test$crim01)
mean(glm.pred==Boston.test$crim01)

#indus brings it back down a bit
glm.fit=glm(crim01~nox+rad+medv+age+tax+ptratio+indus, data=Boston.train)
summary(glm.fit)

glm.probs=predict(glm.fit,Boston.test,type="response")
glm.pred=rep(0,nrow(Boston.test))
glm.pred[glm.probs > 0.50]=1
table(glm.pred,Boston.test$crim01)
mean(glm.pred==Boston.test$crim01)

#indus by itslef doesn't help much
glm.fit=glm(crim01~nox+rad+medv+age+tax+indus, data=Boston.train)
summary(glm.fit)
glm.probs=predict(glm.fit,Boston.test,type="response")
glm.pred=rep(0,nrow(Boston.test))
glm.pred[glm.probs > 0.50]=1
table(glm.pred,Boston.test$crim01)
mean(glm.pred==Boston.test$crim01)


########################
# LDA
lda.fit=lda(crim01~nox+rad+medv+age+tax+ptratio, data=Boston.train)
lda.fit
#NOX,RAD,MEDV,AGE,TAX look good, ptratio seems to help also
lda.pred=predict(lda.fit,Boston.test)$class
table(lda.pred,Boston.test$crim01)
mean(lda.pred==Boston.test$crim01)

########################
# KNN
set.seed(1)
train.Boston = Boston.train[,c("nox","rad","medv","age","tax","ptratio")]
test.Boston =  Boston.test[,c("nox","rad","medv","age","tax","ptratio")]
knn.pred=knn(train.Boston,test.Boston,Boston.train$crim01,k=1)
table(knn.pred,Boston.test$crim01)
mean(knn.pred==Boston.test$crim01)

knn.pred=knn(train.Boston,test.Boston,Boston.train$crim01,k=2)
table(knn.pred,Boston.test$crim01)
mean(knn.pred==Boston.test$crim01)

knn.pred=knn(train.Boston,test.Boston,Boston.train$crim01,k=3)
table(knn.pred,Boston.test$crim01)
mean(knn.pred==Boston.test$crim01)

knn.pred=knn(train.Boston,test.Boston,Boston.train$crim01,k=4)
table(knn.pred,Boston.test$crim01)
mean(knn.pred==Boston.test$crim01)

knn.pred=knn(train.Boston,test.Boston,Boston.train$crim01,k=5)
table(knn.pred,Boston.test$crim01)
mean(knn.pred==Boston.test$crim01)


knn.pred=knn(train.Boston,test.Boston,Boston.train$crim01,k=11)
table(knn.pred,Boston.test$crim01)
mean(knn.pred==Boston.test$crim01)


#So the best I could get LDA/logistic regression was 89%. Using the features optimized with logistic regression I was able 
#to get KNN to perform better, returning a model that got up to 92%. K=1 got to 93%, but K=3 was nearly as good, and the higher
#K might be more robust going forward.

