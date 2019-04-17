library(MASS)
library(ISLR)
library("car")
fix(Boston)

####################################################################
#SIMPLE LINEAR REGRESSION
####################################################################
# fits predictor lstat (x) on response medv (y)
lm.fit = lm(medv~lstat, data = Boston)
summary(lm.fit)
lm.fit
names(lm.fit)

# extract coefficients of the fit
coef(lm.fit)
#confidence intervals for parameters of the fit
confint(lm.fit)

# predicts value of response medv for values of lstat equal 5,10 and 15 and 50 with conf. intevals for each value (conn. with reducible error)
predict(lm.fit, data.frame(lstat=c(5,10,15,50)), interval = "confidence")

#preditcion intervals - reducible and irreducible errors
predict(lm.fit, data.frame(lstat=c(5,10,15,50)), interval = "prediction")

# y~x
plot(formula = medv ~ lstat, data = Boston)
attach(Boston)
plot(lstat, medv, pch='+')
abline(lm.fit, col = "red", lwd = 3)
plot(1:20,1:20, pch = 1:20)

par(mfrow=c(2,2))
#diagnostisc, plot 1,1 is explained below. plot 2,2 is levarage - to find the points out of usual x range.
plot(lm.fit)

#plot 1,1 in the upper plot, plots residuals (value - fit) to check e.g. non-linearity or increaing variance
plot(predict(lm.fit), residuals(lm.fit))
#stutendized residuals - residuals/estimated residual's error. value larger than 3 can be considered outliers, e.g. line 375 has
#unusually large lstat value
plot(predict(lm.fit), rstudent(lm.fit))

#again finding point with maximum levarage
par(mfrow=c(1,1))
plot(hatvalues(lm.fit))
which.max(hatvalues((lm.fit)))

####################################################################
#MULTIPLE LINEAR REGRESSION
####################################################################

#fits medv on lstat AND age
lm.fit = lm(medv ~ lstat + age)
summary(lm.fit)
#F stats >> 1 means that at least one of the variables is correlated with the response

#fits lstat on all other variables
lm.fit = lm(medv~., data = Boston)
summary(lm.fit)


summary(lm.fit)$sigma
summary(lm.fit)$r.sq

# to check colinearity (even if none of pair of the variables is correlated, multiple variables can still be correlated). 
#VIF (variance inflation factor). VIF >= 1. VIF = 1 - complete absence of colinearity. VIF > 5 or 10 - problem with colinearity. 
vif(lm.fit)


#backward selection for parameters selection: excluding age, as it has highest p value:
lm.fit = lm(medv~.-age, data = Boston)
summary(lm.fit)
#or:
lm.fit1 = update(lm.fit, ~.-age)

# interaction (synergy effect) between variables (includes lstat, lstat*age and age), we are removing additive assumption - age and lstat are independent:
summary(lm(medv~lstat*age), data = Boston)

#non-linear transformations of the predictors:
lm.fit = lm(medv~lstat)
lm.fit2 = lm(medv~lstat + I(lstat^2))
summary(lm.fit2)
plot(medv~lstat)
#plot(lm.fit2)

#anova: analysis of variance table to compare quadratic and linear fit - it makes a hypothesis test, H0 - both fits fit the function 
#equally well. We see that F-stat is high and p-value very low - wuadratic fit is far better
anova(lm.fit, lm.fit2)
par(mfrow = c(2,2))
plot(lm.fit)
lplot(lm.fit2)

#fit 5th level polynomial, we look which degrees have p<0.05, one can see that all up to 5, but six is already higher. Fazit: fit quality is improved 
# until 5th order polynomial, one can check R2 for training also.
lm.fit5 = lm(medv ~ poly(lstat,5))
summary(lm.fit5)
par(mfrow = c(2,2))
plot(lm.fit5)

#fit with log transformation
summary(lm(medv ~ log(rm), data = Boston))

####################################################
#qualitataive predictors:
####################################################
# carseats data set:
fix(Carseats)
?Carseats

lm.fit = lm(Sales ~ .+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)
?contrasts

#creating a function :):

LoadLibraries = function(x, y){
  library(ISLR)
  library(MASS)
  print("THe libraries have been loaded")
  print(x*y)
}

LoadLibraries
LoadLibraries(3, 6)

# exercices
#1
fix(Auto)
lm.fit = lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
plot(mpg ~ horsepower, data = Auto)
abline(lm.fit, col = "red")

predict(lm.fit, data.frame(horsepower = c(98)), interval = "confidence")
predict(lm.fit, data.frame(horsepower = c(98)), interval = "prediction")
plot(predict(lm.fit), residuals(lm.fit))
#stutendized residuals - residuals/estimated residual's error. value larger than 3 can be considered outliers, e.g. line 375 has
#unusually large lstat value
plot(predict(lm.fit), rstudent(lm.fit))
#2

pairs(Auto)
# correlation excluding factor/qualitative variables
cor(subset(Auto, select=-c(name)))

lm.fit = lm(mpg ~ .-name, data = Auto)
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
plot(predict(lm.fit), rstudent(lm.fit))

library(car)
library(rms)
#colinearity
vif(lm.fit)
#for parameters with low VIF, >5 or 10 means problems with colinearity
lm.fit2 = lm(log(mpg) ~ sqrt(weight) + year:acceleration  + year + origin, data=Auto)
lm.fit = lm((mpg) ~ sqrt(weight) + year:acceleration  + year + origin, data=Auto)
summary(lm.fit2)



attach(Auto)
lm.fit3=ols(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin)
lm.fit3

summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)

#10
lm.fit = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)

library(ISLR)
library(MASS)

lm.fit = lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)


lm.fit2 = lm(Sales ~ Price + US, data=Carseats)
summary(lm.fit2)
par(mfrow = c(2,2))
plot(lm.fit2)

#hypothesis that lm.fit2 is NOT better than lm.fit, have to fit the same variables I thonl
anova(lm.fit, lm.fit2)

confint(lm.fit)

#11
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

lm.fit = lm(y ~ x+0)
summary(lm.fit)

plot(x,y)
abline(lm.fit)

lm.fit = lm(x ~ y+0)
summary(lm.fit)

lm.fit1=lm(y~x)
lm.fit2=lm(x~y)
t.1=summary(lm.fit1)$coefficients[2,3]
t.2=summary(lm.fit2)$coefficients[2,3]
round(t.1,4) == round(t.2,4)

#12
par(mfrow = c(1,1))
x=rnorm(100)
y=x+rnorm(100)
plot(x,y)
coefficients(lm(x~y+0))
coefficients(lm(y~x+0))

x=rnorm(100)
y=1*x
coefficients(lm(x~y+0))
coefficients(lm(y~x+0))

#13 
set.seed(1)
x = rnorm(100)
eps = rnorm(100, mean = 0, sd = 0.25)
y = -1 + 0.5*x + eps
x_real = 1:100
y_real = -1 + 0.5*x_real
plot(x,y)
plot(x_real,y_real)
coefficients(lm(y ~ x))
lm.fit = lm(y ~ x)
coefficients(lm.fit)
summary(lm.fit)
lm.fit2=lm(y~poly(x,2))
summary(lm.fit2)

abline(lm.fit2, col = "red")

#14 
#collinearity problem
set.seed(1)
x1 = runif(100)
?runif
x2 = 0.5*x1+rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)

cor(data.frame(y=y, x1=x1, x2=x2))
pairs(data.frame(y=y, x1=x1, x2=x2))

plot(x2,y)
lm.fit = lm(y ~ x1+x2)
summary(lm.fit)

lm.fit = lm(y ~ x1)
summary(lm.fit)

lm.fit = lm(y ~ x2)
summary(lm.fit)



x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)

lm.fit=lm(y~x1+x2)
summary(lm.fit)
lm.fit2=lm(y~x1)
summary(lm.fit2)
lm.fit3=lm(y~x2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit, main="y~x1+x2")
par(mfrow=c(2,2))
plot(lm.fit2, main="y~x1")
par(mfrow=c(2,2))
plot(lm.fit3, main="y~x2")
#When we include both x1 and x2 in the model, the point does not appear to be an outlier (between 3,-3 on residuals), 
#but does have a significant leverage value as shown on Cook's distance lines on the residuals vs leverage plot. 
#Also its leverage value is 0.4 whic is high...

#Note: according to http://www.oxfordjournals.org/our_journals/tropej/online/ma_chap5.pdf, 
#a cooks' distance over 1 is a potentail issue, and over 4 is cause for much concern.

#When we have only x1 in the model, the point does appear to be an outlier, however it does not appear to have leverage.
#With only x2 in the model, the point does not appear to be an outlier, however it does have a little more leverage,
#but not apparently too much.

#15
lm.fit.zn=lm(crim~zn,data=Boston)
summary(lm.fit.zn)
lm.fit.indus=lm(crim~indus,data=Boston)
summary(lm.fit.indus)
lm.fit.chas=lm(crim~chas,data=Boston)
summary(lm.fit.chas)
lm.fit.nox=lm(crim~nox,data=Boston)
summary(lm.fit.nox)
lm.fit.rm=lm(crim~rm,data=Boston)
summary(lm.fit.rm)
lm.fit.age=lm(crim~age,data=Boston)
summary(lm.fit.age)
lm.fit.dis=lm(crim~dis,data=Boston)
summary(lm.fit.dis)
lm.fit.rad=lm(crim~rad,data=Boston)
summary(lm.fit.rad)
lm.fit.tax=lm(crim~tax,data=Boston)
summary(lm.fit.tax)
lm.fit.ptratio=lm(crim~ptratio,data=Boston)
summary(lm.fit.ptratio)
lm.fit.black=lm(crim~black,data=Boston)
summary(lm.fit.black)
lm.fit.lstat=lm(crim~lstat,data=Boston)
summary(lm.fit.lstat)
lm.fit.medv=lm(crim~medv,data=Boston)
summary(lm.fit.medv)

par(mfrow=c(1,1))
plot(Boston$zn, Boston$crim)
abline(lm.fit.zn, col="red",lwd=3)

plot(Boston$indus, Boston$crim)
abline(lm.fit.indus, col="red",lwd=3)

lm.fit.all=lm(crim~.,data=Boston)
summary(lm.fit.all)


uni.coef <- c(coefficients(lm.fit.zn)[2],
              coefficients(lm.fit.indus)[2],
              coefficients(lm.fit.chas)[2],
              coefficients(lm.fit.nox)[2],
              coefficients(lm.fit.rm)[2],
              coefficients(lm.fit.age)[2],
              coefficients(lm.fit.dis)[2],
              coefficients(lm.fit.rad)[2],
              coefficients(lm.fit.tax)[2],
              coefficients(lm.fit.ptratio)[2],
              coefficients(lm.fit.black)[2],
              coefficients(lm.fit.lstat)[2],
              coefficients(lm.fit.medv)[2])

multi.coef <- coefficients(lm.fit.all)[-1] #discard intercept

plot(uni.coef,multi.coef)


lm.fit.zn=lm(crim~poly(zn,3),data=Boston)
summary(lm.fit.zn)
lm.fit.indus=lm(crim~poly(indus,3),data=Boston)
summary(lm.fit.indus)
