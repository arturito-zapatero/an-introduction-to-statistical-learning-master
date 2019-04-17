library(ISLR)
library(boot)
data(Wage)

set.seed(123)
errors = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(wage~poly(age, i), data=Wage)
  errors[i] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}
errors

plot(1:10, errors, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))

sd.points = sd(errors)
min.points = min(errors)
abline(h = min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h = min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")
# The third model seems to give a reasonable score as well as being simple. 


fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)
# The second and third order polynomials are both significant at the 0.1% level. 

plot(wage ~ age, data = Wage, col="darkgrey")
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.fit = lm(wage ~ poly(age, 3), data=Wage)
lm.pred = predict(lm.fit, data.frame(age=age.grid))
lines(age.grid, lm.pred, col="blue", lwd=2)




all.cvs = rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut = cut(Wage$age, i)
  lm.fit = glm(wage~age.cut, data=Wage)
  all.cvs[i] = cv.glm(Wage, lm.fit, K=10)$delta[2]
}
plot(2:10, all.cvs[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2)
l = cut(Wage$age, 3)
head(Wage)
# The test error is at a minimum for 8 cuts. 

lm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.pred = predict(lm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)

# -----------------------------------------------
library(MASS)
data(Boston)

attach(Boston)
lm.fit = lm(nox~poly(dis, 3), data=Boston)
summary(lm.fit)
dislim = range(dis)
dis.grid = seq(from=dislim[1], to=dislim[2], by=0.1)
lm.pred = predict(lm.fit, list(dis=dis.grid))
plot(nox~dis, data=Boston, col="darkgrey")
lines(dis.grid, lm.pred, col="red", lwd=2)

rss = rep(NA, 10)
for (i in 1:10) {
  lm.fit = lm(nox ~ poly(dis, i), data=Boston)
  rss[i] = sum(lm.fit$residuals^2)
}

rss

plot(1:10, rss, type="l", pch=2, lwd=2)
# Train RSS monotonically decreasing with increased degree of polynomials. 

cvs = rep(NA, 10)

for (i in 1:10) {
  lm.fit = glm(nox ~ poly(dis, i), data=Boston)
  cvs[i] = cv.glm(Boston, lm.fit,  K=10)$delta[2]
}
cvs
plot(1:10, cvs, type="l", pch=2, lwd=2)
# 4 seems like it gives the best polynomial. 

library(splines)
sp.fit = lm(nox~bs(dis, df=4, knots=c(4, 7, 11)), data=Boston)
summary(sp.fit)
sp.pred = predict(sp.fit, list(dis=dis.grid))
plot(nox~dis, data=Boston, col="darkgrey")
lines(dis.grid, sp.pred, col="red", lwd=2)
# Plot shows the spline fits well, only gets wiggly past 10. 

all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = lm(nox~bs(dis, df=i), data=Boston)
  all.cv[i] = sum(lm.fit$residuals^2)
}
all.cv[-c(1, 2)]

all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(nox~bs(dis, df=i), data=Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K=10)$delta[2]
}
plot(3:16, all.cv[-c(1, 2)], lwd=2, type="l", xlab="df", ylab="CV error")

# ---------------------------------------------
#10
set.seed(1)
library(ISLR)
library(leaps)
attach(College)
train = sample(length(Outstate), length(Outstate)/2)
test = -train
College.train = College[train, ]
College.test = College[test, ]

reg.fit = regsubsets(Outstate~., data=College.train, nvmax=17, method="forward")
reg.summary = summary(reg.fit)
par(mfrow=c(1, 3))

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h=min.cp+0.2*std.cp, col="red", lty=2)
abline(h=min.cp-0.2*std.cp, col="red", lty=2)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')

min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h=min.bic+0.2*std.bic, col="red", lty=2)
abline(h=min.bic-0.2*std.bic, col="red", lty=2)
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R2",type='l', ylim=c(0.4, 0.84))

max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h=max.adjr2+0.2*std.adjr2, col="red", lty=2)
abline(h=max.adjr2-0.2*std.adjr2, col="red", lty=2)
# All cp, BIC and adjr2 scores show that size 6 is the minimum size for the subset for which the scores are
# withing 0.2 standard deviations of optimum. We pick 6 as the best subset size and find best 6 variables using entire data.

reg.fit = regsubsets(Outstate~., data=College, method="forward")
coefi = coef(reg.fit, id=6)
names(coefi)

library(gam)
gam.fit = gam(Outstate ~ Private + s(Room.Board, df=2) + s(PhD, df=2) + s(perc.alumni, df=2) + s(Expend, df=5) + s(Grad.Rate, df=2), data=College.train)
par(mfrow=c(2, 3))
plot(gam.fit, se=T, col="blue")

gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err
gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err / gam.tss
test.rss
# We obtain a test R-squared of 0.77 using GAM with 6 predictors.
# This is a slight improvement over a test RSS of 0.74 obtained using OLS.

summary(gam.fit)
# Non-parametric Anova test shows a strong evidence of non-linear relationship between response and Expend, 
# and a moderately strong non-linear relationship (using p value of 0.05) between response and Grad.Rate or PhD.

#Problem 11

n=100
beta0_t=5
beta1_t=-0.55
beta2_t=1.35
set.seed(10)
x1=rnorm(n,sd=1.1)
x2=rnorm(n,sd=2.3)
e=rnorm(n,mean=0,sd=0.5)
y=x1*beta1_t+x2*beta2_t+beta0_t+e

res=matrix(NA,3,1000,dimnames=list(c("B0","B1","B2"),paste(1:1000)))

b0hat=150
b1hat=100
b2hat=-100

for(i in seq(1000)){
  a = y-b1hat*x1
  b2hat = lm(a~x2)$coef[2]
  a = y-b2hat*x2
  fit2 = lm(a~x1)
  b1hat = fit2$coef[2]
  b0hat = fit2$coef[1]
  res["B0",i]=b0hat
  res["B1",i]=b1hat
  res["B2",i]=b2hat
}

res[,c(1,2,3,4,5,6,7,8,9,1000),drop=F]

r=range(res)
plot(seq(1000),res[1,],type="l",lwd=3,ylim=r,col="blue",xlab="iteration",ylab="coefficient estimate")
lines(res[2,],lwd=3,col="red")
lines(res[3,],lwd=3,col="green")
legend("topright", 
       c("B0","B1","B2"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(3,3,3),col=c("blue","red","green")) # gives the legend lines the correct color and width
fit=lm(y~x1+x2)
abline(h=fit$coef[1],lty=3,lwd=1,col="blue")
abline(h=fit$coef[2],lty=3,lwd=1,col="red")
abline(h=fit$coef[3],lty=3,lwd=1,col="green")
summary(lm(y~x1+x2))
#In this dataset by the 3rd iteration results were nearly as good as they would get, and by the fourth they were pretty much converged.

#Problem 12

n=100
p=100
set.seed(5)
bhats=rnorm(101,sd=100)
btarg=rnorm(101,sd=10)
X=cbind(rep(1,100),matrix(rnorm(100*100),100,100))
e=rnorm(n,mean=0,sd=0.5)
res=matrix(NA,101,1000,dimnames=list(paste(0:100),paste(1:1000)))

## rows from left dot product with columns from right
y=as.vector(btarg%*% t(X))+e
for(i in seq(1000)){
  for (j in 2:101){
    a=y-as.vector((bhats[-j]%*% t(X[,-j])))
    bhats[j]=lm(a~X[,j,drop=T])$coef[2]
  }
  bhats[1]=mean(y-as.vector(bhats[-1]%*% t(X[,-1])))
  res[,i]=bhats
}

rmse_betas=apply(res,2,function(c)mean(sqrt((btarg-c)^2)))

plot(1:1000,rmse_betas,type="l",col="blue",lwd=2,main="Beta RMSE by iteration",xlab="iteration",ylab="Coefficient RMSE")

which.min(rmse_betas)
min(rmse_betas)
#As you can see, the RMSE on the betas is decreasing as a function of the iteration. The minimal value is at iteration 1000 
#(I stopped it there due to runtime), and it decreases slowly but steadily until then.