#Problem 8

#create a simluated dataset
#Parts a-c)

set.seed(1)
X=rnorm(100,mean=0,sd=1)
e=rnorm(100,mean=0,sd=0.5)
B0=150.3
B1=50.5
B2=-10.1
B3=-34.2

Y=B0+ B1*X + B2*(X^2) + B3*(X^3) + e

dat=data.frame(Y=Y,X=X)
library(leaps)
library(ISLR)

#fit up to X^10
fit = lm(Y~poly(X,10,raw=T),data=dat)
summary(fit)
regfit.full=regsubsets(Y~poly(X,10,raw=T),data=dat,nvmax=10)

reg.summary=summary(regfit.full)
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),
       reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",
     type="l")
points(which.min(reg.summary$cp),
       reg.summary$cp[which.min(reg.summary$cp)],
       col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",
     ylab="BIC", type="l")
points(which.min(reg.summary$bic),
       reg.summary$bic[which.min(reg.summary$bic)],
       col="red",cex=2,pch=20)

# BIC
coef(regfit.full,3)
# Cp/adjusted R2
coef(regfit.full,4)
#Each method chose at least a superset of the correct X polynomials. BIC chose the correct ones (X,X^2,X^3). Cp and adjusted R^2 added in X^9

#Part d

regfit.full=regsubsets(Y~poly(X,10,raw=T),data=dat,nvmax=10,method="forward")
reg.summary=summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),
       reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",
     type="l")
points(which.min(reg.summary$cp),
       reg.summary$cp[which.min(reg.summary$cp)],
       col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",
     ylab="BIC", type="l")
points(which.min(reg.summary$bic),
       reg.summary$bic[which.min(reg.summary$bic)],
       col="red",cex=2,pch=20)

# BIC
coef(regfit.full,3)
# Cp
coef(regfit.full,4)
# Adjusted R2
coef(regfit.full,4)
#The same number of parameters were chosen by each method, however now the spurious parameters changed with Cp and
#Adjusted R2. Now Cp and adjusted R^2 use X^5 as the extra.

#Part e

#same but with lasso
library(glmnet)
dat.mat = model.matrix(Y ~ poly(X,10,raw=T),data=dat)[,-1]

cv.out=cv.glmnet(dat.mat,Y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

lasso.mod=glmnet(dat.mat,Y,alpha=1,lambda=bestlam)
coef(lasso.mod)
#Using the optimal lambda chosen by CV, lasso regression choses up to a 3nd degree polynomial (X,X^2,X^3), but includes X^5 as an extra term.

#Part f

#redo with different model and repeat c and e.
set.seed(1)
X=rnorm(100,mean=0,sd=1)
e=rnorm(100,mean=0,sd=0.5)
B0=150.3
B7=33.3


Y=B0+ B7*(X^7) + e

dat=data.frame(Y=Y,X=X)
regfit.full=regsubsets(Y~poly(X,10,raw=T),data=dat,nvmax=10)
reg.summary=summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),
       reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",
     type="l")
points(which.min(reg.summary$cp),
       reg.summary$cp[which.min(reg.summary$cp)],
       col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",
     ylab="BIC", type="l")
points(which.min(reg.summary$bic),
       reg.summary$bic[which.min(reg.summary$bic)],
       col="red",cex=2,pch=20)

# adj R^2
coef(regfit.full,4)

# Cp
coef(regfit.full,2)

# BIC
coef(regfit.full,1)
library(glmnet)
dat.mat=model.matrix(Y~poly(X,10,raw=T),data=dat)[,-1]

cv.out=cv.glmnet(dat.mat,Y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

lasso.mod=glmnet(dat.mat,Y,alpha=1,lambda=bestlam)
coef(lasso.mod)
#This model was difficult for the methods to deal with. BIC chose the correct model though, with the seventh degree polynomial
#being the only one included! Cp added in X^2, and adjusted R^2 added in X^1, X^2, and X^3.

#The lasso on the ohter hand chose two features as well, like Cp. It chose X^7 along with X^9 as the spurious feature though.

#NOTE: I redid this section after realizing in the next chapter that the poly function returns a linear combination 
#of terms, which has the interesting effect of causing the above models to chose $\beta^{1..7}$ rather than only $\beta^7$!!
#This is something to be aware of, and I completely missed this the first time though.

#Problem 9

#In this exercise, we will predict the number of applications received using the other variables in the College data set.
#Part a)

attach(College)
#Split the data set into a training set and a test set.
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(College),rep=TRUE)
test=(!train)

College.train=College[train,,drop=F]
College.test=College[test,,drop=F]
#Part b)

#Fit a linear model using least squares on the training set, and report the test error obtained.
lm.fit=lm(Apps~.,data=College.train)
summary(lm.fit)
pred = predict(lm.fit,College.test)
rss = sum((pred-College.test$Apps)^2)
tss = sum((College.test$Apps-mean(College.test$Apps))^2)
test.rsq=1-(rss/tss)
test.rsq
#where test.rsq is the $R^2$ statistic.

#Part c)

#Fit a ridge regression model on the training set, with $\lambda$ chosen by cross-validation. Report the test error obtained.
### Scale the training data, and scale the test data using the centers/scale
# learned on the training data. 
College.train.X=scale(model.matrix(Apps~.,data=College.train)[,-1],scale=T,center=T)
College.train.Y=College.train$Apps

College.test.X=scale(model.matrix(Apps~.,data=College.test)[,-1],
                     attr(College.train.X,"scaled:center"),
                     attr(College.train.X,"scaled:scale"))

College.test.Y=College.test$Apps

cv.out=cv.glmnet(College.train.X,College.train.Y,alpha=0)
bestlam=cv.out$lambda.min
bestlam

lasso.mod=glmnet(College.train.X,College.train.Y,alpha=0,lambda=bestlam)
pred=predict(lasso.mod,College.test.X,s=bestlam)
rss=sum((pred-College.test$Apps)^2)
tss=sum((College.test$Apps-mean(College.test$Apps))^2)
test.rsq=1-(rss/tss)
test.rsq
#Part d)

#Fit a lasso model on the training set, with $\lambda$ chosen by cross-validation. Report the test error obtained, along with the number of non-zero coefficient estimates.
cv.out=cv.glmnet(College.train.X,College.train.Y,alpha=1)
bestlam=cv.out$lambda.min
bestlam

lasso.mod=glmnet(College.train.X,College.train.Y,alpha=1,lambda=bestlam)
pred=predict(lasso.mod,College.test.X,s=bestlam)
rss=sum((pred-College.test$Apps)^2)
tss=sum((College.test$Apps-mean(College.test$Apps))^2)
test.rsq=1-(rss/tss)
test.rsq

#Number of coefficients equal to 0
sum(coef(lasso.mod)[,1]==0)

names(coef(lasso.mod)[, 1][coef(lasso.mod)[, 1] == 0])
#Part e)

#Fit a PCR model on the training set, with M chosen by cross-validation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(1)
pcr.fit=pcr(Apps~.,data=College.train, scale=TRUE, validation="CV")
summary(pcr.fit) #lowest at M=17
pred=predict(pcr.fit,College.test,ncomp=17)
rss=sum((pred-College.test$Apps)^2)
tss=sum((College.test$Apps-mean(College.test$Apps))^2)
test.rsq=1-(rss/tss)
test.rsq
#Part f)

#Fit a PLS model on the training set, with M chosen by cross-validation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(1)
pls.fit=plsr(Apps~.,data=College.train, scale=TRUE, validation="CV")
summary(pls.fit) #pretty much lowest at 9 comps, certainly closest to lowest there
pred=predict(pls.fit,College.test,ncomp=9)
rss=sum((pred-College.test$Apps)^2)
tss=sum((College.test$Apps-mean(College.test$Apps))^2)
test.rsq=1-(rss/tss)
test.rsq
#Part g)

#Comment on the results obtained. How accurately can we predict the number of college applications received? Is there much difference among 
#the test errors resulting from these five approaches?
#Ordinary least squares, PLS regression, lasso, and PCR regression performed (more or less equally) best. These methods ended up using the 
#same underlying data essentially, since the optimal PCR regression used the same number of underlying variables. PLS regression was able 
#to cut out a few things, chosing a model that used 9 of the possible 17 components, and 83% of the variance, while still performing pretty
#much as well. Interestingly the Lasso, while not performing quite as well, still performed pretty comparably 0.8995 vs 0.9052 
#(a difference of r 0.9052 - 0.8995). The lasso though only set 3 variables to 0 (Enroll (students enrolled), Terminal (pct fac w/ terminal degree), 
  #                                                                               and S.F. Ratio(student/factulty ratio)). It is interesting that
#most of the variables seem to contribute interesting information to the model. Ridge regression performed the poorest at $R^2=0.84$.

#Problem 10

#We have seen that as the number of features used in a model increases, the training error will necessarily decrease, but the test error may not. 
#We will now explore this in a simulated data set.
#Parts a-e)

#Generate a data set with p = 20 features, n = 1000 observations, and an associated quantitative response
#vector generated according to the model $Y=X\beta+\epsilon$ where $\beta$ has some elements that are exactly equal to zero.
library(leaps)

set.seed(1)
X=matrix(rnorm(1000*20),ncol=20,nrow=1000)
colnames(X) <- sprintf("Feature_%d",1:20)
beta=rnorm(20,sd=10)

beta[c(3,7,9,11,13,18)]=0
e=rnorm(1000)
Y=as.vector(X%*%beta+e)
plot(Y,X[,3])

train=sample(1:nrow(X),100) ### FIXME 100 train 900 test
test=(-train)

X.train=X[train,]
X.test=X[test,]
Y.train=Y[train]
Y.test=Y[test]

dat.train=cbind(data.frame(Y=Y.train),X.train)
dat.test=cbind(data.frame(Y=Y.test),X.test)

regfit.best = regsubsets(Y~.,dat=dat.train,nvmax=20)

predict.regsubsets = function(object,newdata,id,...){ 
  form = as.formula(object$call[[2]]) ## extract formula
  mat = model.matrix(form,newdata)
  coefi = coef(object,id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

mse=function(pred,real){
  mean((pred-real)^2)
}


test.mse <- sapply(1:20, function(id){
  pred=predict.regsubsets(regfit.best,dat.test,id)
  mse(pred,Y.test)
})

plot(seq(1:20),test.mse,xlab="Number of Features",
     ylab="Test MSE")
points(which.min(test.mse),test.mse[which.min(test.mse)],
       col="red",cex=2,pch=20)

coef(regfit.best,id=which.min(test.mse))
#My 0 beta features are 3,7,9,11,13, and 18. The lowest test MSE was found at 14/20 features.
#This is indeed the correct number of features which is encouraging (20-6). Best subset selection
#selected against Features 3, 7, 9, 11, 13, 18, so it did really well at finding the true underlying model!
  
#Part g)

beta.rsqb.diffs <- sapply(1:20,function(r){
  coefi<-coef(regfit.best,id=r)
  ncoefi<-names(coefi)
  beta.est <- sapply(1:20,function(i){
    id<-sprintf("Feature_%d",i)
    if(id %in% names(coefi)){
      return(coefi[id])
    }else{
      return(0)
    }
  })
  
  return(sqrt(sum((beta-beta.est)^2)))
})

plot(seq(1:20),beta.rsqb.diffs,xlab="Number of Features",
     ylab="Root Squared Diff Of Betas")
points(which.min(beta.rsqb.diffs),beta.rsqb.diffs[which.min(beta.rsqb.diffs)],
       col="red",cex=2,pch=20)

#The minimum value is the same as before, 14 features, however the cool thing is how much more pronounced the answer is. 
#The dip is really strong between 13 and 14, and then stays small going out to 20.

#Problem 11

#Part a)

#I am going to evalueate each of these methods with 10 Fold CV on the Boston dataset.

library(MASS)
library(ISLR)

## Best Subset
k=10
set.seed(1)
p=ncol(Boston)-1
folds=sample(1:k,nrow(Boston),replace=TRUE)

cv.errors=c()
for(j in 1:k){
  Boston.sub=Boston[folds!=j,]
  #now do CV on this CV subset to choose the best model, and apply
  # it to the whole thing.
  cv.err=matrix(NA,k,p,dimnames=list(NULL,paste(1:p)))
  folds.sub=sample(1:k,nrow(Boston.sub),replace=TRUE)
  
  for(q in 1:k){
    best.fit=regsubsets(crim~.,data=Boston.sub[folds.sub!=q,],nvmax=p)
    for(i in 1:p){
      pred=predict.regsubsets(best.fit,Boston.sub[folds.sub==q,],id=i)
      cv.err[q,i]=mean((Boston.sub$crim[folds.sub==q]-pred)^2)
    }
  }
  
  best.k = which.min(apply(cv.err,2,mean))
  
  best.fit.all=regsubsets(crim~.,data=Boston.sub,nvmax=p)
  pred=predict.regsubsets(best.fit.all,Boston[folds==j,],id=best.k)
  
  cv.errors=c(cv.errors,mean((Boston$crim[folds==j]-pred)^2)) 
}

mean(cv.errors)

## Ridge regression (alpha=0)

cv.errors = sapply(1:k, function(j){
  Boston.X=as.matrix(Boston[,-1])
  Boston.Y=Boston[,1]
  
  cv.out=cv.glmnet(Boston.X[folds!=j,],Boston.Y[folds!=j],alpha=0)
  bestlam=cv.out$lambda.min
  bestlam
  
  lasso.mod=glmnet(Boston.X[folds!=j,],Boston.Y[folds!=j],alpha=0,lambda=bestlam)
  pred=predict(lasso.mod,Boston.X[folds==j,],s=bestlam)  
  return(mean((Boston.Y[folds==j]-pred)^2))
})

mean(cv.errors)

## Lasso (alpha=1)
cv.errors = sapply(1:k, function(j){
  Boston.X=as.matrix(Boston[,-1])
  Boston.Y=Boston[,1]
  
  cv.out=cv.glmnet(Boston.X[folds!=j,],Boston.Y[folds!=j],alpha=1)
  bestlam=cv.out$lambda.min
  bestlam
  
  lasso.mod=glmnet(Boston.X[folds!=j,],Boston.Y[folds!=j],alpha=1,lambda=bestlam)
  pred=predict(lasso.mod,Boston.X[folds==j,],s=bestlam)  
  return(mean((Boston.Y[folds==j]-pred)^2))
})

mean(cv.errors)

## PCR
cv.errors = sapply(1:k, function(j){
  
  pcr.fit=pcr(crim~.,data=Boston[folds!=j,],scale=TRUE,validation="CV")
  res=RMSEP(pcr.fit)
  pcr.best=which.min(res$val[1,,])-1
  
  pred=predict(pcr.fit,Boston[folds==j,],ncomp=pcr.best)  
  return(mean((Boston[folds==j,1]-pred)^2))
})

mean(cv.errors)
#Of these above methods on the Boston dataset using CV for building multiple training/testing splits, 
#and using CV within each CV iteration for choosing optimal parameters for each model, PCR and ridge 
#regression perform best. Lasso performs nearly as well, and best subset selection performs slightly 
#worse than the others.

#The best method, PCR regression, does include all features. It selects a subset of linear combinations 
#of all features though, so some of the variance in some of the features is likely not included, although
#some information from each feature will make it into the final model regardless of the parameter that was
#selected in each CV iteration.

#The same thing goes for the second best method, ridge regression, this one also uses some information 
#from each feature, although it might heavily discount the contribution from some of the features.



#theoretical

par(mfrow=c(2,2))
for(A in c(0,1,5,10)){
  y1=5
  x1=1 # special case where x1 is 1
  b1=seq(-1,6,by=0.05)
  yhat=((y1-b1)^2) + (A*b1^2)
  plot(b1,yhat)
  points(b1[which.min(yhat)],yhat[which.min(yhat)], col="green",cex=4,pch=20)
  abline(v=y1/(1+A),col="red",lwd=3)
}
#Part b)

#Consider (6.13) with $p = 1$. For some choice of $y_1$, $x_1$, and $\alpha > 0$, plot (6.13) as a function of $\beta_1$. Your plot should confirm that (6.13) is solved by (6.15).
opt.y.lasso=function(y,a){
  if(y>(a/2)){
    return(y-(a/2))
  }
  
  if(y < (-a/2)){
    return(y+(a/2))
  }
  if(abs(y) <= (a/2)){
    return(0)
  }
}

par(mfrow=c(2,2))
for(A in c(0,1,5,10)){
  y1=5
  x1=1 # special case where x1 is 1
  b1=seq(-1,6,by=0.05)
  yhat=((y1-b1)^2) + A*abs(b1)
  plot(b1,yhat)
  points(b1[which.min(yhat)],yhat[which.min(yhat)], col="green",cex=4,pch=20)
  abline(v=opt.y.lasso(y1,A),col="red",lwd=3) 
}
