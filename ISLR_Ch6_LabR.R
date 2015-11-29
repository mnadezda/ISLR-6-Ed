#####Best Subset Selection#####
library (ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
sum(is.na(Hitters))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

install.packages('leaps')
library(leaps)
#regsubsets - best subset selection using RSS, by default up to 8 variables
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
names(reg.summary)
#look at impruvement R2
reg.summary$rsq

par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS', type='l')
plot(reg.summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col='red', cex=2, pch=20)
plot(reg.summary$cp, xlab='Number of Variables', ylab='Cp', type='l')
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col='red', cex=2, pch=20)
plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col='red', cex=2, pch=20)

plot(regfit.full, scale='r2')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

coef(regfit.full, 6)

#####Forward and Backward Stepwise Selection#####
regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method='forward')
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~., data=Hitters, nvmax=19, method='backward')
summary(regfit.bwd)
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#####Choosing Among Models Using the Validation Set Approach and Cross-Validation#####
#Validation Set Approach#
set.seed(1)
train=sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test=!train
regfit.best=regsubsets(Salary~., data=Hitters[train,], nvmax=19)
#create matrix of X values relevand for the model from test dataset 
test.mat=model.matrix(Salary~., data=Hitters[test,])
val.errors=rep(NA, 19)
for (i in 1:19) {
  #extract coefficients from best model in each class(best among each number of predictors)
  coefi=coef(regfit.best, id=i)
  #matrix multiplication to get estimates for y for each x, one model per loop
  pred=test.mat[,names(coefi)]%*%coefi
  #test MSE
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
#find minimum among test MSE
which.min(val.errors)
coef(regfit.best, 10)

#make "predict" function for best subset selection
predict.regsubsets=function(object, newdata, id, ...) {
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#rerun best subset selection on the whole dataset to determine coefficients max precisely
regfit.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)

#Cross-Validation using k training sets#
k=10
set.seed(1)
#split into training sets
folds=sample(1:k, nrow(Hitters), replace=TRUE)
#initialise matrix to put cv.errors in
cv.errors=matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))
#calc best model for each number of variables (1-19) on train dataset (1-k) using j element as test and writes into cv.matrix the MSE error for each ji pair
for (j in 1:k) {
  best.fit=regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for (i in 1:19) {
    pred=predict(best.fit, Hitters[folds==j, ], id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
#calc mean of MSE of k tests for models with i number of variables & plots that
mean.cv.errors=apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)], col='red', cex=2, pch=20)
#rerun fitting best model selection using whole dataset and pick the model with optimal number of predictors
reg.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 11)

#####Ridge Regression and the Lasso#####
#to use glmet function for RR & L need to set X & Y matrixes; be sure NA have been removed!!!!!!!!!!
#it only takes numeric qualitative variables (so model.matrix is very usefull, it converts qualitative to dummy variables!!!)
x=model.matrix(Salary~., Hitters)[ ,-1]
y=Hitters$Salary

###Ridge Regression###
install.packages('glmnet')
library(glmnet)
#alpha=0=Ridge Regression; 1=Lasso
#lambda range is difined by default, but using grid here - customized it
#by default glmnet standardize the data, to calcel thet . standardize=FALSE
grid=10^seq(10, -2, length=100)
ridge.mod=glmnet(x,y,alpha=0, lambda=grid)
#obtained coefficients
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[ ,50]
#sum of coefficients decreases and lambda increases
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[ ,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
#compute coefficients when lambda=50
predict(ridge.mod, s=50, type='coefficients')[1:20, ]

#split into training and test datasets and estimate test error of RR
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train, ], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test, ])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod, s=1e10, newx=x[test, ])
mean((ridge.pred-y.test)^2)
#lambda=0 == lm
ridge.pred=predict(ridge.mod, s=0, newx=x[test, ], exact=T)
mean((ridge.pred-y.test)^2)
lm(x~y, subset=train)
predict(ridge.mod, s=0, exact=T, type='coefficients')[1:20, ]

#Cross-Validate to chose lambda and plug this lambda to compute Ridge Regression using full dataset
set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test, ])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out, type='coefficients', s=bestlam)[1:20, ]

###The Lasso###
#look at the change in coefficients with change in lambda
lasso.mod=glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
#use CV to pick lambda, fit model using train dataset and assessing the model using test set
set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test, ])
mean((lasso.pred-y.test)^2)
#fit model using all the data and teh best lambda we found
out=glmnet(x, y, alpha=1, lambda=grid)
lasso.coef=predict(out, type='coefficients', s=bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef!=0]


#####Principal Components Regression#####
###Principar Component Regression###
install.packages('pls')
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation='CV')
summary(pcr.fit)
validationplot(pcr.fit, val.type='MSEP')
#find the best number of components
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation='CV')
validationplot(pcr.fit, val.type='MSEP')
#look at the MSE obtained with optimal number of components 
pcr.pred=predict(pcr.fit, x[test, ], ncomp=7)
mean((pcr.pred-y.test)^2)
#fit model with optimal number of components on the whole dataset
pcr.fit=pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

###Partial Least Squares###
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation='CV')
summary(pls.fit)
validationplot(pls.fit, val.type='MSEP')
pls.pred=predict(pls.fit, x[test, ], ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)



#####Exercises#####
#8. In this exercise, we will generate simulated data, and will then use this data to perform best subset selection.
#a Use the rnorm() function to generate a predictor X of length n = 100, as well as a noise vector e of length n = 100.
X <- rnorm(100)
E <- rnorm(100)
#b Generate a response vector Y of length n = 100 according to the model Y = ??0 + ??1X + ??2X2 + ??3X3 + e, where ??0, ??1, ??2, and ??3 are constants of your choice.
Y <- 5+2*X-0.003*X^2+876*X^3+E
length(Y)

#c Use the regsubsets() function to perform best subset selection in order to choose the best model containing the predictors X,X2, . . .,X10. What is the best model obtained according to
#Cp, BIC, and adjusted R2? Show some plots to provide evidence for your answer, and report the coefficients of the best model obtained.
#Note you will need to use the data.frame() function to create a single data set containing both X and Y .
XY <- data.frame(X, Y)
dim(XY)
regfit.full=regsubsets(Y~poly(X, 10), data = XY, nvmax=10)
summary(regfit.full)
reg.summary <- summary(regfit.full)
reg.summary$rsq
#look at impruvement R2 - after cube - no improvement
par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS', type='l')
points(which.min(reg.summary$rss), reg.summary$rss[which.min(reg.summary$rss)], col='red', cex=2, pch=20)
plot(reg.summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col='red', cex=2, pch=20)
plot(reg.summary$cp, xlab='Number of Variables', ylab='Cp', type='l')
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col='red', cex=2, pch=20)
plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col='red', cex=2, pch=20)
#Based of 'find min/max' picks higher number of variables, but graphs are really flat - lets stick with 3
#RSS - gives 10; RSq - 6; Cp - 5; BIC - 4
coef(regfit.full, 3)
#based on graphs 3 is fine and it gives corect, poly, 3
coef(regfit.full, 4)
#BIC - adds x^6 into
coef(regfit.full, 5)
#Cp- adds x^6 and ^8 
coef(regfit.full, 6)
#RSq - adds x^6, x^8, x^9

#d Repeat (c), using forward stepwise selection and also using backwards stepwise selection. How does your answer compare to the results in (c)?
regfit.fwd=regsubsets(Y~poly(X, 10), data=XY, nvmax=10, method='forward')
summary(regfit.fwd)
regfit.bwd=regsubsets(Y~poly(X, 10), data=XY, nvmax=10, method='backward')
summary(regfit.bwd)

#RSS
par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS', main='Best', type='l')
plot(summary(regfit.fwd)$rss, xlab='Number of Variables', ylab='RSS', main='Forward', type='l')
plot(summary(regfit.bwd)$rss, xlab='Number of Variables', ylab='RSS', main='Backward', type='l')

#BIC
par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', main='Best', type='l')
plot(summary(regfit.fwd)$bic, xlab='Number of Variables', ylab='BIC', main='Forward', type='l')
plot(summary(regfit.bwd)$bic, xlab='Number of Variables', ylab='BIC', main='Backward', type='l')

#Cp
par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(reg.summary$cp, xlab='Number of Variables', ylab='Cp', main='Best', type='l')
plot(summary(regfit.fwd)$cp, xlab='Number of Variables', ylab='Cp', main='Forward', type='l')
plot(summary(regfit.bwd)$cp, xlab='Number of Variables', ylab='Cp', main='Backward', type='l')

#Based on each paramiter every method gives the same estimates
coef(regfit.full, 3)
coef(regfit.fwd, 3)
coef(regfit.bwd, 3)
#graphs look exactly the same, coeffitients are exact same


#e Now fit a lasso model to the simulated data, again using X,X2, . . . , X10 as predictors.  Use cross-validation to select the optimal value of ??.  Create plots of the cross-validation error as a function of ??.  Report the resulting coefficient estimates, and discuss the results obtained.
x=model.matrix(Y~poly(X, 10), data=XY)[ ,-1]
y=XY$Y
set.seed(1)
train=sample(1:nrow(XY), nrow(XY)/2)
test=(-train)
y.test=y[test]
par(mfrow=c(1, 2))
lasso.mod=glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
#use CV to pick lambda, fit model using train dataset and assessing the model using test set
cv.out=cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test, ])
mean((lasso.pred-y.test)^2)
#fit model using all the data and teh best lambda we found
out=glmnet(x, y, alpha=1, lambda=grid)
lasso.coef=predict(out, type='coefficients', s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]
#model with X, x^2, x^3 is picked - so far the best performance so far; but coefficients are really off
#maybe because X^3 coefficient in the formula to generate Y is really high??, try smaller difference between coefficients in the initial formula for Y


#Lasso again using another starting Y
Y1 <- 5+2*X-0.03*X^2+8*X^3+E
XY1 <- data.frame(X, Y1)
x=model.matrix(Y1~poly(X, 10), data=XY1)[ ,-1]
y1=XY1$Y1
set.seed(1)
train=sample(1:nrow(XY1), nrow(XY1)/2)
test=(-train)
y1.test=y1[test]
par(mfrow=c(1, 2))
lasso.mod=glmnet(x[train, ], y1[train], alpha=1, lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train, ], y1[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test, ])
mean((lasso.pred-y1.test)^2)
out=glmnet(x, y1, alpha=1, lambda=grid)
lasso.coef=predict(out, type='coefficients', s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]
#Does much worse than predicting previous formula

#f Now generate a response vector Y according to the model Y = ??0 + ??7X7 + , and perform best subset selection and the lasso. Discuss the results obtained.
Y2 <- 5+8*X^7+E
XY2 <- data.frame(X, Y2)
x=model.matrix(Y2~poly(X, 10), data=XY2)[ ,-1]
y2=XY2$Y2
set.seed(1)
train=sample(1:nrow(XY2), nrow(XY2)/2)
test=(-train)
y2.test=y2[test]
par(mfrow=c(1, 2))
lasso.mod=glmnet(x[train, ], y2[train], alpha=1, lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train, ], y2[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test, ])
mean((lasso.pred-y2.test)^2)
out=glmnet(x, y2, alpha=1, lambda=grid)
lasso.coef=predict(out, type='coefficients', s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]
#gives coefficients for X^(1-7), performes much worse than on the previous example
#coeff intercept 2.89, x^7 54.3 instead of 5 and 8 respectively - also carries out X^(1-5) with huge coeffitients
#smth is wrong, way too bad estimates..



#9. In this exercise, we will predict the number of applications received
using the other variables in the College data set.
?College
dim(College)
summary(College)
str(College)
#the number of applications received - Apps
#All variables but 'Private' are numeric, 'Private' is factor; no NA

#a Split the data set into a training set and a test set.
set.seed(1)
train=sample(1:nrow(College), nrow(College)/5)
length(train)
test=(-train)
College.test=College[test]
summary(College.test)
dim(College.test)
#Does not work for some reason, College.test has less columns then College - WHY?????????????
#b Fit a linear model using least squares on the training set, and report the test error obtained.
lm.fit = lm(Apps~., data=College, subset=train)
summary(lm.fit)
lm.fit = lm(Apps~.-Private, data=College, subset=train)
summary(lm.fit)
#Variables Accept, Top10perc, Outstate are highly significalnt; Top25perc, Enroll & Room.Board are also
cor(College[,-1])
#Accept & Enroll & Outstate are highly (anti-)correlated
lm.fit = lm(Apps~Accept+Outstate+Enroll+Top10perc+Top25perc+Room.Board, data=College, subset=train)
summary(lm.fit)
lm.pred=predict(lm.fit, College.test)
summary(College.test)
#Does not work for some reason, College.test has less columns then College - WHY?????????????


#Try the other method to split dataset
set.seed(1)
train=sample(c(TRUE, FALSE), nrow(College), rep=TRUE)
sum(train)
test=!train
lm.fit = lm(Apps~., data=College, subset=train)
summary(lm.fit)
par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(lm.fit)
#Private, Accept, Top10perc, Top25perc - look significant
lm.fit = lm(Apps~Private+Accept+Top10perc+Top25perc, data=College, subset=train)
summary(lm.fit)
lm.pred=predict(lm.fit, College.test, interval='prediction')
mean((lm.pred - College.test$Apps)^2)
#test MSE in lm is 4118343, based on plot(lm.fit), the model does not represent the data well

#c Fit a ridge regression model on the training set, with ?? chosen by cross-validation. Report the test error obtained.
x=model.matrix(Apps~., College)[ ,-1]
y=College$Apps
y.test=y[!train]
library(glmnet)
#just to check the tendency for coeffitients depending on lambda
ridge.mod=glmnet(x[train, ], y[train], alpha=0, lambda=grid)
par(mfrow=c(1,1), mar=c(4, 4, 4, 4))
plot(ridge.mod)
#graph is really messed up, not a function, some coefficients have double values
#Cross-Validate to chose lambda and plug this lambda to compute Ridge Regression using full dataset
set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test, ])
mean((ridge.pred-y.test)^2)
#test MSE in lm is 2580775, better then lm
out=glmnet(x,y,alpha=0)
predict(out, type='coefficients', s=bestlam)

#d Fit a lasso model on the training set, with ?? chosen by crossvalidation. Report the test error obtained, along with the number of non-zero coefficient estimates.
lasso.mod=glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
#use CV to pick lambda, fit model using train dataset and assessing the model using test set
set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test, ])
mean((lasso.pred-y.test)^2)
#test MSE in lm is 1612366, better then lm & ridge
#fit model using all the data and teh best lambda we found
out=glmnet(x, y, alpha=1)
lasso.coef=predict(out, type='coefficients', s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]
#16 non-zero coeffitients

#e Fit a PCR model on the training set, with M chosen by crossvalidation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(2)
pcr.fit=pcr(Apps~., data=College, scale=TRUE, validation='CV')
summary(pcr.fit)
validationplot(pcr.fit, val.type='MSEP')
#find the best number of components
set.seed(1)
pcr.fit=pcr(Apps~., data=College, subset=train, scale=TRUE, validation='CV')
validationplot(pcr.fit, val.type='MSEP')
#look at the MSE obtained with optimal number of components, here at 2 components MSEP falls sharply and then again at 5 and then slowly; compare models obtained with 5 and 16 components

#5 components
pcr.pred=predict(pcr.fit, x[test, ], ncomp=5)
mean((pcr.pred-y.test)^2)
#fit model with optimal number of components on the whole dataset
pcr.fit=pcr(y~x, scale=TRUE, ncomp=5)
summary(pcr.fit)
#with 5 components mean=3519875, not too good, only 84% of variance explaned, just better then lm, but worse then ridge and lasso

#16 components
set.seed(1)
pcr.fit=pcr(Apps~., data=College, subset=train, scale=TRUE, validation='CV')
validationplot(pcr.fit, val.type='MSEP')
pcr.pred=predict(pcr.fit, x[test, ], ncomp=16)
#fit model with optimal number of components on the whole dataset
pcr.fit=pcr(y~x, scale=TRUE, ncomp=16)
mean((pcr.pred-y.test)^2)
summary(pcr.fit)
#with 16 components mean=1796894, 92% of variance explaned, almost as good as lasso, but cant do much for interpretation of influence of each of the variables on the outcome, so lasso is better

#f Fit a PLS model on the training set, with M chosen by crossvalidation. Report the test error obtained, along with the value of M selected by cross-validation.
set.seed(1)
pls.fit=plsr(Apps~., data=College, subset=train, scale=TRUE, validation='CV')
summary(pls.fit)
validationplot(pls.fit, val.type='MSEP')
pls.pred=predict(pls.fit, x[test, ], ncomp=6)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Apps~., data=College, scale=TRUE, ncomp=6)
summary(pls.fit)
#looks good, with MSE 1626712 at 6 components, error comparable to lasso and PCA, with 16 variables/components each

#g Comment on the results obtained. How accurately can we predict the number of college applications received? Is there much difference among the test errors resulting from these five approaches?
#compare models using R^2
lm.r2 <- 1 - mean((lm.pred - College.test$Apps)^2) / mean((mean(College.test$Apps) - College.test$Apps)^2)
ridge.r2 <- 1 - mean((ridge.pred - y.test)^2) / mean((mean(y.test) - y.test)^2)
lasso.r2 <- 1 - mean((lasso.pred - y.test)^2) / mean((mean(y.test) - y.test)^2)
pcr.r2 <- 1 - mean((pcr.pred - y.test)^2) / mean((mean(y.test) - y.test)^2)
pls.r2 <- 1 - mean((pls.pred - y.test)^2) / mean((mean(y.test) - y.test)^2)
r2 <- c(lm.r2, ridge.r2, lasso.r2, pcr.r2, pls.r2)
r2

lm.mean <-mean((lm.pred - College.test$Apps)^2)
ridge.mean <- mean((ridge.pred - y.test)^2)
lasso.mean <- mean((lasso.pred - y.test)^2)
pcr.mean <- mean((pcr.pred - y.test)^2)
pls.mean <- mean((pls.pred - y.test)^2)
mean <- c(lm.mean, ridge.mean, lasso.mean, pcr.mean, pls.mean)
mean
methods <- c('lm', 'ridge', 'lasso', 'pcr', 'pls')

par(mfrow=c(1,2), mar=c(4, 4, 4, 4))
toplot <- data.frame(cbind(methods, r2, mean))
str(toplot)
dim(toplot)
toplot$methods <- factor(toplot$methods, levels=c('lm', 'ridge', 'lasso', 'pcr', 'pls'))
library(ggplot2)
install.packages('gridExtra')
library(gridExtra)
R2 <- ggplot(toplot, aes(x = methods, y = r2, col='red', group = 1)) + geom_line() + labs(x='Method', y='R^2')
MSE <- ggplot(toplot, aes(x = methods, y = mean, col='red', group = 1)) + geom_line() + labs(x='Method', y='MSE')
grid.arrange(R2, MSE, ncol=2)
#all in all lasso performs the best on this data


#10. We have seen that as the number of features used in a model increases, the training error will necessarily decrease, but the test error may not. We will now explore this in a simulated data set.
#a Generate a data set with p = 20 features, n = 1,000 observations, and an associated quantitative response vector generated according to the model Y = X?? + E, where ?? has some elements that are exactly equal to zero.
set.seed(1)
p <- 20
n <- 1000
X <- matrix(rnorm(n*p, mean=2, sd=0.5), n, p)
e <- rnorm(p)
b <- rnorm(p)
b0 <- sample(20, 5)
for(i in b0) b[i]=0
Y <- X %*% b +e
XY <- data.frame(cbind(Y, X))
colnames(XY) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19', 'X20')

#b Split your data set into a training set containing 100 observations and a test set containing 900 observations.
set.seed(1)
train <- rep(TRUE, n)
train[sample(n, 100)]=FALSE
sum(train)
test=!train
XY.train <- XY[train,]
XY.test <- XY[!train,]

#c Perform best subset selection on the training set, and plot the training set MSE associated with the best model of each size.
library(leaps)
regfit.best=regsubsets(Y~., data=XY.train, nvmax=20)
#create matrix of X values relevant for the model from test dataset 
test.mat=model.matrix(Y~., data=XY.test)
val.errors=rep(NA, 20)
for (i in 1:20) {
  #extract coefficients from best model in each class(best among each number of predictors)
  coefi=coef(regfit.best, id=i)
  #matrix multiplication to get estimates for y for each x, one model per loop
  pred=test.mat[,names(coefi)]%*%coefi
  #test MSE
  val.errors[i]=mean((XY$Y[test]-pred)^2)
}
val.errors

#d Plot the test set MSE associated with the best model of each size.
par(mfrow=c(1,1), mar=c(4, 4, 4, 4))
xaxes <- 1:20
plot(xaxes, val.errors, type='l', col='blue', xlab='Number of Variables', ylab='MSE')

#e For which model size does the test set MSE take on its minimum value? Comment on your results. If it takes on its minimum value for a model containing only an intercept or a model containing all of the features, then play around with the way that you are generating the data in (a) until you come up with a scenario in which the test set MSE is minimized for an intermediate model size.
which.min(val.errors)
coef <- coef(regfit.best, which.min(val.errors))

#f How does the model at which the test set MSE is minimized compare to the true model used to generate the data? Comment on the coefficient values.
coef(regfit.best, which.min(val.errors))
b
compar <- data.frame(cbind(matrix(0, ncol=1, nrow=2), rbind(b, c(rep(0, 20)))))
colnames(compar) <- c('X0', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19', 'X20')
compar[1, 1]<- mean(E)
compar[2, 1]<- coef[1]
compar[2, 2]<- coef[2]
compar[2, 3]<- coef[3]
compar[2, 4]<- coef[4]
compar[2, 5]<- coef[5]
compar[2, 6]<- coef[6]
compar[2, 10]<- coef[7]
compar[2, 11]<- coef[8]
compar[2, 13]<- coef[9]
compar[2, 14]<- coef[10]
compar
coef
#the model looks actually pretty similar to data constraction, intercept is far from E mean, but coefficients are pretty similar

#g Create a plot displaying sqrt(sum of squares for p(set b-estimated b)) Comment on what you observe. How does this compare to the test MSE plot from (d)?
sqrt.sum.sq.B <- rep(NA, 20)
x_cols = colnames(X, do.NULL = FALSE, prefix = "X")
for (i in 1:20) {
  coefi <- coef(regfit.best, id = i)
  sqrt.sum.sq.B[i] <- sqrt(sum((b[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + sum(b[!(x_cols %in% names(coefi))])^2)
}
plot(sqrt.sum.sq.B, xlab = "Number of coefficients", ylab = "Error between estimated and true coefficients", pch = 19, type = "b")

par(mfrow=c(1,2), mar=c(4, 4, 4, 4))
plot(xaxes, val.errors, type='l', col='blue', xlab='Number of Variables', ylab='MSE')
plot(sqrt.sum.sq.B, xlab = "Number of coefficients", ylab = "Error between estimated and true coefficients", pch = 19, type = "b")

#Fit of the model is minimised at 9 variables, while minimal error in fitting coeffitients is when fit just one, possibly parshly because set coeffitients are actually small.
#fitting model by minimising MSE is really not the same as "fitting" coeffitients

#11. We will now try to predict per capita crime rate in the Boston data set.
#a Try out some of the regression methods explored in this chapter,
such as best subset selection, the lasso, ridge regression, and PCR. Present and discuss results for the approaches that you consider.
library(MASS)
str(Boston)

#####Best Subset Selection
#Split your data set into a training set (55 to test)
train <- rep(TRUE, nrow(Boston))
train[sample(nrow(Boston), 55)]=FALSE
sum(train)
test=!train
Boston.train <- Boston[train,]
Boston.test <- Boston[!train,]
#Best subset selection on the training set
library(leaps)
regfit.best=regsubsets(crim~., data=Boston.train, nvmax=13)
#create matrix of X values relevant for the model from test dataset 
test.mat=model.matrix(crim~., data=Boston.test)
val.errors=rep(NA, 13)
for (i in 1:13) {
  #extract coefficients from best model in each class(best among each number of predictors)
  coefi=coef(regfit.best, id=i)
  #matrix multiplication to get estimates for y for each x, one model per loop
  pred=test.mat[,names(coefi)]%*%coefi
  #test MSE
  val.errors[i]=mean((Boston.test$crim-pred)^2)
}
val.errors
par(mfrow=c(1,1), mar=c(4, 4, 4, 4))
plot(1:13, val.errors, type='b', col='blue', xlab='Variable', ylab='MSE', pch=20)
points(which.min(val.errors), val.errors[which.min(val.errors)], col='red', cex=2, pch=20)
val.errors[12]
#MSE take on its minimum value and rerun on full dataset to get coefficients
regfit.best=regsubsets(crim~., data=Boston, nvmax=13)
coef(regfit.best, which.min(val.errors))
#model with 12 variables looks best, MSE at 13.11

#Cross-Validation using k training sets#
k=10
set.seed(1)
folds=sample(1:k, nrow(Boston), replace=TRUE)
cv.errors=matrix(NA, k, 13, dimnames=list(NULL, paste(1:13)))
for (j in 1:k) {
  best.fit=regsubsets(crim~., data=Boston[folds!=j,], nvmax=13)
  for (i in 1:13) {
    pred=predict(best.fit, Boston[folds==j, ], id=i)
    cv.errors[j,i]=mean((Boston$crim[folds==j]-pred)^2)
  }
}
#calc mean of MSE of k tests for models with i number of variables & plots that
mean.cv.errors=apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)], col='red', cex=2, pch=20)
mean.cv.errors[12]
#model with 12 variables looks best, CV.error 41.034
#rerun fitting best model selection using whole dataset and pick the model with optimal number of predictors
reg.best=regsubsets(crim~., data=Boston, nvmax=13)
coef(reg.best, which.min(mean.cv.errors))
#both models got rid of variable 'age'& gave exact same coefficients

#####The Ridge Regression
x=model.matrix(crim~., Boston)[ ,-1]
y=Boston$crim
y.test=y[!train]
grid=10^seq(10, -2, length=100)
library(glmnet)
#Cross-Validate to chose lambda and plug this lambda to compute Ridge Regression using full dataset
set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test, ])
mean((ridge.pred-y.test)^2)
#test MSE in ridge is 13.33, a bit worse then Best Subset Selection with 13.11
out=glmnet(x,y,alpha=0)
predict(out, type='coefficients', s=bestlam)

#####The Lasso
lasso.mod=glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
#use CV to pick lambda, fit model using train dataset and assessing the model using test set
set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test, ])
mean((lasso.pred-y.test)^2)
#test MSE in Lasso is 13.20 - marginaly better then Ridge, but worse then Best subset
#fit model using all the data and teh best lambda we found
out=glmnet(x, y, alpha=1)
lasso.coef=predict(out, type='coefficients', s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]
#12 non-zero coeffitients, as in Best Subset 'age' was excluded

#####PCR
library(pls)
set.seed(2)
pcr.fit=pcr(crim~., data=Boston, scale=TRUE, validation='CV')
summary(pcr.fit)
validationplot(pcr.fit, val.type='MSEP')
#find the best number of components
set.seed(1)
pcr.fit=pcr(crim~., data=Boston, subset=train, scale=TRUE, validation='CV')
validationplot(pcr.fit, val.type='MSEP')
#look at the MSE obtained with optimal number of components 
pcr.pred=predict(pcr.fit, x[test, ], ncomp=13)
mean((pcr.pred-y.test)^2)
#fit model with optimal number of components on the whole dataset
pcr.fit=pcr(y~x, scale=TRUE, ncomp=13)
summary(pcr.fit)
#Here all 13 predictors are used and only 45.4% variance is explaned
#MSE is 13.11 - same as Best sublet
#All in all, best subset gives better prediction for this case, since it perform variable selection







