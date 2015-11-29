#####The Validation Set Approach#####
library(ISLR)
set.seed(1)
train=sample(dim(Auto)[1], dim(Auto)[1]/2)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
#estimated MSE for linear regression - less is better
mean((Auto$mpg-predict(lm.fit, Auto))[-train]^2)
#quadratic nd cubic regression
lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((Auto$mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((Auto$mpg-predict(lm.fit3, Auto))[-train]^2)

#pick different training/test set
set.seed(2)
train=sample(dim(Auto)[1], dim(Auto)[1]/2)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
mean((Auto$mpg-predict(lm.fit, Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((Auto$mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((Auto$mpg-predict(lm.fit3, Auto))[-train]^2)
#can see that "on average" ;) quadratic regression performs better

#####Leave-One-Out Cross-Validation#####
glm.fit=glm(mpg~horsepower, data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower, data=Auto)
coef(lm.fit)
library(boot)
cv.err=cv.glm(Auto, glm.fit)
#gives LOOCV statistic (2 numbers which for LOOCV are the same; first is standard and second is bias-corrected version, for kFCV they are different)
cv.err$delta

#Fit increasingly complex polynomial fits
cv.error=rep(0,5)
for (i in 1:5) {
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
#Sharp drop in MSE between linear and quadratic, then - the same


#####f-Fold Cross-Validation#####
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10
#Sharp drop in MSE between linear and quadratic, then - the same

#####The Bootstrap#####
##Estimating the Accuracy of a Statistic of Interest##
alpha.fn=function(data, index) {
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
#here index tells which observations to pick
alpha.fn(Portfolio, 1:100)
#randomly select 100 observations in 1 to 100 range, with replacement; Doing this command repeatedly and getting stdev of the outcomes will give us bootstrap
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
#actuall bootstrap function, alpha.fn, defined earlier, tells what paramiter bootstrap shall estimate
boot(Portfolio, alpha.fn, R=1000)

##Estimating teh Accuracy of a Linear Model##
#Set the function to estimate LR coefficients and do it using full Auto dataset
boot.fn=function(data, index)
  return(coef(lm(mpg~horsepower, data=Auto, subset=index)))
boot.fn(Auto, 1:392)

#create bootstrap estimates for the coefficients
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
boot.fn(Auto, sample(392, 392, replace=T))
#running the command 2 times gives slightly different result - by analysing stdev of the result - get bootstrap, or automated:
boot(Auto, boot.fn, 1000)
#for standard formulas Standard Errors are already embedded in lm, etc.:
summary(lm(mpg~horsepower, data=Auto))$coef
#get different result because embedded way calculates errors based of assumptions(1 - linear model is correct, 2-all variation comes from "ei"), so bootstarp errors are more accurate

#same for quadratic lm model 
boot.fn=function(data, index)
  return(coefficients(lm(mpg~horsepower+I(horsepower^2), data=Auto, subset=index)))
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef


#####Applied#####
#5. In Chapter 4, we used logistic regression to predict the probability of default using income and balance on the Default data set. We will now estimate the test error of this logistic regression model using the validation set approach. Do not forget to set a random seed before beginning your analysis.
#a Fit a logistic regression model that uses income and balance to predict default.

#set dummy variable for Default (convert 'Yes/No' column into '1/0')
summary(Default)
Default.01=rep(0, nrow(Default))
Default.01[Default[,1]=='Yes']='1'
Default.01=as.numeric(Default.01)
sum(Default.01)
Default.01=cbind(Default, Default.01)
summary(Default.01)

glm.fit=glm(Default.01~income+balance, data=Default.01, family=binomial)
summary(glm.fit)

#b Using the validation set approach, estimate the test error of this model. In order to do this, you must perform the following steps:
#i. Split the sample set into a training set and a validation set.
set.seed(1)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
dim(Default.train)
Default.test <- Default.01[-train,]
dim(Default.test)
Default01.test <- Default.01$Default.01[-train]
length(Default01.test)
  
#ii. Fit a multiple logistic regression model using only the training observations.
glm.fit=glm(Default.01~income+balance, data=Default.01, subset=train, family=binomial)
summary(glm.fit)

#iii. Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'

#iv. Compute the validation set error, which is the fraction of the observations in the validation set that are misclassified.
table(glm.pred, Default01.test)
(115+28)/5000
1-mean(glm.pred==Default01.test)
#2.86% error rate

#c Repeat the process in (b) three times, using three different splits of the observations into a training set and a validation set. Comment on the results obtained.
set.seed(2)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
Default.test <- Default.01[-train,]
Default01.test <- Default.01$Default.01[-train]
glm.fit=glm(Default.01~income+balance, data=Default.01, subset=train, family=binomial)
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'
1-mean(glm.pred==Default01.test)
#2.76% error rate
set.seed(3)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
Default.test <- Default.01[-train,]
Default01.test <- Default.01$Default.01[-train]
glm.fit=glm(Default.01~income+balance, data=Default.01, subset=train, family=binomial)
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'
1-mean(glm.pred==Default01.test)
#2.48% error rate
set.seed(4)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
Default.test <- Default.01[-train,]
Default01.test <- Default.01$Default.01[-train]
glm.fit=glm(Default.01~income+balance, data=Default.01, subset=train, family=binomial)
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'
1-mean(glm.pred==Default01.test)
#2.62% error rate
# error rate varies from dataset to dataset.

#d Now consider a logistic regression model that predicts the probability of default using income, balance, and a dummy variable for student. Estimate the test error for this model using the validation set approach. Comment on whether or not including a dummy variable for student leads to a reduction in the test error rate.
set.seed(1)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
Default.test <- Default.01[-train,]
Default01.test <- Default.01$Default.01[-train]
glm.fit=glm(Default.01~income+balance+student, data=Default.01, subset=train, family=binomial)
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'
1-mean(glm.pred==Default01.test)
#2.88%
set.seed(2)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
Default.test <- Default.01[-train,]
Default01.test <- Default.01$Default.01[-train]
glm.fit=glm(Default.01~income+balance+student, data=Default.01, subset=train, family=binomial)
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'
1-mean(glm.pred==Default01.test)
#2.86% error rate
set.seed(3)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
Default.test <- Default.01[-train,]
Default01.test <- Default.01$Default.01[-train]
glm.fit=glm(Default.01~income+balance+student, data=Default.01, subset=train, family=binomial)
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'
1-mean(glm.pred==Default01.test)
#2.48% error rate
set.seed(4)
train <- sample(dim(Default.01)[1], dim(Default.01)[1]/2)
Default.train <- Default.01[train,]
Default.test <- Default.01[-train,]
Default01.test <- Default.01$Default.01[-train]
glm.fit=glm(Default.01~income+balance+student, data=Default.01, subset=train, family=binomial)
glm.probs=predict(glm.fit, Default.test, type='response')
glm.pred=rep(0, nrow(Default.test))
glm.pred[glm.probs>0.5]='1'
1-mean(glm.pred==Default01.test)
#2.74% error rate
# error rate varies from dataset to dataset, but in the same rangeas those obtained with simplier model, with no student variable.


#6. We continue to consider the use of a logistic regression model to predict the probability of default using income and balance on the Default data set. In particular, we will now compute estimates for the standard errors of the income and balance logistic regression coefficients in two different ways: (1) using the bootstrap, and (2) using the standard formula for computing the standard errors in the glm() function. Do not forget to set a random seed before beginning your analysis.
#a Using the summary() and glm() functions, determine the estimated standard errors for the coefficients associated with income and balance in a multiple logistic regression model that uses both predictors.
glm.fit=glm(Default.01~income+balance, data=Default.01, family=binomial)
summary(glm.fit)
#income Std.Error 4.985e-06, balance Std. Error 2.274e-04

#b Write a function, boot.fn(), that takes as input the Default data set as well as an index of the observations, and that outputs the coefficient estimates for income and balance in the multiple logistic regression model.
boot.fn <- function(data, index) 
return(coef(glm(Default.01~income+balance, data=data, subset=index, family=binomial)))
boot.fn(Default.01, train)

#c Use the boot() function together with your boot.fn() function to estimate the standard errors of the logistic regression coefficients for income and balance.
set.seed(1)
boot(Default.01, boot.fn, 100)
#income Std.Error 4.127740e-06, balance Std. Error 2.105660e-04; bootstrap estimated Std. 

#d Comment on the estimated standard errors obtained using the glm() function and using your bootstrap function.
#Errors are similar to those obtained directly from summary, but different (no assumptions made in this way opose to assumptions that linear model is correct & that variability does not depend on X made in calculating Std. Errors during glm fitting)


#7. In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be used in order to compute the LOOCV test error estimate. Alternatively, one could compute those quantities using just the glm() and predict.glm() functions, and a for loop. You will now take this approach in order to compute the LOOCV error for a simple logistic regression model on the Weekly data set. Recall that in the context of classification problems, the LOOCV error is given in (5.4).
#a Fit a logistic regression model that predicts Direction using Lag1 and Lag2.
glm.fit=glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)
summary(glm.fit)

#b Fit a logistic regression model that predicts Direction using Lag1 and Lag2 using all but the first observation.
glm.fit=glm(Direction~Lag1+Lag2, data=Weekly[-1,], family=binomial)
summary(glm.fit)

#c Use the model from (b) to predict the direction of the first observation. You can do this by predicting that the first observation will go up if P(Direction="Up"|Lag1, Lag2) > 0.5. Was this observation correctly classified?
glm.probs=predict(glm.fit, Weekly[1,], type='response')
glm.pred <- rep('Down', 1)
glm.pred[glm.probs>0.5]='Up'
table(glm.pred, Weekly$Direction[1])
mean(glm.pred==Weekly$Direction[1])
#The observation was wrongly classified

#d Write a for loop from i = 1 to i = n, where n is the number of observations in the data set, that performs each of the following steps:
#i. Fit a logistic regression model using all but the ith observation to predict Direction using Lag1 and Lag2.
#ii. Compute the posterior probability of the market moving up for the ith observation.
#iii. Use the posterior probability for the ith observation in order to predict whether or not the market moves up.
#iv. Determine whether or not an error was made in predicting the direction for the ith observation. If an error was made, then indicate this as a 1, and otherwise indicate it as a 0.
cv.error=rep(0,dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
  glm.fit=glm(Direction~Lag1+Lag2, data=Weekly[-i,], family=binomial)
  glm.probs=predict(glm.fit, Weekly[i,], type='response')
  glm.pred <- rep('Down', 1)
  glm.pred[glm.probs>0.5]='Up'
  responce <- mean(glm.pred==Weekly$Direction[1])
  cv.error[responce==0]=1
}

#e Take the average of the n numbers obtained in (d)iv in order to obtain the LOOCV estimate for the test error. Comment on the results.
mean(cv.error)
#The model does not predict correctly ever, the mean of cv.error is 1

#8. We will now perform cross-validation on a simulated data set.
#a Generate a simulated data set as follows:
#> set .seed (1)
#> y=rnorm (100)
#> x=rnorm (100)
#> y=x-2* x^2+ rnorm (100)
#In this data set, what is n and what is p? Write out the model used to generate the data in equation form.
set .seed (1)
y=rnorm (100)
x=rnorm (100)
y=x-2*x^2+ rnorm (100)
#n- number of observations, is 100 and p-value is 2

#b Create a scatterplot of X against Y . Comment on what you find.
par(mfrow=c(1, 1))
plot(x, y)
#y is quadratic function of x with negative coeffitient of x^2

#c Set a random seed, and then compute the LOOCV errors that result from fitting the following four models using least squares:
#i. Y = ??0 + ??1X + e
#ii. Y = ??0 + ??1X + ??2X2 + e
#iii. Y = ??0 + ??1X + ??2X2 + ??3X3 + e
#iv. Y = ??0 + ??1X + ??2X2 + ??3X3 + ??4X4 + e.
#Note you may find it helpful to use the data.frame() function to create a single data set containing both X and Y .

set .seed (1)
y=rnorm (100)
x=rnorm (100)
y=x-2*x^2+ rnorm (100)
XY <- data.frame(cbind(x, y))
str(XY)
summary(XY)

cv.error1=rep(0,4)
for (i in 1:4) {
  glm.fit=glm(y~poly(x, i), data=XY)
  cv.error1[i]=cv.glm(XY, glm.fit)$delta[1]
}
cv.error1

set .seed (2)
y=rnorm (100)
x=rnorm (100)
y=x-2*x^2+ rnorm (100)
XY <- data.frame(cbind(x, y))
str(XY)
summary(XY)

cv.error2=rep(0,4)
for (i in 1:4) {
  glm.fit=glm(y~poly(x, i), data=XY)
  cv.error2[i]=cv.glm(XY, glm.fit)$delta[1]
}
cv.error2

set .seed (3)
y=rnorm (100)
x=rnorm (100)
y=x-2*x^2+ rnorm (100)
XY <- data.frame(cbind(x, y))
str(XY)
summary(XY)

cv.error3=rep(0,4)
for (i in 1:4) {
  glm.fit=glm(y~poly(x, i), data=XY)
  cv.error3[i]=cv.glm(XY, glm.fit)$delta[1]
}
cv.error3

cv.error <- data.frame(cbind(cv.error1, cv.error2, cv.error3))
cv.error

#got a bit different errors in each iteration

#e Which of the models in (c) had the smallest LOOCV error? Is this what you expected? Explain your answer.
#Linear model clearly has a very bad fit, quardatic model is optimal (not surprising, because thats how the data was generated), increasing polynomial does not improve the model

#f Comment on the statistical significance of the coefficient estimates
that results from fitting each of the models in (c) using
least squares. Do these results agree with the conclusions drawn
based on the cross-validation results?

glm.fit=glm(y~x, data=XY)
summary(glm.fit)
glm.fit=glm(y~poly(x,2), data=XY)
summary(glm.fit)
glm.fit=glm(y~poly(x,3), data=XY)
summary(glm.fit)
glm.fit=glm(y~poly(x,4), data=XY)
summary(glm.fit)
#As expected and consistently with CV, statistical significance for coefficient estimates for x^1(in linear model), x^3 & x^4 models is low, but for x^2 is high (and x^1 in case of polynomial models)

#9. We will now consider the Boston housing data set, from the MASS library.
#a Based on this data set, provide an estimate for the population mean of medv. Call this estimate ??.

library(MASS)
?Boston
m <- mean(Boston$medv)
m

#b Provide an estimate of the standard error of ????. Interpret this result. Hint: We can compute the standard error of the sample mean by dividing the sample standard deviation by the square root of the number of observations.
std.error_m <- sd(Boston$medv)/nrow(Boston)^(1/2)
std.error_m

#c Now estimate the standard error of ???? using the bootstrap. How does this compare to your answer from (b)?
std.error_m_boot.fn=function(data, index)
  return(mean(data[index]))

#Try mannualy
set.seed(2)
std.error_m_boot.fn(Boston$medv, sample(nrow(Boston), nrow(Boston), replace=T))
set.seed(3)
std.error_m_boot.fn(Boston$medv, sample(nrow(Boston), nrow(Boston), replace=T))
#bootstrap
boot(Boston$medv, std.error_m_boot.fn, R=1000)
#Bootstrap gives slightly different (smaller) number for std.error as using formula on all of the data, mean is exact same


#d Based on your bootstrap estimate from (c), provide a 95% confidence interval for the mean of medv. Compare it to the results obtained using t.test(Boston$medv).
#Hint: You can approximate a 95% confidence interval using the formula [???? ??? 2SE(????), ???? + 2SE(????)].
m <- 22.53281
SE <- 0.4056164
low <- (m - 2*SE)
high <- m + 2*SE
cat('boot.95int [', low, high, ']') 
t.test(Boston$medv)
#The intervals are very similar, t.test one is a bit tighter

#e Based on this data set, provide an estimate, ????med, for the median value of medv in the population.
median(Boston$medv)

#f We now would like to estimate the standard error of ????med. Unfortunately, there is no simple formula for computing the standard error of the median. Instead, estimate the standard error of the median using the bootstrap. Comment on your findings.
median_boot.fn=function(data, index)
  return(median(data[index]))
boot(Boston$medv, median_boot.fn, R=1000)
#Std.Error of the median is smaller than std.error of the mean

#g Based on this data set, provide an estimate for the tenth percentile of medv in Boston suburbs. Call this quantity ????0.1. (You
                                                      can use the quantile() function.)
m0.1 <- quantile(Boston$medv, 0.1)
m0.1

#h Use the bootstrap to estimate the standard error of ????0.1. Comment on your findings.
m0.1_boot.fn=function(data, index)
  return(quantile(data[index], 0.1))
boot(Boston$medv, m0.1_boot.fn, R=1000)
#Std.error is the highest here 8compared to mean & median, I guess closer you get to the 'ends' of the dataset - the higher error in measurements you get. Looking at mean and median influence of datapoints from each end evens out.











