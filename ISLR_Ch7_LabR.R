library(ISLR)
str(Wage)
#####Polynomial regression#####
#Uses orthogonly polynomials
fit=lm(wage~poly(age,4), data=Wage)
coef(summary(fit))
#Uses non-orthogonly polynomials (fit2 (-, a, b))
fit2=lm(wage~poly(age,4, raw=T),data=Wage)
coef(summary(fit2))
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
fit2b=lm(wage~cbind(age, age^2, age^3, age^4) ,data=Wage)
coef(fit2b)
#Make range of age for which want predictions and use predict, also specify that we want standard error as well
agelims=range(Wage$age)
age.grid=seq(from=agelims[1], to=agelims[2])
preds=predict (fit, newdata=list(age=age.grid), se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2* preds$se.fit)

par(mfrow =c(1,2), mar=c(4.5 ,4.5 ,1 ,1), oma=c(0,0,4,0))
plot(Wage$age, Wage$wage, xlim=agelims, cex =.5, col ="darkgrey")
title ("Degree -4 Polynomial", outer =T)
lines(age.grid, preds$fit, lwd=2, col ="blue")
matlines (age.grid, se.bands, lwd =1, col ="blue", lty=3)

#Orthogonal or not - will give the same fitted values:
preds2 = predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit))

#Looking for the best degree of polynomial
fit.1 = lm(wage~age, data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#since poly() gives orthogonal polynomials and just look at the highest polynomial model - same p-values
coef(summary(fit.5))
#anova() works for both orthogonal or not polynomials & other components
fit.1= lm(wage~education+age, data=Wage)
fit.2= lm(wage~education+poly(age, 2), data=Wage)
fit.3= lm(wage~education+poly(age, 3), data=Wage)
anova(fit.1, fit.2, fit.3)
#Instead of anova could use CV

#Predict responce of wage above 250K
fit=glm(I(wage>250)~poly(age, 4), data=Wage, family=binomial)
preds=predict(fit, newdata=list(age=age.grid), se=T)
#calculate logit and confidence intervals, or change type to responce, However, the corresponding confidence intervals would not have been sensible because we would end up with negative probabilities!
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
preds=predict(fit, newdata =list(age=age.grid), type="response", se=T)
plot(Wage$age, I(Wage$wage >250), xlim=agelims, type ="n", ylim=c(0,.2))
points(jitter(Wage$age), I((Wage$wage >250)/5), cex =.5, pch ="|", col ="darkgrey")
lines(age.grid, pfit, lwd=2, col ="blue")
matlines(age.grid, se.bands, lwd =1, col ="blue", lty=3)

#Step function
# Cut picks break points; using breaks() can set wanted breaks
table(cut(Wage$age, 4))
fit=lm(wage~cut(age, 4), data=Wage)
coef(summary(fit))
pred=predict(fit, Wage, id=4)
length(pred)

#Splines
#bs() by default fits cubes
library(splines)
fit=lm(wage~bs(age, knots=c(25 ,40 ,60)) ,data=Wage)
pred=predict(fit, newdata=list(age=age.grid), se=T)
plot(Wage$age, Wage$wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se, lty ="dashed")
lines(age.grid, pred$fit-2*pred$se, lty ="dashed")

dim(bs(Wage$age, knots=c(25, 40, 60)))
dim(bs(Wage$age, df=6))
attr(bs(Wage$age, df=6), 'knots')

#Natural spline
fit2=lm(wage~ns(age, df=4), data=Wage)
pred2=predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col ="red", lwd =2)

#Smooth spline
plot(Wage$age, Wage$wage, xlim=agelims, cex =.5, col ="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(Wage$age, Wage$wage, df =16)
fit2=smooth.spline(Wage$age, Wage$wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd =2)
legend ("topright", legend =c("16 DF " ,"6.8 DF"), col=c("red","blue"), lty=1, lwd=2, cex=.8)

#Local Regression
plot(Wage$age, Wage$wage, xlim=agelims, cex =.5, col ="darkgrey")
title ("Local Regression")
fit=loess(wage~age, span =.2, data=Wage)
fit2=loess(wage~age, span =.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col ="red", lwd =2)
lines(age.grid ,predict(fit2, data.frame(age=age.grid)), col ="blue", lwd =2)
legend ("topright", legend =c("Span=0.2", "Span =0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

#Generalized Additive Models
#fit model using natural splines
gam1=lm(wage~ns(year, 4)+ns(age, 5)+education, data=Wage)
install.packages('gam')
library(gam)
#use smoothing splines
gam.m3=gam(wage~s(year, 4)+s(age, 5)+education, data=Wage)
par(mfrow=c(2,3))
plot(gam.m3, se=TRUE ,col="blue")
plot.gam(gam1, se=TRUE, col='red')

gam.m1=gam(wage~s(age, 5)+education, data=Wage)
gam.m2=gam(wage~year+s(age, 5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test='F')
summary(gam.m3)

preds=predict(gam.m2, newdata=Wage)

#Local regression in GAM
gam.lo=gam(wage~s(year, df=4)+lo(age, span=0.7)+education, data=Wage)
plot.gam(gam.lo, se=TRUE, col='green')
gam.lo.i=gam(wage~lo(year, age, span=0.5)+education, data=Wage)
install.packages('akima')
library(akima)
plot(gam.lo.i)

#Logistic regression in GAM
gam.lr=gam(I(wage>250)~s(year, df=5)+education, family='binomial', data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=TRUE, col='green')
table(Wage$education, I(Wage$wage>250))
#no high earners- so exclude the category to get better fit
gam.lr.s=gam(I(wage>250)~year+s(age, df=5)+education, family='binomial', data=Wage, subset=(education!='1. <HS Grad'))
plot(gam.lr.s, se=T, col='green')

#Applied
#6. In this exercise, you will further analyze the Wage data set considered throughout this chapter.
#a Perform polynomial regression to predict wage using age. Use cross-validation to select the optimal degree d for the polynomial.
#What degree was chosen, and how does this compare to the results of hypothesis testing using ANOVA? Make a plot of the resulting polynomial fit to the data.

#Cross-Validation using k training sets#
k=10
set.seed(1)
#split into training sets
folds=sample(1:k, nrow(Wage), replace=TRUE)
#initialise matrix to put cv.errors in
cv.errors=matrix(NA, k, 20, dimnames=list(NULL, paste(1:20)))
#calc best model for each degree of polynomial (1-20) on train dataset (1-k) using j element as test and writes into cv.matrix the MSE error for each jd pair
for (j in 1:k) {
  for (d in 1:20) {
  fit=lm(wage~poly(age,d), data=Wage[folds!=j,])
  pred=predict(fit, Wage[folds==j, ], id=d)
  cv.errors[j,d]=mean((Wage$wage[folds==j]-pred)^2)
  }
}
cv.errors
#calc mean of MSE of k tests for models with i number of variables & plots that
mean.cv.errors=apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b', xlab='Degree of polynomial', ylab='Mean Cross-Validation Error')
best.poly=which.min(mean.cv.errors)
points(best.poly, mean.cv.errors[best.poly], col='red', cex=2, pch=20)
#rerun fitting best model selection using whole dataset and pick the model with optimal number of predictors
fit.best.poly=lm(wage~poly(age, best.poly), data=Wage)
coef(fit.best.poly)
good.enough.poly=4
fit.enough.poly=lm(wage~poly(age, good.enough.poly), data=Wage)
coef(fit.enough.poly)
#Since we did not run anova for more then 5 degrees of polynomial, cant compare the 9 degrees, but up to 5 degrees the best model is fitting poly 4 and coefficients of CV and anova are exactly the same

agelims=range(Wage$age)
age.grid=seq(from=agelims[1], to=agelims[2])
preds.best.poly=predict(fit.best.poly, newdata=list(age=age.grid), se=TRUE)
se.bands.best.poly=cbind(preds.best.poly$fit+2*preds.best.poly$se.fit, preds.best.poly$fit-2* preds.best.poly$se.fit)
preds.enough.poly=predict(fit.enough.poly, newdata=list(age=age.grid), se=TRUE)
se.bands.enough.poly=cbind(preds.enough.poly$fit+2*preds.enough.poly$se.fit, preds.enough.poly$fit-2* preds.enough.poly$se.fit)

par(mfrow =c(1,1), mar=c(4.5 ,4.5 ,1 ,1), oma=c(0,0,4,0))
plot(Wage$age, Wage$wage, xlim=agelims, cex =.5, col ="darkgrey")
title ("Degree -4 and 9 Polynomial", outer =T)
lines(age.grid, preds.best.poly$fit, lwd=2, col ="blue")
matlines (age.grid, se.bands.best.poly, lwd =1, col ="blue", lty=3)
lines(age.grid, preds.enough.poly$fit, lwd=2, col ="red")
matlines (age.grid, se.bands.enough.poly, lwd =1, col ="red", lty=3)
#Fit with poly 9 is clearly more wavy then with 4, hard to tell if its better or worse, I guess smoother function is more natural


#b Fit a step function to predict wage using age, and perform crossvalidation to choose the optimal number of cuts. Make a plot of the fit obtained.
#Could not make the loop dtructure using lessons code - internet helped
library(boot)
cvs <- rep(NA, 20)
set.seed(1)
for (d in 2:20) {
  Wage$age.cut <- cut(Wage$age, d)
  fit <- glm(wage ~ age.cut, data = Wage)
  cvs[d] <- cv.glm(Wage, fit, K = 10)$delta[1]
}

plot(2:20, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "b")
best.step <- which.min(cvs)
points(best.step, cvs[best.step], col = "red", cex = 2, pch = 20)
points(8, cvs[8], col = "blue", cex = 2, pch = 20)

fit.best.step=glm(wage ~ cut(age, best.step), data = Wage)
fit.good.step=glm(wage ~ cut(age, 8), data = Wage)

agelims=range(Wage$age)
age.grid=seq(from=agelims[1], to=agelims[2])
preds.best.step=predict(fit.best.step, newdata=list(age=age.grid), se=TRUE)
se.bands.best.step=cbind(preds.best.step$fit+2*preds.best.step$se.fit, preds.best.step$fit-2* preds.best.step$se.fit)
preds.good.step=predict(fit.good.step, newdata=list(age=age.grid), se=TRUE)
se.bands.good.step=cbind(preds.good.step$fit+2*preds.good.step$se.fit, preds.good.step$fit-2* preds.good.step$se.fit)

par(mfrow =c(1,2), mar=c(4.5,4.5 ,1 ,1))
plot(Wage$age, Wage$wage, xlim=agelims, cex =.5, col ="darkgrey", sub='Best Step Fit')
lines(age.grid, preds.best.step$fit, lwd=2, col ="red")
matlines (age.grid, se.bands.best.step, lwd =1, col ="red", lty=3)
plot(Wage$age, Wage$wage, xlim=agelims, cex =.5, col ="darkgrey", sub='Good Step Fit')
lines(age.grid, preds.good.step$fit, lwd=2, col ="blue")
matlines(age.grid, se.bands.good.step, lwd =1, col ="blue", lty=3)


#7. The Wage data set contains a number of other features not explored in this chapter, such as marital status (maritl), job class (jobclass), and others. Explore the relationships between some of these other predictors and wage, and use non-linear fitting techniques in order to fit flexible models to the data. Create plots of the results obtained, and write a summary of your findings.
#Have a look at what we have
str(Wage)
par(mfrow =c(1,2), mar=c(4.5,4.5 ,1 ,1))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

gam0=gam(wage~s(year, df=4)+lo(age, span=0.7)+education, data=Wage)
gam1=gam(wage~s(year, df=4)+lo(age, span=0.7)+education+maritl, data=Wage)
gam2=gam(wage~s(year, df=4)+lo(age, span=0.7)+education+jobclass, data=Wage)
gam3=gam(wage~s(year, df=4)+lo(age, span=0.7)+education+maritl+jobclass, data=Wage)
anova(gam0, gam1, gam2, gam3)
#Fit(s) 1-3 are all superior over 0
par(mfrow =c(1,3), mar=c(3,2 ,1 ,1))
plot(gam0, se=T, col='green')
par(mfrow =c(1,4), mar=c(3,2 ,1 ,1))
plot(gam1, se=T, col='red')
par(mfrow =c(2,3), mar=c(3,2 ,1 ,1))
plot(gam3, se=T, col='blue')
gam4=gam(wage~s(year, df=4)+lo(age, span=0.7)+education+maritl+jobclass+race, data=Wage)
gam5=gam(wage~s(year, df=4)+lo(age, span=0.7)+education+maritl+jobclass+health, data=Wage)
anova(gam0, gam1, gam2, gam3, gam4, gam5)
#contrary, nor Race nor health do not add much to the model


#8. Fit some of the non-linear models investigated in this chapter to the Auto data set. Is there evidence for non-linear relationships in this data set? Create some informative plots to justify your answer.
str(Auto)
set.seed(1)
train=sample(dim(Auto)[1], dim(Auto)[1]/2)
fit0=lm(mpg~horsepower, data=Auto, subset=train)
MSE0 <- mean((Auto$mpg-predict(fit0, Auto))[-train]^2)
coef(summary(fit0))
#stat signif
fit1=lm(mpg~poly(horsepower, 4), data=Auto, subset=train)
coef(summary(fit1))
#stat signif till poly 2
MSE1 <- mean((Auto$mpg-predict(fit1, Auto))[-train]^2)
fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
coef(summary(fit2))
#stat signif
MSE2 <- mean((Auto$mpg-predict(fit2, Auto))[-train]^2)
MSE <- c(MSE0, MSE1, MSE2)
print(MSE)
#improvement over linear model using poly 4 &2, poly 2 is the best - evidence of non-linear relation
hp.lims=range(Auto$horsepower)
hp.grid=seq(from=hp.lims[1], to=hp.lims[2])
fit2=lm(mpg~poly(horsepower, 2), data=Auto)
pred.fit2=predict(fit2, newdata=list(horsepower=hp.grid), se=T)
par(mfrow =c(1,1), mar=c(4.5 ,4.5 ,1 ,1))
plot(Auto$horsepower, Auto$mpg, col="gray", main ='Horsepower squared')
lines(hp.grid, pred.fit2$fit, lwd=2)
lines(hp.grid, pred.fit2$fit+2*pred.fit2$se, lty ="dashed")
lines(hp.grid, pred.fit2$fit-2*pred.fit2$se, lty ="dashed")

#Add other variables to poly2 horsepower
fit3 <- gam(mpg~poly(horsepower, 2) + cylinders + weight, data=Auto, subset=train)
summary(fit3)
#stat signif cylinders do not add much, but weight does
MSE3 <- mean((Auto$mpg-predict(fit3, Auto))[-train]^2)
fit4 <- gam(mpg~poly(horsepower, 2) + weight, data=Auto, subset=train)
summary(fit4)
MSE4 <- mean((Auto$mpg-predict(fit4, Auto))[-train]^2)
MSE <- c(MSE, MSE3, MSE4)
print(MSE)
#improvement by adding weight based on MSE
#just want to check for fit2 , fit3, fit4 using anova()
fit2 <- gam(mpg~poly(horsepower, 2), data=Auto, subset=train)
anova(fit2,fit3, fit4)
#not surprising, same thing here, adding weight as a variable improves the model
par(mfrow =c(1,3), mar=c(3,2 ,1 ,1))
plot(fit4, se=T, col='green')

#fit model using natural splines
fit5=lm(mpg~ns(horsepower, 4), data=Auto, subset=train)
summary(fit5)
#all significant
MSE5 <- mean((Auto$mpg-predict(fit5, Auto))[-train]^2)
#use smoothing splines
fit6=gam(mpg~s(horsepower, 4), data=Auto, subset=train)
summary(fit6)
MSE6 <- mean((Auto$mpg-predict(fit6, Auto))[-train]^2)
par(mfrow=c(2,3))
plot.gam(fit5, se=TRUE ,col="blue")
plot.gam(fit6, se=TRUE, col='red')
MSE <- c(MSE, MSE5, MSE6)
print(MSE)
#fits 5&6 are very similar, try to add weight & cylinders
fit7=lm(mpg~ns(horsepower, 4)+weight, data=Auto, subset=train)
summary(fit7)
#all significant
MSE7 <- mean((Auto$mpg-predict(fit7, Auto))[-train]^2)
#use smoothing splines
fit8=gam(mpg~s(horsepower, 4)+weight, data=Auto, subset=train)
summary(fit8)
MSE8 <- mean((Auto$mpg-predict(fit8, Auto))[-train]^2)
par(mfrow=c(2,3))
plot.gam(fit7, se=TRUE ,col="blue")
plot.gam(fit8, se=TRUE, col='red')
MSE <- c(MSE, MSE7, MSE8)
print(MSE)
#both show improvement, fit8 is a bbit better
#try to add cylinders to fit8
fit9=gam(mpg~s(horsepower, 4)+weight+cylinders, data=Auto, subset=train)
summary(fit9)
MSE9 <- mean((Auto$mpg-predict(fit9, Auto))[-train]^2)
MSE <- c(MSE, MSE9)
print(MSE)
#improves just a little bit but not significantly
#I would go with fit8

#This question uses the variables dis (the weighted mean of distances to five Boston employment centers)  and nox (nitrogen oxides concentration in parts per 10 million) from the Boston data. We will treat dis as the predictor and nox as the response.
#a Use the poly() function to fit a cubic polynomial regression to predict nox using dis. Report the regression output, and plot the resulting data and polynomial fits.
library(MASS)
str(Boston)
poly3 <- lm(nox~poly(dis, 3), data=Boston)
coef(summary(poly3))
dislims=range(Boston$dis)
dis.grid=seq(from=dislims[1], to=dislims[2])
preds=predict(poly3, newdata=list(dis=dis.grid), se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2* preds$se.fit)
par(mfrow =c(1,1), mar=c(4.5 ,4.5 ,1 ,1), oma=c(0,0,4,0))
plot(Boston$dis, Boston$nox, xlim=dislims, cex =.5, col ="darkgrey")
title ("Degree 3 Polynomial", outer =T)
lines(dis.grid, preds$fit, lwd=2, col ="blue")
matlines(dis.grid, se.bands, lwd =1, col ="blue", lty=3)
#Looks good to me; wide confidence intervals on the ends

#b Plot the polynomial fits for a range of different polynomial degrees (say, from 1 to 10), and report the associated residual sum of squares.
RSS <- rep(NA, 10)
for (i in 1:10) {
  fit <- lm(nox~poly(dis, i), data=Boston)
  RSS[i] <- sum(fit$residuals^2)
}
plot(1:10, RSS, xlab = "Degree of polynomial", ylab = "RSS", type = "b")
#RSS constantly decreasing up to 10, try with more polynomial, get tighter fit with increasing polynomial, need to check CV or training&test set

#c Perform cross-validation or another approach to select the optimal degree for the polynomial, and explain your results.
library(boot)
cvs <- rep(NA, 10)
set.seed(1)
for (i in 1:10) {
  fit <- glm(nox~poly(dis, i), data=Boston)
  cvs[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, cvs, xlab = "Cuts", ylab = "Test MSE", type = "b")
best.step <- which.min(cvs)
points(best.step, cvs[best.step], col = "red", cex = 2, pch = 20)
#poly4 works best

#d Use the bs() function to fit a regression spline to predict nox using dis. Report the output for the fit using four degrees of freedom. How did you choose the knots? Plot the resulting fit.
library(splines)
fit=lm(nox~bs(dis, df=4) ,data=Boston)
pred=predict(fit, newdata=list(dis=dis.grid), se=T)
plot(Boston$dis, Boston$nox, col="gray")
lines(dis.grid, pred$fit, lwd=2)
lines(dis.grid, pred$fit+2*pred$se, lty ="dashed")
lines(dis.grid, pred$fit-2*pred$se, lty ="dashed")
#set the model using df, quantiles used automatically to chose position of knots

#e Now fit a regression spline for a range of degrees of freedom, and
plot the resulting fits and report the resulting RSS. Describe the
results obtained.
RRS <- rep(NA, 16)
set.seed(1)
for (i in 3:16) {
  fit <- lm(nox~bs(dis, i), data=Boston)
  RRS[i] <- sum(fit$residuals^2)
}
plot(3:16, RRS[3:16], xlab = "DF", ylab = "RRS", type = "b")
best.step <- which.min(RRS)
points(best.step, RRS[best.step], col = "red", cex = 2, pch = 20)
#DF 14 works best

#f Perform cross-validation or another approach in order to select the best degrees of freedom for a regression spline on this data. Describe your results.
cvs <- rep(NA, 16)
set.seed(1)
for (i in 3:16) {
  fit <- glm(nox~bs(dis, i), data=Boston)
  cvs[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}

k=10
set.seed(1)
folds=sample(1:k, nrow(Boston), replace=TRUE)
cv.errors=matrix(NA, k, 14, dimnames=list(NULL, paste(3:16)))
for (j in 1:k) {
  for (d in 3:16) {
    fit=lm(nox~bs(dis,d), data=Boston[folds!=j,])
    pred=predict(fit, Boston[folds==j, ], id=d)
    cv.errors[j,d-2]=mean((Boston$nox[folds==j]-pred)^2)
  }
}
#Both ways get this error MSG, do not know how to solve: In bs(dis, degree = 3L, knots = structure(c(1.79651428571429,  ... : some 'x' values beyond boundary knots may cause ill-conditioned bases
#Seems like its not a big deal if the ranges are not too off between training and test data sets
#using the first approach - go forward

cvs <- rep(NA, 16)
set.seed(1)
for (i in 3:16) {
  fit <- glm(nox~bs(dis, i), data=Boston)
  cvs[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(3:16, cvs[3:16], xlab = "DF", ylab = "CV", type = "b")
best.step <- which.min(cvs)
points(best.step, cvs[best.step], col = "red", cex = 2, pch = 20)
fit=lm(nox~bs(dis,12), data=Boston)
pred=predict(fit, newdata=list(dis=dis.grid), se=T)
plot(Boston$dis, Boston$nox, col="gray")
lines(dis.grid, pred$fit, lwd=2)
lines(dis.grid, pred$fit+2*pred$se, lty ="dashed")
lines(dis.grid, pred$fit-2*pred$se, lty ="dashed")

#10. This question relates to the College data set.
#a Split the data into a training set and a test set. Using out-of-state
tuition as the response and the other variables as the predictors,
perform forward stepwise selection on the training set in order
to identify a satisfactory model that uses just a subset of the
predictors.
?College
dim(College)
set.seed(1)
train=sample(c(TRUE, FALSE), nrow(College), rep=TRUE)
test=!train
library(leaps)
regfit.fw=regsubsets(Outstate~., data=College[train,], nvmax=17, method='forward')
reg.summary <- summary(regfit.fw)

par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS', type='l')
plot(reg.summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col='red', cex=2, pch=20)
abline(h=max(reg.summary$adjr2) + sd(reg.summary$adjr2), col='red')
abline(h=max(reg.summary$adjr2) - sd(reg.summary$adjr2), col='red')

plot(reg.summary$cp, xlab='Number of Variables', ylab='Cp', type='l')
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col='red', cex=2, pch=20)
abline(h=min(reg.summary$cp) + sd(reg.summary$cp), col='red')
abline(h=min(reg.summary$cp) - sd(reg.summary$cp), col='red')

plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col='red', cex=2, pch=20)
abline(h=min(reg.summary$bic) + sd(reg.summary$bic), col='red')
abline(h=min(reg.summary$bic) - sd(reg.summary$bic), col='red')

#either optimum, based on RSq, Cp or BIC is fine, since all of them are withine 2StDev from each other
#lets go with 6 variables (as based on BIC)
regfit.best=regsubsets(Outstate~., data=College, nvmax=17, method='forward')
coef(regfit.best, 6)
#Private+Room.Board+PhD+perc.alumni+Expend+Grad.Rate

#b Fit a GAM on the training data, using out-of-state tuition as the response and the features selected in the previous step as the predictors. Plot the results, and explain your findings.
#lets fit natural splines
str(College)
gam.col1 <- gam(Outstate~Private+s(Room.Board,2)+s(PhD, 3)+s(perc.alumni, 3)+s(Expend, 4) +s(Grad.Rate, 4), data=College[train,])
par(mfrow=c(2,3))
plot(gam.col1, se=TRUE ,col="blue")

gam.col2 <- gam(Outstate~Private+s(Room.Board,3)+s(PhD, 3)+lo(perc.alumni, span=0.7)+lo(Expend, span=0.7) +lo(Grad.Rate, span=0.2), data=College[train,])
par(mfrow=c(4,3))
plot(gam.col1, se=TRUE ,col="blue")
plot(gam.col2, se=TRUE ,col="green")

#c Evaluate the model obtained on the test set, and explain the results obtained.
anova(gam.col1, gam.col2, test='F')
summary(gam.col1)
summary(gam.col2)
#no significant difference between the models in train fit
preds1=predict(gam.col1, newdata=College[test,])
preds2=predict(gam.col2, newdata=College[test,])
MSE1 <- mean((College$Outstate[test]-preds1)^2)
MSE2 <- mean((College$Outstate[test]-preds2)^2)
print(c(MSE1, MSE2))
#gam.col1 has lower MSE

TSS <- mean((College$Outstate[test] - mean(College$Outstate[test]))^2) 
RSS1 <- 1-MSE1/TSS
RSS2 <- 1-MSE2/TSS
print(c(RSS1, RSS2))
#gam.col1 explains 76% variability, while the second model only 72%

#d For which variables, if any, is there evidence of a non-linear relationship with the response?
summary(gam.col1)
#Variable 'Expend' showes non-linear relation to 'Outstate'

#11. In Section 7.7, it was mentioned that GAMs are generally fit using a backfitting approach. The idea behind backfitting is actually quite simple. We will now explore backfitting in the context of multiple linear regression.
#Suppose that we would like to perform multiple linear regression, but we do not have software to do so. Instead, we only have software to perform simple linear regression. Therefore, we take the following iterative approach: we repeatedly hold all but one coefficient estimate fixed at its current value, and update only that coefficient estimate using a simple linear regression. The process is continued until convergence???that is, until the coefficient estimates stop changing. We now try this out on a toy example.
#a Generate a response Y and two predictors X1 and X2, with n = 100.

X1 <- rnorm(100, mean=5, sd=2)
X2 <- rnorm(100, mean=3, sd=4)
Y <- 0.2*X1^2 -12*X2

#b Initialize ????1 to take on a value of your choice. It does not matter what value you choose.
b1 <- rnorm(1, mean=4, sd=1)

#Keeping ????1 fixed, fit the model
Y ??? ?? ??1X1 = ??0 + ??2X2 + .
You can do this as follows:
  > a=y-beta1 *x1
> beta2=lm(a???x2)$coef [2]














