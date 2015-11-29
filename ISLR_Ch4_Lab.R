#10. This question should be answered using the Weekly data set, which is part of the ISLR package. This data is similar in nature to the Smarket data from this chapter???s lab, except that it contains 1, 089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.
#a. Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?
library(ISLR)
summary(Weekly)
attach(Weekly)
plot(Weekly)
plot(Volume, Year)
cor(Weekly[,-9])
#Data is pretty similar to Smarket: Lag1-5 & Today are in the same range; Volume correlates with Year (based on the plot and correlation coefficient)

#b Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)
#Only Lag2 is statistically significant predictor base on its low p-value for Z-statistic 

#c Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.
glm.probs=predict(glm.fit, type='response')
dim(Weekly)
contrasts(Direction)
glm.pred=rep('Down', 1089)
glm.pred[glm.probs>0.5]='Up'
table(glm.pred, Direction)
mean(glm.pred==Direction)
#Computed model is not much better than a random guess. Taking into account that confusion matrix was calculated using training dataset, one would expect this model to perform even worse on a test dataset.

#d Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010).
train=(Year<2009)
Weekly.test=Weekly[!train,]
Direction.test=Direction[!train]

glm.fit=glm(Direction~Lag2, data=Weekly, subset=train, family=binomial)
glm.probs=predict(glm.fit, Weekly.test, type='response')
glm.pred=rep('Down', 104)
glm.pred[glm.probs>0.5]='Up'
table(glm.pred, Direction.test)
mean(glm.pred==Direction.test)

#e Repeat (d) using LDA.
lda.fit=lda(Direction~Lag2, data=Weekly, subset=train)
lda.pred=predict(lda.fit, Weekly.test)
lda.class=lda.pred$class
table(lda.class, Direction.test)
mean(lda.class==Direction.test)

#f Repeat (d) using QDA.
qda.fit=qda(Direction~Lag2, data=Weekly, subset=train)
qda.pred=predict(qda.fit, Weekly.test)
qda.class=qda.pred$class
table(qda.class, Direction.test)
mean(qda.class==Direction.test)

#g Repeat (d) using KNN with K = 1.
train.X=cbind(Lag2)[train,]
test.X=cbind(Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(data.frame(train.X), data.frame(test.X), train.Direction, k=1)
table(knn.pred, Direction.test)
mean(knn.pred==Direction.test)

#h Which of these methods appears to provide the best results on this data?

#1. Logistic Regression and Linear Discriminant Analysis give identicall results and best perform on the data, giving 62.5% accurate prediction.
#2. Quadratic Discriminant Analysis and K-Nearest Neighbors seem to overfit the data, with 58.7% and 50% accuracy, respectively.

#i Experiment with different combinations of predictors, including possible transformations and interactions, for each of the methods. Report the variables, method, and associated confusion matrix that appears to provide the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier.
#Varying K in KNN
knn.pred=knn(data.frame(train.X), data.frame(test.X), train.Direction, k=3)
table(knn.pred, Direction.test)
mean(knn.pred==Direction.test)

knn.pred=knn(data.frame(train.X), data.frame(test.X), train.Direction, k=5)
table(knn.pred, Direction.test)
mean(knn.pred==Direction.test)

knn.pred=knn(data.frame(train.X), data.frame(test.X), train.Direction, k=4)
mean(knn.pred==Direction.test)
#Setting k=4 seem to be optimal for KNN performance, but it is still worse that LR & LDA

#Playing around with parametric models
glm.fit=glm(Direction~Lag1+Lag2+Volume, data=Weekly, subset=train, family=binomial)
glm.probs=predict(glm.fit, Weekly.test, type='response')
glm.pred=rep('Down', 104)
glm.pred[glm.probs>0.5]='Up'
table(glm.pred, Direction.test)
mean(glm.pred==Direction.test)

glm.fit=glm(Direction~Lag1+Lag2, data=Weekly, subset=train, family=binomial)
glm.probs=predict(glm.fit, Weekly.test, type='response')
glm.pred=rep('Down', 104)
glm.pred[glm.probs>0.5]='Up'
table(glm.pred, Direction.test)
mean(glm.pred==Direction.test)
glm.pred=rep('Down', 104)
glm.pred[glm.probs>0.7]='Up'
table(glm.pred, Direction.test)
mean(glm.pred==Direction.test)
glm.pred=rep('Down', 104)
glm.pred[glm.probs>0.3]='Up'
table(glm.pred, Direction.test)
mean(glm.pred==Direction.test)

lda.fit=lda(Direction~Lag2+Lag1, data=Weekly, subset=train)
lda.pred=predict(lda.fit, Weekly.test)
lda.class=lda.pred$class
table(lda.pred, Direction.test)
mean(lda.class==Direction.test)

qda.fit=qda(Direction~Lag2+Lag1, data=Weekly, subset=train)
qda.pred=predict(qda.fit, Weekly.test)
qda.class=qda.pred$class
table(qda.class, Direction.test)
mean(qda.class==Direction.test)

#More predictors we add - the worse the prediction gets; Lag2 as the only predictor seems to be optimal for these models
#LR & LDA behave the same under analysed paramiters

#11. In this problem, you will develop a model to predict whether a given car gets high or low gas mileage based on the Auto data set.
#a Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function. Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables.
summary(Auto)
dim(Auto)
mpg01 = rep(0, 392)
mpg01[Auto$mpg>median(Auto[,1])]=1
sum(mpg01)
Auto.01 <- data.frame(Auto, mpg01)
summary(Auto.01)

#b Explore the data graphically in order to investigate the association between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings.
par(mfrow = c(3, 3))
plot(mpg01 ~ ., Auto.01)
#Although, data points corresponding to mpg01 overlap, displacement, horsepower and weight might be usefull for predicting mpg01
library(ggplot2)
Auto.01.cyl <- as.data.frame(table(mpg01, Auto$cylinders))
ggplot(Auto.01.cyl, aes(x=Var2, y=mpg01))+ geom_point(size=Auto.01.cyl$Freq/2)+ labs(x='Cylinders', y = 'MPG')
Auto.01.year <- as.data.frame(table(mpg01, Auto$year))
ggplot(Auto.01.year, aes(x=Var2, y=mpg01)) + geom_point(size=Auto.01.year$Freq)+ labs(x='Year', y = 'MPG')
Auto.01.origin <- as.data.frame(table(mpg01, Auto$origin))
ggplot(Auto.01.origin, aes(x=Var2, y=mpg01)) + geom_point(size=Auto.01.origin$Freq/2)+ labs(x='Origin', y = 'MPG')
cor(Auto.01[,-9])
#Cilinders and origin might help as well, overlap of the data does not seem that big
#Base on the graphs and correlation coeffitients, acceleration and year do not seem to be as usefull as other variables, judging one variable at the time. A lot of variables (cylinders, displacement, horsepower, weight are correlated)

#c Split the data into a training set and a test set.
train=(Auto.01$year>80)
Auto.01.test=Auto.01[!train,]
mpg01.test=mpg01[!train]
dim(Auto.01)
dim(Auto.01.test)
(392-334)/392
#Took 15% of data into Test

#d Perform LDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?
library(MASS)
lda.fit=lda(mpg01~cylinders+displacement+weight, data=Auto.01, subset=train)
lda.pred=predict(lda.fit, Auto.01.test)
lda.class=lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class==mpg01.test)
#does not look too bad, correct 86.8%
lda.fit=lda(mpg01~cylinders+displacement+weight+horsepower, data=Auto.01, subset=train)
lda.pred=predict(lda.fit, Auto.01.test)
lda.class=lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class==mpg01.test)
#got worse 84%
lda.fit=lda(mpg01~cylinders, data=Auto.01, subset=train)
lda.pred=predict(lda.fit, Auto.01.test)
lda.class=lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class==mpg01.test)
#got worse 72%
lda.fit=lda(mpg01~displacement, data=Auto.01, subset=train)
lda.pred=predict(lda.fit, Auto.01.test)
lda.class=lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class==mpg01.test)
#got worse 84%
lda.fit=lda(mpg01~cylinders+displacement+weight+origin, data=Auto.01, subset=train)
lda.pred=predict(lda.fit, Auto.01.test)
lda.class=lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class==mpg01.test)
#got better 87.1%
#try if can delete one of the predictors
lda.fit=lda(mpg01~displacement+cylinders+origin, data=Auto.01, subset=train)
lda.pred=predict(lda.fit, Auto.01.test)
lda.class=lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class==mpg01.test)
#got still 87.1%
#and the winner is the mpg01~cylinders+displacement+origin with 87.1%

#e Perform QDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?
qda.fit=qda(mpg01~displacement+cylinders+origin, data=Auto.01, subset=train)
#got error msg, 'rank deficiency', because cylinders & origin are considered factors!? and that seems to be a problem
qda.fit=qda(mpg01~displacement, data=Auto.01, subset=train)
qda.pred=predict(qda.fit, Auto.01.test)
qda.class=qda.pred$class
table(qda.class, mpg01.test)
mean(qda.class==mpg01.test)
#just 56%, correctly identifies all '0'
qda.fit=qda(mpg01~displacement+horsepower+weight, data=Auto.01, subset=train)
qda.pred=predict(qda.fit, Auto.01.test)
qda.class=qda.pred$class
table(qda.class, mpg01.test)
mean(qda.class==mpg01.test)
#got lower, 52%
qda.fit=qda(mpg01~horsepower+weight, data=Auto.01, subset=train)
qda.pred=predict(qda.fit, Auto.01.test)
qda.class=qda.pred$class
table(qda.class, mpg01.test)
mean(qda.class==mpg01.test)
#got lower, 49%
#Seems like QDA is not a good model, not much better than a random guess
#Maybe I should solve the issue with possible "factor" variables (though not sure how)

#f Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?
glm.fit=glm(mpg01~cylinders+displacement+origin, data=Auto.01, subset=train, family=binomial)
summary(glm.fit)
#high p-values - no variables seem relevant
glm.probs=predict(glm.fit, Auto.01.test, type='response')
dim(Auto.01.test)
glm.pred=rep(0, 334)
glm.pred[glm.probs>0.5]=1
table(glm.pred, mpg01.test)
mean(glm.pred==mpg01.test)
#got 72% - much better than QDA, but this version is worse than
glm.fit=glm(mpg01~cylinders+displacement+origin+weight+year+horsepower, data=Auto.01, subset=train, family=binomial)
summary(glm.fit)
glm.fit=glm(mpg01~weight, data=Auto.01, subset=train, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, Auto.01.test, type='response')
glm.pred=rep(0, 334)
glm.pred[glm.probs>0.5]=1
table(glm.pred, mpg01.test)
mean(glm.pred==mpg01.test)
#76%
glm.fit=glm(mpg01~cylinders, data=Auto.01, subset=train, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, Auto.01.test, type='response')
glm.pred=rep(0, 334)
glm.pred[glm.probs>0.5]=1
table(glm.pred, mpg01.test)
mean(glm.pred==mpg01.test)
#71%
glm.fit=glm(mpg01~cylinders+weight, data=Auto.01, subset=train, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, Auto.01.test, type='response')
glm.pred=rep(0, 334)
glm.pred[glm.probs>0.5]=1
table(glm.pred, mpg01.test)
mean(glm.pred==mpg01.test)
#76%
#Seems like the best I can do here is 76%: better than QDA but worse than LDA

#g Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to perform the best on this data set?
train.X=cbind(Auto.01$cylinder, Auto.01$displacement, Auto.01$weight, Auto.01$origin)[train,]
test.X=cbind(Auto.01$cylinder, Auto.01$displacement, Auto.01$weight, Auto.01$origin)[!train,]
train.mpg01=Auto.01$mpg01[train]
set.seed(1)
library(class)
knn.pred=knn(train.X, test.X, train.mpg01, k=1)
table(knn.pred, mpg01.test)
mean(knn.pred==mpg01.test)
#53%
knn.pred=knn(train.X, test.X, train.mpg01, k=3)
table(knn.pred, mpg01.test)
mean(knn.pred==mpg01.test)
#80% - thats max for these 4 predictors
#lets check if can get rid of some of the predictors
train.X=cbind(Auto.01$cylinder, Auto.01$displacement, Auto.01$weight)[train,]
test.X=cbind(Auto.01$cylinder, Auto.01$displacement, Auto.01$weight)[!train,]
train.mpg01=Auto.01$mpg01[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.mpg01, k=3)
table(knn.pred, mpg01.test)
mean(knn.pred==mpg01.test)
#80%
train.X=cbind(Auto.01$displacement, Auto.01$weight, Auto.01$origin)[train,]
test.X=cbind(Auto.01$displacement, Auto.01$weight, Auto.01$origin)[!train,]
train.mpg01=Auto.01$mpg01[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.mpg01, k=3)
table(knn.pred, mpg01.test)
mean(knn.pred==mpg01.test)
#80%
train.X=cbind(Auto.01$displacement, Auto.01$weight)[train,]
test.X=cbind(Auto.01$displacement, Auto.01$weight)[!train,]
train.mpg01=Auto.01$mpg01[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.mpg01, k=3)
table(knn.pred, mpg01.test)
mean(knn.pred==mpg01.test)
#80%
#any less - gets worse
#here 2 predictors, displacement & weight at K=3 seem to be optimal
#All in all, in this dataset, LDA seem to be optimal with 87% accurate prediction base on displacement+cylinders+origin predictors
#once again the graphs
plot(mpg01 ~ displacement, Auto.01)
Auto.01.cyl <- as.data.frame(table(mpg01, Auto$cylinders))
ggplot(Auto.01.cyl, aes(x=Var2, y=mpg01))+ geom_point(size=Auto.01.cyl$Freq/2)+ labs(x='Cylinders', y = 'MPG')
Auto.01.origin <- as.data.frame(table(mpg01, Auto$origin))
ggplot(Auto.01.origin, aes(x=Var2, y=mpg01)) + geom_point(size=Auto.01.origin$Freq/2)+ labs(x='Origin', y = 'MPG')

#wonder if mpg~year correlation makes the test set is not vsery fair
#try another split into train/test set
#'random picking'??
set.seed(1)
rands <- rnorm(nrow(Auto))
test <- rands > quantile(rands,0.75)
Auto.01.train <- Auto.01[!test,]
Auto.01.test <- Auto.01[test,]

#12. This problem involves writing functions.
#a Write a function, Power(), that prints out the result of raising 2 to the 3rd power. In other words, your function should compute 23 and print out the results.
#Hint: Recall that x^a raises x to the power a. Use the print() function to output the result.

Power <- function(x) {
  x <- 2^3
  return(x)
}
Power(3)

#b Create a new function, Power2(), that allows you to pass any two numbers, x and a, and prints out the value of x^a. You can do this by beginning your function with the line Power2 =function (x,a){ You should be able to call your function by entering, for instance, > Power2 (3,8) on the command line. This should output the value of 38, namely, 6, 561.
Power2 <- function(x, a) {
  z <- x^a
  return(z)
}
Power2(3, 2)

#c Using the Power2() function that you just wrote, compute 10^3, 8^17, and 131^3
Power2(10, 3)
Power2(8,17)
Power2(131,3)

#d Now create a new function, Power3(), that actually returns the result x^a as an R object, rather than simply printing it to the screen.
Power3 <- function(x, a) {
  result <- x^a
  return(result)
}
Power2(3, 2)

#e Now using the Power3() function, create a plot of f(x) = x2. The x-axis should display a range of integers from 1 to 10, and the y-axis should display x2. Label the axes appropriately, and use an appropriate title for the figure. Consider displaying either the x-axis, the y-axis, or both on the log-scale. You can do this by using log=??????x??????, log=??????y??????, or log=??????xy?????? as arguments to the plot() function.

par(mfrow=c(1,1))
plot(Power3(1:10, 2), xlab='X', ylab='X squared', main ='Square Function')
plot(Power3(1:10, 2), xlab='X', ylab='X squared', main ='Square Function', log='xy')

#f Create a function, PlotPower(), that allows you to create a plot
of x against x^a for a fixed a and for a range of values of x. For
instance, if you call

PlotPower <- function(x, a) {
  return(plot(Power3(x, a), xlab='X', ylab='X squared', main ='Custom Polynomial (or not..) Function'))
}
par(mfrow=c(1,1))
PlotPower(1:10, -2)



#13. Using the Boston data set, fit classification models in order to predict whether a given suburb has a crime rate above or below the median.
#Explore logistic regression, LDA, and KNN models using various subsets of the predictors. Describe your findings.
?Boston
str(Boston)
summary(Boston)

crim01 = rep(0, 506)
crim01[Boston$crim>median(Boston[,1])]=1
sum(crim01)
Boston.01 <- data.frame(Boston, crim01)
str(Boston01)
plot(Boston.01)
par(mfrow = c(7,2))
plot(crim ~ zn, data=Boston.01, main='proportion of residential land zoned for lots over 25,000 sq.ft.')
plot(crim ~ indus, data=Boston.01, main='proportion of non-retail business acres per town')
plot(crim ~ chas, data=Boston.01, main='Charles River dummy variable')
plot(crim ~ nox, data=Boston.01, main='NO')
plot(crim ~ rm, data=Boston.01, main='average number of rooms per dwelling')
plot(crim ~ age, data=Boston.01, main='proportion of owner-occupied units built prior to 1940')
plot(crim ~ dis, data=Boston.01, main='weighted mean of distances to five Boston employment centres')
plot(crim ~ rad, data=Boston.01, main='index of accessibility to radial highways')
plot(crim ~ tax, data=Boston.01, main='full-value property-tax rate per 10,000')
plot(crim ~ ptratio, data=Boston.01, main='pupil-teacher ratio by town')
plot(crim ~ black, data=Boston.01, main='proportion of blacks by town')
plot(crim ~ lstat, data=Boston.01, main='lower status of the population (percent)')
plot(crim ~ medv, data=Boston.01, main='median value of owner-occupied homes in 1000s')

cor(Boston.01[,-4-9])
#Base on the graphs and cor, indus+age+dis+tax to try, but these variables are highly (anti)-correlated between each other

#Split dataset into train (80%) and test (20%)
set.seed(1)
pick <- rnorm(nrow(Boston))
test <- pick > quantile(pick,0.8)
Boston.train <- Boston.01[!test,]
Boston.test <- Boston.01[test,]
crim01.test=Boston.01$crim01[test]
dim(Boston.train)
dim(Boston.test)
length(crim01.test)

#LR
glm.fit=glm(crim01~indus+age+dis+tax, data=Boston.train, family=binomial)
summary(glm.fit)
#p-value for indus is high - discard from the model
glm.fit=glm(crim01~age+dis+tax, data=Boston.train, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, Boston.test, type='response')
glm.pred=rep(0, nrow(Boston.test))
glm.pred[glm.probs>0.5]='1'
table(glm.pred, crim01.test)
mean(glm.pred==crim01.test)

glm.fit=glm(crim01~age+tax, data=Boston.train, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, Boston.test, type='response')
glm.pred=rep(0, nrow(Boston.test))
glm.pred[glm.probs>0.5]='1'
table(glm.pred, crim01.test)
mean(glm.pred==crim01.test)
#89.1%
glm.fit=glm(crim01~nox+rad, data=Boston.train, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, Boston.test, type='response')
glm.pred=rep(0, nrow(Boston.test))
glm.pred[glm.probs>0.5]='1'
table(glm.pred, crim01.test)
mean(glm.pred==crim01.test)
#91.1%
glm.fit=glm(crim01~nox+rad+tax, data=Boston.train, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, Boston.test, type='response')
glm.pred=rep(0, nrow(Boston.test))
glm.pred[glm.probs>0.5]='1'
table(glm.pred, crim01.test)
mean(glm.pred==crim01.test)
#90.1%
#Seems like nox+rad are predicting the best, with 91% accuracy

#LDA
lda.fit=lda(crim01~nox+rad, data=Boston.train)
lda.pred=predict(lda.fit, Boston.test)
lda.class=lda.pred$class
table(lda.class, crim01.test)
mean(lda.class==crim01.test)
#same result as with LR, 91.1% accuracy, but errors are in different proportion

#QDA
qda.fit=qda(crim01~nox+rad, data=Boston.train)
qda.pred=predict(qda.fit, Boston.test)
qda.class=qda.pred$class
table(qda.class, crim01.test)
mean(qda.class==crim01.test)
#lower, with 88.1% accuracy

qda.fit=qda(crim01~nox+tax, data=Boston.train)
qda.pred=predict(qda.fit, Boston.test)
qda.class=qda.pred$class
table(qda.class, crim01.test)
mean(qda.class==crim01.test)
#same as before, 88.1%, as rad and tax are highly correlated

#KNN
train.X=cbind(Boston.train$nox, Boston.train$rad)
test.X=cbind(Boston.test$nox, Boston.test$rad)
train.crim01=Boston.01$crim01[!test]
set.seed(1)
library(class)
knn.pred=knn(train.X, test.X, train.crim01, k=1)
table(knn.pred, crim01.test)
mean(knn.pred==crim01.test)
#94%!
knn.pred=knn(train.X, test.X, train.crim01, k=4)
table(knn.pred, crim01.test)
mean(knn.pred==crim01.test)
#same 94%

#All in all, LR &LDA seem to perform similarly with highest accuracy I could get at 91%, using nox+rad/tax as predictors. To note though, ratio of sensitivity to specificity different for these methods - one might pick one of these approaches over the other because of this. 
#QDA performs worse of all, with 88%
#KNN seem to perform the best with 94% accuracy using nox+rad as predictors. K1-4 gives the same accuracy, but moving forward choosing higher k (k=3 or 4) might give more stable prediction.



