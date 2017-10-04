## Exercise 4.7.11
## Summer 2017

# 11. In this problem, you will develop a model to predict whether a given
# car gets high or low gas mileage based on the Auto data set.
# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below
# its median. You can compute the median using the median()
# function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and
# the other Auto variables.
# (b) Explore the data graphically in order to investigate the association between mpg01 and the other features. Which of the other
# features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings.
# (c) Split the data into a training set and a test set.
# (d) Perform LDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?
# (e) Perform QDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?
# (f) Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with
# mpg01 in (b). What is the test error of the model obtained?
# (g) Perform KNN on the training data, with several values of K, in
# order to predict mpg01. Use only the variables that seemed most
# associated with mpg01 in (b). What test errors do you obtain?
# Which value of K seems to perform the best on this data set?

## Load data

getwd()
auto=read.csv("Auto.csv", header=TRUE, na.strings="?")
auto=na.omit(auto)
dim(auto)
attach(auto)

# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below
# its median. You can compute the median using the median()
# function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and
# the other Auto variables.

mpg01 = rep(0, nrow(auto))
mpg01[mpg > median(mpg)] = 1
mpg01 = as.factor(mpg01)
auto = data.frame(auto, mpg01)
summary(auto)

# (b) Explore the data graphically in order to investigate the association between mpg01 and the other features. Which of the other
# features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings.

## Scatterplots between predictors and response mpg01.
pairs(mpg01~., data=auto)

## Boxplot between predictors associated with mpg01.
par(mfrow=c(3,3))
plot(mpg01, mpg, main="mpg01 vs mpg", xlab="low/high mpg", ylab="mpg", col=rainbow(2))
plot(mpg01, cylinders, main="mpg01 vs cylinders", xlab="low/high mpg", ylab="cylinders", col=rainbow(2))
plot(mpg01, displacement, main="mpg01 vs displacement", xlab="low/high mpg", ylab="Displacement", col=rainbow(2))
plot(mpg01, horsepower, main="mpg01 vs horsepower", xlab="low/high mpg", ylab="Horsepower", col=rainbow(2))
plot(mpg01, weight, main="mpg01 vs weight", xlab="low/high mpg", ylab="weight", col=rainbow(2))
plot(mpg01, acceleration, main="mpg01 vs acceleration", xlab="low/high mpg", ylab="Acceleration", col=rainbow(2))
plot(mpg01, origin, main="mpg01 vs origin", xlab="low/high mpg", ylab="origin", col=rainbow(2))
plot(mpg01, year, main="mpg01 vs year", xlab="low/high mpg", ylab="year", col=rainbow(2))

## Correlation matrix between auto variables.
cor(subset(auto, select=c(-name,-mpg01)))

## After examining the scatterplots, boxplots, and the correlation matrix between variables associated with mpg01, 
## we chose the following 5 variables as predictors for mpg01: 
## cylinders, displacement, horsepower, weight, and year.

## Reasoning:
## Initially, we looked at the scatterplots to have a general view on the relationship between variables. The scatterplots 
## showed variable mpg has a strong association with response mpg01. However, we excluded mpg as a predictor 
## for our classification model because 1) mpg is used for generating mpg01 response value, 2) mpg predictor generated overly 
## optimistic result for the logistic regression model with an error rate of 0.00 and a warning message saying glm.fit: algorithm 
## did not converge, and fitted probabilities numerically 0 or 1 occurred.

## We excluded name variable because the value tends to be arbitrary and neither numeric nor categorical in nature.
## Then we examined the boxplots and excluded acceleration variable since the mpg01 vs acceleration boxplot did not show meaningful 
## association between acceleration and mpg01 because the min/max, interquartile range as well as median values are similar between 
## low/high mpg at acceraltion 15. Also the correlation matrix index between acceleration, origin and mpg has a low value of 
## 0.423, 0.565 indicating weak assocaitions therefore acceleration and orgin were removed. 
## The rest of the boxplots showed strong seperation between predictors and the class response mpg01.
## Lastly, the correlation matrix showed the correlation index between cylinders, displacement, horsepower, weight, 
## year with respect to mpg are -0.777, -0.805, -0.778, -0.832, 0.580 respectively. These variable has relative strong
## correlation between predictors and response such as weight and mpg which has a correlation index of -0.832 implying the more heavy
## the car, the lower the mpg gets. Based on these reasoning, we chose the above 5 variable as predictors for our classification model.

# (c) Split the data into a training set and a test set.

## We chose to split the data by year because auto data has been indexed by year. Using the modulus operator will
## generate a systematic partition between training and test data based on the auto dataset. The modulus operator will alternate 
## between even and odd years to split data into training and test dataset.

## Seperate auto data into training data test data.
training <- (year%%2==0)
test <- !training
trainingData = auto[training,]
testData = auto[test,]

# (d) Perform LDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?

## Load Library
library(MASS)

## Perform LDA on the training data
lda.fit = lda(mpg01~cylinders+displacement+horsepower+weight+year, data=trainingData)
lda.fit

## Predict mpg01 using test data with significant variables associated with mpg01.
lda.pred = predict(lda.fit, testData)

## Display objects in the model collection
names(lda.pred)

## Display confusion matrix
table(Predicted=lda.pred$class, Actual=testData$mpg01)

## Print Error Rate. Test Error rate is about 0.1139
sprintf("Test Error rate for lda model: %.2f%%", mean(lda.pred$class!=testData$mpg01)*100)

# (e) Perform QDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in (b). 
# What is the test error of the model obtained?

## Perform QDA on the training data
qda.fit = qda(mpg01~cylinders+displacement+horsepower+weight+year, data=trainingData)
qda.fit

## Predict mpg01 using test data with significant variables associated with mpg01.
qda.pred = predict(qda.fit, testData)

## Display objects in the model collection
names(qda.pred)    

## Display confusion matrix
table(Predicted=qda.pred$class, Actual=testData$mpg01)

## Print Error Rate. Test Error rate is about 0.1139
sprintf("Test Error rate for qda model: %.2f%%", mean(qda.pred$class!=testData$mpg01)*100)

# (f) Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with
# mpg01 in (b). What is the test error of the model obtained?

## Perform Logistic Regression on the training data
fit.glm = glm(mpg01~cylinders+displacement+horsepower+weight+year, family=binomial("logit"), data=trainingData)
summary(logisticModel)

## Predict mpg01 using significant variables associated with mpg01.
probs <- predict(fit.glm, auto[!training,], type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1

## Display confusion matrix
table(Predicted=pred.glm, Actual=mpg01[!training])

## Print Error Rate. Test Error rate is about 0, suggesting logistic regress works best for this dataset.
sprintf("Test Error rate for logistic model: %.2f%%", mean(pred.glm != mpg01[!training])*100)

# (g) Perform KNN on the training data, with several values of K, in
# order to predict mpg01. Use only the variables that seemed most
# associated with mpg01 in (b). What test errors do you obtain?
# Which value of K seems to perform the best on this data set?

## Import class
library(class)

## Get the 
train.X = cbind(cylinders, displacement, horsepower, weight, year)[training, ]
test.X = cbind(cylinders, displacement, horsepower, weight, year)[test, ]
train.mpg01 = mpg01[training]

set.seed(88)

## K = 1, KNN Test Error Rate: 15.38%
knn.pred = knn(train.X, test.X, train.mpg01, k = 1)
sprintf("Test Error rate for KNN model: %.2f%%", mean(knn.pred != mpg01[test])*100)

## K = 5, KNN Test Error Rate: 14.29%
knn.pred = knn(train.X, test.X, train.mpg01, k = 5)
sprintf("Test Error rate for KNN model: %.2f%%", mean(knn.pred != mpg01[test])*100)

## K = 20, KNN Test Error Rate: 13.74%
knn.pred = knn(train.X, test.X, train.mpg01, k = 20)
sprintf("Test Error rate for KNN model: %.2f%%", mean(knn.pred != mpg01[test])*100)

## K = 25, KNN Test Error Rate: 14.29%
knn.pred = knn(train.X, test.X, train.mpg01, k = 25)
sprintf("Test Error rate for KNN model: %.2f%%", mean(knn.pred != mpg01[test])*100)

## The best K value for the KNN classification model is 20 which generated the lowest 
## error rate of 13.74% among the other K values (15.38%, 14.29%).
