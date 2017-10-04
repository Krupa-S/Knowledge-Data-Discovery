## Exercise 6.8.10
## Summer 2017

# 10. We have seen that as the number of features used in a model increases,
# the training error will necessarily decrease, but the test error may not.
# We will now explore this in a simulated data set.
# (a) Generate a data set with p = 20 features, n = 1,000 observations, and an associated quantitative response vector generated
# according to the model Y = Xβ + ε, where β has some elements that are exactly equal to zero.
# (b) Split your data set into a training set containing 100 observations
# and a test set containing 900 observations.
# (c) Perform best subset selection on the training set, and plot the
# training set MSE associated with the best model of each size.
# (d) Plot the test set MSE associated with the best model of each
# size.
# (e) For which model size does the test set MSE take on its minimum
# value? Comment on your results. If it takes on its minimum value
# for a model containing only an intercept or a model containing
# all of the features, then play around with the way that you are
# generating the data in (a) until you come up with a scenario in
# which the test set MSE is minimized for an intermediate model
# size.
# (f) How does the model at which the test set MSE is minimized
# compare to the true model used to generate the data? Comment
# on the coefficient values.
# (g) Create a plot displaying ' p j=1(βj − β ˆj r)2 for a range of values
# of r, where β ˆj r is the jth coefficient estimate for the best model
# containing r coefficients. Comment on what you observe. How
# does this compare to the test MSE plot from (d)?

# (a) Generate a data set with p = 20 features, n = 1,000 observations, and an associated quantitative response vector generated
# according to the model Y = Xβ + ε, where β has some elements that are exactly equal to zero.

set.seed(88)
p = 20
n = 1000
x = matrix(rnorm(p*n), n, p)
B = rnorm(p)

## Setting some elements in B to zero.

B[4] = 0
B[5] = 0
B[13] = 0
B[16] = 0
B[20] = 0

## Create random err vector.

err = rnorm(n)

## Create response vector based on linear model Y = Xβ + ε.

y = x %*% B + err

# (b) Split your data set into a training set containing 100 observations
# and a test set containing 900 observations.

train = sample(seq(n), 100, replace = FALSE)

y.train = y[train, ]
y.test = y[-train, ]
x.train = x[train, ]
x.test = x[-train, ]

# (c) Perform best subset selection on the training set, and plot the
# training set MSE associated with the best model of each size.

library(leaps)
regfit = regsubsets(y ~ ., data = data.frame(x = x.train, y = y.train), nvmax = p)
train.matrix = model.matrix(y ~ ., data = data.frame(x = x.train, y = y.train), nvmax = p)
train.errors = rep(NA, p)

## Storing training errors for the number of preditors incrementally.

for (i in 1:p) {
  coefi = coef(regfit, id = i)
  pred = train.matrix[, names(coefi)] * coefi
  train.errors[i] = mean((y.train - pred)^2)
}
plot(train.errors, ylab = "Training MSE", xlab="Number of Predictors", pch = 20, type = "b", col="purple")

# (d) Plot the test set MSE associated with the best model of each
# size.

test.matrix = model.matrix(y ~ ., data = data.frame(x = x.test, y = y.test), nvmax = p)
test.errors = rep(NA, p)

for (i in 1:p) {
  coefi = coef(regfit, id = i)
  pred = test.matrix[, names(coefi)] * coefi
  test.errors[i] = mean((y.test - pred)^2)
}
plot(test.errors, ylab = "Test MSE", xlab="Number of Predictors", pch = 20, type = "b", col="blue")

# (e) For which model size does the test set MSE take on its minimum
# value? Comment on your results. If it takes on its minimum value
# for a model containing only an intercept or a model containing
# all of the features, then play around with the way that you are
# generating the data in (a) until you come up with a scenario in
# which the test set MSE is minimized for an intermediate model
# size.

points(which.min(test.errors), test.errors[which.min(test.errors)], pch = 20, col="red")
sprintf("The best model size with the lowest MSE is: %d", which.min(test.errors))

## The test MSE plot showed a minimum MSE value when the predictor size is 18 as shown in the 
## red dot in the graph, and console output. We obtained this model by setting 5 variables 4, 5, 13, 16, 20
## to zero. We experimented with the model by setting different combinations of variables to zero, and even some model produce 
## optimal MSE result, the number of predictors are either too low or too high, therefore undesirable.
## Then we continue experimenting by repeating the same procedure of splitting data into training and testing, and steps (a) to (e) to
## find the best model with lowest MSE with an intermediate predictor size.
## From all experiments we conducted, we concluded that the model with 15 variables yielded the lowest MSE and best model performance. 

# (f) How does the model at which the test set MSE is minimized
# compare to the true model used to generate the data? Comment
# on the coefficient values.

summary(regfit)
coef(regfit, which.min(test.errors))

## The 15th model gives the lowest MSE compared to the MSE of the full model 
## and caught all zero coefficient values except five of the 20 incremental models.
## The models that did not catch all zero coeffcient are model 16th, 17th, 18th, 19th, and 20th.
## The 15th model gives the lowest MSE compared to the MSE of the true full model, thus generating the best model 
## performance utilizing an intermediate model predictor size.

# (g) Create a plot displaying ' p j=1(βj − β ˆj r)2 for a range of values
# of r, where β ˆj r is the jth coefficient estimate for the best model
# containing r coefficients. Comment on what you observe. How
# does this compare to the test MSE plot from (d)?

val.errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)

x_cols = colnames(x, do.NULL = FALSE, prefix="x.")

for (i in 1:p)
{
  coefi = coef(regfit, id = i)
  
  a[i] = length(coefi) - 1
  
  b[i] = sqrt(sum((B[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) +sum(B[!(x_cols %in% names(coefi))])^2)
}

plot(x = a, y = b, xlab = "Number of Coefficients", ylab = "Estimated vs True Coefficients Error", pch = 20, col="red")
sprintf("The minimal difference between estimated and true coefficient is with coeffient number: %d", which.min(b))

## The console output and the plot above shows the model with 15 coefficients (excluding intercept) minimizes 
## the difference between the estimated and true coefficient error. In this case, it shows that the best model we obtained with 
## 15 predictors suggests a more accurate prediction with minimal difference between the Estimated and the True Coefficient Error.

