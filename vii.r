## Exercise 8.4.8
## Summer 2017

# 8. In the lab, a classification tree was applied to the Carseats data set after converting Sales into a qualitative response variable. Now we will
# seek to predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.
# (a) Split the data set into a training set and a test set.
# (b) Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?
# (c) Use cross-validation in order to determine the optimal level of
# tree complexity. Does pruning the tree improve the test MSE?
# (d) Use the bagging approach in order to analyze this data. What
# test MSE do you obtain? Use the importance() function to determine which variables are most important.
# (e) Use random forests to analyze this data. What test MSE do you
# obtain? Use the importance() function to determine which variables are most important. Describe the effect of m, the number of
# variables considered at each split, on the error rate
# obtained.

# (a) Split the data set into a training set and a test set.

library(ISLR)
attach(Carseats)
set.seed(1)

train = sample(400, 200)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

# (b) Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?

library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

## From the summary, we can conclude that 6 variables are used to construct tree. 
## These Variables are ShelveLoc, Price, Age, Advertising, Income, CompPrice.
## Deviance is sum of squared errors for regression trees i.e. 2.36 

## Plot the regression tree.

plot(tree.carseats)
text(tree.carseats, pretty=0)

## ShelveLoc refers to the quality of the shelving location for the car seats at each site
## The tree predicts a median Sales unit value of $2,249 for Shelveloc values of Bad and medium, 
## Age less than 66.5 and price less than 132.

## Show prediction and test MSE.
pred.carseats = predict(tree.carseats, Carseats.test)
sprintf("Test Error rate is: %.2f", mean((Carseats.test$Sales - pred.carseats)^2))

## The Test MSE obtained by using regression tree is 4.15. The variables contribute significantly to the
## model are ShelveLoc, Price, Age, Advertising, Income, CompPrice.
## Thus, square root of the MSE is around 2.034, which indicates that model leads 
## to test predictions that are within around $2,034 of the true median of unit Sales value
## at each location.

# (c) Use cross-validation in order to determine the optimal level of
# tree complexity. Does pruning the tree improve the test MSE?

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
points(which.min(cv.carseats$dev)+1, cv.carseats$dev[which.min(cv.carseats$dev)], pch=20, col="red")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

## From above plots, the best tree size is 9 because it produces the lowest 
## cv.carseats$dev value of 1039.212 and a Test MSE of 4.99 which is lower than size 8 with a test MES of 5.10.

## cv.carseats$dev[7] = 1044.469
## cv.carseats$dev[8] = 1039.212
## cv.carseats$dev[9] = 1039.212
## cv.carseats$dev[10] = 1041.308

## Size 8, MSE = 5.09085
## size = 9, MSE = 4.99
## Size 10, MSE = 4.819708

pruned.carseats = prune.tree(tree.carseats, best = 9)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty=0)
pred.pruned = predict(pruned.carseats, Carseats.test)
sprintf("Test Error rate after pruning the tree: %.2f", mean((Carseats.test$Sales - pred.pruned)^2))

## The Test MSE is 4.99. Pruning the tree actually increased the MSE which did not improve the previous unpruned 
## test MSE at all.

# (d) Use the bagging approach in order to analyze this data. What
# test MSE do you obtain? Use the importance() function to determine which variables are most important.

library(randomForest)
bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
bag.pred = predict(bag.carseats, Carseats.test)
sprintf("Test Error rate for random forests: %.2f", mean((Carseats.test$Sales - bag.pred)^2))
importance(bag.carseats)

## The test MSE lowered to 2.60 after randomForest function has been applied with all 10 variables 
## randomly sampled as candidates at each split. 
## The important variables contributes to sales produced by randomForest are listed below,
## Price with 57.82 %IncMSE, ShelveLoc with 43.05 %IncMSE, Age with 19.87 %IncMSE.

##             %IncMSE       IncNodePurity
## CompPrice   14.4124562    133.731797
## Income       6.5147532     74.346961
## Advertising 15.7607104    117.822651
## Population   0.6031237     60.227867
## Price       57.8206926    514.802084
## ShelveLoc   43.0486065    319.117972
## Age         19.8789659    192.880596
## Education    2.9319161     39.490093
## Urban       -3.1300102      8.695529
## US           7.6298722     15.723975

# (e) Use random forests to analyze this data. What test MSE do you
# obtain? Use the importance() function to determine which variables are most important. Describe the effect of m, the number of
# variables considered at each split, on the error rate obtained.

rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, importance = TRUE)
rf.pred = predict(rf.carseats, Carseats.test)
sprintf("Test Error rate for random forests: %.2f", mean((Carseats.test$Sales - rf.pred)^2))
importance(rf.carseats)

## The test MSE generated by random forest function raised from previous 2.60 to 2.89. The downsize of mtry value from 10 to 5 resulted
## an increase in test MSE of 0.29 (2.89-2.60). Changing randomForest attribute mtry value influence test MSE that a lower mtry value increases 
## the test MSE which worsens the test accuracy. As the number of variables selected at each split increases, the more accurate the prediction 
## will likely be, which results in a lower test MSE.
## The important variables with respect to Sales are the same variables: Price, ShelveLoc, and Age as shown at the table below with 
## sigfinicant higher %IncMSE values.
 
##             %IncMSE        IncNodePurity
## CompPrice   12.0259791     124.81403
## Income       5.5542673     106.15418
## Advertising 12.0466048     136.15204
## Population   0.3136897      81.68162
## Price       45.9639857     457.15711
## ShelveLoc   36.2789679     271.76488
## Age         20.8537727     196.72182
## Education    2.9005332      54.16980
## Urban       -0.6888196      11.86848
## US           6.9739759      23.64075

