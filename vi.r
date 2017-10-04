## Exercise 7.9.7
## Summer 2017

# 7. The Wage data set contains a number of other features not explored
# in this chapter, such as marital status (maritl), job class (jobclass),
# and others. Explore the relationships between some of these other
# predictors and wage, and use non-linear fitting techniques in order to
# fit flexible models to the data. Create plots of the results obtained,
# and write a summary of your findings.

## Explore categorial data in wage dataset.

library(ISLR)
attach(Wage)
set.seed(1)

View(Wage)

summary(race)
summary(health)
summary(maritl)
summary(jobclass)
summary(education)
summary(health_ins)

par(mfrow = c(3, 2))
plot(race, wage, col=rainbow(5), xlab="Race", ylab="Wage")
plot(health, wage, col=rainbow(5), xlab="Health", ylab="Wage")
plot(maritl, wage, col=rainbow(5), xlab="Marital Status", ylab="Wage")
plot(jobclass, wage, col=rainbow(5), xlab="Job Class", ylab="Wage")
plot(education, wage, col=rainbow(5), xlab="Education", ylab="Wage")
plot(health_ins, wage, col=rainbow(5), xlab="Health Insurance", ylab="Wage")

## The plots along with the data summaries reveal several relationships.
## First, the race plot suggests that Asian people earn more wage than other races due to an imbalanced dataset with 
## White population of 2480, Asian 190, Black 293, and Other 37. Therefore, even the box plot shows that Aisan earns more 
## wage than other races, the relationship is biased and does not represent the realistic population for each race group given 
## the imbalaced dataset.
## The graph also showed that very healthy people, married couples, information industry workers, people 
## with advanced degree and health insurace tend to earn higher wages.
## But the data attribute summaries shows that marital status and health insurance also has large imbalanced 
## dataset. Therefore, making the marital status and health insurance relationships with respect to wage less credible.

fit = lm(wage ~ race, data = Wage)
summary(fit)

fit = lm(wage ~ health, data = Wage)
summary(fit)

fit = lm(wage ~ maritl, data = Wage)
summary(fit)

fit = lm(wage ~ jobclass, data = Wage)
summary(fit)

fit = lm(wage ~ education, data = Wage)
summary(fit)

fit = lm(wage ~ health_ins, data = Wage)
summary(fit)

## The linear model summary shows attributes that contributes significantly to the wage outcome variable are ranked based on
## coefficient values below. These summaries suggests that the qualitative attibute education contributes most 
## positively to the wage outcome variable; People with advanced degree will earn more wages than those with lower level degrees.

## Predictor variable and their coefficient.
## ----------------------------------------
## education 66.813 (advanced degree)
## health_ins -27.9216(no)
## maritl 26.126 (married)
## jobclass 17.272
## Health 14.065

fit.1 = lm(wage~jobclass+age, data=Wage)
fit.2 = lm(wage~jobclass+poly(age,2), data=Wage)
fit.3 = lm(wage~jobclass+poly(age,3), data=Wage)
anova(fit.1, fit.2, fit.3)

## Using polynomial regression to compare three models by raising each of the original age predictor to different power.
## In this case, the summary showed fit.2 model produces the lowest P-value of 2.2e-16.

## We specify the degrees of freedom for the the year variable to be 4 degrees of freedom, and variable age to
## have 5 degrees of freedom. Since jobclass is a qualitative variable, it was converted into two dummy variables. 
## We use the gam() function in gam() order to fit a Generalized Additive Model using these components.

library(gam)
gam.m3 = gam(wage~s(year,4)+s(age,5)+jobclass, data=Wage)
par(mfrow=c(2,2))
plot.gam(gam.m3, se=TRUE, col="blue")

gam.m1 = gam(wage~s(age,5)+jobclass, data=Wage)
gam.m2 = gam(wage~year+s(age,5)+jobclass, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")

## The GAM with a linear function of year is better than the absence of variable year which results a p-value of 9.047e-05.
## The gam.m2 Model is preferred over gam.m3 Model which has a p-value of 0.6639. 
## This shows that a linear model is sufficient in this case.

summary(gam.m3)

## Summary Output
## --------------
## Anova for Nonparametric Effects
## Npar             Df Npar F  Pr(F)    
## (Intercept)                          
## s(year, 4)        3  0.519 0.6692    
## s(age, 5)         4 39.343 <2e-16 ***

## From the summary above, the large P-value for year reinforce our conclusion from the anova test that a linear function is 
## sufficient for this variable.

gam.lo = gam(wage~s(year, df=4) + lo(age, span=0.6)+jobclass, data=Wage)
plot.gam(gam.lo, se=TRUE, col="purple")

## We used local regression for the age variable with a span of 0.6, and used a logistic 
## regression GAM and spliting wage response into binary based on 200 k wage.

gam.lr = gam(I(wage>200)~year+s(age, df=5) + jobclass, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=TRUE, col="orange")

## As the table below shows that information industry workers have more population.

table(jobclass, I(wage>200))

## jobclass         FALSE TRUE
## 1. Industrial   1523   21
## 2. Information  1378   78

gam.lr.s = gam(I(wage>200)~year+s(age, df=5) + jobclass, family=binomial, data=Wage)
plot(gam.lr.s, se=TRUE, col ="orange")

## The graphs plotted above the first two functions are natural splines in year and age with 5 degrees of freedom for the age.
## The third plot displays a step function fitting for the qualitative variable jobclass.

## After exploring the dataset for the predictor variables and different graphs, we found that
## the dataset for the qualitative predictor variables jobclass and education are well balanced with each levels of the variable
## evenly distributed. Out of the qualitative variables from the dataset, jobclass and education contributes significantly to the 
## model as the linear model coefficients, anova test comparisons, gam logistic regression, and jobclass table output have shown.

