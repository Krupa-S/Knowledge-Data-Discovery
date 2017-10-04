## Exercise 2.4.8
## Summer 2017

# 8. This exercise relates to the College data set, which can be found in
# the file College.csv. It contains a number of variables for 777 different
# universities and colleges in the US. The variables are
# Private : Public/private indicator
# Apps : Number of applications received
# Accept : Number of applicants accepted
# Enroll : Number of new students enrolled
# Top10perc : New students from top 10% of high school class
# Top25perc : New students from top 25% of high school class
# F.Undergrad : Number of full-time undergraduates
# P.Undergrad : Number of part-time undergraduates
# Outstate : Out-of-state tuition
# Room.Board : Room and board costs
# Books : Estimated book costs
# Personal : Estimated personal spending
# PhD : Percent of faculty with Ph.D.a€?s
# Terminal : Percent of faculty with terminal degree
# S.F.Ratio : Student/faculty ratio
# perc.alumni : Percent of alumni who donate
# Expend : Instructional expenditure per student
# Grad.Rate : Graduation rate
# Before reading the data into R, it can be viewed in Excel or a text
# editor.
# (a) Use the read.csv() function to read the data into R. Call the
# loaded data college. Make sure that you have the directory set
# to the correct location for the data.
# (b) Look at the data using the fix() function. You should notice
# that the first column is just the name of each university. We dona€?t
# really want R to treat this as data. However, it may be handy to
# have these names for later. Try the following commands:2.4 Exercises 55
# > rownames ( college )= college [,1]
# > fix ( college )
# You should see that there is now a row.names column with the
# name of each university recorded. This means that R has given
# each row a name corresponding to the appropriate university. R
# will not try to perform calculations on the row names. However,
# we still need to eliminate the first column in the data where the
# names are stored. Try
# > college = college [,-1]
# > fix ( college )
# Now you should see that the first data column is Private. Note
# that another column labeled row.names now appears before the
# Private column. However, this is not a data column but rather
# the name that R is giving to each row.
# (c) i. Use the summary() function to produce a numerical summary
# of the variables in the data set.
# ii. Use the pairs() function to produce a scatterplot matrix of
# the first ten columns or variables of the data. Recall that
# you can reference the first ten columns of a matrix A using
# A[,1:10].
# iii. Use the plot() function to produce side-by-side boxplots of
# Outstate versus Private.
# iv. Create a new qualitative variable, called Elite, by binning
# the Top10perc variable. We are going to divide universities
# into two groups based on whether or not the proportion
# of students coming from the top 10 % of their high school
# classes exceeds 50 %.
# > Elite =rep (" No", nrow( college ))
# > Elite [ college$Top10perc >50]=" Yes "
# > Elite =as. factor ( Elite )
# > college = data. frame (college , Elite )
# Use the summary() function to see how many elite universities there are. Now use the plot() function to produce
# side-by-side boxplots of Outstate versus Elite.
# v. Use the hist() function to produce some histograms with
# differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow=c(2,2)) useful:
# it will divide the print window into four regions so that four
# plots can be made simultaneously. Modifying the arguments
# to this function will divide the screen in other ways.
# vi. Continue exploring the data, and provide a brief summary
# of what you discover.

# (a) Use the read.csv() function to read the data into R. Call the loaded data college. Make sure that you have the directory set to the correct location for the data.

getwd()
college=read.csv("College.csv", header=TRUE, na.strings="?")
dim(college)
attach(college)

# (b) Look at the data using the fix() function. You should notice that the first column is just the name of each university. We dont really want R to treat this as data. However, it may be handy to have these names for later. Try the following commands:

rownames(college) = college[,1]
college = college[,-1]
fix(college)

# (c)i. Use the summary() function to produce a numerical summary of the variables in the data set.

summary(college)

# (c)ii. Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. Recall that you can reference the first ten columns of a matrix A using A[,1:10].

pairs(college[,1:10])

# (c)iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.

plot(Private, Outstate, col=rainbow(2), main="Outstate VS Private", xlab="Private / Public", ylab="Outstate Tuition")

# (c)iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. We are going to divide universities into two groups based on whether or not the proportion of students coming from the top 10 % of their high school classes exceeds 50 %.

Elite = rep("No", nrow(college))
Elite[Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college) 
## There are 78 Elite universities and 699 non-elite universities.

plot(Elite, Outstate, col=rainbow(2), main="Outstate VS Elite", xlab="Elite", ylab="Outstate Tuition")

# (c)v. Use the hist() function to produce some histograms with differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow=c(2,2)) useful:
# it will divide the print window into four regions so that four plots can be made simultaneously. Modifying the arguments to this function will divide the screen in other ways.

par(mfrow=c(2,3))
hist(Enroll, breaks=5, main="Enroll Histogram", xlab="Enroll", col="red")
hist(Accept, breaks=6, main="Accept Histogram", xlab="Accept", col="blue")
hist(Top10perc, breaks=7, main="Top10perc Histogram", xlab="Top10perc", col="orange")
hist(Books, breaks=8, main="Books Histogram", xlab="Books", col="green")
hist(P.Undergrad, breaks=9, main="P.Undergrad Histogram", xlab="P.Undergrad", col="orange")
hist(PhD, breaks=10, main="PhD Histogram", xlab="PhD", col="purple")
		
# (c)vi. Continue exploring the data, and provide a brief summary of what you discover.

## Graph plot
par(mfrow=c(2,2))
plot(Top10perc, Grad.Rate, main="Grad.Rate VS Top10perc", xlab="Top10perc", ylab="Graduation Rate", col="blue")
plot(Outstate, Grad.Rate, main="Graduation Rate vs Outstate Tuition", xlab = "Outstate Tuition", ylab = "Graduation Rate")
plot(Elite, perc.alumni, main="% of Alumni Donate VS Elite", xlab="Elite", ylab="% of Alumni Donate VS Elite", col=rainbow(3))
plot(Elite, Outstate, col=rainbow(2), main="Outstate VS Elite", xlab="Elite", ylab="Outstate Tuition")

## There is a positive linear relationship between Top10perc and Graduation Rate. It suggests 
## the more students come from top 10% of high school class within a university, the higher the graduation rate will be.
## There are strong categorical relationships between Elite and Outstate Tuition fee, and Elite and perc.alumni. It indicates if the university
## is Elite, the Outstate Tuition will be more expensive and there will be more alumni willing to donate to the university.
## There is a positive linear relationship between Graduation Rate and Outstate Tuition indicating that the 
## higher the Outstate Tuition, the more committed the students are to graduate from universities.

## Calculate percentage of accpetance
percentage_of_accpetance = acceptance_rate = Accept / Apps

## what university has smallest acceptance rate
college[ which.min( percentage_of_accpetance ), ]

## what university has maximum acceptance rat
college[ which.max( percentage_of_accpetance ), ]

## Princeton University has the lowest acceptance rate. Emporia State University has 
## the highest accpetance rate from the applications they receive.
