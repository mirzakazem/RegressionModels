# 1) 
#a) Import the data base.
ad = read.csv("Advertising.csv", header=T) 

#b) Find the number of observations and the number of variables in this database. 
n=dim(ad)[1]
p=dim(ad)[2]

#c) Save the variables that you would like to use in a table called "simpledata".
simpledata=list(TV= ad$TV, Sales= ad$Sales) 

# d) Visualize the summary of the variables "TV" and "sales". Comment please.
summary(simpledata$TV)
summary(simpledata$Sales)

# Min value in the dataset
# 25% of the values are less than or equal: 1st Qu
# Median the that value in the middle of the dataset (half of the values smaller where the other hal is larger)
# Mean = sum of xi/n
# 75% of the values are less or equal 3rd Qu: 
# Max value in the dataset


# e) Plot the scatter plot of the variables "sales" according to the variable "TV".
plot(simpledata$TV,simpledata$Sales,main='The Scatter plot of the Data ',xlab='TV',ylab='Sales',col='red',pch=19)

# f) Plot the boxplot of the variables "sales" et "TV". Comment please.

# Box plot 
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(simpledata$TV, main="TV")  # box plot for 'TV'
boxplot(simpledata$Sales, main="Sales")  # box plot for 'Sales'

#no suspected outliers, the two plots look consistent 

# g) Find the correlation between the two variables. Comment please.
cor(simpledata$TV, simpledata$Sales)  # calculate correlation between TV and Sales 

# positive correlation detected (very close to one). so there is a strong correlation between TV and Sales. therefore, when TV value increase, sales values increase also

# 2) Build the simple linear regression model that predicts the sales according to the values of 
#    investments in the Television
linMod=lm(formula=Sales~TV, data=ad)

# 3
# a) Capture the model summary.
summary(linMod)

# b) Comment on the p-values
#Beta 0 = 7.032
#Beta1 0.0475
# Intercept P value has two stars (high confidence)
# TV P value < 0.05 and has three stars (very high confidence)
# TV p value is extermly small (we reject H0)
#which indicate there is a strong relation between the two variables
# TV variable is Significant

# c) Comment on the R-squared value.
# R squared is more than 0.6 which
# indicates that the model explains 60% of the variability of the response data around its mean.

# d) Compute the confidence interval on a level of 95 %
confint(linMod, level=0.95)

# 4) Linear regression plots diagnostic:

par(mfrow=c(2,2)) 
plot(linMod)

# 5) For an investments in the Televesion of 51.7, what are the estimated sales?
#    What is the prediction confidence interval?

predict(linMod,data.frame(TV=51.7),interval="prediction",level=0.95)

# 6) Do you suggest a more restreint model?
# yes, we can opmtimze the model by collecting more data
# and by using the other variables (radio and newspaper)

