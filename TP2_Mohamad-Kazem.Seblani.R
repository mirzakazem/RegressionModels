# 1) 
# a) Import the data base.
ad = read.csv("Advertising.csv", header=T) 

#b) Find the number of observations and the number of variables in this database. 
n=dim(ad)[1]
p=dim(ad)[2]

#c) Save the variables that you would like to use in a table called "simpledata".
simpledata=list(TV= ad$TV,Radio=ad$Radio,Newspaper=ad$Newspaper, Sales= ad$Sales) 

# d)Visualize the summary of the variables. Comment please.
summary(simpledata$TV)
summary(simpledata$Radio)
summary(simpledata$Newspaper)
summary(simpledata$Sales)


# Min value in the dataset
# 25% of the values are less than or equal: 1st Qu
# Median the that value in the middle of the dataset (half of the values smaller where the other hal is larger)
# Mean = sum of xi/n
# 75% of the values are less or equal 3rd Qu: 
# Max value in the dataset

#    e) Plot the scatter plot of the variables "sales" according to the variables.

plot(simpledata$TV,simpledata$Sales,main='The Scatter plot of the Data ',xlab='TV',ylab='Sales',col='red',pch=19)
text(simpledata$TV,simpledata$Sales, row.names(simpledata), cex=0.6, pos=4, col="red")

plot(simpledata$Radio,simpledata$Sales,main='The Scatter plot of the Data ',xlab='Radio',ylab='Sales',col='red',pch=19)
text(simpledata$Radio,simpledata$Sales, row.names(simpledata), cex=0.6, pos=4, col="red")

plot(simpledata$Newspaper,simpledata$Sales,main='The Scatter plot of the Data ',xlab='Newspaper',ylab='Sales',col='red',pch=19)
text(simpledata$Newspaper,simpledata$Sales, row.names(simpledata), cex=0.6, pos=4, col="red")

#    f) Plot the boxplot of the variables. Comment please.
# Box plot 
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(simpledata$TV, main="TV")
boxplot(simpledata$Radio, main="Radio")
boxplot(simpledata$Newspaper, main="Newspaper")
boxplot(simpledata$Sales, main="Sales")

# two outliars found in the newspaper variable (further the upper fence)

#    g) Find the correlation between the variables. Comment please.

cor(simpledata )


# 2) Build the simple linear regression model that predicts the sales according to the values of 
#    investments in media
linMod=lm(Sales~TV + Radio + Newspaper , simpledata)

# 3) Linear regression diagnostic:
#    a) Capture the model summary.
summary(linMod)

#    b) Comment on the p-values.
# except the p value of the newspaper, all the other p values of all the variables is extremly small 
# which is indicates there is a strong relationship between the dependant and the independant variables (except newspaper variable)

#    c) Comment on the R-squared value.
# the R-squared values is almost 0.9 which is very good, since it is 
# telling us that the model explains around 90% of the variability of the response data around its mean.

#    d) Compute the confidence interval on a level of 95 %
confint(linMod, level=0.95)

# 4) Linear regression plots diagnostic::
#    Comment on the plots of the linea model.

par(mfrow=c(2,2)) 
plot(linMod)

# 5) For an investement in TV of 51.7, in radio of 12.2 and in newspaper of 19.1, what are the estimated sales?
#    What is the prediction confidence interval?

predict(linMod,data.frame(TV= 51.7, Newspaper=19.1 , Radio= 12.2),interval="prediction",level=0.95)

# 6) Do you suggest a more restreint model?
# yes, removing the outliars. but generally speaking our model is good enough, expecially comparing to the simple one.