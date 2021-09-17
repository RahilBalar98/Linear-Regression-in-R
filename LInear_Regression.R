#Linear Regression

data.in = read.csv("/home/rahilbalar98/Desktop/STAT_652/Datasets/Wine Quality.csv")
head(data.in)

print(colnames(data.in))

#Method-1 for extracting subset of features from the dataset
data = data.in[,c(4,8,9,10,11)]
head(data)

#Method-2 for extracting subset of features from the dataset
install.packages("dplyr")
library(dplyr)
data = select(data.in, residual.sugar, density, pH, sulphates, alcohol)
head(data)

#Renaming a feature/variable
colnames(data)[1] = "sugar"
head(data)

#Make a Scatterplot Matrix
pairs(data)

#Data pre-processing - Removing the outlier in the sulphates variable
#Method-1 First, we will find the index of the point with the largest value of sulphates, then we will remove this point from data
ind.outlier = which.max(data$sulphates)
data1=data[-ind.outlier,]
head(data1)

nrow(data)
nrow(data1)
pairs(data1)

#Method 2: The filter() function, The filter() function takes your data frame as its
# first input, then all the conditions you want the output 
# data frame to satisfy, with different conditions separated by commas.
data1 = filter(data, sulphates < 1.4) # 1.4 comes from the scatterplot matrix

nrow(data)
nrow(data1)

#Now let's fit a separate Linear Regression to predict alcohol using each
# of our explanatory variables
fit.sugar = lm(alcohol ~ sugar, data = data1)
fit.density = lm(alcohol ~ density, data = data1)
fit.pH = lm(alcohol ~ pH, data = data1)
fit.sulphates = lm(alcohol ~ sulphates, data = data1)

#Now that we've fit these models, let's see how they do
summary(fit.sugar)
summary(fit.density)
summary(fit.pH)
summary(fit.sulphates)


#Each of these regression models only concerns two variables.
#We can plot that. The with() function can save us some typing
#here. Rather than explain the details, it's best to just
#see the with() function in action. We also use the abline()
#function to add our regression line to the plot
with(data1, plot(sugar, alcohol))
abline(fit.sugar)
with(data1, plot(density, alcohol))
abline(fit.density)
with(data1, plot(pH, alcohol))
abline(fit.pH)
with(data1, plot(sulphates, alcohol))
abline(fit.sulphates)


