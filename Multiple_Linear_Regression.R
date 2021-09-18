#Multiple Linear Regression
data.in = read.csv("/home/rahilbalar98/Desktop/STAT_652/Datasets/Wine Quality.csv")
library(dplyr)
data = select(data.in, residual.sugar, density, pH, sulphates, alcohol)

colnames(data)[1] = "sugar"

ind.outlier = which.max(data$sulphates)
data1 = data[-ind.outlier,]

pairs(data1)
library(rgl)
open3d()
plot3d(data1$alcohol ~ data1$density + data1$pH, col = "blue")


fit2 = lm(alcohol ~ density + pH, data = data1)
summary(fit2)

#First, we construct a grid of values for our two predictors
#using seq() and expand.grid(). Next, we compute the fitted 
#values of our regression model for all the predictor values in 
#this grid. Finally, we use the persp3d() function to plot the 
#regression surface using the predictor and fitted values on our 
#grid.
summary(data1) # Get a range of values for the predictors
vals.density = seq(from = 0.98, to = 1.005, by = 0.001)
vals.pH = seq(from = 2.7, to = 4.1, by = 0.03)
print(vals.density)
print(vals.pH)

#Create a data frame with all combinations of the predictor
#values we just made
pred.grid = data.frame(expand.grid(density = vals.density, pH = vals.pH))

#Get fitted alcohol values for all predictor combinations
#in our grid using the predict() function.
pred.alcohol = predict(fit2, newdata = pred.grid)

#Plot our regression surface using persp3d(), and add in the
#data using points3d()
open3d()
persp3d(x = vals.density, y = vals.pH, z = pred.alcohol,col = "orange")
points3d(data1$alcohol ~ data1$density + data1$pH)
