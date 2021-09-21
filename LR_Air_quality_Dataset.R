#1
help(airquality)
AQ = airquality[,1:4]
pairs(AQ)


#2
fit.Solar.R = lm(Ozone ~ Solar.R, data = AQ)
summary(fit.Solar.R)
with(AQ, plot(Solar.R, Ozone))
abline(fit.Solar.R)

fit.Wind = lm(Ozone ~ Wind, data = AQ)
summary(fit.Wind)
with(AQ, plot(Wind, Ozone))
abline(fit.Wind)

fit.Temp = lm(Ozone ~ Temp, data = AQ)
summary(fit.Temp)
with(AQ, plot(Temp, Ozone))
abline(fit.Temp)


#3
library(rgl)
open3d()
plot3d(AQ$Ozone ~ AQ$Temp + AQ$Wind, col='blue')


fit2 = lm(Ozone ~ Temp + Wind, data=AQ)
summary(fit2)
summary(AQ)
vals.Temp = seq(from = 53, to = 100, by = 2)
vals.Wind = seq(from = 1.0, to = 24, by = 1)

print(vals.Temp)
print(vals.Wind)
pred.grid = data.frame(expand.grid(Wind = vals.Wind, Temp = vals.Temp))

pred.Ozone = predict(fit2, newdata = pred.grid)
open3d()
persp3d(x = vals.Wind, y = vals.Temp, z = pred.Ozone, col = "orange")

