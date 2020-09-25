getwd()
data(mtcars)
head(mtcars)

# Transform certain variables into factors
mtcars$cyl  <- factor(mtcars$cyl)
mtcars$vs   <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am   <- factor(mtcars$am,labels=c("Automatic","Manual"))

## Regression Analysis

aggregate(mpg~am, data = mtcars, mean)

D_automatic <- mtcars[mtcars$am == "Automatic",]
D_manual <- mtcars[mtcars$am == "Manual",]
t.test(D_automatic$mpg, D_manual$mpg)

## P = 0.001374 hus we can state this is a significant difference. Now to quantify this.

init <- lm(mpg ~ am, data = mtcars)
summary(init)

## This shows us that the average MPG for automatic is 17.1 MPG, while manual is 7.2 MPG higher.
## The R2 value is 0.36 thus telling us this model only explains us 36% of the variance. 
##As a result, we need to build a multivariate linear regression.

betterFit <- lm(mpg~am + cyl + disp + hp + wt, data = mtcars)
anova(init, betterFit)

#This results in a p-value of 8.637e-08, and we can claim the betterFit model is significantly better than our init simple model. 
## We double-check the residuals for non-normality (Appendix - Plot 3)
##and can see they are all normally distributed and homoskedastic

summary(betterFit)

## The model explains 86.64% of the variance and as a result, cyl, disp, hp, wt did affect the correlation 
##between mpg and am. Thus, we can say the difference between automatic and manual transmissions is 1.81 MPG.

##Plotting 
##Plot 1 - Boxplot of MPG by transmission type
boxplot(mpg ~ am, data = mtcars, col = (c("red","blue")), ylab = "Miles Per Gallon", xlab = "Transmission Type")

##Plot 2 - Pairs plot for the data set
pairs(mpg ~ ., data = mtcars)

##Plot 3 - Check residuals
par(mfrow = c(2,2))
plot(betterFit)
     