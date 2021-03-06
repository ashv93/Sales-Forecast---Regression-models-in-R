setwd("C:/Users/Aswini/Desktop/ABI")
b <- read.csv("newproject.csv")
ordered <- b
summary(ordered)
str(ordered)
boxplot(ordered)
salesout <- ordered
#removing outliers
CleanData <- subset(salesout , Unit.Price< 25 & Profit > 10 & Profit < 110 & Shipping.Cost < 10 & Sales < 500 & Discount < 0.75000 )
#dataset without outliers
boxplot(CleanData)
#checking for missig values
sum(is.na(CleanData))
CleanData$Order.Priority <- as.character(as.numeric(CleanData$Order.Priority))
CleanData$Ship.Mode <- as.character(as.numeric(CleanData$Ship.Mode))
CleanData$Province <- as.character(as.numeric(CleanData$Province))
CleanData$Region <- as.character(as.numeric(CleanData$Region))
CleanData$Customer.Segment <- as.character(as.numeric(CleanData$Customer.Segment))
CleanData$Product.Category <- as.character(as.numeric(CleanData$Product.Category))
CleanData$Product.Sub.Category <- as.character(as.numeric(CleanData$Product.Sub.Category))
CleanData$Product.Name <- as.character(as.numeric(CleanData$Product.Name))
CleanData$Product.Container <- as.character(as.numeric(CleanData$Product.Container))
str(CleanData)
#multicollinearity check
library(usdm)
vif(CleanData)

#Model selection - backward elimination method
fullmodel <- lm(Sales ~., data=CleanData)  
stepselb <- step(fullmodel, direction="backward")

# after eliminating the reduntant variables
#Multiple regression with new model

NewModel <- lm(Sales ~ Order.Date + Order.Priority + Order.Quantity +Ship.Mode + 
                 Profit + Province + Customer.Segment + Product.Name, data = CleanData)

summary(NewModel)

#splitting data
set.seed(0)
train1 = sample(1:nrow(x), 7.5*nrow(x)/10)
test1 = (-train)
nrow(CleanData)
nrow(CleanData[train1,])
nrow(CleanData[test1,])
trainModel <- lm(Sales ~  Order.Priority + Order.Quantity +Ship.Mode + 
                  Profit + Province + Customer.Segment + Product.Name, data = CleanData[train1,])

summary(trainModel)

#Prediction with 95% percent confidence interval
prediction <- predict(trainModel,data = CleanData[train1,], interval ="confidence")
summary(prediction)

prediction <- predict(trainModel,data = CleanData[test1,],interval ="confidence")
summary(prediction)

#rmse of multiple linear regression
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(trainModel$residuals)


#ridge regression
z <- data.matrix(CleanData)
x <- as.matrix(z[,-4])
y <- as.matrix(z[,4])
class(x)
class(y)
colnames(y) <- "sales"
nrow(x)
nrow(y)
grid = 10^seq(5, -2, length = 100)
library(glmnet)
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)
set.seed(0)
train = sample(1:nrow(x), 7.5*nrow(x)/10)
test = (-train)
y.test = y[test]
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[test, ])
sqrt(mean((ridge.bestlambdatrain - y.test)^2))

#lasso regression
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)

dim(coef(lasso.models)) 
coef(lasso.models) 

#Running 10-fold cross validation to choose lamba
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
sqrt(mean((lasso.bestlambdatrain - y.test)^2))


