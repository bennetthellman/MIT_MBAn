library(tidyverse)
library(miscTools)
library(Metrics)
library(ggcorrplot)
library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lars)
library(leaps)
library(gbm)
library(MASS)
library(caret)
library(leaps)
library(Rcpp)
library(ISLR)
library(leaps)
library(Metrics)
library(miscTools)
library(glmnet)
library(arsenal)





#1.
setwd("/Users/bennetthellman/Desktop/OneDrive - Massachusetts Institute of Technology/Edge/HWs/Pset1")
train = read.csv("train_data.csv")
test = read.csv("test_data.csv")
sites = rbind(train,test)
sites.const = read.csv("site_const_data.csv")
model.1 = lm(annual.profit~ . - store.number, data=sites)
summary(model.1)
newpred.1 = predict(model.1,newdata=sites.const)
value <- sum(newpred.1)
R2 <- summary(model.1)$r.squared


######################################################################
#2.
num_vars<-train%>%dplyr::select(-c(store.number, state))
#%>%convert(num(nearcomp, freestand, sqft, intersect, pop, ))
corr <- round(cor(num_vars), 3)
ggcorrplot(cor(corr))
select(sites, -"state")



######################################################################
#3. 
#Kathleen's original model
og_mod<-lm(annual.profit~ agg.inc + sqft + col.grad + com60, data=train)
og_mod_pred = predict(og_mod,newdata=test)
new_pred = predict(og_mod, newdata = sites.const)
value <- sum(new_pred)
train.mean <- mean(train$annual.profit)
SSE <- sum((og_mod_pred - test$annual.profit)^2)
SST <- sum((train.mean - test$annual.profit)^2)
OSR2 <- 1 - SSE/SST
R2 <- summary(og_mod)$r.squared
value
OSR2
R2



#saturated model
sat_mod<-lm(annual.profit~ ., data=train)
sat_mod_pred = predict(sat_mod,newdata=test)
new_pred = predict(sat_mod, newdata = sites.const)
value <- sum(new_pred)
train.mean <- mean(train$annual.profit)
SSE <- sum((sat_mod_pred - test$annual.profit)^2)
SST <- sum((train.mean - test$annual.profit)^2)
OSR2 <- 1 - SSE/SST
R2 <- summary(sat_mod)$r.squared
value
OSR2
R2

#Self-Built Model
bmod<-lm(annual.profit~ agg.inc + sqft + col.grad + com60, data=train)
summary(bmod)
bmod<-lm(annual.profit~ agg.inc + sqft + col.grad + com60 + lci, data=train)
summary(bmod)
bmod<-lm(annual.profit~ agg.inc + sqft + col.grad  + com60 + lci + nearcomp, data=train)
summary(bmod)
bmod<-lm(annual.profit~ agg.inc + sqft + col.grad  + com60 + lci + nearcomp + intersect, data=train)
summary(bmod)


bmod_pred_train = predict(bmod,newdata=train)
bmod_pred = predict(bmod,newdata=test)
new_pred = predict(bmod, newdata = sites.const)
value <- sum(new_pred)
train.mean <- mean(train$annual.profit)
SSE <- sum((bmod_pred - test$annual.profit)^2)
SST <- sum((train.mean - test$annual.profit)^2)
OSR2 <- 1 - SSE/SST
R2 <- summary(b_mod)$r.squared
value
OSR2
R2




######################################################################
#4
set.seed(15072) 
train.control <- trainControl(method = "cv", number = 10)
n.predictors <- ncol(train) - 1

step.model <- train(annual.profit ~., data = train,
                    method = "leapForward", 
                    nvmax = n.predictors,
                    trControl = train.control
)
step.model$results




predict.regsubsets = function(object,newdata,id,...){
  form = as.formula(object$call[[2]]) # Extract the formula used when we called regsubsets()
  mat = model.matrix(form,newdata)    # Build the model matrix
  coefi = coef(object,id=id)          # Extract the coefficiants of the ith model
  xvars = names(coefi)                # Pull out the names of the predictors used in the ith model
  mat[,xvars]%*%coefi               # Make predictions using matrix multiplication
}

# Assign each observation to a single fold
k=10
folds = sample(1:k, nrow(train), replace = TRUE)
v = 14
# Create a matrix to store the results of our upcoming calculations
cv_errors = matrix(NA, k, v, dimnames = list(NULL, paste(1:v)))
for(j in 1:k){
  
  # The perform best subset selection on the full dataset, minus the jth fold
  best_fit = regsubsets(annual.profit~., data = train[folds!=j,], nvmax=v)
  
  # Inner loop iterates over each size i
  for(i in 1:v){
    
    # Predict the values of the current fold from the "best subset" model on i predictors
    pred = predict(best_fit, train[folds==j,], id=i)
    
    # Calculate the MSE, store it in the matrix we created above
    cv_errors[j,i] = mean((train$annual.profit[folds==j]-pred)^2)
  }
}

# Take the mean of over all folds for each model size
mean_cv_errors = apply(cv_errors, 2, mean)

# Find the model size with the smallest cross-validation error
min = which.min(mean_cv_errors)

# Plot the cross-validation error for each model size, highlight the min
plot(mean_cv_errors, type='b')
points(min, mean_cv_errors[min][1], col = "red", cex = 2, pch = 20)

reg_best = regsubsets(annual.profit~., data = train, nvmax = 19)
round(coef(reg_best, 11), 2)


fcv_mod <-lm(annual.profit~  lci + nearcomp + nearmil + freestand + sqft + pop + agg.inc + col.grad + drive + public + home, data=train)
summary(fcv_mod)
fcv_pred_train = predict(fcv_mod,newdata=train)
fcv_pred = predict(fcv_mod,newdata=test)
new_pred = predict(fcv_mod, newdata = sites.const)
value <- sum(new_pred)
train.mean <- mean(train$annual.profit)
SSE <- sum((fcv_pred - test$annual.profit)^2)
SST <- sum((train.mean - test$annual.profit)^2)
OSR2 <- 1 - SSE/SST
value
OSR2
rSquared(train$annual.profit, resid = train$annual.profit-fcv_pred_train)


######################################################################
#5
x.sites = model.matrix(Kathleen.Previous.Prediction ~ . - 1, 
                       data=sites.const)
x.train = model.matrix(annual.profit ~ . - 1  , 
                       data=train) # The "-1" just mean that we are excluding the constant term (that is, the intercept)
x.train<-x.train[,-13]
x.test<-x.test[,-13]

#had to delete because OK is not in the sites.const dataset
y.train = train$annual.profit # Here, we are only including the dependent variable.
x.test = model.matrix(annual.profit ~ . - 1,
                      data=test) 
y.test = test$annual.profit
lambdas.lasso <- exp(seq(10, -5, -.01))
cv.lasso <- cv.glmnet(x.train,
                      y.train,alpha=1,
                      lambda=lambdas.lasso,
                      nfolds=10, 
                      ncv =100)
cv.lasso
plot(cv.lasso)
lasso.lambda.cv <- cv.lasso$lambda.min
lasso.lambda.1SE.cv <- cv.lasso$lambda.1se

lasso.final <- glmnet(x.train,y.train,alpha=1,lambda=lasso.lambda.cv)
summary(lasso.final)
new_pred = predict(lasso.final, x.sites)
value <- sum(new_pred)
pred.test.final.train <- predict(lasso.final,x.train)
pred.test.final <- predict(lasso.final,x.test)
R2.lasso.final <- 1-sum((pred.test.final.train-train$annual.profit)^2)/sum((mean(train$annual.profit)-train$annual.profit)^2)
OSR2.lasso.final <- 1-sum((pred.test.final-test$annual.profit)^2)/sum((mean(train$annual.profit)-test$annual.profit)^2)
round(coefficients(lasso.final), 2)
OSR2.lasso.final
R2.lasso.final
value

#




