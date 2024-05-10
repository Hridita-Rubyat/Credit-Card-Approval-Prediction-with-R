rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 

# install packages
install.packages("earth")
install.packages("kknn")
install.packages("nnet")


#load the packages we need
library(broom)
library(earth)
library(kknn)
library(nnet)

credit_card = read.csv("hw4_data_card.csv", header = T,stringsAsFactors = T, sep = ",")
str(credit_card)

#credit_card$age_squared = credit_card$age^2
#credit_card$income_squared = credit_card$income^2

set.seed(1)

#1#
is_training = runif(nrow(credit_card)) < 0.9

data_train = subset(credit_card, is_training)
data_valid = subset(credit_card, !is_training)



#(a)#
model1 = lm(card ~ . - reports, data = data_train)
summary(model1)

#(b)#
model2 = lm(card ~., data = data_train)
summary(model2)

#(c)#

model3 = lm(card ~ reports + age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active + I(income^2), data = data_train)
summary(model3)


#(d)#

model4 = lm(card ~ . + I(income^2) + I(age^2), data = data_train)
summary(model4)


#2#
diff_model1 = data_valid$card - predict(model1, data_valid)
diff_model2 = data_valid$card - predict(model2, data_valid)
diff_model3 = data_valid$card - predict(model3, data_valid)
diff_model4 = data_valid$card - predict(model4, data_valid)

mean(diff_model1^2)^0.5
mean(diff_model2^2)^0.5
mean(diff_model3^2)^0.5
mean(diff_model4^2)^0.5

#Model 3 is the best model as it has the lowest RMSE

# estimate the full dataset using the best model
final_model = model3 = lm(card ~ reports + age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active + I(income^2), data = credit_card)
summary(final_model)

#3#
### K-Fold Cross Validation

# set seeds for repeatable results from random number generators
set.seed(1)

# set the number of folds K
nFold = 5

# step 1: Randomly assign which fold each row is in 
valNum = floor(runif(nrow(credit_card))*nFold) + 1
head(valNum)

# create a matrix where we store prediction error 
model_perfomance = matrix(NA, nFold, 4)

# loop through each fold for cross validation
for (i in 1:nFold){
  # step 2i: Get the training and validation data for this fold
  trainData = subset(credit_card, valNum != i)
  validData = subset(credit_card, valNum == i)
  
  # step 2ii: Estimate the model for this training data
  model1 = lm(card ~ . - reports, data = trainData)
  model2 = lm(card ~., data = trainData)
  model3 = lm(card ~ reports + age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active + I(income^2), data = trainData)
  model4 = lm(card ~ . + I(income^2) + I(age^2), data = trainData)
  
  # step 2iii: Calculate out of sample RMSE for this validData
  valid1 = mean((validData$card - predict(model1, validData))^2)^0.5
  valid2 = mean((validData$card - predict(model2, validData))^2)^0.5
  valid3 = mean((validData$card - predict(model3, validData))^2)^0.5
  valid4 = mean((validData$card - predict(model4, validData))^2)^0.5
  
  # store model performance
  model_perfomance[i, ] = c(valid1, valid2, valid3,valid4)
}

# step 3: Check Average Model Performance
colMeans(model_perfomance)

# step 4: Now which one is the best? 
#Model3 has the best model specification as it has smallest RMSE

#4#
#Mars Model#
#mars_model1 = earth(card ~., data = data_train, nfold=5, keepxy=TRUE, trace = 2, thres = 0.1)
#plotmo(mars_model1)

#mean((validData$card - predict(mars_model1, validData))^2)^0.5


# set seeds for repeatable results from random number generators
set.seed(1)

# set the number of folds K
nFold = 5

# step 1: Randomly assign which fold each row is in 
valNum = floor(runif(nrow(credit_card))*nFold) + 1
head(valNum)

# create a matrix where we store prediction error 
model_earth_perfomance = matrix(NA, nFold, 1)

# loop through each fold for cross validation
for (i in 1:nFold){
  # step 2i: Get the training and validation data for this fold
  trainData = subset(credit_card, valNum != i)
  validData = subset(credit_card, valNum == i)
  
  # step 2ii: Estimate the model for this training data
  mars_model1 = earth(card ~., data = trainData, trace = 2, thres = 0.1)
  
  # step 2iii: Calculate out of sample RMSE for this validData
  valid1 = mean((validData$card - predict(mars_model1, validData))^2)^0.5
  
  # store model performance
  model_earth_perfomance[i, ] = c(valid1)
}

# step 3: Check Average Model Performance
colMeans(model_earth_perfomance)


#K-nearest neighbor

# set seeds for repeatable results from random number generators
set.seed(1)

# set the number of folds K
nFold = 5

# step 1: Randomly assign which fold each row is in 
valNum = floor(runif(nrow(credit_card))*nFold) + 1
head(valNum)

# create a matrix where we store prediction error 
model_kknn_perfomance = matrix(NA, nFold, 1)

# loop through each fold for cross validation
for (i in 1:nFold){
  # step 2i: Get the training and validation data for this fold
  trainData = subset(credit_card, valNum != i)
  validData = subset(credit_card, valNum == i)
  
  # step 2ii: Estimate the model for this training data
  kkn_model1 = kknn(card ~., trainData, validData, k = 7, distance = 1)
  
  # step 2iii: Calculate out of sample RMSE for this validData
  valid1 = mean((validData$card - kkn_model1$fitted.values)^2)^0.5
  
  # store model performance
  model_kknn_perfomance[i, ] = c(valid1)
}

# step 3: Check Average Model Performance
colMeans(model_kknn_perfomance)

kkn_model1 = kknn(card ~., trainData, validData, k = 7, distance = 1)
valid1 = mean((validData$card - kkn_model1$fitted.values)^2)^0.5