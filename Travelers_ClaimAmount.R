# Travelers Case Competition
# Vinod Nakkala & Pradeep Jeyachandran
# Nov 2016

library(dplyr)
library(caret)

#setwd("C:/Users/HP/Desktop/Travelers Case Competition")
#train = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_train.csv")
#valid = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_valid.csv")
#test = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_hold.csv")
setwd("C:/Users/nakka/Downloads/Travelers")
train = read.csv("C:/Users/nakka/Downloads/Travelers/Train_clmAmount.csv")
test = read.csv("C:/Users/nakka/Downloads/Travelers/Test_clmAmount.csv")
dim(train)
dim(test)
colnames(train)
train$exp_risk = NULL
train$area_ratio = NULL
train$agecat_ratio = NULL
train$exp_risk_ratio = NULL
train$veh_ratio = NULL
train$veh_age_ratio = NULL
View(train)
test$exp_risk = NULL
test$area_ratio = NULL
test$agecat_ratio = NULL
test$exp_risk_ratio = NULL
test$veh_ratio = NULL
test$veh_age_ratio = NULL
test$Probs = NULL
summary(as.factor(train$clm))
train_ones = subset(train, train[,'clm'] == 1)
test_ones = subset(test, test[,'clm'] == 1)
dim(train_ones)
dim(test_ones)

X_features = colnames(train_ones)
#X_features <- c("veh_value","veh_age","gender","area","agecat","exp_risk","veh_ratio","exp_risk_ratio")
X_test = test_ones

param <- list(  objective           = "reg:linear", 
                booster             = "gblinear",
                eval_metric         = "auc",  # maximizing for auc
                eta                 = 0.002,   # learning rate - Number of Trees
                max_depth           = 7,      # maximum depth of a tree
                subsample           = .9,     # subsample ratio of the training instance
                colsample_bytree    = .87,    # subsample ratio of columns 
                min_child_weight    = 1,      # minimum sum of instance weight (defualt)
                scale_pos_weight    = 1       # helps convergance bc dataset is unbalanced
) 


  X_train = as.data.frame(train_ones)
  train_new <- sparse.model.matrix(X_train$clmAmount ~ ., data = X_train)
  dtrain <- xgb.DMatrix(data=train_new, label=X_train$clmAmount)
  model_xgb <- xgb.train(   params              = param, 
                            data                = dtrain, 
                            nrounds             = 50, 
                            verbose             = 1,
                            maximize            = FALSE
    )
  
  X_test$Target <- -1
  testing <- sparse.model.matrix(Target ~ ., data = X_test)
  preds <- predict(model_xgb, testing) 
  preds
  summary(preds)
  sum(train_ones$clmAmount)
  vimp <- xgb.importance(model = model_xgb, feature_names = X_features)
  
  
  source("./LinearRegression.R")
  ClaimAmount = train_ones$clmAmount
  ClaimAmount1 = log(ClaimAmount)
  train_ones$clmAmount = NULL
  model_lr <- LinearRegression(train_ones,ClaimAmount, test_ones, cv=8, metric="rmse")
  test_lr = model_lr$test
  plot(density(log(ClaimAmount)))
  summary(ClaimAmount)
  summary(test_lr$pred_lr)
  