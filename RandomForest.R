## loading libraries
install.packages("pROC")
library(pROC)
suppressWarnings(library(pROC))
library(randomForest)


RandomForestRegression_CV <- function(X_train,y,X_test=data.frame(),cv=5,ntree=50,nodesize=5,seed=123,metric="mae")
{
  score <- function(a,b,metric)
  {
    switch(metric,
           mae = sum(abs(a-b))/length(a),
           rmse = sqrt(sum((a-b)^2)/length(a)))
  }
  
  cat("Preparing Data\n")
  X_train$order <- seq(1, nrow(X_train))
  X_train$result <- as.numeric(y)
  
  set.seed(seed)
  X_train$randomCV <- floor(runif(nrow(X_train), 1, (cv+1)))
  
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    X_build <- subset(X_train, randomCV != i, select = -c(order, randomCV))
    X_val <- subset(X_train, randomCV == i) 
    
    model_rf <- randomForest(result ~., data = X_build, ntree = ntree, nodesize = nodesize)
    
    pred_rf <- predict(model_rf, X_val)
    X_val <- cbind(X_val, pred_rf)
    
    if (nrow(X_test) > 0)
    {
      pred_rf <- predict(model_rf, X_test)
    }
    
    cat("CV Fold-", i, " ", metric, ": ", score(X_val$result, X_val$pred_rf, metric), "\n", sep = "")
    
    if (i == 1)
    {
      output <- X_val
      if (nrow(X_test) > 0)
      {
        X_test <- cbind(X_test, pred_rf)
      }      
    }
    
    if (i > 1)
    {
      output <- rbind(output, X_val)
      if (nrow(X_test) > 0)
      {
        X_test$pred_rf <- (X_test$pred_rf * (i-1) + pred_rf)/i
      }            
    }
    
    gc()
  } 
  
  output <- output[order(output$order),]
  cat("\nRandomForest ", cv, "-Fold CV ", metric, ": ", score(output$result, output$pred_rf, metric), "\n", sep = "")
  
  output <- subset(output, select = c("order", "pred_rf"))
  return(list(output, X_test))  
}


## 5-fold cross validation and scoring

## Usage
#source("./RandomForest.R")
#model_rf_1 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=235,metric="rmse")
#model_rf_2 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=357,metric="rmse")
#model_rf_3 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=13,metric="rmse")
#model_rf_4 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=753,metric="rmse")
#model_rf_5 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=532,metric="rmse")
#submit <- data.frame("Id" = test_rf_1$Id,"Prediction" = 0.2*exp(test_rf_1$pred_rf) + 0.2*exp(test_rf_2$pred_rf) + 0.2*exp(test_rf_3$pred_rf) + 0.2*exp(test_rf_4$pred_rf) + 0.2*exp(test_rf_5$pred_rf))
# write.csv(submit, "./submit.csv", row.names=F)





#
#
