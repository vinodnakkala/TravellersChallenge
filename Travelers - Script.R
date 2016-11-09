
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
train = read.csv("C:/Users/nakka/Downloads/Travelers/Kangaroo_train.csv")
valid = read.csv("C:/Users/nakka/Downloads/Travelers/Kangaroo_valid.csv")
test = read.csv("C:/Users/nakka/Downloads/Travelers/Kangaroo_hold.csv")

test1 = test

# Combining training and validation sets
train_val = rbind(train,valid)

# claimcst0 and numclaims have to be set to NULL as they can't be used
train_val$claimcst0 = NULL
train_val$numclaims = NULL
test1$numclaims = NULL
test1$clm=NULL
# Feature Engineering
# From preliminary analysis veh_body seems to have high significance
# We compute the ratio of 1's to overall no of 1's & 0's in clm for each level of veh_body and use it as another independent
train_val$exp_risk= ifelse(train_val$exposure > 0.75, 4, ifelse(train_val$exposure > 0.5, 3,
                    ifelse(train_val$exposure > 0.25,2,1)))
test1$exp_risk= ifelse(test1$exposure > 0.75, 4, ifelse(test1$exposure > 0.5, 3,
                ifelse(test1$exposure > 0.25,2,1)))

train_val$exp_risk = ordered(train_val$exp_risk)
test1$exp_risk = ordered(test1$exp_risk)


############ Visualization ################################
outcomes <- test1 %>% group_by(as.factor(test1$veh_body), as.factor(test1$clm)) %>% summarise (num_cust = n())
colnames(outcomes) <- c("veh_body","clm","Count")
ggplot(outcomes , aes(x = outcomes$veh_body, y = outcomes$Count, fill = outcomes$clm)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  labs(y = 'Proportion of 0s',x = 'Country',title = 'Target 0/1')  + coord_flip()
View(outcomes)

outcomes1 <- train_val %>% group_by(as.factor(train_val$veh_body), as.factor(train_val$clm)) %>% summarise (num_cust = n())
colnames(outcomes1) <- c("veh_body","clm","Count")
ggplot(outcomes1 , aes(x = outcomes1$veh_body, y = outcomes1$Count, fill = outcomes1$clm)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  labs(y = 'Proportion of 0s',x = 'Country',title = 'Target 0/1')  + coord_flip()
#View(outcomes1)
summary(train_val$veh_body)
############ Visualization ################################


# Combining certain levels of veh_body

train_val$veh_body = as.character(train_val$veh_body)

train_val$veh_body = ifelse(train_val$veh_body == "RDSTR","RARE",ifelse(train_val$veh_body == "MCARA",
  "RARE", ifelse(train_val$veh_body =="BUS","RARE", ifelse(train_val$veh_body =="CONVT","RARE", train_val$veh_body))))
                            
train_val$veh_body = as.factor(train_val$veh_body)

test1$veh_body = as.character(test1$veh_body)
test1$veh_body = ifelse(test1$veh_body == "RDSTR","RARE",ifelse(test1$veh_body == "MCARA",
"RARE", ifelse(test1$veh_body =="BUS","RARE", ifelse(test1$veh_body =="CONVT","RARE", test1$veh_body))))

test1$veh_body = as.factor(test1$veh_body)
##############################################################
cat_encoding = function(data, catvar, target)
{
  outcomes <- data %>% group_by(as.factor(data[,catvar]), as.factor(data[,target])) %>% summarise (num_cust = n())
  colnames(outcomes) <- c(catvar,target,"Count")
  
  # Computing ratio of 1's to no of 1's+0's in our target variable by each level of variable veh_body
  outcomes2 = aggregate(outcomes[["Count"]], by=list(Category=outcomes[[catvar]]), FUN=sum)
  colnames(outcomes2) = c(catvar, 'no_of_zero_and_ones')
  outcomes3 = subset(outcomes, outcomes[,target] == 1)
  outcomes3 = left_join(outcomes3,outcomes2,by = catvar)
  outcomes3$ratio = outcomes3$Count/outcomes3$no_of_zero_and_ones
  outcomes3 = subset(outcomes3, select=c(catvar, "ratio"))
  
  #putting everything together
  data = left_join(data,outcomes3,by = catvar)
  
  return(data)
}

train_val = cat_encoding(train_val, 'veh_body', 'clm')

#train_val$clm = as.factor(train_val$clm)
#test$clm = as.factor(test$clm)
#View(Bal_train_1)
# Mapping the same ratio values onto our test set

temp = train_val[,c('veh_body','ratio')]
temp = unique(temp)
test1 = left_join(test1,temp,by = 'veh_body')
#############################################################################################
train_val$veh_ratio = train_val$ratio
test1$veh_ratio = test1$ratio
train_val$ratio = NULL
test1$ratio = NULL
# For Vehicle Age
train_val$veh_age = as.factor(train_val$veh_age)
test1$veh_age = as.factor(test1$veh_age)
train_val = cat_encoding(train_val, 'veh_age', 'clm')
temp = train_val[,c('veh_age','ratio')]
temp = unique(temp)
test1 = left_join(test1,temp,by = 'veh_age')
train_val$veh_age_ratio = train_val$ratio
test1$veh_age_ratio = test1$ratio
train_val$ratio = NULL
test1$ratio = NULL
# For Area
train_val$area = as.factor(train_val$area)
test1$area = as.factor(test1$area)
train_val = cat_encoding(train_val, 'area', 'clm')
temp = train_val[,c('area','ratio')]
temp = unique(temp)
test1 = left_join(test1,temp,by = 'area')
train_val$area_ratio = train_val$ratio
test1$area_ratio = test1$ratio
train_val$ratio = NULL
test1$ratio = NULL
# For Age category
train_val$agecat = as.factor(train_val$agecat)
test1$agecat = as.factor(test1$agecat)
train_val = cat_encoding(train_val, 'agecat', 'clm')
temp = train_val[,c('agecat','ratio')]
temp = unique(temp)
test1 = left_join(test1,temp,by = 'agecat')
train_val$agecat_ratio = train_val$ratio
test1$agecat_ratio = test1$ratio
train_val$ratio = NULL
test1$ratio = NULL
# Exposure risk ratio
train_val$exp_risk = as.factor(train_val$exp_risk)
test1$exp_risk = as.factor(test1$exp_risk)
train_val = cat_encoding(train_val, 'exp_risk', 'clm')
temp = train_val[,c('exp_risk','ratio')]
temp = unique(temp)
test1 = left_join(test1,temp,by = 'exp_risk')
train_val$exp_risk_ratio = train_val$ratio
test1$exp_risk_ratio = test1$ratio
train_val$ratio = NULL
test1$ratio = NULL
#######################################
#train_val$agecat_ratio = NULL
#train_val$veh_age_ratio = NULL
#train_val$area_ratio = NULL
#test1$agecat_ratio = NULL
#test1$veh_age_ratio = NULL
#test1$area_ratio = NULL
#train_val$exposure = NULL
#test1$exposure = NULL
#train_val$veh_body = NULL
#test1$veh_body = NULL
##############################################################################################
#View(train_val)
#View(test1)
# Balancing dataset
TestId = test1$id
train_val$id = NULL
test1$id = NULL
#subsetting only rows with clm=0
train_val_zeros = subset(train_val, train_val[,'clm'] == 0)
#nrow(train_val_zeros)
#View(train_val_zeros)
train_val_ones = subset(train_val, train_val[,'clm'] == 1)


# Randomize our zeroes dataset
train_val_zeros <- train_val_zeros[sample(nrow(train_val_zeros)),]
# We have 3084 ones. To balance we need to sample around 3084 zeroes from train_val_zeroes
# train_val_zeroes has 42155 rows which can be split into 12 samples of 3250 each and one sample of 3155


sample_1 = train_val_zeros[1:3155,]
Bal_train_1 = rbind(train_val_ones,sample_1)
j=3156
for(i in 2:12)
{
  assign(paste("Bal_train",i,sep="_"), rbind(train_val_ones,assign(paste("sample",i,sep="_"),train_val_zeros[j:(j+3249),])))
  j=j+1
}

# checking if everything is proper
nrow(sample_2)
nrow(sample_5)
nrow(sample_6)
nrow(sample_12)

nrow(Bal_train_5)
nrow(Bal_train_1)
nrow(Bal_train_9)
nrow(Bal_train_12)



# Modeling
# We build 12 models on the 12 Balanced datasets and average the predictions with equal weightage

test1$clm = NULL

source("./LogisticRegression1.R")

#Store Ids for later use
#TrainId = Bal_train_1$id


list_object_names = sprintf("Bal_train_%s", 1:12)
list_df = lapply(list_object_names, get)

for (i in 1:length(list_df))
{
  df_train = list_df[i]
  df_train = as.data.frame(df_train)
  train_target = df_train$clm
  df_train$clm = NULL
  model_lr <- LogisticRegression(df_train,train_target, test1, cv=5, metric="logloss")

  if(i==1)
  {
    test_lr <- model_lr$test
    test_lr <- data.frame("id"=TestId, "pred_lr"=test_lr$pred_lr)
    test_lr$avg_pred = test_lr$pred_lr
    test_lr$pred_lr = NULL
  }

  if(i>1)
  {
    test_lr <- cbind(test_lr, model_lr$test)
    test_lr <- data.frame("id"=TestId, "pred_lr"=test_lr$pred_lr, 'avg_pred'= test_lr$avg_pred)
    test_lr$avg_pred =  (test_lr$avg_pred  + test_lr$pred_lr)
    test_lr$pred_lr = NULL
  }
  if( i == length(list_df)) 
  {
    test_lr$avg_pred = (test_lr$avg_pred)/i 
  }
}

# Results

test_lr$Actuals = test$clm
test_lr$Temp = ifelse(test_lr$avg_pred > 0.5, 1,0)
attach(test_lr)
#View(test_lr)
confusionMatrix(Temp,Actuals)

#### xgboost ################
#X_features <- c("veh_value","veh_age","gender","area","agecat","exp_risk","veh_ratio","exp_risk_ratio")

#vimp <- xgb.importance(model = model_xgb, feature_names = X_features)
#View(vimp)

################ Looping 12 times #########################
library(Matrix)
library(xgboost)
X_features = colnames(Bal_train_1)
X_features <- c("veh_value","veh_age","gender","area","agecat","exp_risk","veh_ratio","exp_risk_ratio")
X_test = test1

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",  # maximizing for auc
                eta                 = 0.002,   # learning rate - Number of Trees
                max_depth           = 7,      # maximum depth of a tree
                subsample           = .9,     # subsample ratio of the training instance
                colsample_bytree    = .87,    # subsample ratio of columns 
                min_child_weight    = 1,      # minimum sum of instance weight (defualt)
                scale_pos_weight    = 1       # helps convergance bc dataset is unbalanced
) 

for (i in 1:length(list_df))
{
  X_train = as.data.frame(list_df[i])
  train_new <- sparse.model.matrix(Bal_train_1$clm ~ ., data = Bal_train_1)
  train_new <- sparse.model.matrix(X_train$clm ~ ., data = X_train)
  dtrain <- xgb.DMatrix(data=train_new, label=X_train$clm)
  model_xgb <- xgb.train(   params              = param, 
                            data                = dtrain, 
                            nrounds             = 50, 
                            verbose             = 1,
                            maximize            = FALSE
  )
#  vimp <- xgb.importance(model = model_xgb, feature_names = X_features)
  test1$Target <- -1
  testing <- sparse.model.matrix(Target ~ ., data = test1)
  preds <- predict(model_xgb, testing) 
  
  if(i==1)
  {
    test_xgb <- data.frame("id"=TestId, "pred_xgb"=preds)
    test_xgb$avg_pred = test_xgb$pred_xgb
    test_xgb$pred_xgb = NULL
  }
  
  if(i>1)
  {
    test_xgb$pred_xgb <- preds
    test_xgb$avg_pred =  (test_xgb$avg_pred  + test_xgb$pred_xgb)
    test_xgb$pred_xgb = NULL
  }
  if( i == length(list_df)) 
  {
    test_xgb$avg_pred = (test_xgb$avg_pred)/i 
  }
}
test_xgb$modelPredicted <- ifelse(test_xgb$avg_pred<0.5, 0, 1)
confusionMatrix(test_xgb$modelPredicted,Actuals)
test1$Target = NULL

##################### End of loop & Ensemble of LR & XGB #############################
#test1$EnsembleProbs = (test_xgb$avg_pred+test_lr$avg_pred)/2
#test1$EnsemblePredict = ifelse(test1$EnsembleProbs<0.5, 0, 1)
#confusionMatrix(test1$EnsemblePredict,Actuals)
#test1$EnsembleProbs = NULL
#test1$EnsemblePredict = NULL
#View(test1)
######################   Random Forest ##############################################
source("./RandomForest1.R")
for (i in 1:length(list_df))
{
  df_train = list_df[i]
  df_train = as.data.frame(df_train)
  trainedTarget = df_train$clm
  df_train$clm = NULL
  model_rf <- RandomForestRegression_CV(df_train,trainedTarget,test1,cv=5,ntree=25,nodesize=5,seed=753,metric="rmse")
  if(i==1)
  {
    model_rf = as.data.frame(model_rf)
    test_rf <- as.data.frame(model_rf)
    test_rf <- data.frame("id"=TestId, "pred_rf"=model_rf$pred_rf)
    test_rf$avg_pred = test_rf$pred_rf
    test_rf$pred_rf = NULL
  }
  
  if(i>1)
  {
    test_rf <- cbind(test_rf, model_rf)
    test_rf <- data.frame("id"=TestId, "pred_rf"=test_rf$pred_rf, 'avg_pred'= test_rf$avg_pred)
    test_rf$avg_pred =  (test_rf$avg_pred  + test_rf$pred_rf)
    test_rf$pred_rf = NULL
  }
  if( i == length(list_df)) 
  {
    test_rf$avg_pred = (test_rf$avg_pred)/i 
  }
}

View(test_rf)
View(model_rf)
test_rf$Temp = ifelse(test_rf$avg_pred > 0.5, 1,0)
confusionMatrix(test_rf$Temp,Actuals)

View(test1)
test1$clm = test_xgb$modelPredicted
test1$Actual = Actuals
test1$Probs=test_xgb$avg_pred

train_val$clmAmount = train_val1$claimcst0
write.csv(train_val,"Train_clmAmount.csv",row.names = FALSE)
write.csv(test1,"Test_clmAmount.csv",row.names = FALSE)

################ Claim Amount Prediction ###############