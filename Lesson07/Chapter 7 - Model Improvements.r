
#Load Libraries
library(mlbench)
library(randomForest)
library(dplyr)

#Load the data
data(PimaIndiansDiabetes)
df<-PimaIndiansDiabetes
#Explore the length and breadth of the data
str(df)


#Setting seed for reproducability 
library(caret)
set.seed(2019)

#Create 70% train and 30% test dataset
train_index <- sample(seq_len(nrow(df)),floor(0.7 * nrow(df)))
train <- df[train_index,]
test <- df[-train_index,]
print("Training Dataset shape:")
print(dim(train))
print("Test Dataset shape:")
print(dim(test))

#Train model on train dataset
model <-randomForest(diabetes~.,data=train, mtry =3)
print(model)

#Predict on test dataset
y_predicted <- predict(model, newdata = test)

#Create Confusion-Matrix
results<-confusionMatrix(y_predicted, test$diabetes, positive= 'pos')
print("Confusion Matrix  (Test Data)- ")
print(results$table)

#Print overall Accuracy
results$overall[1]



library(caret)
set.seed(2019)

#Define function for 4 fold cross validation
train_control = trainControl(method = "cv",  number=5, savePredictions = TRUE,verboseIter = TRUE)

#Defining the value of mtry =3 (to match our previous example)
parameter_values = expand.grid(mtry=3)

#Fit the model
model_rf_kfold<- train(diabetes~., data=df, trControl=train_control, 
                    method="rf",  metric= "Accuracy", 
                    tuneGrid = parameter_values)
#Print Overall Accuracy (Averaged across all folds)
model_rf_kfold$results[2]

#Print the detailed prediction dataset
print("Shape of Prediction Dataset")
print(dim(model_rf_kfold$pred))

print("Prediction detailed results - ")
head(model_rf_kfold$pred) #print first 6 rows
tail(model_rf_kfold$pred) #print last 6 rows

print("Accuracy across each Fold-")
model_rf_kfold$resample

print(paste("Average Accuracy :",mean(model_rf_kfold$resample$Accuracy)))
print(paste("Std. Dev Accuracy :",sd(model_rf_kfold$resample$Accuracy)))


#Define function for 4 fold cross validation
set.seed(2019)
train_control = trainControl(method = "LOOCV", savePredictions = TRUE)

#Defining the value of mtry =3 (to match our previous example)
parameter_values = expand.grid(mtry=3)

#Fit the model
model_rf_LOOCV<- train(diabetes~., data=df, trControl=train_control, 
                    method="rf",  metric= "Accuracy", 
                    tuneGrid = parameter_values)
#Print Overall Accuracy (Averaged across all folds)
print(model_rf_LOOCV$results[2])

#Print the detailed prediction dataset
print("Shape of Prediction Dataset")
print(dim(model_rf_LOOCV$pred))

print("Prediction detailed results - ")
head(model_rf_LOOCV$pred) #print first 6 rows
 
tail(model_rf_LOOCV$pred) #print last 6 rows


#Grid Search Optimization : Random Forest
#Define the Cross Validation method
set.seed(2019)
train_control = trainControl(method = "cv",  number=5, savePredictions = TRUE)

#Define Parameter Grid
parameter_grid = expand.grid(mtry=c(1,2,3,4,5,6))

#Fit the model with cross validation and grid search optimization
model_rf_gridSearch<- train(diabetes~., data=df, trControl=train_control, 
               method="rf",  metric= "Accuracy", 
               tuneGrid = parameter_grid)

#Print Overall Accuracy (Averaged across all folds)
print("Accuracy across hyperparameter Combinations:")
print(model_rf_gridSearch$results[,1:2])

#Print the detailed prediction dataset
print("Shape of Prediction Dataset")
print(dim(model_rf_gridSearch$pred))

print("Prediction detailed results - ")
print(head(model_rf_gridSearch$pred)) #print the first 6 rows

print(tail(model_rf_gridSearch$pred)) #print the last 6 rows

print("Best value for Hyperparameter 'mtry':")
print(model_rf_gridSearch$bestTune)

print("Final (Best) Model ")
print(model_rf_gridSearch$finalModel)

#Plot the grid metrics
library(repr)
options(repr.plot.width=8, repr.plot.height=5)
plot(model_rf_gridSearch)

#Grid Search Optimization - XGBoost
set.seed(2019)
#Define the Cross Validation method
train_control = trainControl(method = "cv",  number=5, savePredictions = TRUE)

#Define Parameter Grid
parameter_grid = expand.grid(nrounds = c(30,50,60,100),
                             eta=c(0.01,0.1,0.2,0.3),
                             max_depth = c(2,3,4,5),
                             gamma = c(1),
                             colsample_bytree = c(0.7),
                             min_child_weight = c(1)  ,
                             subsample = c(0.6)
                            )

#Fit the model with cross validation and grid search optimization
model_xgb_gridSearch<- train(diabetes~., data=df, trControl=train_control, 
               method="xgbTree",  metric= "Accuracy",
                            tuneGrid = parameter_grid)

#Print the detailed prediction dataset
print("Shape of Prediction Dataset")
print(dim(model_xgb_gridSearch$pred))

print("Prediction detailed results - ")
head(model_xgb_gridSearch$pred) #print the first 6 rows

tail(model_xgb_gridSearch$pred) #print the last 6 rows

print("Best values for all selected Hyperparameters:")
model_xgb_gridSearch$bestTune

#Print Overall Accuracy (Averaged across all folds)
print("Average results across different combination of Hyperparameter Values")
model_xgb_gridSearch$results %>% arrange(desc(Accuracy)) %>% head(5)

library(repr)
options(repr.plot.width=8, repr.plot.height=5)
plot(model_xgb_gridSearch)


#Random Search Optimization - Random Forest

#Define the Cross Validation method
set.seed(2019)
train_control = trainControl(method = "cv",  number=5, savePredictions = TRUE)

#Fit the model with cross validation and grid search optimization
model_rf_randomSearch<- train(diabetes~., data=df, trControl=train_control, 
                               method="rf",  metric= "Accuracy",tuneLength = 35)

#Print the detailed prediction dataset
print("Shape of Prediction Dataset")
print(dim(model_rf_randomSearch$pred))

print("Prediction detailed results - ")
head(model_rf_randomSearch$pred) #print the first 6 rows

tail(model_rf_randomSearch$pred) #print the last 6 rows

print("Best values for all selected Hyperparameters:")
model_rf_randomSearch$bestTune

#Print Overall Accuracy (Averaged across all folds)
model_rf_randomSearch$results %>% arrange(desc(Accuracy)) %>% head(5)

plot(model_rf_randomSearch)


#Random Search Optimization - XGBoost
set.seed(2019)

#Define the Cross Validation method
train_control = trainControl(method = "cv",  number=5, savePredictions = TRUE)

#Fit the model with cross validation and random search optimization
model_xgb_randomSearch<- train(diabetes~., data=df, trControl=train_control, 
                               method="xgbTree",  metric= "Accuracy",tuneLength = 15)

#Print the detailed prediction dataset
print("Shape of Prediction Dataset")
print(dim(model_xgb_randomSearch$pred))

print("Prediction detailed results - ")
head(model_xgb_randomSearch$pred) #print the first 6 rows

tail(model_xgb_randomSearch$pred) #print the last 6 rows

print("Best values for all selected Hyperparameters:")
model_xgb_randomSearch$bestTune

#Print Overall Accuracy (Averaged across all folds for each hyperparameter combination)
model_xgb_randomSearch$results %>% arrange(desc(Accuracy)) %>% head(5)


set.seed(2019)
library(MlBayesOpt)
model_rf_bayesain <- rf_opt(train_data = train,
                       train_label = diabetes,
                       test_data = test,
                       test_label = diabetes,
                       mtry_range = c(1L, ncol(df)-1),
                       num_tree = 50,
                       init_points = 10,
                       n_iter = 10,
                       acq = "poi", eps = 0, 
                       optkernel = list(type = "exponential", power =2))



set.seed(2019)
model_xgb_bayesian <- xgb_opt(train, diabetes, test, diabetes, 
                              objectfun ='binary:logistic', evalmetric='logloss',
                              eta_range = c(0.1, 1L), max_depth_range = c(2L, 8L),
                              nrounds_range = c(70, 160L), #subsample_range = c(0.1, 1L),
                              bytree_range = c(0.4, 1L), init_points = 4, n_iter = 10, 
                              acq = "poi", eps = 0, optkernel = list(type = "exponential", power =2))

