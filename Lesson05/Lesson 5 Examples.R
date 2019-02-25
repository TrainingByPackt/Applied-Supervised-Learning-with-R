#Code snippet 5.1

#Load the weather data directly into df
library(rattle.data)
data(weatherAUS)
df <- weatherAUS
str(df)



#Code snippet 5.2
df$RISK_MM <- NULL    #Remove the column "RISK_MM"
temp_df<-as.data.frame(
sort(
round(
sapply(df, function(y) sum(length(which(is.na(y)))))/dim(df)[1],2)
)
                  		        )
colnames(temp_df) <- "NullPerc"                                
print(temp_df)                                 


#Code snippet 5.3
#Select the last 4 columns to drop that have >30% null values
cols_to_drop <-tail(rownames(temp_df),4)

#Remove the rows that have any null values
df_new<- na.omit(df[,!names(df) %in% cols_to_drop])

print("Shape of data after dropping columns:")
print(dim(df_new))

#Double check if we still have any  NULL values
temp_df<-as.data.frame(sort(round(sapply(df_new, 
function(y) sum(length(which(is.na(y)))))/dim(df)[1],2)))
colnames(temp_df) <- "NullPerc"                                
print(temp_df)             



#Code snippet 5.4
#Engineer time-based features from the Date variable.
library(lubridate). #Provides handy date-time related functions
df_new$day <- day(df_new$Date)
df_new$month <- month(df_new$Date)
df_new$dayofweek <- wday(df_new$Date)
df_new$quarter <- quarter(df_new$Date)
str(df_new[,c("day","month","dayofweek","quarter")])
#Delete the previous 'Date' column
df_new$Date <- NULL


#Code snippet 5.5
#Explore the location frequency
location_dist <- df_new %>% 
                    group_by(Location) %>% 
                    summarise(Rain  = sum(ifelse(RainTomorrow =="Yes",1,0)), cnt=n()) %>%
                    mutate(pct = Rain/cnt) %>% 
                    arrange(desc(pct))
print(paste("#Distinct locations :",dim(location_dist)[1]))
print(summary(location_dist))



#Code snippet 5.6
location_dist$Location <- as.character(location_dist$Location)
location_list <- c(head(location_dist$Location,5),tail(location_dist$Location,5))
print("Final list of locations - ")
print(location_list)

#Engineer the new location with reduced levels
df_new$Location <- as.character(df_new$Location)
df_new$new_location <- factor(ifelse(df_new$Location %in% location_list,df_new$Location,"Others"))
#Delete the old 'Location' variable
df_new$Location <- NULL
temp <- df_new %>% mutate(loc = as.character(new_location)) %>%
                    group_by(as.character(loc)) %>% 
                    summarise(Rain  = sum(ifelse(RainTomorrow =="Yes",1,0)), cnt=n()) %>%
                    mutate(pct = Rain/cnt) %>% 
                    arrange(desc(pct))
print(temp)




#Code snippet 5.7
#Setting seed for reproducibility
set.seed(2019)

#Creating a list of indexes for the training dataset (70%)
train_index <- sample(seq_len(nrow(df_new)),floor(0.7 * nrow(df_new)))

#Split the data into test and train
train <- df_new[train_index,]
test <- df_new[-train_index,]

#Build the logistic regression variable 
model <- glm(RainTomorrow ~ MinTemp + Rainfall + WindGustSpeed + 
                          		            WindSpeed3pm +Humidity3pm + Pressure3pm +
RainToday +  Temp3pm + Temp9am, 
data=train,
family=binomial(link='logit'))
summary(model)



#Code snippet 5.8
print("Distribution of labels in the data-")
print(table(df_new$RainTomorrow)/dim(df_new)[1])

#predict results on Train data
print("Training data results -")
pred_train <-factor(ifelse(predict(model,newdata=train,type = "response") > 0.5,"Yes","No"))
#Create the Confusion Matrix
train_metrics <- confusionMatrix(pred_train, train$RainTomorrow,positive="Yes")
print(train_metrics)


#Predict results on Test data
print("Test data results -")
pred_test <-factor(ifelse(predict(model,newdata=test,type = "response") > 0.5,"Yes","No"))
#Create the Confusion Matrix
test_metrics <- confusionMatrix(pred_test, test$RainTomorrow,positive="Yes")
print(test_metrics)



#Code snippet 5.9
#Fit the Logistic Regression model with all the available independent variables
model <- glm(RainTomorrow~., data=train ,family=binomial(link='logit'))

#Predict on train dataset
print("Training data results-")
pred_train <-factor(ifelse(predict(model,newdata=train,type = "response") >= 0.5,"Yes","No"))

#Create the Confusion Matrix
train_metrics <- confusionMatrix(pred_train, train$RainTomorrow,positive="Yes")
print(train_metrics)


#Predict results on test data
print("Test data results -")
pred_test <-factor(ifelse(predict(model,newdata=test,type = "response") > 0.5,"Yes","No"))

#Create the Confusion Matrix
test_metrics <- confusionMatrix(pred_test, test$RainTomorrow,positive="Yes")
print(test_metrics)




#Code snippet 5.10
library(rpart)
library(rpart.plot)

#Build the CART model with all variables
tree_model <- rpart(RainTomorrow~.,data=train)

#plot the Cost Parameter 
plotcp(tree_model)

#Plot the tree
rpart.plot(tree_model,uniform=TRUE, main="Predicting RainFall")

#Make predictions on the train data
print("Training data results -")
pred_train <- predict(tree_model,newdata = train,type = "class")
confusionMatrix(pred_train, train$RainTomorrow,positive="Yes")


#Make predictions on the test data
print("Test data results -")
pred_test <- predict(tree_model,newdata = test,type = "class")
confusionMatrix(pred_test, test$RainTomorrow,positive="Yes")



#Code snippet 5.11
library(randomForest)
#Build Random Forest Model with all independent features available
rf_model <- randomForest(RainTomorrow ~ . , data = train, 
ntree = 100,  
importance = TRUE, 
maxnodes=60)

#Evaluate on training data
print("Training data results -")
pred_train <- predict(rf_model,newdata = train,type = "class")
confusionMatrix(pred_train, train$RainTomorrow,positive="Yes")

#Evaluate on test data
print("Test data results -")
pred_test <- predict(rf_model,newdata = test,type = "class")
confusionMatrix(pred_test, test$RainTomorrow,positive="Yes")

#Plot Feature Importance
varImpPlot(rf_model)




#Code snippet 5.12
target<- "RainTomorrow"
categorical_columns <- c("RainToday","WindGustDir","WindDir9am","WindDir3pm", "new_location")
numeric_columns <- setdiff(colnames(train),c(categorical_columns,target))

#Convert factor variables to Character – This would be helpful in one-hot encoding
df_new <- df_new %>% mutate_if(is.factor, as.character)
#Use the dummyVars function from caret package to transform features into one-hot encoded form
dummies <- dummyVars(~ RainToday +  
WindGustDir + WindDir9am + 
WindDir3pm + new_location, data = df_new)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_new))

#Combine numeric and one-hot encoded variables
df_final <- cbind(df_new[,numeric_columns],df_all_ohe)
#Converting the target variable as numeric
y <- ifelse(df_new[,target] == "Yes",1,0)

#Create train and test datasets
set.seed(2019)
train_index <- sample(seq_len(nrow(df_final)),floor(0.7 * nrow(df_final)))

xgb.train <- df_final[train_index,]
y_train<- y[train_index]
xgb.test <- df_final[-train_index,]
y_test <- y[-train_index]

#Build an XGBoost model
xgb <- xgboost(data = data.matrix(xgb.train), 
               label = y_train, 
               eta = 0.01,
               max_depth = 6, 
               nround=200, 
               subsample = 1,
               colsample_bytree = 1,
               seed = 1,
               eval_metric = "logloss",
               objective = "binary:logistic",
               nthread = 4
)

#Evaluate the model on train data
print("Training data results -")
pred_train <- factor(ifelse(predict(xgb,data.matrix(xgb.train),type="class")>0.5,1,0))
confusionMatrix(pred_train,factor(y_train),positive='1')

#Evaluate the model on train data
print("Test data results -")
pred_test <- factor(ifelse(predict(xgb,data.matrix(xgb.test),type="class")>0.5,1,0))
confusionMatrix(pred_test,factor(y_test),positive='1')



#Code Snippet 5.13
#Increasing Precision at the cost of Recall for XGBoost
print("Training data results -")
pred_train <- factor(ifelse(predict(xgb,data.matrix(xgb.train),type="class")>0.53,1,0))
confusionMatrix(pred_train,factor(y_train),positive='1')

print("Test data results -")
pred_test <- factor(ifelse(predict(xgb,data.matrix(xgb.test),type="class")>0.53,1,0))
confusionMatrix(pred_test,factor(y_test),positive='1')




#Code snippet 5.14
#Scale the input dataset to 0,1 range for Deep Learning networks 
standardizer <- preProcess(x_train, method='range',rangeBounds=c(0,1))

#Use the transformations on train dataset to replicate test data
x_train_scaled <- predict(standardizer,newdata=x_train)
x_test_scaled <- predict(standardizer,newdata=x_test)

#Create a place-holder for the number of predictor variables
predictors <-  dim(x_train_scaled)[2]

#Define a Deep Neural Network Model Architecture
dl_model <-  keras_model_sequential()  %>% 
  layer_dense(units = 250, activation = 'relu', input_shape =c(predictors)) %>% 
  layer_dense(units = 250, activation = 'relu' ) %>% 
  layer_dense(units = 250, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'sigmoid') 

#Define model optimizer, loss function and metrics to evaluate
dl_model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)
summary(dl_model)

#Fit the data
history <- dl_model %>% fit(
  as.matrix(x_train_scaled), as.matrix(y_train), 
  epochs = 10, batch_size = 32, 
  validation_split = 0.2
)
#Predict on Training Data
print("Training data results - ")
pred_train <- factor(ifelse(predict(dl_model, as.matrix(x_train_scaled))>0.5,1,0))
confusionMatrix(pred_train,factor(y_train),positive='1')

#Predict on Test Data
pred_test <- factor(ifelse(predict(dl_model, as.matrix(x_test_scaled))>0.5,1,0))
confusionMatrix(pred_test,factor(y_test),positive='1')

