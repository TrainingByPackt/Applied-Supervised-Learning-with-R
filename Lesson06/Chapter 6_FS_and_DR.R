#Read the Beijing PM2.5 data set

setwd("C:\\Users\\Karthik\\OneDrive - Data Science and Analytics Consulting LLP\\Book - Packt - Supervised Learning with R\\Chapters\\Chapter 3 Introduction to Supervised Learning\\Datasets\\Beijing PM2.5")

PM25 <- read.csv("PRSA_data_2010.1.1-2014.12.31.csv")

# create datetime from year, month, day and hour
# sort the dataframe by datetime
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(grid)
library(zoo)

##Moving average

#Combine year, month and day and hour into a datetime variable

PM25$datetime <- with(PM25, ymd_h(sprintf('%04d%02d%02d%02d', year, month, day,hour)))

#Remove any row with a NA in a column
PM25_subset <- na.omit(PM25[,c("datetime","pm2.5")])

# Using zoo structure, computing the moving average every 3 hours
PM25_three_hour_pm25_avg <- rollapply(zoo(PM25_subset$pm2.5,PM25_subset$datetime), 3, mean)

# convert the output of moving average into data.frame
PM25_three_hour_pm25_avg <- as.data.frame(PM25_three_hour_pm25_avg)

# putting the timestamp in rownames into main columns
PM25_three_hour_pm25_avg$timestamp <- row.names(PM25_three_hour_pm25_avg)

# Get rid of the rownames (optional)
row.names(PM25_three_hour_pm25_avg) <- NULL

# Rename the columns
colnames(PM25_three_hour_pm25_avg) <- c("avg_pm25","timestamp")

# Create two levels based on PM2.5 average. 0 implies normal and 1 = above normal value 

PM25_three_hour_pm25_avg$pollution_level <- ifelse(PM25_three_hour_pm25_avg$avg_pm25 <= 35, 0,1)

#Randomly select 10 rows
r_index <- sample(nrow(PM25_three_hour_pm25_avg),10)

#Print
PM25_three_hour_pm25_avg[r_index,]

#Histrogram of avg_pm25
ggplot(PM25_three_hour_pm25_avg, aes(x=avg_pm25))+
  geom_histogram(binwidth = 30,color="darkblue", fill="lightblue")

# Quartiles on avg_pm25
avg_pm25 <- PM25_three_hour_pm25_avg$avg_pm25
quartiles = quantile(round(avg_pm25), seq(0,1, .25), include.lowest=F)

# Vertical Lines on quartile points
ggplot(PM25_three_hour_pm25_avg, aes(x=avg_pm25))+
  geom_histogram(binwidth = 30,color="darkblue", fill="lightblue")+
  geom_vline(xintercept=quartiles,
             color="blue", linetype="dashed", size=1)

#Adding a new variable quartile in the dataset
PM25_three_hour_pm25_avg$avg_pm25_quartiles <- as.integer(cut(avg_pm25,breaks=quantile(round(avg_pm25), seq(0,1, .25), include.lowest=T)))

#One-Hot encoding
setwd("C:\\Users\\Karthik\\OneDrive - Data Science and Analytics Consulting LLP\\Book - Packt - Supervised Learning with R\\Chapters\\Chapter 6 Feature Selection and Dimensionality Reduction\\Dataset")
OzoneData <- read.csv("ozone1.csv", stringsAsFactors=F)

library(caret)
OzoneData$Day_of_week <- as.factor(OzoneData$Day_of_week) 
OzoneData_OneHot <- dummyVars(" ~ .", data = OzoneData)

OzoneData_OneHot <- data.frame(predict(OzoneData_OneHot, newdata = OzoneData))

head(OzoneData_OneHot)

#Activity

cbwd_one_hot <- dummyVars(" ~ cbwd", data = PM25)
cbwd_one_hot <- data.frame(predict(cbwd_one_hot, newdata = PM25))

#PM25$cbwd <- NULL

PM25 <- cbind(PM25, cbwd_one_hot)


#Chi-Square

path="C:\\Program Files\\Java\\jdk1.8.0_92"

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME=path)

#install.packages("rJava")
#install.packages("FSelector")

library(rJava)
library(FSelector)#For method
library(mlbench)# For data

#Calculate the chi square statistics 
weights<- chi.squared(ozone_reading~., OzoneData)

# Print the results 
print(weights)

# Select top five variables
subset<- cutoff.k(weights, 5)

# Print the final formula that can be used in classification
f<- as.simple.formula(subset, "Class")
print(f)

#Log transformation

#Natural Log
log(10000)

# 10 times bigger value
log(100000)

# 100 times bigger value
log(1000000)

PM25_three_hour_pm25_avg$log_avg_pm25 <- log(PM25_three_hour_pm25_avg$avg_pm25)

#Histrogram of avg_pm25
ggplot(PM25_three_hour_pm25_avg, aes(x=avg_pm25))+
  geom_histogram(color="darkblue", fill="lightblue")

ggplot(PM25_three_hour_pm25_avg, aes(x=log_avg_pm25))+
  geom_histogram(color="darkblue", fill="lightblue")

# ensure the results are repeatable
OzoneData <- read.csv("ozone1.csv", stringsAsFactors=F)

set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
# calculate correlation matrix
correlationMatrix <- cor(OzoneData)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

library(corrplot)

corrplot(correlationMatrix)


#RFE using RF

library(party)

#cf1 <- cforest(pm2.5 ~ . , data= na.omit(PM25[,c("month","DEWP","TEMP","PRES","Iws","pm2.5")]), control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest

#varimp(cf1) # get variable importance, based on mean decrease in accuracy

#varimpAUC(cf1)  # more robust towards class imbalance.

library(randomForest)
pm25_model_rf <- randomForest(pm2.5 ~ . , data = na.omit(PM25[,c("month","DEWP","TEMP","PRES","Iws","pm2.5")]), ntree=25,importance=TRUE, nodesize=5)

pm25_model_rf

pm25_model_rf$importance

pm25_model_rf$rsq

varImpPlot(pm25_model_rf)

#PCA

#dataset which contains Violent Crime Rates by US State
dim(OzoneData)
colnames(OzoneData)
## [1] 50  4


#Finding means for all variables.

#finding mean of all 
apply(OzoneData,2,mean)

#Finding variance of all variables.
apply(OzoneData,2,var) 

##     Murder    Assault   UrbanPop       Rape 
##   18.97047 6945.16571  209.51878   87.72916

#There is a lot of difference in variances of each variables. In PCA mean does not play a major role, but variance plays a major role in defining principal components so very large differences in variance value of a variable will definately dominate the principal components. We need to standardize the variables so as to get mean \(\mu=0\) and variance \(\sigma^2=1\). To standardize we use formula \(x' = \frac{x - mean(x)}{sd(x)}\).

#The function prcomp() will do the needful of standardizing #the variables.

OzoneData$Day_of_week <- as.numeric(OzoneData$Day_of_week)
pca.out<-prcomp(OzoneData,scale=TRUE)

#summary of the PCA
summary(pca.out)

names(pca.out)
## [1] "sdev"     "rotation" "center"   "scale"    "x"

#Now as we can see maximum % of variance is explained by #\(PC_1\), and all PCs are mutually uncorrelated. Around 62 % #of variance is explained by \(PC_1\).

#library(devtools)
#install_github("vqv/ggbiplot", force=TRUE)

library(ggbiplot)

ggbiplot(pca.out)

#Let's build a biplot to understand better.

biplot(pca.out,scale = 0, cex=0.65)

#Variable Clustering

#install.packages("Hmisc")
library(Hmisc)
set.seed(1)

v <- varclus(as.matrix(OzoneData), similarity="spear")  # spearman is the default anyway
v    # invokes print.varclus
print(round(v$sim,2))
plot(v)

#LDA

PM25_three_hour_pm25_avg$timestamp <- as.POSIXct(PM25_three_hour_pm25_avg$timestamp, format= "%Y-%m-%d %H:%M:%S",tz="GMT")

#JOIN (merge) two data.frame on timestamp to stack other environmental variables along with PM2.5 into one data.frame
PM25_for_LDA <- merge(PM25_three_hour_pm25_avg, PM25[,c("datetime","TEMP","DEWP","PRES","Iws","cbwd","Is","Ir")], by.x = "timestamp",by.y = "datetime")

PM25_for_LDA = PM25_for_LDA[,c("TEMP","PRES","DEWP","Iws","Is","Ir","pollution_level")]

# Split dataset
index = sample(1:nrow(PM25_for_LDA), round(nrow(PM25_for_LDA)*0.6 ), replace = FALSE)
LDA_train = PM25_for_LDA[ index, ]
LDA_test = PM25_for_LDA[ -index, ]

library( MASS )
LDA_model = lda( pollution_level ~ ., data = LDA_train )

projected_data = as.matrix( LDA_train[, 1:6] ) %*%
  LDA_model$scaling

#Project some 100 randomly selected projected values
set.seed(100)
index <- sample(nrow(projected_data),100, replace = FALSE)
plot( projected_data[index], col = LDA_train[,7], pch = 19 )

# Model Testing
LDA_test_reduced = LDA_test[, !( names( LDA_test ) %in% c( "pollution_level" ) ) ]  
LDA_model_results = predict( LDA_model, LDA_test_reduced )

# Results - Confusion Matrix
library( caret )
c_t = table( LDA_model_results$class, LDA_test$pollution_level )
print( confusionMatrix( c_t ) )

# dimension reduced Dataset
new_LDA_train = as.matrix( LDA_train[,1:6] ) %*%
  LDA_model$scaling
new_LDA_train = as.data.frame( new_LDA_train )
new_LDA_train$pollution_level = LDA_train$pollution_level

#Test
new_LDA_test = as.matrix( LDA_test[,1:6] ) %*%
  LDA_model$scaling
new_LDA_test = as.data.frame( new_LDA_test )
new_LDA_test$pollution_level = LDA_test$pollution_level

#Using the projected data, lets fit a logistic model, you could any other classification model as well

PM25_logit_model_on_LDA <- glm(pollution_level ~ ., data = new_LDA_train,family=binomial(link='logit'))

#Model Evaluation on testing data

# Model Testing
predicted_LDA = predict(PM25_logit_model_on_LDA, newdata = new_LDA_test,type="response")

#Predict 1 if probability > 0.5
predicted <- ifelse(predicted_LDA>0.5, 1,0)
actual <- new_LDA_test$pollution_level

#Confusion Matrix
confusionMatrix(predicted, actual)
