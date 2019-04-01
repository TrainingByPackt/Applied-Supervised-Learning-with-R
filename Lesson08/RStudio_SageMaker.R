#load the libraries
library(reticulate)
library(readr)

#Start the sagemaker sesssion
sagemaker <- import('sagemaker')
session <- sagemaker$Session()
bucket <- session$default_bucket()
role_arn <- session$expand_role('sagemaker-service-role')

#Install the package ‘mlbench’ and load the data for our use-case.
#Setting seed for reproducability
set.seed(2019)

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)

df<- PimaIndiansDiabetes

#We will be developing a xgboost model instead of a logistic regression model on the same use-case as the previous one, therefore we need the target variable and all independent variables in a numeric type. Also, Sagemaker expects the data in a form where the first column is the target variable.

df$diabetes <- ifelse(df$diabetes == "yes",1,0)

#Place the target variable as the first column in the dataset.
df<- df[,c(9,1:8)]

#Create 70% train and 30% test datasets
train_index <- sample(seq_len(nrow(df)),floor(0.7 * nrow(df)))
train <- df[train_index,]
test <- df[-train_index,]

#Write the data into memory and upload it into an S3 bucket
write_csv(train, 'diabetes_train.csv', col_names = FALSE)
write_csv(test, 'diabetes_test.csv', col_names = FALSE)


s3_train <- session$upload_data(path = 'diabetes_train.csv',
                                bucket = bucket,
                                key_prefix = 'data')
s3_test <- session$upload_data(path = 'diabetes_test.csv',
                               bucket = bucket,
                               key_prefix = 'data')

#Define the train and test (validation data) for the sagemaker session

s3_train_input <- sagemaker$s3_input(s3_data = s3_train,
                                     content_type = 'csv')
s3_test_input <- sagemaker$s3_input(s3_data = s3_test,
                                    content_type = 'csv')

#Define the container for the estimator and the output folder in S3.
#Sagemaker provides AWS optimized pre-configured containers that can be leveraged directly for model training. We would need to choose a container based from the same region where our resources are hosted. In this case it is ‘us-east-1’.

containers <- list('us-west-2' =
'433757028032.dkr.ecr.us-west-2.amazonaws.com/xgboost:latest',
                   'us-east-1' =
'811284229777.dkr.ecr.us-east-1.amazonaws.com/xgboost:latest',
                   'us-east-2' =
 '825641698319.dkr.ecr.us-east-2.amazonaws.com/xgboost:latest',
                   'eu-west-1' =
 '685385470294.dkr.ecr.eu-west-1.amazonaws.com/xgboost:latest')

#Select the container for the estimator
container <- containers[session$boto_region_name][[1]]

#Define the output folder
s3_output <- paste0('s3://', bucket, '/output')

#Define the sagemaker estimator, the job and the input data. Here we would need to provide the type of instance that we would like to use for the model training process. We will choose ‘ml.m5.large’. You can read more about the different types of instances that can be used for the model training purpose here - https://aws.amazon.com/sagemaker/pricing/instance-types/

estimator <- sagemaker$estimator$Estimator(image_name = container,
                                           role = role_arn,
                                           train_instance_count = 1L,
                                           train_instance_type = 'ml.m5.large',
                                           train_volume_size = 30L,
                                           train_max_run = 3600L,
                                           input_mode = 'File',
                                           output_path = s3_output,
                                           output_kms_key = NULL,
                                           base_job_name = NULL,
                                           sagemaker_session = NULL)


estimator$set_hyperparameters(num_round = 100L)
job_name <- paste('sagemaker-train-xgboost', format(Sys.time(), '%H-%M-%S'),
                             sep = '-')

input_data <- list('train' = s3_train_input,
                   'validation' = s3_test_input)



#Fit the defined model.
#The model training will take a while. In the background, sagemaker will provision an instance that was defined by us in the model definition, trigger the necessary background operations to orchestrate the training process and finally train the model.

estimator$fit(inputs = input_data, job_name = job_name)

#Deploy the train model as an endpoint.
#We will have to again provide the type of instance that we would want sagemaker to provision for the model inference. Since, this is just a sample model, we can choose the instance with the lowest configuration. This process will again take some time, as sagemaker will orchestrate a series of services in the background to deploy the model as an endpoint.

model_endpoint <- estimator$deploy(initial_instance_count = 1L,
                                   instance_type = 'ml.t2.medium')

model_endpoint$content_type <- 'text/csv'
model_endpoint$serializer <- sagemaker$predictor$csv_serializer

#After the model endpoint is created, we can test the endpoint by invoking it with the right form of test data. Let us prepare a sample test data feed for a quick test.

#Make a copy of the test dataset
one_test <- test

#Delete the target variable
one_test$diabetes<-NULL

#Create a single test sample
test_sample <- as.matrix(one_test[7, ])

#Delete the column names
dimnames(test_sample)[[2]] <- NULL

#Make prediction using the model endpoint on the sample data that we created in the previous step.

#Invoke sagemaker endpoint and pass the test data
predictions <- model_endpoint$predict(test_sample)

#print result
print(ifelse(predictions>0.5,"yes",'no'))
