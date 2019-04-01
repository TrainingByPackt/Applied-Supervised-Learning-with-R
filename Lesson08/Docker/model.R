library(mlbench)
data(PimaIndiansDiabetes)
df<-PimaIndiansDiabetes

#Create the model
model<-glm(diabetes~pregnant+glucose+pressure,
           data=df,
           family=binomial(link='logit'))

#Define the endpoint as a function with the additional construct
#' @get /predict_data
function(pregnant, glucose, pressure){
  #Convert the parameters into numeric
  pregnant <- as.numeric(pregnant)
  glucose <- as.numeric(glucose)
  pressure <- as.numeric(pressure)
  #Create a dataframe with the same column names
  sample <- data.frame(pregnant=pregnant,glucose=glucose,pressure=pressure)
  #Make a prediction
  y_pred<-ifelse(predict(model,newdata=sample)>0.5,"yes","no")
  #Return the result
  list(Answer=y_pred)
}
