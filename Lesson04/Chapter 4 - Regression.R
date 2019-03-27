setwd("C:\\Users\\Karthik\\OneDrive - Data Science and Analytics Consulting LLP\\Book - Packt - Supervised Learning with R\\Chapters\\Chapter 3 - Introduction to Supervised Learning\\Datasets\\Beijing PM2.5")

#Read the Beijing PM2.5 data set
PM25 <- read.csv("PRSA_data_2010.1.1-2014.12.31.csv")

#Convert month into factor variable
PM25$month <- as.factor(PM25$month)

#Linear Model with Interaction terms of DEWP,TEMP and month
multiple_PM25_linear_model <- lm(pm2.5 ~ Iws + DEWP*TEMP*month, data = PM25)

#Model Summary
summary(multiple_PM25_linear_model)

#Diagnostics Plot
par(mfrow = c(2,2))
plot(multiple_PM25_linear_model)


# Diagnostics Plot


#Define the linear function
linear_function <- function(x){return (5+(12*x)-(3*x))}

#Define the quadratic function
quadractic_function <- function(x){return (5+(12*x)-(3*(x^2)))}


#Generate the uniform random numbers (x)
set.seed(100)
uniform_random_x <- runif(50, min=0, max=15)

#Generate the linear values (y) using (x)
linear_values_y <- linear_function(uniform_random_x) + rnorm(50,mean = 0, sd =sqrt(2))

#Generate the quadrtic values (y) using (x)
quadratic_values_y <- quadractic_function(uniform_random_x) + rnorm(50,mean = 0, sd =sqrt(2))

df <- data.frame(linear_values_y, quadratic_values_y, uniform_random_x)

#lm model fit for linear_values_y using uniform_random_x

model_df_linear <- lm(linear_values_y ~ uniform_random_x, data = df)

plot(df$linear_values_y,model_df_linear$fitted.values)

#Diagnostic plot for linear relationship
par(mfrow = c(2,2))
plot(model_df_linear)

#lm model fit for quadratic_values_y using uniform_random_x

model_df_quad <- lm(quadratic_values_y ~ uniform_random_x, data = df)

#Diagnostic plot for non-linear relationship
par(mfrow = c(2,2))
plot(model_df_quad)

#install.packages("quantreg") 
library(quantreg)

#25th Quantile
quantile_regression_PM25_025 <- rq(pm2.5 ~ DEWP+TEMP+Iws, data = PM25, tau = 0.25)

summary(quantile_regression_PM25_025)

#Different Quantile
quantile_regression_PM25_all <- rq(pm2.5 ~ DEWP+TEMP+Iws, data = PM25, tau = seq(0.25,0.99,by = 0.25))

summary(quantile_regression_PM25_all)


#Different Quantile - More granular
quantile_regression_PM25_granular <- rq(pm2.5 ~ DEWP+TEMP+Iws, data = PM25, tau = seq(0.05,0.95,by = 0.05))

plot_granular <- summary(quantile_regression_PM25_granular)

plot(plot_granular)

#Generate random numbers
quadractic_function <- function(x){return (5+(12*x)-(3*(x^2)))}

uniform_random_x <- runif(50, min=0, max=15)

quadratic_values_y <- quadractic_function(uniform_random_x) + rnorm(50,mean = 0, sd =sqrt(2))

df <- data.frame(quadratic_values_y,uniform_random_x)

library(ggplot2)
ggplot(df, aes(x=uniform_random_x,y=quadratic_values_y))+
  geom_point()

par(mfrow = c(2,2))
plot(lm(quadratic_values_y~uniform_random_x,data=df))

#Polynomial Regression

#Convert month into factor variable
PM25$month <- as.factor(PM25$month)

#Polynomial Regression On Beijing pm2.5 with quadratic and cubic term
multiple_PM25_poly_model <- lm(pm2.5 ~ DEWP^2 + TEMP + Iws + DEWP*TEMP*month, data = PM25)

#Model Summary
summary(multiple_PM25_poly_model)

#Diagnostics Plot
par(mfrow = c(2,2))
plot(multiple_PM25_poly_model)

#Linear Regression

multiple_PM25_linear_model$coefficients

#Ridge regression

library(glmnet)
PM25 <- na.omit(PM25)
X <- as.matrix(PM25[,c("DEWP","TEMP","Iws")])
Y <- PM25$pm2.5

set.seed(100) #Setting the seed to get similar results.
model_ridge = cv.glmnet(X,Y,alpha = 0,lambda = 10^seq(4,-1,-0.1))

#Optimal value of lambda after cross validation
optimal_lambda <- model_ridge$lambda.min

#Coefficient values from the model fit
ridge_coefficients <- predict(model_ridge, s = optimal_lambda, type = "coefficients")

ridge_coefficients

ridge_prediction <- predict(model_ridge, s = optimal_lambda, newx = X)

#LASSO Regression

set.seed(100) #Setting the seed to get similar results.
model_LASSO = cv.glmnet(X,Y,alpha = 1,lambda = 10^seq(4,-1,-0.1))

#Optimal value of lambda after cross validation
optimal_lambda_LASSO <- model_LASSO$lambda.min

#Coefficient values from the model fit
LASSO_coefficients <- predict(model_LASSO, s = optimal_lambda_LASSO, type = "coefficients")

LASSO_coefficients

#Prediction from the model
LASSO_prediction <- predict(model_LASSO, s = optimal_lambda_LASSO, newx = X)

#Elastic Net Regression

set.seed(100) #Setting the seed to get similar results.
model_elanet = cv.glmnet(X,Y,alpha = 0.5,lambda = 10^seq(4,-1,-0.1))

#Optimal value of lambda after cross validation
optimal_lambda_elanet <- model_LASSO$lambda.min

#Coefficient values from the model fit
elanet_coefficients <- predict(model_elanet, s = optimal_lambda_elanet, type = "coefficients")

elanet_coefficients

#Prediction from the model
elanet_prediction <- predict(model_elanet, s = optimal_lambda_elanet, newx = X)

#Comparision
multiple_PM25_linear_model <- lm(pm2.5 ~ Iws + DEWP + TEMP, data = PM25)

#Residual Standard Error(RSE) of linear regression
sqrt(sum(multiple_PM25_linear_model$residuals^2)/41753)

#Residual Standard Error(RSE) of ridge regression
sqrt(sum((Y-ridge_prediction)^2)/41753)

#Residual Standard Error(RSE) of LASSO regression
sqrt(sum((Y-LASSO_prediction)^2)/41753)

#Residual Standard Error(RSE) of Elastic Net regression
sqrt(sum((Y-elanet_prediction)^2)/41753)

#Poisson REgression

setwd("C:\\Users\\Karthik\\OneDrive - Data Science and Analytics Consulting LLP\\Book - Packt - Supervised Learning with R\\Chapters\\Chapter 4 - Regression\\Dataset")

library(foreign)

df_health <- read.dta("health.dta")

poisson_regression_health <- glm(NONDOCCO ~ ., data = df_health, family=poisson(link=log))

summary(poisson_regression_health)

#Dispersion
summary.glm(poisson_regression_health)$dispersion

library(ggplot2)

df_pred_actual <- data.frame(cbind(df_health$NONDOCCO,poisson_regression_health$fitted.values))

colnames(df_pred_actual) <- c("actual_NONDOCCO","predicted_NONDOCCO")

ggplot(df_pred_actual, aes(x=actual_NONDOCCO, y =predicted_NONDOCCO))+
  geom_point()

#Sample Mean
s_mean <- mean(df_health$NONDOCCO)
s_mean

#Sample Variance
s_variance <- var(df_health$NONDOCCO)
s_variance

#Overdispersion
s_variance/s_mean

#Cox-Regression
library(survival)

#Lung Cancer Data
df_lung_cancer <- lung

#Head
head(df_lung_cancer)

# Lung Cancer Data where status 2 represents death
df_lung_cancer$SurvObject <- with(df_lung_cancer, Surv(time, status == 2))

#Summary
summary(df_lung_cancer)

#Cox Proportional Hazards Regression Model
cox_regression <- coxph(SurvObject ~ age + sex + ph.karno + wt.loss, data =  df_lung_cancer)

cox_regression