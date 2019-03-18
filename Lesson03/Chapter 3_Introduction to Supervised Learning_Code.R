#Chapter 3 - Introduction to Supervised Learning

#Read the Beijing PM2.5 data set
PM25 <- read.csv("PRSA_data_2010.1.1-2014.12.31.csv")


#Structure of data with sample values
str(PM25)

#Summary Statistics
summary(PM25)


###transform date and time info into datetime 

plot_pm25 <- PM25 %>%
  select(datetime, pm2.5) %>%
  na.omit() %>%
  ggplot() + 
  geom_point(aes(x = datetime, y = pm2.5), size = 0.5, alpha = 0.75) +
  ylab("PM2.5")

plot_TEMP <- PM25 %>%
  select(datetime, TEMP) %>%
  na.omit() %>%
  ggplot() + 
  geom_point(aes(x = datetime, y = TEMP), size = 0.5, alpha = 0.75) +
  ylab("TEMP")

plot_DEWP <- PM25 %>%
  select(datetime, DEWP) %>%
  na.omit() %>%
  ggplot() + 
  geom_point(aes(x = datetime, y = DEWP), size = 0.5, alpha = 0.75) +
  ylab("DEWP")

plot_PRES <- PM25 %>%
  select(datetime, PRES) %>%
  na.omit() %>%
  ggplot() + 
  geom_point(aes(x = datetime, y = PRES), size = 0.5, alpha = 0.75) +
  ylab("PRES")


grid.newpage()
grid.draw(rbind(ggplotGrob(plot_pm25), ggplotGrob(plot_TEMP),ggplotGrob(plot_DEWP),ggplotGrob(plot_PRES), size = "last"))

###
PM25 <- PM25 %>%
  mutate(date = make_date(year, month, day),
         datetime = make_datetime(year, month, day, hour)) %>%
  arrange(datetime)

df_PM25 <- PM25 %>%
  select(date,DEWP,TEMP) %>%
  #group_by(year, month, hour) %>%
  #summarise(hourly_avg_pm25 = mean(pm2.5, na.rm = TRUE))
  gather(key = "variable", value = "value", -date)

library(ggplot2)
ggplot(df_PM25, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

#ggplot(data = PM25) +
#  geom_point(mapping = aes(x = month, y = pm2.5, 
#                           color = year))


#Visual Inspection of relationship between variables

#Correlation Analysis

library(corrplot)

corr <- cor(PM25[!is.na(PM25$pm2.5),c("pm2.5","DEWP","TEMP","PRES","Iws","Is","Ir")])

corrplot(corr)

#Scatterplots to explore relationship between PM2.5 levels and other factors

library(ggplot2)

#scatterplot between DEWP and Pm2.5 with month used for color

# PM2.5 Vs DEWP
ggplot(data = PM25, aes(x = DEWP, y = pm2.5, color = month)) +
  geom_point() +
  geom_smooth(method='auto',formula=y~x, colour = "red", size =1.5)

# PM2.5 Vs TEMP
ggplot(data = PM25, aes(x = TEMP, y = pm2.5, color = month)) +
  geom_point() +
  geom_smooth(method='auto',formula=y~x, colour = "red", size =1.5)

#scatterplot between DEWP and Pm2.5 with hour of day used for color and separate view by month of the year

# PM2.5 Vs DEWP
ggplot(data = PM25, aes(x = DEWP, y = pm2.5, color = hour)) +
  geom_point() +
  geom_smooth(method='auto',formula=y~x, colour = "red", size =1) +
  facet_wrap(~ month, nrow = 4)

# PM2.5 Vs TEMP
ggplot(data = PM25, aes(x = TEMP, y = pm2.5, color = hour)) +
  geom_point() +
  geom_smooth(method='auto',formula=y~x, colour = "red", size =1) +
  facet_wrap(~ month, nrow = 4)

# Simple Regression Model

simple_PM25_linear_model <- lm(pm2.5 ~ DEWP, data = PM25)

summary(simple_PM25_linear_model)
# Multiple Linear Regression Model

multiple_PM25_linear_model <- lm(pm2.5 ~ DEWP+TEMP+Iws, data = PM25)

summary(multiple_PM25_linear_model)

# Logistic Regression Model

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

# Convert the character values into POSIXct for consistency
PM25_three_hour_pm25_avg$timestamp <- as.POSIXct(PM25_three_hour_pm25_avg$timestamp, format= "%Y-%m-%d %H:%M:%S",tz="GMT")

#JOIN (merge) two data.frame on timestamp to stack other environmental variables along with PM2.5 into one data.frame
PM25_for_class <- merge(PM25_three_hour_pm25_avg, PM25[,c("datetime","TEMP","DEWP","PRES","Iws","cbwd","Is","Ir")], by.x = "timestamp",by.y = "datetime")

# Build a logistic regression model with DEWP, TEMP and Iws variables
PM25_logit_model <- glm(pollution_level ~ DEWP + TEMP + Iws, data = PM25_for_class,family=binomial(link='logit'))

#Model Summary

summary(PM25_logit_model)

#Model Evaluation on training data

predicted <- ifelse(PM25_logit_model$fitted.values>0.5, 1,0)
actual <- PM25_for_class$pollution_level

library(caret)

confusionMatrix(predicted, actual)

#ROC Curve

library(ROCR)
pred1 <- prediction(predict(PM25_logit_model), PM25_for_class$pollution_level)
perf1 <- performance(pred1,"tpr","fpr")

auc <- performance(pred1,"auc")
as.numeric(auc@y.values)

plot(perf1)


####### Evaluation #####


y_predicted <- predict(multiple_PM25_linear_model, data = PM25)
y_actual <- PM25[!is.na(PM25$pm2.5),"pm2.5"]

#Mean Absolute Error (MAE)

MAE <- mean(abs(y_actual - y_predicted))

#Mean Square Error (MAE)

MSE <- mean((y_actual - y_predicted)^2)

#Root Mean Square Error (RMSE)

RMSE <- sqrt(mean((y_actual - y_predicted)^2))

#Rsquare

model_summary <- summary(multiple_PM25_linear_model)

model_summary$r.squared
model_summary$adj.r.squared

#Mean Reciprocal Rank (MRR)
Query_RR_Vector <- c(1/3,1/4,1)

MRR <- sum(Query_RR_Vector)/length(Query_RR_Vector)

```