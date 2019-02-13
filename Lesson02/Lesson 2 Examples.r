
library(dplyr)
library(ggplot2)
library(repr)
library(cowplot)
options(repr.plot.width=12, repr.plot.height=4)

#Code snippet 2.1
df <- read.csv("/Users/jmoolay/Personal/Packt/Supervised Learning with R/Chapters/Chapter 2/Data/bank-additional/bank-additional-full.csv",sep=';')
str(df)


#Code snippet 2.2
library(ggplot2)
print(summary(df$age))
print(paste("Std.Dev:",round(sd(df$age),2)))
ggplot(data=df,aes(y=age)) + geom_boxplot(outlier.colour="black")


#Code snippet 2.3
ggplot(data=df,aes(x=age)) + 
    geom_histogram(bins=10,fill="blue",color="black", alpha =0.5)  + 
    ggtitle("Histogram for Age") + theme_bw()  


#Code snippet 2.4
ggplot(data=df,aes(x=age)) + geom_density(fill="red",alpha =0.5) + 
                             ggtitle("Density Plot for Age") + 
                             theme_bw()


#Code snippet 2.5
#Automate plotting numeric variables

library(cowplot)
#Define a function to plot histograms for all numeric columns 
plot_grid_numeric <- function(df,list_of_variables,ncols=2){
    plt_matrix<-list()
    i<-1
    for(column in list_of_variables){
        plt_matrix[[i]]<-ggplot(data=df,aes_string(x=column)) + 
            geom_histogram(binwidth=2,fill="blue",color="black", alpha =0.5)  +
            ggtitle(paste("Histogram for varaible:",column)) + theme_bw()
            i<-i+1
            }
    plot_grid(plotlist=plt_matrix,ncol=2)
}
summary(df[,c("campaign","pdays","previous","emp.var.rate")])

#Call the defined function to plot the hisitogram
plot_grid_numeric(df,c("campaign","pdays","previous","emp.var.rate"),2)


#Code snippet 2.6
#Study the next set of numeric variables
summary(df[,c("nr.employed","euribor3m","cons.conf.idx","duration")])
plot_grid_numeric(df,c("nr.employed","euribor3m","cons.conf.idx","duration"),2)


#Code snippet 2.7
#Use the dplyr library function to perform grouped data aggregation
marital_distribution <- df %>% group_by(marital) %>% summarize(Count = n()) %>% 
                                mutate(Perc.Count = round(Count/sum(Count)*100))
print(marital_distribution)


#Code snippet 2.8
ggplot(data = marital_distribution,aes(x=marital,y=Perc.Count)) + 
                        geom_bar(stat="identity",fill="blue",alpha=0.6) + 
                        geom_text(aes(label=marital_distribution$Perc.Count, vjust = -0.3))


#Code snippet 2.9
#Define label positions
plot_breaks = 100 - (cumsum(marital_distribution$Perc.Count)  - marital_distribution$Perc.Count/2)

#Define labels for the plots
plot_labels <- paste0(marital_distribution$marital,"-",marital_distribution$Perc.Count,"%")

#Setting plot size for better visuals
options(repr.plot.width=12, repr.plot.height=8)

#Creating the pie chart
ggplot(data = marital_distribution,aes(x=1,y=Perc.Count, fill=marital)) + 
              geom_bar(stat="identity") + #Creates the base bar visual
               coord_polar(theta ="y")  + #Creates the pie chart
               scale_y_continuous(breaks=plot_breaks, labels = plot_labels,position = "left") + 
               theme(axis.text.x = element_text(angle = 30, hjust =1)) + #rotates the labels
               theme(text = element_text(size=15)) + #increases the font size for the legend
               ggtitle("Percentage Distribution of Marital Status") #Adds the plot title



#Code snippet 2.10
#Automate plotting categorical variables
library(cowplot)

#Define a function to plot histograms for all numeric columns 
plot_grid_categorical <- function(df,list_of_variables,ncols=2){
    plt_matrix <- list()
    i<-1
    #Iterate for each variable
    for(column in list_of_variables){
       #Creating a temporary dataframe with the aggregation
        var.dist <- df %>% group_by_(column) %>% 
                                       summarize(Count = n()) %>% 
                                       mutate(Perc.Count = round(Count/sum(Count)*100,1))

        options(repr.plot.width=12, repr.plot.height=10)

        plt_matrix[[i]]<-ggplot(data = var.dist,aes_string(x=column,y="Perc.Count")) +
            geom_bar(stat="identity",fill="blue",alpha=0.6) + #Defines the bar plot
            geom_text(label=var.dist$Perc.Count,vjust=-0.3)+  #Adds the labels
            theme(axis.text.x = element_text(angle = 90, vjust = 1)) + #rotates the labels
            ggtitle(paste("Percentage Distribution of variable:",column))  #Creates the title +

            i<-i+1
        }

        plot_grid(plotlist=plt_matrix,ncol=ncols) #plots the grid
    
}

#call summary statistics
summary(df[,c("job","education","default","contact")])

#Call the defined function to plot the hisitogram
plot_grid_categorical(df,c("job","education","default","contact"),2)



#Code snippet 2.11
#call summary statistics
summary(df[,c("loan","month","day_of_week","poutcome")])
#Call the defined function to plot the hisitogram
plot_grid_categorical(df,c("loan","month","day_of_week","poutcome"),2)


#Code snippet 2.12
#call summary statistics
summary(df[,c("y","housing")])

#Call the defined function to plot the hisitogram
plot_grid_categorical(df,c("y","housing"),2)


#Code snippet 2.13
#Scatter plot
options(repr.plot.width=8, repr.plot.height=3)
ggplot(data=df,aes(x=emp.var.rate,y=nr.employed)) + geom_point(size=4) + 
ggtitle("Scatterplot of Employment variation rate v/s Number of Employees")



#Code snippet 2.14
temp <- df %>% group_by(y) %>% summarise(Avg.Age = round(mean(age),2),
                                       Num.Records = n())
print(temp)
options(repr.plot.width=8, repr.plot.height=3)

ggplot(data= temp, aes(x=y, y=Avg.Age)) + 
        geom_bar(stat="identity",fill="blue",alpha= 0.5) +   #Creates the bar plot
        geom_text(label=temp$Avg.Age,vjust=-0.3)+  #Adds the label
        ggtitle(paste("Average.Age across target outcome"))  #Creates the title



#Code snippet 2.15
plot_bivariate_numeric_and_categorical <- function(df,target,list_of_variables,ncols=2){
    target<-sym(target) #Defined for converting text to column names
    plt_matrix <- list()
    i<-1
    
for(column in list_of_variables){
        col <-sym(column) #defined for converting text to column name

        temp <- df %>% group_by(!!sym(target)) %>% 
                       summarise(Avg.Val = round(mean(!!sym(col)),2))
        options(repr.plot.width=12, repr.plot.height=8) #Defines plot size
            plt_matrix[[i]]<-ggplot(data= temp, aes(x=!!sym(target), y=Avg.Val)) + 
            geom_bar(stat="identity",fill="blue",alpha= 0.5) +   
            geom_text(label=temp$Avg.Val,vjust=-0.3)+  #Adds the labels
            ggtitle(paste("Average",column,"across target outcomes"))  #Creates the title 
    

            i<-i+1
    }
    plot_grid(plotlist = plt_matrix,ncol=ncols)

}
print("Distribution of records across target outcomes-")
print(table(df$y))
plot_bivariate_numeric_and_categorical(df,"y",c("campaign","pdays","previous","emp.var.rate"),2)



#Code snippet 2.16
plot_bivariate_numeric_and_categorical(df,"y",
c("cons.price.idx","cons.conf.idx", "euribor3m", "nr.employed"),2)


#Code snippet 2.17
#Create a temp aggregation dataset
temp <- df %>% group_by(y,marital) %>% summarise(Count = n()) 

#Define plot size
options(repr.plot.width=12, repr.plot.height=4)

#Plot teh chart with frequency distribution
ggplot(data = temp,aes(x=marital,y=Count,fill=y)) + 
    geom_bar(stat="identity") + 
    ggtitle("Distribution of target 'y' across Marital Status")



#Code snippet 2.18
#create a temp aggregation dataset
temp <- df %>% group_by(y,marital) %>% 
            summarise(Count = n()) %>% 
            ungroup() %>% 	#This function ungroups the previously grouped dataframe
            group_by(marital) %>%
            mutate(Perc = round(Count/sum(Count)*100)) %>%
            arrange(marital)
#Define the plot size
options(repr.plot.width=12, repr.plot.height=4)

#Plot the percentage distribution 
ggplot(data = temp,aes(x=marital,y=Perc,fill=y)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(label = Perc), size = 5, hjust = 0.5, vjust = 0.3, position = "stack") + 
    ggtitle("Distribution of target 'y' percentage across Marital Status")



#Code snippet 2.19
#create a temp aggregation dataset
plot_bivariate_categorical <-  function(df, target, list_of_variables){
    target <- sym(target) #Converting the string to a column reference
    i <-1 
    plt_matrix <- list()
    for(column in list_of_variables){
        col <- sym(column) 
        temp <- df %>% group_by(!!sym(target),!!sym(col)) %>% 
            summarise(Count = n()) %>% 
            ungroup() %>% #This fucntion ungroups the previously grouped dataframe
            group_by(!!sym(col)) %>%
            mutate(Perc = round(Count/sum(Count)*100)) %>%
            arrange(!!sym(col))
    #Define the plot size
    options(repr.plot.width=14, repr.plot.height=12)
        
    #Plot the chart with frequency distribution
    plt_matrix[[i]]<- ggplot(data = temp,aes(x=!!sym(col),y=Count,fill=!!sym(target))) + 
        geom_bar(stat="identity") + 
        geom_text(aes(label = Count), size = 3, hjust = 0.5, vjust = -0.3, position = "stack") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 1)) + #rotates the labels
        ggtitle(paste("Distribution of target 'y'  frequency across",column))
    i<-i+1

    #Plot the percentage distribution 
    plt_matrix[[i]] <- ggplot(data = temp,aes(x=!!sym(col),y=Perc,fill=!!sym(target))) + 
            geom_bar(stat="identity") + 
            geom_text(aes(label = Perc), size = 3, hjust = 0.5, vjust = -1, position = "stack") + 
            theme(axis.text.x = element_text(angle = 90, vjust = 1)) + #rotates the labels
            ggtitle(paste("Distribution of target 'y' percentage across",column))
    i <- i+1
        
    }
    plot_grid(plotlist = plt_matrix, ncol=2)
}
plot_bivariate_categorical(df,"y",c("job","education"))



#Code snippet 2.21
plot_bivariate_categorical(df,"y",c("loan","contact"))


#Code snippet 2.22
#Hypothesis testing for categorical dependent variable and continuous independent variable
#Convert the dependent variable into a factor type
df$y <- factor(df$y)

#Perform Logistic regression
h.test <- glm(y ~ emp.var.rate, data = df, family = "binomial")

#Print test summary
summary(h.test)


#Code snippet 2.23
#Hypothesis testing for categorical dependent variable and continuous independent variable

#Perform Logistic regression
h.test2 <- glm(y ~ euribor3m, data = df, family = "binomial")

#Print test summary
summary(h.test2)


#Code snippet 2.24
#Hypothesis testing for categorical dependent variable and categorical independent variable

#Create a flag for "Single" clients
df$single_flag <- as.factor(ifelse(df$marital == "single","single","other"))
sample <- table(df$y, df$single_flag)
print(sample)

#Perform chi squared test
h.test3 <- chisq.test(sample)

#Print test summary
print(h.test3)



#Code snippet 2.25
#Hypothesis #4 and #5
#Prepare the independent variable
df$job_flag <- as.factor(ifelse(df$job %in% c("student","retired"),as.character(df$job),"other"))
df$contact <- as.factor(df$contact)


sample4 <- table(df$y, df$job_flag)
print("Frequency table for Job")
print(sample4)

#Perform test for hypothesis #4
h.test4 <- chisq.test(sample4)

#Print test summary for hypothesis #4
print("Hypothesis #4 results")
print(h.test4)

print("Frequency table for Contact")
sample5 <- table(df$y, df$contact)
print(sample5)

#Perform test
h.test5 <- chisq.test(sample5)

#Print test summary for hypothesis #5
print("Hypothesis #5 results")
print(h.test5)


