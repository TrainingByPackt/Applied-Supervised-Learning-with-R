## Exercise 1: Download a zip file and unzip

#2.	Set the working directory
wd <- "<WORKING DIRECTORY>"
setwd(wd)

#3.	Download the zip file containing the datasets using download.file() method

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip"

destinationFileName <- "bank.zip"

download.file(url, destinationFileName,method = "auto", quiet=FALSE)

#4.	Unzip the file in the working directory using unzip() method

#Choose a file and save its file path in R (for windows) or otherwise specify the complete path
zipFile<-file.choose()

# Define the folder where the zip file is unzipped
outputDir <- wd

# unzip the file
unzip(zipFile,exdir=outputDir)


## Exercise 2: Read a CSV file and summarize its column

#1.	Using read.csv, load the bank-full.csv into a data.frame

df_bank_detail <- read.csv("bank-full.csv", sep = ';')

#2.	Print the summary of the data.frame


## Exercise 3: Reading a JSON file and storing the data in data.frame

library(jsonlite)
json_file <- "crop.json"
json_data <- jsonlite::fromJSON(json_file, flatten = TRUE)

#Second element in the list contains the data frame with crop production value
crop_production <- data.frame(json_data[[2]])

#Renaming the columns
colnames(crop_production) <- c("S.No","District","Area","Production","PTY")

#Print top 6 rows
head(crop_production)

## Exercise 4: Reading a CSV file with TEXT column and storing the data in VCorpus

library(tm)
#Read top 10 reviews
review_top_10 <- read.csv("Reviews_Only_Top_10_Records.csv")

#store the text column in VCorpus
review_corpus <- VCorpus(VectorSource(review_top_10$Text))

#Inspect the structure of first two reviews
inspect(review_corpus[1:2])

#Using lapply cast the first review as character and print
lapply(review_corpus[1:2], as.character)

## Data Structures in R

#Vector
colnames(crop_production)<- c("S.No","District","Area","Production","PTY")

#Subset the value in the second index
c_names[2]

# Matrix

# Generate 16 random numbers drawn from a Binomial distribution with parameter size = 100 and probability of success = 0.4

r_numbers <- rbinom(n = 16, size = 100, prob = 0.4)

#Store it in a matrix
matrix(r_numbers, nrow = 4, ncol = 4)

#Using lapply cast the first review as character and print
lapply(review_corpus[1:2], as.character)

# Convert characters to lower case
top_2_reviews <- review_corpus[1:2]
top_2_reviews <- tm_map(top_2_reviews,content_transformer(tolower))
lapply(top_2_reviews[1], as.character)

#Remove stopwords like a, the, an, etc.

top_2_reviews <- tm_map(top_2_reviews,removeWords, stopwords("english"))
lapply(top_2_reviews[1], as.character)

#Remove extra white spaces between words
top_2_reviews <- tm_map(top_2_reviews,stripWhitespace)
lapply(top_2_reviews[1], as.character)

Stemming
top_2_reviews <- tm_map(top_2_reviews,stemDocument)
lapply(top_2_reviews[1], as.character)

# Document Term Matrix
dtm <- DocumentTermMatrix(top_2_reviews)
inspect(dtm)
# Store the results in a matrix
dtm_matrix <- as.matrix(dtm)

# Dimension of the matrix - 2 documents and 37 words
dim(dtm_matrix)

# Count of the word "product" in document 1
dtm_matrix[1,"product"]

# List

# Generate 16 random numbers drawn from a Binomial distribution with parameter size = 100 and probability of success = 0.4
r_numbers <- rbinom(n = 16, size = 100, prob = 0.4)

# Pick 16 alphabets from English LETTERS without repetitions
r_characters <- sample(LETTERS, 16, FALSE)

#Put r_numbers and r_characters into a single list
list(r_numbers, r_characters)

# Store and retrieve from a list
r_list <- list(r_numbers, r_characters)

# Retrieve values in the character vector
r_list[[2]]

# Retrieve third value in the character vector
(r_list[[2]])[1]

# Generate 16 random numbers drawn from a Binomial distribution with parameter size = 100 and probability of success = 0.4
r_numbers <- rbinom(n = 16, size = 100, prob = 0.4)

# Pick 18 alphabets from English LETTERS without repetitions
r_characters <- sample(LETTERS, 18, FALSE)

# Put r_numbers and r_characters into a single list
list(r_numbers, r_characters)

#Data Frame

# Generate 16 random numbers drawn from a Binomial distribution with parameter size = 100 and probability of success = 0.4

r_numbers <- rbinom(n = 16, size = 100, prob = 0.4)

# Pick 18 alphabets from English LETTERS without repetitions

r_characters <- sample(LETTERS, 18, FALSE)

# Put r_numbers and r_characters into a single data.frame

data.frame(r_numbers, r_characters)


#Data.table

library(data.table)

# Read using fread() method of data.table package
system.time(fread("Reviews_Full.csv"))

# Read using read.csv() of base package
system.time(read.csv("Reviews_Full.csv"))

## Data Processing and Transformation 

#cbind
# Generate 16 random numbers drawn from a Binomial distribution with parameter size = 100 and probability of success = 0.4

r_numbers <- rbinom(n = 16, size = 100, prob = 0.4)

# Print r_numbers
r_numbers

# Pick 16 alphabets from English LETTERS without repetitions

r_characters <- sample(LETTERS, 18, FALSE)

# Print r_characters
r_characters

cbind(r_numbers, r_characters)

#rbind

rbind(r_numbers, r_characters)

#merge

set.seed(100)

# generating 16 random number between 1:30 without replacement
r_numbers <- sample(1:30,10, replace = FALSE)

# generating 16 characters from English alphabet with replacement
r_characters <- sample(LETTERS, 10, TRUE)

df_one <- cbind(as.data.frame(r_numbers), as.data.frame(r_characters))
df_one

# set.seed for preserving the same random numbers over multiple runs
set.seed(200)

# generating 16 random number between 1:30 without replacement
r_numbers <- sample(1:30,10, replace = FALSE)

# generating 16 characters from English alphabet with replacement
r_characters <- sample(LETTERS, 10, TRUE)

df_two <- cbind(as.data.frame(r_numbers), as.data.frame(r_characters))
df_two

#Inner JOIN
merge(df_one, df_two, by = "r_numbers")

#Left JOIN
merge(df_one, df_two, by = "r_numbers", all.x = TRUE)

#Right JOIN
merge(df_one, df_two, by = "r_numbers", all.y = TRUE)

#Full JOIN
merge(df_one, df_two, by = "r_numbers", all = TRUE)

#reshape

# Top 5 rows of the dataset
head(iris)

# Create a variable called Type based on the following condition
iris$Type <- ifelse((iris$Sepal.Width>2 & iris$Sepal.Width <=3),"TYPE 1","TYPE 2")

# Seperately take only the columns Type, Sepal.Width and Species
df_iris <- iris[,c("Type","Sepal.Width","Species")]

# Reshape the df_iris into wide data frame
reshape(df_iris,idvar = "Species", timevar = "Type", direction = "wide")

#aggregate

aggregate(formula =. ~ Species, data = iris, FUN = mean)

#apply family of functions

#apply

# Create a 100 x 100 matrix of random letters
r_characters <- matrix(sample(LETTERS, 10000, replace = TRUE), ncol = 100, nrow = 100)

# function to count the number of vowels in a given array
c_vowel <- function(x_char){
  return(sum(x_char %in% c("A","I","O","U")))
}
# apply function to run through each column of the matrix and apply the c_vowel function
apply(r_characters, MARGIN = 2, c_vowel)

#lapply

# Create two list of 100 of random letters of each size 100
r_characters <- list(a=sample(LETTERS, 100, replace = TRUE),
                     b=sample(LETTERS, 100, replace = TRUE))

# lapply function to run through on list and b and apply the c_vowel function to count the vowels
lapply(r_characters, c_vowel)

# Check the class of the output
out_list <- lapply(r_characters, c_vowel)
class(out_list)

#sapply

sapply(r_characters, c_vowel)

# Check the class of the output
out_vector <- sapply(r_characters, c_vowel)
class(out_vector)

#tapply

# calculate standard deviation of Sepal length for each iris species
tapply(iris$Sepal.Length, iris$Species,sd)


# calculate standard deviation of Sepal Width for each iris species
tapply(iris$Sepal.Width, iris$Species,sd)


## Useful Packages

#a.	Interesting in knowing the average bank balance of blue-collar jobs by their marital status
df_bank_detail <- read.csv("bank-full.csv", sep = ';')

library(dplyr)

df_bank_detail %>%
  filter(job == "blue-collar") %>%
  group_by(marital) %>%
  summarise(
    cnt = n(),
    average = mean(balance, na.rm = TRUE)
  )

#b.	Interesting in knowing bank balance of customers with secondary education and default as "yes"
df_bank_detail %>%
  mutate(sec_edu_and_default = ifelse((education == "secondary" & default == "yes"), "yes","no")) %>%
  select(age, job, marital,balance, sec_edu_and_default) %>%
  filter(sec_edu_and_default == "yes") %>%
  group_by(marital) %>%
  summarise(
    cnt = n(),
    average = mean(balance, na.rm = TRUE)
  )

#tidyr

library(tidyr)

set.seed(100)

# 5 person names
r_name <- c("John", "Jenny", "Michael", "Dona", "Alex")

# generating 16 random number between 1:30 without replacement
r_food_A <- sample(1:150,5, replace = FALSE)

# generating 16 random number between 1:30 without replacement
r_food_B <- sample(1:150,5, replace = FALSE)

#Create data frame
df_untidy <- data.frame(r_name, r_food_A, r_food_B)
df_untidy

# Using gather() method from tidyr
df_long <- df_untidy %>%
  gather(food, calories, r_food_A:r_food_B)
df_long

#spread() works the other way around of gather(), it takes a "long" format and convert it into wide format.

df_long %>%
  spread(food,calories)

#separate() is useful in places where columns are combination of values for making it a key column of for other purpose. We can separate out the key if it has a common separator character.

key <- c("John.r_food_A", "Jenny.r_food_A", "Michael.r_food_A", "Dona.r_food_A", "Alex.r_food_A", "John.r_food_B", "Jenny.r_food_B", "Michael.r_food_B", "Dona.r_food_B", "Alex.r_food_B")

calories <- c(74, 139, 52, 141, 102, 134, 27, 94, 146, 20)

df_large_key <- data.frame(key,calories)  
df_large_key

df_large_key %>%
  separate(key, into = c("name","food"), sep = "\\.")

#plyr

library(plyr)

# A slight tweaked version of c_vowel function we created in earlier example
c_vowel <- function(x_char){
  return(sum(as.character(x_char[,"b"]) %in% c("A","I","O","U")))
}

set.seed(101)
r_characters <- data.frame(a=rep(c("Split_1","Split_2","Split_3"),1000),
                           b= sample(LETTERS, 3000, replace = TRUE))

#a.	Input = data.frame to output = list


dlply(r_characters,.(a), c_vowel)

#b.	Input = data.frame to output = array

daply(r_characters,.(a), c_vowel)

#c.	Input = data.frame to output = data.frame

ddply(r_characters,.(a), c_vowel)

#Data Visualization

#Scatterplot

df_bank_detail <- read.csv("bank-full.csv", sep = ';')

ggplot(data = df_bank_detail) +
  geom_point(mapping = aes(x = age, y = balance, color = job))

#Exercise 5: Draw three scatter plot between age and balance each for single, divorced and married

ggplot(data = df_bank_detail) +
  geom_point(mapping = aes(x = age, y = balance, color = job)) +
  facet_wrap(~ marital, nrow = 1)

#Line chart

ggplot(data = df_bank_detail) +
  geom_smooth(mapping = aes(x = age, y = balance, linetype = marital))

# Histogram
ggplot(data = df_bank_detail) +
  geom_bar(mapping = aes(x=job, fill = y)) +
  theme(axis.text.x = element_text(angle=90, vjust=.8, hjust=0.8))

# Boxplot

tapply(df_bank_detail$age, df_bank_detail$job, summary)

ggplot(data = df_bank_detail, mapping = aes(x=job, y = age, fill = job)) +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle=90, vjust=.8, hjust=0.8))
