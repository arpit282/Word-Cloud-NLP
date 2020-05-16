###########################################################################
###############          Logistic regression                 ###########


setwd("E:/R (Data Science)/excel files")


data <- read.csv("binary.csv")

str(data)

data$admit <- as.factor(data$admit)
data$rank <- as.factor(data$rank)


############ training and testing data #############
set.seed(123)
id <- sample(2,nrow(data),replace = T,prob = c(.8,.2))

train <- data[id == 1, ]
test <- data[id ==2,]


##### Model ######

model <-glm(admit ~ gpa+rank,train,family = "binomial")

summary(model)

# prediction training dataset
p1<-predict(model,train,type="response")
head(p1)
head(train)

# misclassification train
pred1 <- ifelse(p1>.5,1,0)
tab1 <- table(prediction=pred1,actual=train$admit)
tab1

sum(diag(tab1))/sum(tab1)


# misclassification on test dataset
p2 <-predict(model,test,type="response")
head(p2)
head(test)


pred2 <- ifelse(p2>.5,1,0)
tab2 <- table(predication =pred2,actual = test$admit)
tab2


sum(diag(tab2))/sum(tab2)


#######################  K-Nearest Neighbour (KNN)  #####################3


data <- read.csv("Prostate_Cancer.csv")

data <- data[,-1]

str(data)

t <-table(data$diagnosis_result)
pie(t)


# Normalisation 

normalise <- function(x){
  return( (x - min(x)) / (max(x)- min(x)))     ### 0-1
}


data_n <- as.data.frame(lapply(data[2:9], normalise))

### training and test set 
train <- data_n[1:70,]
test <- data_n[71:100,]


### train and test labels
train_lab <- data[1:70,1]
test_lab <- data[71:100,1]
table(test_lab)

### Knn 
install.packages("class")
library(class)

k <- sqrt(nrow(data))  #### K is determined by square root of no . of rows

model <- knn(train = train,test = test,cl = train_lab, k= 9)
table(model)


t1 <- table(Prediction =model, Actual=test_lab)
sum(diag(t1))/sum(t1)


############################################################################
#################    Multinomial Logistic regression  ######################

data <- read.csv("Cardiotocographic.csv")
str(data)


# factorise the NSP 
data$NSP <- as.factor(data$NSP)



# train and test dataset
id <- sample(2,nrow(data),replace = T,prob = c(.8,.2))
train <- data[id ==1,]
test <- data[id==2,]

# Model Multinomial Logistic Regression
install.packages("nnet")
library(nnet)

train$NSP <- relevel(train$NSP,ref = "1")
model <- multinom(NSP ~.,train)


# prediction of training data

p <- predict(model,train)
tab <- table(p,train$NSP)
sum(diag(tab))/sum(tab)

# prediction of testing set
p1 <- predict(model,test)
tab1 <- table(p1,test$NSP)

sum(diag(tab1))/sum(tab1)



n <- table(train$NSP)
n/sum(n)

tab/colSums(tab)


n1 <- table(test$NSP)
n1/sum(n1)
tab1/colSums(tab1)


#################################################################################
######################   R Studio Start Up   ###################################

data <- c(1,2,3,4,5)
data


name <- c("roy","lilly","jai")
name



grade <- c(89,67,98,34,90,78,56)
grade

name <- c("ajay","dev","jasmine","aditi","jai","amit","abhisht")

####data frame

dataframe <- data.frame(grade,name)

str(dataframe)
dataframe$grade
dataframe$name

dim(dataframe)
summary(dataframe)


head(dataframe)
tail(dataframe)

# data in R

data(iris)
iris
str(iris)
summary(iris)
head(iris)
tail(iris)
head(iris[1:3,])

#Qualitative data :Barplot
tab <- table(iris$Species)
barplot(tab,main = "Species in Barplot",xlab = "Species",ylab = "Frequency",
        col = rainbow(3))



# pie Chart
pie(tab,main = "Pie chart of species",col = rainbow(3))


# histogram
hist(iris$Sepal.Length,main = "Histogram",xlab = "Sepal length",col = rainbow(12),
     breaks = 15,xlim = c(4,8),ylim = c(0,20))

# two Variables
plot(iris$Sepal.Length,iris$Sepal.Width,col="blue")

pairs(iris)

# package
install.packages("psych")
library(psych)
pairs.panels(iris)



############################################################################
####################    Data Visualisation   ########################

data <- read.csv("vehicle.csv")
install.packages("psych")
library(psych)

pairs.panels(data[2:6])

#### Scatter Plot
library(ggplot2)

ggplot(data,aes(Mileage,fm,col=State))+geom_boxplot()


# histogram
hist(data$lh,xlim = c(0,25),breaks = 30,col = rainbow(12),main = "Histogram",
     xlab = "labour cost")


# barplot
barplot(data$Mileage)


#plot
plot(data$State)

# pie
pie(table(data$State))


# boxplot
boxplot(Mileage~State,data)

# Multi-plot
pairs(data[2:6])


# 3d Scatter Plot

library(scatterplot3d)
scatterplot3d(data$lh,data$lc,data$mc)

##### Correlation
mtcars

str(mtcars)
y <- cor(mtcars)
library(ggplot2)
library(ggcorrplot)

corr <- round(y,1)

ggcorrplot(corr,lab = T,lab_size = 3,colors = c("blue","springgreen2","tomato2"),method = "square",
           type= "lower")


###########    Interactive Diagram

data <- read.csv("binary.csv")

library(networkD3)
simpleNetwork(data,fontSize = 16,nodeColour = "red")

#########################################################################
#####################  TEXT MINING (WORD CLOUD)    #######################


library(wordcloud)
library(tm)
library(SnowballC)

# Data intake
modi <- readLines("modi's new speech.txt")

# Corpus formation
modi_corpus <- Corpus(VectorSource(modi))
inspect(modi_corpus)

# Cleaning of data
modi_clean <- tm_map(modi_corpus,removeNumbers)
inspect(modi_clean[2:6])

modi_clean <- tm_map(modi_corpus,tolower)
modi_clean <- tm_map(modi_corpus,stripWhitespace)
modi_clean <- tm_map(modi_corpus,removePunctuation)
modi_clean <- tm_map(modi_corpus,removeWords,stopwords("en"))
modi_clean <- tm_map(modi_corpus,stemDocument)




# word cloud formation
cloud <- wordcloud(modi_clean,min.freq = 2)
cloud <- wordcloud(modi_clean,min.freq = 2,colors = brewer.pal(8,"Dark2"),random.order = F,
                   rot.per = .50)


















