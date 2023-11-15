# SAS_R: Airline Comment Analysis

# Identify satisfied / unsatisfied customer 
## (1) Forecast the satisfaction level of the customer with supervised machine learning model 
### a. Set file path
getwd()
setwd('/Users/irislee/SAS_R Business Analytics/HW3')

### b, Import libraries
library(tidyverse)
library(randomForest)
library(dplyr)

### c. Data loading and pre-processing 資料讀取與前處理
airline = read_csv('/Users/irislee/SAS_R Business Analytics/HW3/airline_survey.csv') 

sapply(airline,function(X) sum(is.na(X))) # check for NAs -> "Arrival Delay in Minutes": 310 rows with empty values
names(airline) = names(airline) %>% make.names() # remove empty values
airline = airline[complete.cases(airline),] # analyze complete data only
sapply(airline,function(X) sum(is.na(X))) # check for NAs again

str(airline) # check the data types of each column
airline = as.data.frame(airline) # convert data type from tibble to data.frame

airline$Gender = as.factor(airline$Gender) # convert all categorical data types from 'character' to 'factor'
airline$Customer.Type = as.factor(airline$Customer.Type)
airline$Type.of.Travel = as.factor(airline$Type.of.Travel)
airline$Class = as.factor(airline$Class)
airline$satisfaction = as.factor(ifelse(airline$satisfaction == 'satisfied',1,0))
airline = airline[,-1] # remove the first column

### d. Split the data into the training set and the testing set.
n = nrow(airline);n # 103594
trainI <- sample(1:n,  size = round(0.8*n)) # take 80% of the data as the training set.
traind <- airline[trainI,]
testd <- airline[-trainI,] # take others as the testing set.

### e. Fit the model, predict the result with the testing data, and calculate the accuracy of the prediction.
rf <- randomForest(satisfaction ~., traind, importance=TRUE, ntree=100) 

pred = predict(rf, newdata = testd)

cm = table(Real = testd$satisfaction, Predict = pred)
cm
accuracy = sum(diag(cm))/sum(cm)
accuracy # >> 0.96

## (2) Identify important variables that influence customer satisfaction
importance(rf)

# ---------------------------------------------------------------------------------
# Customer clustering
## (1) Customer partition
### a. import libraries
library(factoextra)
library(useful)

### b. Represent categorical with 1 and 0：gender、customer.type, type.of.travel, and class
str(airline)
airline2 = airline %>% 
  mutate(
    gender = ifelse(Gender == 'Male',1,0),
    customer.type = ifelse(Customer.Type == 'Loyal Customer',1,0),
    type.of.travel = ifelse(Type.of.Travel == 'Business travel',1,0),
    class.business = ifelse(Class == 'Business',1,0),
    class.eco.plus = ifelse(Class == 'Eco Plus',1,0),
    class.eco = ifelse(Class == 'Eco',1,0)
  )

### c. Select variables for clustering：age, gender, customer.type, type.of.travel, class, and inflight.service
airline2 = airline2[,c(4,8,12,14,15,16,17,20,21,25,26,27,28,29,30)]
str(airline2)

### d. Reorder the variables and randomly select 2% of the data for clustering
airline2 = airline2 %>% 
  dplyr::select(Age,gender,customer.type,type.of.travel,class.business,class.eco.plus,class.eco,everything()) %>%
  sample_frac(0.02) # 約2000筆

### e. Determine the optimal number of clusters using the Elbow Method: 4
fviz_nbclust(airline2, 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 20          # max number of clusters to consider
) + geom_vline(xintercept = 4, linetype = 2)
km <- kmeans(airline2, centers=4,nstart=20)

### f. Plot the result of customer clustering
plot(km,data=airline2) 


## (2) Check basic statistics for each cluster
km$centers # 各群中心點
km$withinss # 群內變異
km$tot.withinss
table(km$cluster) # 各群資料筆數

### a. Integrate the original data with the clustering results
c = km$cluster
result = cbind(airline2,c)
result

### b. Examine the distribution of variables within each cluster
#### Age distribution within each cluster
ggplot(result, aes(x=as.factor(c), y=Age)) + 
  geom_boxplot()

#### Distribution of satisfaction with in-flight wi-fi services within each cluster.
ggplot(result, aes(x=as.factor(c), y=Inflight.wifi.service)) + 
  geom_boxplot()

#### Distribution of satisfaction with food within each cluster.
ggplot(result, aes(x=as.factor(c), y=Food.and.drink)) + 
  geom_boxplot()

#### Distribution of satisfaction with seat comfort within each cluster.
ggplot(result, aes(x=as.factor(c), y=Seat.comfort)) + 
  geom_boxplot()

#### Distribution of satisfaction with leg room within each cluster.
ggplot(result, aes(x=as.factor(c), y=Leg.room.service)) + 
  geom_boxplot() 

#### Distribution of satisfaction with inflight entertainment within each cluster.
ggplot(result, aes(x=as.factor(c), y=Inflight.entertainment)) + 
  geom_boxplot()

#### Distribution of satisfaction with cleanliness within each cluster.
ggplot(result, aes(x=as.factor(c), y=Cleanliness)) + 
  geom_boxplot()

#### Distribution of satisfaction with on board service/ inflight service within each cluster.
ggplot(result, aes(x=as.factor(c), y=On.board.service)) + 
  geom_boxplot()

ggplot(result, aes(x=as.factor(c), y=Inflight.service)) + 
  geom_boxplot()

#----------- 
#### Distribution of gender within each cluster.
ggplot(data =result) +
  geom_bar(aes( x = gender)) + 
  facet_wrap( ~ c) 

#### Distribution of customer type within each cluster.
ggplot( data =result) +
  geom_bar(aes(x = customer.type)) + 
  facet_wrap( ~ c)

#### Distribution of type of travel within each cluster.
ggplot(data =result) +
  geom_bar(aes(x = type.of.travel)) + 
  facet_wrap( ~ c)

#### Distribution of class within each cluster.
ggplot(data =result) +
  geom_bar(aes(x = class.business)) + 
  facet_wrap( ~ c)

ggplot( data =result) +
  geom_bar(aes(x = class.eco.plus)) + 
  facet_wrap( ~ c)

ggplot( data =result) +
  geom_bar(aes(x = class.eco)) + 
  facet_wrap( ~ c)


