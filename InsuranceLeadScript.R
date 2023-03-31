################# Reading Data, Cleansing, and Building a Model #########################
#########################################################################################
######################## Carlos Castro ##################################################
library(tidyverse)

### 1 - import csv file to Global Environment
Data <- read.csv("LeadsPredictionRawData.csv", header = TRUE, sep = ",")
View(Data)


### 3 - Display info about the Dataframe
summary(Data)
nrow(Data)
ncol(Data)

#Number of Nulls
sum(is.na(Data))

cat("\n Variables with number of missing values \n")
sapply(Data, function(x) sum(is.na(x)) )


#Number of unique values
length(unique(Data))


cat("\n Variables and the # of unique values \n")
sapply(Data, function(x) length(unique(x)))


    #Remove Redundant Columns
Data$ID = NULL

    ########Omit NA values CONVERTS 50,000 VARIABLES TO 30,000 Variables#####
Data <- na.omit(Data)
view(Data)

    #Dealing with blank values and splitting characters
Data[Data== "" | Data == " "] <- NA


    #Final dataframe with no missing values
Data %>% na.omit(Data)

sum(is.na(Data))
View(Data)


    #########Drop Health.Indicator and Upper_Age, Holding_Policy_Duration (HAS MOST NA VALUES SO IT'S BEST TO DROP THEM) ################
Data <- subset(Data, select = -c(Health.Indicator, Upper_Age, Holding_Policy_Duration, Holding_Policy_Type))



  # CORRELEATION
cor(Data$Region_Code, Data$Response)
cor(Data$Reco_Policy_Cat, Data$Response)
cor(Data$Reco_Policy_Cat, Data$Response)

### 4 - Datatypes
# Numeric (2, 3.14)
# Integer (5L)
# Logical 
# Character ('a','b'.'100)
# Complex (5+4i)
# Raw 

class(Data$ID)
class(Data$City_Code)
class(Data$Region_Code)
class(Data$Accomodation_Type)
class(Data$Reco_Insurance_Type)
class(Data$Upper_Age)
class(Data$Lower_Age)
class(Data$Is_Spouse)
class(Data$Health.Indicator)
class(Data$Holding_Policy_Duration)
class(Data$Holding_Policy_Type)
class(Data$Reco_Policy_Cat)
class(Data$Reco_Policy_Premium)
class(Data$Response)


#Test Code for GGPlot
Data %>%
  ggplot(Data, aes(x = Holding_Policy_Duration,
                                        y = Response))+
  geom_point(size = 3)+
  geom_line(color = "Red")

#Count bar Graph of Response (Target Variable)
table(Data$Response)
barplot(table(Data$Response),
        ylim=c(0,45000),
        xlab="Response",
        ylab="Counts",
        col="dodgerblue")

#What is the probability of a customer responding to a 0 or 1?
#Lets see if customers with spouses are more likely to respond?
Data %>% filter(Data$Response =='1') %>% group_by(Is_Spouse )%>%count() ->data_spouse

plot <- ggplot(data = data_spouse, aes(x=Is_Spouse, y = n,  fill=Is_Spouse)) + geom_bar(stat = "identity")


#Lets see if customers who rent or own are more likely to respond?
Data %>% filter(Data$Response =='1') %>% group_by(Accomodation_Type )%>%count() ->data_accomodation
ggplot(data = data_accomodation, aes(x=Accomodation_Type, y = n,  fill=Accomodation_Type )) + geom_bar(stat = "identity")


################################Buidling algorithm########################################
#Packages library(dplyr)
#library(rpart)
#library(rpart.plot)
#library(rattle)
library(caret) 
library(dplyr)
library(stats)
library(randomForest)
library(ggplot2)


#Load Data into myData object
myData = Data

#Data manipulation of changing column data types
myData$Response <- as.factor(myData$Response)
myData$Reco_Policy_Cat <- as.numeric(myData$Reco_Policy_Cat)
myData$Lower_Age <- as.numeric(myData$Lower_Age)
myData$Region_Code <- as.numeric(myData$Region_Code)


# Variable Selection 
str(myData)


#We have to many values at 0 and a few at 1, this is a Downsample to balance the class
myData <- downSample(myData, y = myData$Response)



#Splitting Data in Training and Testing
index = sample(2,nrow(myData), replace = TRUE, prob=c(0.8,0.2))


#Training Data
Training = myData[index == 1,]
# Testing Data
Testing = myData[index == 2,]

#####################################################################
#####################################################################
############################ Method 1 ###############################
##### Extract the predictors and response variable from the  data (To prevent overfitting) ####
x <- Training[, -9]
y <- Training[, 9]

# downsample the data
downsampled_data <- downSample(x, y)

# train a random forest model on the downsampled data
model <- randomForest(downsampled_data)

# make predictions on new data using the trained model
new_data <- myData[19597:39193,]
new_predictions <- model$predict(new_data)


# Extract the table representation of the confusion matrix using the table method
conf_matrix_table <- conf_matrix$table

# Convert the table into a data frame using the as.data.frame() function
conf_matrix_table <- as.data.frame(conf_matrix_table)
colnames(conf_matrix_table)

# set the number of folds for cross-validation
nfolds <- 5

# train a random forest model using cross-validation
RFM = randomForest(downsampled_data, nfolds = nfolds)
print(RFM)


# train a random forest model using cross-validation
RFM = randomForest(x, y, nfolds = nfolds)

# evaluate the model's performance on the test data
test_X <- Testing[, -9]
test_y <- Testing[, 9]
test_predictions <- predict(RFM, test_X)
test_error <- mean(test_y != test_predictions)
print(RFM)

##############################################
##############################################
##### WITHOUT DOWNSAMPLE RUN CODE BELOW ######

############################ Method 2 ###############################
# Random Forest Model
RFM = randomForest(Response~., data = Training)

print(RFM)

plot(RFM)
# Evaluating Model Accuracy
Response_Pred = predict(RFM, Training)
Training$Response_Pred = Response_Pred
view(Testing)


# Confusion Matrix
CFM = table(Training$Response, Training$Response_Pred)


Classification_Accuracy = sum(diag(CFM)/sum(CFM))
Classification_Accuracy


conf_matrix <- confusionMatrix(Training$Response, Training$Response_Pred)

# Extract the table representation of the confusion matrix using the table method
conf_matrix_table <- conf_matrix$table

# Convert the table into a data frame using the as.data.frame() function
conf_matrix_table <- as.data.frame(conf_matrix_table)



colnames(conf_matrix_table)

#Plotting the confusion matrix using the ggplot() function
ggplot(conf_matrix_table, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "orange", high = "green") +
  labs(title = "Confusion Matrix",
       x = "True Response",
       y = "Predicted Response")

summary(CFM)


# Calculate the accuracy, precision, and recall using the conf_matrix object
accuracy <- conf_matrix$overall[1]
precision <- conf_matrix$byClass[1]
recall <- conf_matrix$byClass[2]

# Print the values
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")



###################################################################################
#################Cart Model/linear regression model ###############################
cart_model <- cart(Dataframe$Upper_Age, Dataframe$Response)


# Performs stratified random split of the data set
set.seed(2)


#Split data 
dataSplit = caret::createDataPartition(Dataframe$Response, p = 0.8, list = FALSE)

#assign training and testing set
trainingData = Dataframe %>% slice(dataSplit)
testingData = Dataframe %>% slice(-dataSplit)

#Classification And Regression Training (Caret)
#Build Linear Model Regression Training Model
model <- train(Response ~ ., Dataframe = trainingData,
               method = "lm",
               na.action = na.omit,
               preProcess = c("scale", "center"),
               trControl = trainControl(method="none")
)

# Fit a linear regression model to the training data
model <- lm(Response ~ ., data = trainingData)

# Evaluate the model's performance on the test data
predictions <- predict(model, testingData)

model.trainingData <- predict(model, trainingData) # Apply model to make prediction
model.testingData <- predict(model, testingData)

plot(trainingData$Response ,model.trainingData, col="blue")
plot(testingData$Response ,model.testingData, col = "blue")

summary(model)

# Making a decision tree 
DecisionTreeNode <- setRefClass("DecisionTreeNode",
                        fields = list(
                                  x = "data.frame",
                                  y = "ANY",
                                  Is_Spouse =  "logical",
                                  City_Code = "character",
                                  Region_Code = "integer",
                                  Accomodation_Type = "character",
                                  Reco_Insurance_Type = "character",
                                  Upper_Age = "integer",
                                  Lower_Age = "integer",
                                  Holding_Policy_Duration = "character",
                                  Holding_Policy_Type = "numeric",
                                  Reco_Policy_Cat = "integer",
                                  Reco_Policy_Premium = "numeric",
                                  Response = "integer"),
                        methods = list(
                                  initialize = function(...){},
                                  information_gain = function(mask){},
                                  max_information_gain_split = function(feature){},
                                  best_feature_split = function(){},
                                  split_node = function(){},
                                  predict_row = function(row){},
                                  predict = function(features){}
                                )
                            )

# Decision Tree Node
print.DecisionTreeNode <- function(node,level=0){
  response <- paste("|->",node$Accomodation_Type)
  if(level < node$Upper_Age){
    if(!is.null(nodeleft)){
      response <- paste0(response,"\n",paste(rep(" ",2*(level+1)),collapse=" "),print(nodeleft,level+1))
    }
    if(!is.null(noderight)){
      response <- paste0(response,"\n",paste(rep(" ",2*(level+1)),collapse=" "),print(noderight,level+1))
    }
  }
  
  if(level==0) {
    cat(response)
  } else {
    return(response)
  }
}






