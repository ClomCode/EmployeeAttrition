# ----------------------------------------------
# MACHINE LEARNING AND VISUALISATION PROJECT
# ----------------------------------------------
# Before use ensure the working directory and file paths are all changed to the correct location
# See line 11 and 131 in this file
# 
# Clear Data environment
rm(list = ls())

# Sets working directory
setwd("-")

# Library management
mylibs <- c("readxl", "ggplot2", "ggcorrplot", "psych", "lattice", "caret",
            "neuralnet", "dplyr", "e1071", "Hmisc", "survival", "kernlab",
            "corrplot", "ggcorrplot", "OneR", "ROCR", "C50", "ggpubr", "pROC")
library(pacman)
pacman::p_load(char=mylibs,install=TRUE,character.only=TRUE)

# Load function script
source("Functions.R")

# Dataset 
filename = "EmployeeData.csv"

# Calls ReadData function to read employee attrition dataset
dataset <- ReadData(filename)

# ----------------------------------------------
# Data Exploration 1
# ----------------------------------------------
str(dataset)
head(dataset)
summary(dataset)
describe(dataset)
table(dataset$STATUS)
unique(dataset$city_name)
unique(dataset$job_title)
unique(dataset$BUSINESS_UNIT)
unique(dataset$job_title)

# ----------------------------------------------
# Data Preparation 
# ----------------------------------------------
# Exploration showed that data requires some preparation
# Data prep function is used to get data into form that is needed
sub.dataset <- DataPreparation(dataset)
visual.dataset <- DataVisuals(dataset)
str(sub.dataset)

# Categorical Data Encoding
# Calls the NumericData function to convert all non-target variables to numeric type
sub.dataset <- NumericData(sub.dataset)

str(sub.dataset)
# Converts target variable to factor type
sub.dataset[,ncol(sub.dataset)] <- as.factor(sub.dataset[,ncol(sub.dataset)])

################## Non-Normalized Data #############################
#rows1 <- sample(nrow(sub.dataset2))
#sub.dataset2 <- sub.dataset2[rows1, ]
#sub.dataset2[,ncol(sub.dataset2)] <- as.factor(sub.dataset2[,ncol(sub.dataset2)])

# 0-1 normalization method for non-normal distributed data
sub.data.norm <- as.data.frame(lapply(sub.dataset[,(1:ncol(sub.dataset)-1)], NormalizeData))

str(sub.data.norm)

# Adds STATUS column back to the dataset after normalization
sub.data.norm <- cbind(sub.data.norm, STATUS = sub.dataset$STATUS)

# ----------------------------------------------
# Data Exploration 2
# ----------------------------------------------
prop.table(table(sub.dataset$STATUS)) * 100 

#correlation_matrix<-cor(sub.data.norm)
#ggcorrplot(correlation_matrix)

# Graphs
visual.dataset %>%
  filter(termreason_desc == "Resignaton") %>%
  select(city_name, termreason_desc, job_title) %>%
  ggplot(aes(x = city_name, fill = job_title)) +
  geom_bar() +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  labs(x = "Population Centres",
       y = "Count",
       title = "Resignations of Job categories by Population centres") 


visual.dataset %>%
  filter(termreason_desc == "Resignaton") %>%
  select(gender_full, termreason_desc, job_title) %>%
  ggplot(aes(x = job_title, fill = gender_full)) +
  geom_bar() +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  labs(x = "Job Category",
       y = "Count",
       title = "Resignations of Genders by Job Categories") 


visual.dataset %>%
  filter(STATUS == "TERMINATED") %>%
  select(city_name, termreason_desc) %>%
  ggplot(aes(x = city_name, fill = termreason_desc)) +
  geom_bar() +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  labs(x = "Population centres",
       y = "Count",
       title = "Termination reasons by Population centres") 


visual.dataset %>%
  filter(STATUS == "TERMINATED") %>%
  select(gender_full, termreason_desc) %>%
  ggplot(aes(x = termreason_desc, fill = gender_full)) +
  geom_bar() +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  labs(x = "Termination Reason",
       y = "Count",
       title = "Terminations of Genders by Termination reasons") 

# ----------------------------------------------
# Data Modelling + Results
# ----------------------------------------------
Results.File.Name <- "-"
FileHeader(Results.File.Name)

# Randomize the order of normalised dataset
set.seed(120)
rows <- sample(nrow(sub.data.norm))
sub.data.norm <- sub.data.norm[rows, ]

# Sequence of data ratios for test/train
data.train.percent  <- seq(from = 0.1, to = 0.9, length.out = 9)


for (t in data.train.percent){
  
  # Creates an index of data partitions each loop for Normaliztn
  index <- createDataPartition(sub.data.norm$STATUS, p=t, list=FALSE) # index of training
  data.train <- sub.data.norm[index,]
  data.test <- sub.data.norm[-index,]
  
  # Creates an index of data partitions each loop for Un Normaliztn
  
  #index <- createDataPartition(sub.dataset2$STATUS, p=t, list=FALSE) # index of training
  #data.train <- sub.dataset2[index,]
  #data.test <- sub.dataset2[-index,]
  
  
  set.seed(120)
  # Creating the C5 Model
  C5.Model <- C5.0(STATUS ~ age + length_of_service + serve_business + job_title + city_name +
                     store_name + male + STATUS_YEAR + headoffice, data = data.train, trials=5)
  #plot(C5.Model)
  set.seed(120)
  # Creating the naiveBayes Model
  Naive.Model <- naiveBayes(STATUS ~ age + length_of_service + serve_business + job_title + city_name +
                              store_name + male + STATUS_YEAR + headoffice, data = data.train, laplace=0)
  # Creating the ksvm Model
  KSVM.Model <- ksvm(STATUS ~ age + length_of_service + serve_business + job_title + city_name +
                       store_name + male + STATUS_YEAR + headoffice, data = data.train, kernel = "rbfdot", C = 5)
  # Creating the randomForest Model
  RForest.Model <- randomForest::randomForest(STATUS ~ age + length_of_service + serve_business + job_title + city_name +
                                                store_name + male + STATUS_YEAR + headoffice, data = data.train)
  
  # Creating the knn Model
  metric <- "Accuracy"
  control <- trainControl(method = "repeatedcv" , repeats = 10)
  KNN.Model <- train(STATUS ~ age + length_of_service + serve_business + job_title + city_name +
                       store_name + male + STATUS_YEAR + headoffice, data = data.train, method = "knn" , metric = metric , trControl = control )
  set.seed(120)
  # Creating the Neural Network Model
  #NN.Model <- neuralnet(STATUS~age + length_of_service + serve_business + job_title + city_name +
                          #store_name + male + STATUS_YEAR + headoffice, data = data.train, hidden=3,linear.output = F,stepmax = 1e6)
  
  ########### Creating the Neural Network Model#############
  #softplus <- function(x) {
  #log(1 + exp(x))
  #}
  set.seed(120)
  #NN.Model <- neuralnet(STATUS~age + length_of_service + serve_business + job_title + city_name +
  #store_name + male + STATUS_YEAR + headoffice, data = data.train, hidden=2,linear.output = F,stepmax = 1e6,act.fct = softplus())
  #plot(NN.Model)
  
  
  C5.Predicted <- predict(C5.Model, newdata = data.test[,1:ncol(data.test)-1])
  Naive.Predicted <- predict(Naive.Model, newdata = data.test[,1:ncol(data.test)-1])
  KSVM.Predicted <- predict(KSVM.Model, newdata = data.test[,1:ncol(data.test)-1])
  RForest.Predicted <- predict(RForest.Model, newdata = data.test[,1:ncol(data.test)-1])
  KNN.Predicted <- predict (KNN.Model , newdata = data.test[,1:ncol(data.test)-1])
  
  #NN.Predicted <- neuralnet::compute(NN.Model,data.test[,1:ncol(data.test)-1])
  #Predicted_Net_Results <- NN.Predicted$net.result
  #Decision_Making_Operations_1 <- ifelse(Predicted_Net_Results >= 0.3, 1, 0)
  #Decision_Making_Operations_2 <- Decision_Making_Operations_1[,1] &Decision_Making_Operations_1[,2]
  #Predicted_Labels <- lapply(Decision_Making_Operations_2, as.numeric)
  #Predicted_Labels <- ifelse(Predicted_Labels==1, "ACTIVE","TERMINATED")
  #Predicted_Labels <- matrix(unlist(Predicted_Labels), ncol = 1, byrow = TRUE)
  #Predicted_Labels <- as.factor(Predicted_Labels)
  
  #if(nlevels(data.test$STATUS) == nlevels(Predicted_Labels)){
    #NN.cm <- confusionMatrix(data.test$STATUS, Predicted_Labels)
  #}
  
  C5.cm <- confusionMatrix(C5.Predicted, data.test$STATUS)
  print(C5.cm)
  Naive.cm <- confusionMatrix(Naive.Predicted, data.test$STATUS)
  #print(Naive.cm)
  KSVM.cm <- confusionMatrix(KSVM.Predicted, data.test$STATUS)
  #print(KSVM.cm)
  RForest.cm <- confusionMatrix(RForest.Predicted, data.test$STATUS)
  #print(RForest.cm)
  KNN.cm <- confusionMatrix(KNN.Predicted, data.test$STATUS)
  #print(KNN.cm)
  # Writes performance metrics to file
  
  WritePerformanceMetrics(C5.cm, Results.File.Name)
  WritePerformanceMetrics(Naive.cm, Results.File.Name)
  WritePerformanceMetrics(KSVM.cm, Results.File.Name)
  WritePerformanceMetrics(RForest.cm, Results.File.Name)
  WritePerformanceMetrics(KNN.cm,Results.File.Name)
  #WritePerformanceMetrics(NN.cm,Results.File.Name)

}

Performance.Table <- read.table(file = File.Name,
                                header = TRUE, sep = ",", dec = ".")


# Shows confusion matrix
print(C5.cm)
#print(Naive.cm)
#print(KSVM.cm)
#print(RForest.cm)
#print(NN.cm)

ROCCurve(Naive.Predicted, data.test$STATUS)
ROCCurve(KSVM.Predicted, data.test$STATUS)
ROCCurve(RForest.Predicted, data.test$STATUS)
ROCCurve(KNN.Predicted, data.test$STATUS)
ROCCurve(C5.Predicted, data.test$STATUS)

print("|-------------END-------------|")
