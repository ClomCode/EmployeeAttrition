### Attrition Functions
print("Loaded Functions.R")


ReadData <- function(csv.filename){
  # ----------------------------------------------
  # ReadData()
  # Reads CSV file from directory
  # Args:
  #  csv.filename: String of CSV Filename
  # 
  # Returns:
  #  Dataframe of the CSV file
  # ----------------------------------------------
  dataset <- read.csv(csv.filename, encoding="UTF-8", stringsAsFactors = FALSE)
  
  print(paste("CSV dataset", csv.filename, "has been read. Observations=", nrow(dataset)))
  return(dataset)
}



DataPreparation <- function(dataset){
  # ----------------------------------------------
  # DataPreparation()
  # Contains the full data preparation of dataset
  # Args:
  #  dataset: Dataframe
  # 
  # Returns:
  #  Dataframe of prepped data
  # ----------------------------------------------
  
  # Removing duplicated EmployeeID for most recent entry
  dataset = dataset[order(dataset[,'EmployeeID'],-dataset[,'age']),]
  dataset = dataset[!duplicated(dataset$EmployeeID),]
  
  
  # city_name is reworked to a simpler describer of population
  
  pop.places = c("Vancouver" = 2463431, "Terrace" = 15723, "Nanaimo" = 104936,
                 "Nelson" = 18307, "Kelowna" = 194882, "Victoria" = 367770,
                 "Kamloops" = 103811, "Fort St John" = 28396, "Surrey" = 517887,
                 "Vernon" = 61334, "Quesnel" = 23146, "Chilliwack" = 101512,
                 "Dawson Creek" = 12178, "Squamish" = 19893, "New Westminster" = 70996,
                 "Port Coquitlam" = 58612, "Cortes Island" = 1035, "Burnaby" = 232755,
                 "Bella Bella" = 1019, "Cranbrook" = 26083, "Williams Lake" = 18277, 
                 "Prince George" = 86622, "Richmond" = 198309, "Grand Forks" = 4049,
                 "West Vancouver" = 42473, "Abbotsford" = 180518, "Aldergrove" = 12007, 
                 "Langley" = 25888, "North Vancouver" = 52898, "White Rock" = 19952, 
                 "New Westminister" = 70996, "Fort Nelson" = 3366, "Haney" = 82256	,
                 "Valemount" = 1021, "Ocean Falls" = 203	, "Princeton" = 2828,
                 "Trail" = 9313, "Dease Lake" = 54, "Pitt Meadows" = 18573, 
                 "Blue River" = 157 )
  length(pop.places)
  
  category = case_when(
    pop.places >= 100000 ~ "City",
    pop.places >= 10000 ~ "Rural",
    pop.places < 10000 ~ "Remote"
  )
  pop.names <- names(pop.places)
  pop.places <- category
  names(pop.places) <- pop.names
  dataset$city_name <- recode(dataset$city_name, !!!pop.places)
    
  
  
  # job_title is reworked into Executive, Board, Manager and Employee
  
  Board <- c("CEO", "VP Stores" , "Director, Recruitment", "VP Human Resources", "VP Finance",
             "Director, Accounts Receivable", "Director, Accounting",
             "Director, Employee Records", "Director, Accounts Payable",
             "Director, HR Technology", "Director, Investments",
             "Director, Labor Relations", "Director, Audit", "Director, Training",
             "Director, Compensation")
  
  Executive <- c("Exec Assistant, VP Stores", "Exec Assistant, Legal Counsel",
                 "CHief Information Officer", "Exec Assistant, Human Resources", "Exec Assistant, Finance")
  
  Manager <- c("Customer Service Manager", "Processed Foods Manager", "Meats Manager",
               "Bakery Manager", "Produce Manager", "Store Manager", "Trainer", "Dairy Manager")
  
  Employee <- c("Meat Cutter", "Dairy Person", "Produce Clerk", "Baker", "Cashier",
                "Shelf Stocker", "Recruiter", "HRIS Analyst", "Accounting Clerk",
                "Benefits Admin", "Labor Relations Analyst", "Accounts Receiveable Clerk",
                "Accounts Payable Clerk", "Auditor", "Compensation Analyst",
                "Investment Analyst", "Systems Analyst", "Corporate Lawyer", "Legal Counsel")
  length(Board) + length(Executive) + length(Manager) + length(Employee)
  JobsCategoriser <- function(job){
    # ----------------------------------------------
    # JobsCategoriser()
    # Splits jobs in dataset into 4 smaller categories 
    # Args:
    #  job: String of data
    # 
    # Returns:
    #  String of updated data
    # ----------------------------------------------
    if (job %in% Board) return("Board")
    if (job %in% Executive) return("Executive")
    if (job %in% Manager) return("Manager")
    if (job %in% Employee) return("Employee")
  }
  
  for(i in 1:nrow(dataset)) dataset$job_title[i] <- JobsCategoriser(dataset$job_title[i])
  
  # department_name rework into Business or Customer based
  
  unique(dataset$department_name)
  
  Serve.Customer = c("Meats", "Produce", "Bakery", "Dairy", "Processed Foods", "Customer Service")
  
  Serve.Business = c("Executive", "Store Management", "Recruitment", "Training",
                     "Labor Relations", "HR Technology", "Employee Records",
                     "Compensation", "Legal", "Accounts Receiveable", "Information Technology",
                     "Accounts Payable", "Audit", "Accounting" , "Investment")
  length(Serve.Business) + length(Serve.Customer)
  DepartmentCategoriser <- function(department){
    # ----------------------------------------------
    # DepartmentCategoriser()
    # Splits jobs in dataset into 4 smaller categories 
    # Args:
    #  job: String of data
    # 
    # Returns:
    #  String of updated data
    # ----------------------------------------------
    if (department %in% Serve.Business) return("Serve Business")
    if (department %in% Serve.Customer) return("Serve Customer")
  }
  
  for(i in 1:nrow(dataset)) dataset$department_name[i] <- DepartmentCategoriser(dataset$department_name[i])
  
  dataset <- dataset %>%
    mutate(serve_business = ifelse(dataset$department_name == "Serve Business", 1, 0))
  
  # Changing gender and business to binary dummy variables
  
  dataset <- dataset %>%
    mutate(male = ifelse(dataset$gender_short == "M", 1, 0)) %>%
    mutate(headoffice = ifelse(dataset$BUSINESS_UNIT == "HEADOFFICE", 1, 0))
  
  # Removing any insignificant variables to create a smaller dataset
  
  sub.cols = c('EmployeeID'
               ,'age'
               ,'length_of_service'
               ,'serve_business'
               ,'job_title'
               ,'city_name'
               ,'store_name'
               ,'male'
               ,'STATUS_YEAR'
               ,'headoffice'
               ,'STATUS')
  
  return(sub.dataset <- dataset[,sub.cols])
  
}

DataVisuals <- function(dataset){
  # ----------------------------------------------
  # DataPreparation()
  # Contains the full data preparation of dataset
  # Args:
  #  dataset: Dataframe
  # 
  # Returns:
  #  Dataframe of prepped data
  # ----------------------------------------------
  
  # Removing duplicated EmployeeID for most recent entry
  dataset = dataset[order(dataset[,'EmployeeID'],-dataset[,'age']),]
  dataset = dataset[!duplicated(dataset$EmployeeID),]
  
  
  # city_name is reworked to a simpler describer of population
  
  pop.places = c("Vancouver" = 2463431, "Terrace" = 15723, "Nanaimo" = 104936,
                 "Nelson" = 18307, "Kelowna" = 194882, "Victoria" = 367770,
                 "Kamloops" = 103811, "Fort St John" = 28396, "Surrey" = 517887,
                 "Vernon" = 61334, "Quesnel" = 23146, "Chilliwack" = 101512,
                 "Dawson Creek" = 12178, "Squamish" = 19893, "New Westminster" = 70996,
                 "Port Coquitlam" = 58612, "Cortes Island" = 1035, "Burnaby" = 232755,
                 "Bella Bella" = 1019, "Cranbrook" = 26083, "Williams Lake" = 18277, 
                 "Prince George" = 86622, "Richmond" = 198309, "Grand Forks" = 4049,
                 "West Vancouver" = 42473, "Abbotsford" = 180518, "Aldergrove" = 12007, 
                 "Langley" = 25888, "North Vancouver" = 52898, "White Rock" = 19952, 
                 "New Westminister" = 70996, "Fort Nelson" = 3366, "Haney" = 82256	,
                 "Valemount" = 1021, "Ocean Falls" = 203	, "Princeton" = 2828,
                 "Trail" = 9313, "Dease Lake" = 54, "Pitt Meadows" = 18573, 
                 "Blue River" = 157 )
  length(pop.places)
  
  category = case_when(
    pop.places >= 100000 ~ "City",
    pop.places >= 10000 ~ "Rural",
    pop.places < 10000 ~ "Remote"
  )
  pop.names <- names(pop.places)
  pop.places <- category
  names(pop.places) <- pop.names
  dataset$city_name <- recode(dataset$city_name, !!!pop.places)
  
  
  
  # job_title is reworked into Executive, Board, Manager and Employee
  
  Board <- c("CEO", "VP Stores" , "Director, Recruitment", "VP Human Resources", "VP Finance",
             "Director, Accounts Receivable", "Director, Accounting",
             "Director, Employee Records", "Director, Accounts Payable",
             "Director, HR Technology", "Director, Investments",
             "Director, Labor Relations", "Director, Audit", "Director, Training",
             "Director, Compensation")
  
  Executive <- c("Exec Assistant, VP Stores", "Exec Assistant, Legal Counsel",
                 "CHief Information Officer", "Exec Assistant, Human Resources", "Exec Assistant, Finance")
  
  Manager <- c("Customer Service Manager", "Processed Foods Manager", "Meats Manager",
               "Bakery Manager", "Produce Manager", "Store Manager", "Trainer", "Dairy Manager")
  
  Employee <- c("Meat Cutter", "Dairy Person", "Produce Clerk", "Baker", "Cashier",
                "Shelf Stocker", "Recruiter", "HRIS Analyst", "Accounting Clerk",
                "Benefits Admin", "Labor Relations Analyst", "Accounts Receiveable Clerk",
                "Accounts Payable Clerk", "Auditor", "Compensation Analyst",
                "Investment Analyst", "Systems Analyst", "Corporate Lawyer", "Legal Counsel")
  length(Board) + length(Executive) + length(Manager) + length(Employee)
  JobsCategoriser <- function(job){
    # ----------------------------------------------
    # JobsCategoriser()
    # Splits jobs in dataset into 4 smaller categories 
    # Args:
    #  job: String of data
    # 
    # Returns:
    #  String of updated data
    # ----------------------------------------------
    if (job %in% Board) return("Board")
    if (job %in% Executive) return("Executive")
    if (job %in% Manager) return("Manager")
    if (job %in% Employee) return("Employee")
  }
  
  for(i in 1:nrow(dataset)) dataset$job_title[i] <- JobsCategoriser(dataset$job_title[i])
  
  # department_name rework into Business or Customer based
  
  unique(dataset$department_name)
  
  Serve.Customer = c("Meats", "Produce", "Bakery", "Dairy", "Processed Foods", "Customer Service")
  
  Serve.Business = c("Executive", "Store Management", "Recruitment", "Training",
                     "Labor Relations", "HR Technology", "Employee Records",
                     "Compensation", "Legal", "Accounts Receiveable", "Information Technology",
                     "Accounts Payable", "Audit", "Accounting" , "Investment")
  length(Serve.Business) + length(Serve.Customer)
  DepartmentCategoriser <- function(department){
    # ----------------------------------------------
    # DepartmentCategoriser()
    # Splits jobs in dataset into 4 smaller categories 
    # Args:
    #  job: String of data
    # 
    # Returns:
    #  String of updated data
    # ----------------------------------------------
    if (department %in% Serve.Business) return("Serve Business")
    if (department %in% Serve.Customer) return("Serve Customer")
  }
  
  for(i in 1:nrow(dataset)) dataset$department_name[i] <- DepartmentCategoriser(dataset$department_name[i])
  
  dataset <- dataset %>%
    mutate(serve_business = ifelse(dataset$department_name == "Serve Business", 1, 0))
  
  # Changing gender and business to binary dummy variables
  
  dataset <- dataset %>%
    mutate(male = ifelse(dataset$gender_short == "M", 1, 0)) %>%
    mutate(headoffice = ifelse(dataset$BUSINESS_UNIT == "HEADOFFICE", 1, 0))
  
  
  return(visual.dataset <- dataset)
  
}



NumericData <- function(dataset){
  # ----------------------------------------------
  # NumericData()
  # Changes all but the last field to numeric data types
  # Args:
  #  dataset: a dataframe
  # 
  # Returns:
  #  Dataframe of numerics
  # ----------------------------------------------
  for (i in 1:(ncol(dataset)-1)) {
    if (is.character(dataset[, i])==TRUE){
      for(j in 1:nrow(dataset)){
        ascis <- as.numeric(charToRaw(dataset[j, i]))
        dataset[ j, i] <- sum(ascis)
      }
    }
    dataset[,i] <- as.numeric(dataset[,i])
  }
return(data.frame(dataset))
}




NormalizeData<-function(x){
  # ----------------------------------------------
  # NormalizeData()
  # Rescales dataframe to bound between [0.0, 1.0]
  # Args:
  #  Vector of values
  # 
  # Returns:
  #  Vector of values between [0.0, 1.0]
  # ----------------------------------------------
  return((x - min(x)) / (max(x) - min(x)))
}



FileHeader <- function(File.Name){
  # ----------------------------------------------
  # FileHeader()
  # Adds a file header to the file
  # Args:
  #  File.Name: String of file name
  # 
  # 
  #  
  # ----------------------------------------------
  cat("TR-Data%, TS-Data%, Accuracy%, Sensitivity%, Specificity%",
      "---------------------------------", file = File.Name, sep = "\n") 
}


CreateNN <- function(data.train, data.test){
  # ----------------------------------------------
  # CreateNN()
  # Creates a NeuralNetwork model
  # Args:
  #  data.train: Data to train the neural network
  #  data.test: Data to test the neural network
  # 
  # Returns:
  #  A vector of predicted FACTOR types 
  # ----------------------------------------------
  neuralnet.model <- neuralnet(STATUS ~ length_of_service + serve_business + job_title + city_name + 
                                 store_name + male + STATUS_YEAR + headoffice, 
                               data = data.train, hidden = 1, stepmax = 1e+07, rep = 1)
  
  # Prediction parameters with the model
  Predicted.Parameters <- neuralnet::compute(neuralnet.model,
                                             data.test[,1:ncol(data.test)-1])
  
  Predicted.Net.Results <- Predicted.Parameters$net.result
  
  Decision.Making.Operations.1 <- ifelse(Predicted.Net.Results >= 0.2, 1, 0)
  Decision.Making.Operations.2 <- Decision.Making.Operations.1[,1] & Decision.Making.Operations.1[,2]
  Predicted.Labels <- lapply(Decision.Making.Operations.2, as.numeric)
  Predicted.Labels <- ifelse(Predicted.Labels==1, "ACTIVE","TERMINATED")
  Predicted.Labels <- matrix(unlist(Predicted.Labels), ncol = 1, byrow = TRUE)
  Predicted.Labels <- as.factor(Predicted.Labels)
  
  return(Predicted.Labels)
}


WritePerformanceMetrics <- function(cm, File.Name){
  # ----------------------------------------------
  # WritePerformanceMetrics()
  #
  # Args:
  # cm: A confusion matrix
  # File.header: A string of filename
  #
  # Returns:
  # A text file of performance metrics
  # ----------------------------------------------
  return(cat(t*100, ",",
             (1-t)*100, ",",
             format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t",
             format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t",
             format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t",
             format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t",
             format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t",
             format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n",
             file = File.Name, sep = " ", append = TRUE)
  )
}

ROCCurve <- function(model.predicteds, data.test.predictor){
  # ----------------------------------------------
  # ROCCurve()
  # Plots a ROC curve for model
  # Args:
  #  model.predicteds: list of predicted values from model
  #  data.test.predictor: list of predictor values from data
  # 
  # Returns:
  #  ROC Curve plots
  # ----------------------------------------------
  # ROC curve visualization 
  model.predicteds <- as.numeric(as.factor(model.predicteds)) - 1
  roctarget <- roc(data.test.predictor, model.predicteds)
  
  roctarget$auc
  
  ggroc(roctarget, 
        color = "blue", 
        size = 1,
        legacy.axes = T) + 
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = 2, alpha  =  0.01) + 
    theme_pubr() +
    labs(title = sprintf("ROC Curve (AUC = %.5f)", roctarget$auc), # plot the auc as title
         x = "1 - Specificity (or fpr)", 
         y = "Sensitivity (or tpr)")
}
