#Importing the necassary Packages.
library(C50)
library(gmodels)

#Loadnig the data set.
data <- read.csv(file.choose())
dat <- data
str(data)

#Editing the data set as per the requirements
Sales_Result <- NULL                          #Creating a separate column
Sales_Result <- ifelse(data$Sales > 7.490,1,0)#That categorises as 0 or 1 based on if the sales is below or above 7.490
data[,"Sales_Result"] <- Sales_Result         #Adding the column to the data set.


#Factorising the characer variables as categories.
data$ShelveLoc <- as.factor(data$ShelveLoc)
data$Urban <- as.factor(data$Urban)
data$US <- as.factor(data$US)
data$Sales_Result <- as.factor(data$Sales_Result)

#Separting the data set based on sales are high(i.e. 1) or low(i.e. 0)
sales_high <- data[data$Sales_Result == "1",] 
sales_low <- data[data$Sales_Result == "0",]

#combing the a parts of sales_high and sales low to create train and test set.
data_train <- rbind(sales_high[1:120,], sales_low[1:120,])
data_test <- rbind(sales_high[121:199,], sales_low[121:201,])

#Training the model using C50 function.
trained_model <- C5.0(data_train, data_train$Sales_Result)
plot(trained_model)

#Calculatng the Accuracy of the trained model
mean(data_train$Sales_Result == predict(trained_model, data_train))

#Predicting the decison tree from the trained model
pred_test <- predict(trained_model, newdata = data_test)
#Calculating the Accuracy of the predicted data set
mean(pred_test == data_test$Sales_Result)

#To Check if the  prdiction is correct we use crosstable of gmodels package.
CrossTable(data_test$Sales_Result, pred_test)
