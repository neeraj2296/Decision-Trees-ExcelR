install.packages("party")
install.packages("knitr")
#Including the necassary packages
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)

#Loading the data set
Fraud <- read.csv(file.choose())

#Factorising the data
Fraud$Marital.Status <- as.factor(Fraud$Marital.Status)
Fraud$Undergrad <- as.factor(Fraud$Undergrad)
Fraud$Urban <- as.factor(Fraud$Urban)
str(Fraud)
#Visuaising the daa
hist(Fraud$Taxable.Income)

#Categorising the data as risky or not risky
# Splitting data into training and testing.
# splitting the data based on Sales
Risky_Good = ifelse(Fraud$Taxable.Income<= 30000, "Risky", "Not Risky")
FC = data.frame(Fraud,Risky_Good)
FC$Risky_Good <- as.factor(FC$Risky_Good)
str(FC)

#Creatng the train and test set fromm the actual data
FC_train <- FC[1:400,]
FC_test <- FC[401:600,]

#Creating the Decision Tree
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % Not Risky patients
# using the training Data 
png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

#Predicting the data set the trained model.
pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))#Creating a dataframe for predictions
pred_tree["final"] <- NULL#Adding a column called final for indicating the prediction results
pred_test_df <- predict(op_tree,newdata=FC_test)#Coercing with the prediction results

#Calculating the accuracy of the prediction from the model
mean(pred_test_df==FC_test$Risky_Good) # looks like Accuracy = 84.5 %

#Checking if the prediction is correct USing 
CrossTable(FC_test$Risky_Good,pred_test_df)#Crosstable
confusionMatrix(FC_test$Risky_Good,pred_test_df)#Confusion Matrix
