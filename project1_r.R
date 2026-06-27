#install dplyr
library(dplyr)

#importing the CSV file
credit_data <- read.csv("credit_default.csv")

#selecting specific columns into edited data frame
credit_data_edit <- credit_data[, c("LIMIT_BAL", "AGE", "EDUCATION", "PAY_0", "PAY_2",
                       "BILL_AMT1","default_payment_next_month")]

#Checking for missing values
anyNA(credit_data_edit)

#examining the structure of credit_data
str(credit_data_edit)

#creating a table of default
table(credit_data_edit$default_payment_next_month)

#assigning labels to the target variable
credit_labels <- as.factor(credit_data_edit$default_payment_next_month)

#summarizing the data set
summary(credit_data_edit[c("PAY_0","PAY_2","BILL_AMT1")])

#creating normalize function
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

#testing normalization function
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

#normalizing all numeric features
credit_data_norm <- as.data.frame(lapply(credit_data_edit[1:6], normalize))

#confirming that normalization worked
summary(credit_data_norm[c("PAY_0","PAY_2","BILL_AMT1")])

#installing rsample package
library(caret)

# setting seed for reproducibility to 25
set.seed(25)

#Creating the 80/20 split in the data
train_index <- createDataPartition(credit_labels, p = 0.8, list = FALSE)

#identifying training and testing data
train_data <-credit_data_norm[train_index,]
test_data <- credit_data_norm[-train_index,]

#setting training and testing labels
train_labels <- credit_labels[train_index]
test_labels  <- credit_labels[-train_index]

#loading library
library(gmodels)

#defining k's for testing
k_values <- c(3,7,15)

#creating an empty data frame for evaluation metrics
results_table <- data.frame(k = integer(), 
                            Train_Accuracy = numeric(), Test_Accuracy = numeric(), 
                            Train_Precision = numeric(), Test_Precision = numeric(),
                            Train_Recall = numeric(), Test_Recall = numeric(),
                            Train_F1 = numeric(), Test_F1 = numeric())

#running a for loop to evaluate all k_values
for (k_val in k_values) {
  credit_pred <- class::knn(train = train_data, 
                        test = test_data,
                        cl = train_labels,
                        k=k_val)
  train_pred <- class::knn(train = train_data, 
                           test = train_data,  
                           cl = train_labels,
                           k=k_val)
  cat("\n=======================================\n")
  cat("CrossTable for k =", k_val, "\n")
  CrossTable(x = test_labels, y = credit_pred, prop.chisq = FALSE)
 
  #making confusion matrices
  test_cm <- confusionMatrix(credit_pred, test_labels, positive = "1")
  train_cm <- confusionMatrix(train_pred, train_labels, positive = "1")
  
  #setting evaluation metrics
  test_acc  <- test_cm$overall['Accuracy']
  test_prec <- test_cm$byClass['Pos Pred Value']
  test_rec  <- test_cm$byClass['Sensitivity']
  test_f1   <- test_cm$byClass['F1']
  
  train_acc  <- train_cm$overall['Accuracy']
  train_prec <- train_cm$byClass['Pos Pred Value']
  train_rec  <- train_cm$byClass['Sensitivity']
  train_f1   <- train_cm$byClass['F1']
  
  
  #filling in values in the results table
  results_table <- rbind(results_table, data.frame(k = k_val, 
                                                   Train_Accuracy = train_acc, 
                                                   Test_Accuracy = test_acc, 
                                                   Train_Precision = train_prec, 
                                                   Test_Precision = test_prec, 
                                                   Train_Recall = train_rec, 
                                                   Test_Recall = test_rec, 
                                                   Train_F1 = train_f1, 
                                                   Test_F1 = test_f1))
}
cat("\n=======================================\n")

#printing the confusion matrix and statistics
print(cm)
print(train_cm)

cat("\n=======================================\n")

#printing table comparing all three values of k
cat("Final Performance Comparison\n")
print(results_table)

library(ggplot2)
library(tidyr)

#creating a plot of accuracy vs k
accuracy_data <- results_table %>%
  select(k, Train_Accuracy, Test_Accuracy) %>%
  pivot_longer(cols = c(Train_Accuracy, Test_Accuracy), names_to = "Dataset", values_to = "Accuracy")

acc_plot <- ggplot(accuracy_data, aes(x = factor(k), y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  theme_minimal() +
  labs(title = "Model Accuracy: Training vs. Testing", 
       x = "k (Number of Neighbors)", 
       y = "Accuracy Score") +
  scale_fill_manual(values = c("Test_Accuracy" = "darkred", "Train_Accuracy" = "lightblue"))

print(acc_plot)

#selecting columns to include in the bar plot of precision and recall
#reshaping and assigning names for plot
pr_data <- results_table %>%
  select(k, Test_Precision, Test_Recall) %>%
  pivot_longer(cols = c(Test_Precision, Test_Recall), names_to = "Metric", values_to = "Score")

#plotting precision and recall vs k
pr_plot <- ggplot(pr_data, aes(x = factor(k), y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  theme_minimal() +
  labs(title = "Test Precision and Recall by k-Value", 
       x = "k (Number of Neighbors)", 
       y = "Score") +
  scale_fill_manual(values = c("Test_Precision" = "pink", "Test_Recall" = "blue"))

print(pr_plot)
