
-----------------------------------------------------------------------------------------
############ Checkpoint-1: Data Understanding and Preparation of Master File ############
-----------------------------------------------------------------------------------------
  
internet_data <- read.csv("internet_data.csv", stringsAsFactors=FALSE, header = TRUE)
View(internet_data)

`customer_data.(1)` <- read.csv("customer_data (1).csv", stringsAsFactors=FALSE,header = TRUE)
View(`customer_data.(1)`)

churn <- read.csv("churn_data.csv", stringsAsFactors=FALSE, header = TRUE)
View(churn_data)

# By viewing all the three datasets, the common key column is customerID. Hence, the datasets
# shall be merged by customerID.

## Merging datasets:
churn1 <- merge(internet_data, `customer_data.(1)`, by = "customerID")
churn <- merge(churn1, churn, by = "customerID")

str(churn)
# This indicates that there are 7043 observations and 21 variables including one key 
# variable "customerID".
# Moreover, there are many variables which are of character datatype instead of factor.

summary(churn)


-----------------------------------------------------------------------------------------
####################### Installing and Loading Required Packages ########################
-----------------------------------------------------------------------------------------

  library(ggplot2)
  library(MASS)
  library(car)
  library(e1071)
  library(ROCR)
  library(caret)
  library(class)
  library(Hmisc)
  library(pROC)
  

-----------------------------------------------------------------------------------------
######################## Checkpoint-2: Exploratory Data Analysis ########################
-----------------------------------------------------------------------------------------
  
  ## Partner 
  
  ggplot(churn, aes( x = Partner, fill = factor(Churn))) + 
  geom_bar(position = "Dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),"%\n",..count..), y=0.5*..count..),
             geom="text", colour="black", 
             size=4, 
             position=position_dodge(width=1), 
             vjust = -0.3)

### Customers who does not have partners are more likely to churn and the churn rate is almost double
### to that of customers who have a partner.

## Dependents

ggplot(churn, aes( x = Dependents, fill = factor(Churn))) + 
  geom_bar(position = "Dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),"%\n",..count..), y=0.5*..count..), 
             geom="text", colour="black", 
             size=4, 
             position=position_dodge(width=1), 
             vjust = -0.3)

## Customers who does not have dependents are most likely to churn.
## Customers who does not have dependents are 5 times more likely to churn compared to customers who have dependents.

## Tenure 

ggplot(churn, aes( x = tenure, fill = factor(Churn))) + 
  geom_histogram(binwidth = 1, position = "dodge")+ 
  scale_x_continuous(breaks = c(1:6))

## Customers whose tenure is between 1 and 5 mostly tend to churn.
## The customers who churn tend to reduce as the tenure increases.

  ## InternetService

ggplot(churn, aes( x = InternetService,fill = factor(Churn))) + 
  geom_bar(position = "Dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),"%\n",..count..), y=0.5*..count..), 
             geom="text", colour="black", 
             size=4, 
             position=position_dodge(width=1), 
             vjust = -0.3)

## Customers who use Fibre optics tend to churn the most.
## Number of customers choosing the DSL Type of connection are more and are less likely to churn.
## Almost 73% of all the customers who choose the Fiberoptic type of connection are tending to churn.

## OnlineSecurity

ggplot(churn, aes( x = OnlineSecurity,fill = factor(Churn))) + 
  geom_bar(position = "Dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),"%\n",..count..), y=0.5*..count..), 
             geom="text", colour="black", 
             size=4, 
             position=position_dodge(width=1), 
             vjust = -0.3)

## Customers who does not take Online security tend to churn the most.

## OnlineBackup

ggplot(churn, aes( x = OnlineBackup,fill = factor(Churn))) + 
  geom_bar(position = "Dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),"%\n",..count..), y=0.5*..count..), 
             geom="text", colour="black", 
             size=4, 
             position=position_dodge(width=1), 
             vjust = -0.3)

## Customers who does not take OnlineBackup tend to churn the most.

##  Contract 

ggplot(churn, aes( x = Contract,fill = factor(Churn))) + 
  geom_bar(position = "Dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),"%\n",..count..), y=0.5*..count..), 
             geom="text", colour="black", 
             size=4, 
             position=position_dodge(width=1), 
             vjust = -0.3)

## Customers whose contract is month to month mostly tend to churn.
## Almost 75% of the customer show choose month to month type contract tend to churn.
## As the contract time increases less number of customers tend to churn.

## PaymentMethod 

ggplot(churn, aes( x = PaymentMethod,fill = factor(Churn))) + 
  geom_bar(position = "Dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),"%\n",..count..), y=0.5*..count..), 
             geom="text", 
             colour="black", 
             size=4, 
             position=position_dodge(width=1), 
             vjust = -0.3)

## Customers who use Electronic check mode of payment mostly tend to churn.

## MonthlyCharges 

ggplot(churn, aes( x = MonthlyCharges,fill = factor(Churn))) + 
  geom_bar(position = "Dodge", binwidth = 05) +
  scale_x_continuous(breaks = c(15:25,45:50, 58:69,70:105))

## Customers whose Monthly charges are between 70 and 105 Mostly tend to churn.
## Customers whose montly charges are between 45 and 50 tend to churn in second place.
## customers whose montlycharges is between 15 and 25 Mostly donot tend to churn.
## Customers whose Monthly charges are between 58 and 69 also donot mostly tend to churn 
#  and are in second place of non churned customers  
  
  
-----------------------------------------------------------------------------------------
############################ Checkpoint-3: Data Preparation #############################
-----------------------------------------------------------------------------------------

  ############### De-duplication of Data:
  
# The customer IDs as are unique key columns the duplication of data shall be checked 
# on it.

sum(duplicated(churn$customerID))
# The number of duplicated rows are 0. Hence, it shall be concluded that the data does 
# not contain any duplicated values.
  
  ############### Data Type Transformation:
  
# Rechecking the structure of "churn" dataset.

str(churn)

# 1) customerID:
churn$customerID <- as.factor(churn$customerID)

# 2) MultipleLines
churn$MultipleLines <- as.factor(churn$MultipleLines)

# 3) InternetService
churn$InternetService <- as.factor(churn$InternetService)

# 4) OnlineSecurity
churn$OnlineSecurity <- as.factor(churn$OnlineSecurity)

# 5) OnlineBackup
churn$OnlineBackup <- as.factor(churn$OnlineBackup)

# 5) DeviceProtection
churn$DeviceProtection <- as.factor(churn$DeviceProtection)

# 6) TechSupport
churn$TechSupport <- as.factor(churn$TechSupport)

# 7) StreamingTV
churn$StreamingTV <- as.factor(churn$StreamingTV)

# 8) StreamingMovies
churn$StreamingMovies <- as.factor(churn$StreamingMovies)

# 9) gender
churn$gender <- as.factor(churn$gender)
  
# 10) SeniorCitizen
churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)

# 11) Partner
churn$Partner <- as.factor(churn$Partner)

# 12) Dependents
churn$Dependents <- as.factor(churn$Dependents)

# 13) PhoneService
churn$PhoneService <- as.factor(churn$PhoneService)

# 14) Contract
churn$Contract <- as.factor(churn$Contract)

# 15) PaperlessBilling
churn$PaperlessBilling <- as.factor(churn$PaperlessBilling)

# 16)  PaymentMethod
churn$PaymentMethod <- as.factor(churn$PaymentMethod)

# 17) Churn
churn$Churn <- as.factor(churn$Churn)

# Rechecking the structure again
str(churn)

# All the variables are in correct format.

  
  ############### Missing Value Imputation:

# The "churn" dataset is checked for missing values across all the columns individually 
# and combindly.

sapply(churn, function(x) sum(is.na(x)))
sum(is.na(churn))

# There are 11 missing values in "TotalCharges" column.
typeof(churn$TotalCharges)
# Total charges is of double type.
summary(churn$TotalCharges)
# From the summary of churn dataset it is evident that:
# Median: 1397.0
# Mean  : 2283.0

# As the totalcharges column is the total billing amount, thus, the NA values can be 
# replaced by the mean
churn$TotalCharges[which(is.na(churn$TotalCharges) == TRUE)] <- mean(churn$TotalCharges, na.rm = T)


  ############### Outlier Treatment:

# Outliers shall be checked for the three continuous variables.

# 1) tenure:

boxplot(churn$tenure)

quantile(churn$tenure, probs = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 
                                 0.8, 0.9, 0.95, 0.99))
# Outliers are not observed in "tenure".

# 2) MonthlyCharges:

boxplot(churn$MonthlyCharges)

quantile(churn$MonthlyCharges, probs = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 
                                 0.8, 0.9, 0.95, 0.99))
# Outliers are not observed in MonthlyCharges.

# 3) TotalCharges:

boxplot(churn$TotalCharges)

quantile(churn$TotalCharges, probs = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 
                                 0.8, 0.9, 0.95, 0.96,0.97,0.98,0.99))

# Outliers are not observed in TotalCharges.


-----------------------------------------------------------------------------------------
############################# Checkpoint 4: Model Building ##############################
-----------------------------------------------------------------------------------------
  
  
  ############### KNN Model:--------------------
  
#  I) Data Preparation:  
  
#  Creation of Dummy Variables:
# Dummy Variables shall be created for all the variables with factor as th datatype.
 
# 1) MultipleLines:
  dummy_1 <- data.frame(model.matrix(~churn$MultipleLines, data = churn))
  View(dummy_1)  
# The first column "X.Intercept" needs to be removed from dummy_1. This removal of 
# column needs to be done for all the dummies. 
  dummy_1 <- dummy_1[, -1]
  
# 2) InternetService:
  dummy_2 <- data.frame(model.matrix(~churn$InternetService, data = churn))
  dummy_2 <- dummy_2[, -1]

# 3) OnlineSecurity:
  dummy_3 <- data.frame(model.matrix(~churn$OnlineSecurity, data = churn))
  dummy_3 <- dummy_3[, -1]

# 4) OnlineBackup:
  dummy_4 <- data.frame(model.matrix(~churn$OnlineBackup, data = churn))
  dummy_4 <- dummy_4[, -1]
  
# 5) DeviceProtection:
  dummy_5 <- data.frame(model.matrix(~churn$DeviceProtection, data = churn))
  dummy_5 <- dummy_5[, -1]

# 6) TechSupport:
  dummy_6 <- data.frame(model.matrix(~churn$TechSupport, data = churn))
  dummy_6 <- dummy_6[, -1]

# 7) StreamingTV:
  dummy_7 <- data.frame(model.matrix(~churn$StreamingTV, data = churn))
  dummy_7 <- dummy_7[, -1]
  
# 8) StreamingMovies:
  dummy_8 <- data.frame(model.matrix(~churn$StreamingMovies, data = churn))
  dummy_8 <- dummy_8[, -1]

# 9) gender:
  dummy_9 <- data.frame(model.matrix(~churn$gender, data = churn))
  dummy_9 <- dummy_9[, -1]
  dummy_9 <- as.data.frame(dummy_9)
  names(dummy_9)[names(dummy_9) == 'dummy_9'] <- 'churn.genderMale'
  
# 10) SeniorCitizen:
  dummy_10 <- data.frame(model.matrix(~churn$SeniorCitizen, data = churn))
  dummy_10 <- dummy_10[, -1]
  dummy_10 <- as.data.frame(dummy_10)
  names(dummy_10)[names(dummy_10) == 'dummy_10'] <- 'churn.SeniorCitizen1'
  
# 11) Partner:
  dummy_11 <- data.frame(model.matrix(~churn$Partner, data = churn))
  dummy_11 <- dummy_11[, -1]
  dummy_11 <- as.data.frame(dummy_11)
  names(dummy_11)[names(dummy_11) == 'dummy_11'] <- 'churn.PartnerYes'
  
# 12) Dependents:
  dummy_12 <- data.frame(model.matrix(~churn$Dependents, data = churn))
  dummy_12 <- dummy_12[, -1]
  dummy_12 <- as.data.frame(dummy_12)
  names(dummy_12)[names(dummy_12) == 'dummy_12'] <- 'churn.DependantsYes'
  
# 13) PhoneService:
  dummy_13 <- data.frame(model.matrix(~churn$PhoneService, data = churn))
  dummy_13 <- dummy_13[, -1]
  dummy_13 <- as.data.frame(dummy_13)
  names(dummy_13)[names(dummy_13) == 'dummy_13'] <- 'churn.PhoneServiceYes'
  
# 14) Contract:
  dummy_14 <- data.frame(model.matrix(~churn$Contract, data = churn))
  dummy_14 <- dummy_14[, -1]

# 15) PaperlessBilling:
  dummy_15 <- data.frame(model.matrix(~churn$PaperlessBilling, data = churn))
  dummy_15 <- dummy_15[, -1]
  dummy_15 <- as.data.frame(dummy_15)
  names(dummy_15)[names(dummy_15) == 'dummy_15'] <- 'churn.PaperlessBillingYes'
  
# 16) PaymentMethod:
  dummy_16 <- data.frame(model.matrix(~churn$PaymentMethod, data = churn))
  dummy_16 <- dummy_16[, -1]
  
# Dummy variables are created for all the factor variables.
  
# Combining all the dummies and continuous variables.
# As customerID is not an important variable affecting the churning of customers, 
# it shall be removed from the churn_final dataset.
  
  churn_SVM <- churn
  
  churn <- churn[,c(14,19,20,21)]
  
  churn <- cbind(churn, dummy_1, dummy_2, dummy_3, 
                       dummy_4, dummy_5, dummy_6, dummy_7, dummy_8, dummy_9, 
                       dummy_10, dummy_11, dummy_12, dummy_13, dummy_14,
                       dummy_15, dummy_16)
  
  
## Scaling Continuous Variables  

  # The continuous variables are scaled to reduce the effect of the magnitude of the variables 
  # affecting the model. Thus, they are standardized.
  
# 1) tenure:
  churn$tenure <- scale(churn$tenure)
  
# 2) MonthlyCharges
  churn$MonthlyCharges <- scale(churn$MonthlyCharges)
  
# 3) 
  churn$TotalCharges <- scale(churn$TotalCharges)

# Analyzing the structure and summary of "churn_final" dataframe.
  str(churn)
  summary(churn)
  # There are finally 30 variables and one response variable and 7043 observations.
  
  #  II) Splitting Data: 
  set.seed(100)
  indices <- sample(1:nrow(churn),0.7*nrow(churn))
  # Creating training dataset:
  train <- churn[indices, ]
  View(train)
  # Creating testing dataset:
  test <- churn[-indices, ]
  View(test)
  # Creating class of true labels:
  response <- train[, 4]
  response1 <- test[,4]
  

  
  # III) KNN-Modelling
  #KNN - 16 Nearest Neighbours
  
  impknn5 <- knn(train[,-4], test[,-4], response, k = 16, prob = TRUE)
  
  summary(impknn5)
  
  table(impknn5,response1)
  
  confusionMatrix(impknn5, response1, positive ="Yes")
  
  train_labels <- train$Churn
  
  test_labels <- test$Churn
  
  churning <- ifelse(test$Churn == "No", 1,0)

  pred <- prediction(attr(impknn5, "prob"),churning)
  
  perf <- performance(pred,"tpr", "fpr")
  
  ## plotting the ROC Curve 
  
  plot(perf,colorize = T,print.cutoffs.at=seq(0,1,0.25))
  
  ## calculating auc 
  
  auc(test$Churn, attr(impknn5, "prob"))
  
  ## Area under the curve is 0.7405
  
  train <- train[,-32]
  
  model <- train(train$Churn~., data = train, method = 'knn', tuneGrid = expand.grid(.k = 1:50), 
                metric = 'Accuracy', trControl = trainControl(method='repeatedcv', 
                  number=10, repeats=15))
  
  ## plot(model)
  
  
  
  
  ############### Naive-Bayes Model:--------------------

  # Removing the label column (type of mushroom) from the test data
  test1 <- test[, -4]
  
  # Now we will run Naive Bayes algorithm on this data: Using the e1071 package
  model <- naiveBayes(Churn ~. , data = train)
  
  # The model shall be validated with each observation in the test data. 
  pred <- predict(model, test1)
  table(pred, test$Churn)
  # Considering Churned customers as positive
  # The table() represents that :
  #     1) Churned Actually - Predicted Churned           :  458   (True Positive Rate)
  #     2) Did not Churn Actually - Predicted Churned     :  437   (False Positive Rate)
  #     3) Churned Actually - Predicted Not Churned       :  118   (True Negative Rate)
  #     4) Did not Churn Actually - Predicted Not Churned :  1100  (False Positive Rate)
  
  confusionMatrix(pred, test$Churn)
  # The confusion matrix indicates:
  #     1) Accuracy       :   0.664 
  #     2) Sensitivity    :   0.5875 
  #     3) Specificty     :   0.8681 
  
  # ROC with probabilities
  
  pred_raw <- predict(model, test1, type = "raw")
  pred_probab <- pred_raw[, 2]
  churning <- ifelse(test$Churn == "Yes", 1, 0)
  predict1 <- prediction(pred_probab, churning)
  prf <- performance(predict1, "tpr", "fpr")
  plot(prf,colorize = T,print.cutoffs.at=seq(0,1, by = 0.25))
  
  auc(test$Churn, pred_probab)  
  
  ## Area under the curve: 0.8358

  
  ############### Logistic Regression Model:--------------------
  
  # I) Logistic Regression - Modelling
  
  model_1 <- glm(formula = Churn ~., data = train, family = "binomial")
  
  summary(model_1)
  
  best_model = stepAIC(model_1, direction = "both")
  
  # The best_model obtained from stepAIC() is renamed as model_2.
  model_2 <- best_model
  model_2
  summary(model_2)
  vif(model_2)

  

  # Eliminating TotalCharges as this has low significance and High VIF.
  
  
  model_3 <- glm(formula = Churn ~ tenure + churn.MultipleLinesNo.phone.service + 
                   churn.MultipleLinesYes + churn.InternetServiceFiber.optic + 
                   churn.InternetServiceNo + churn.OnlineSecurityYes + churn.TechSupportYes + 
                   churn.StreamingTVYes + churn.StreamingMoviesYes + churn.SeniorCitizen1 + 
                   churn.ContractOne.year + churn.ContractTwo.year + churn.PaperlessBillingYes + 
                   churn.PaymentMethodElectronic.check, family = "binomial", 
                 data = train)
  
  summary(model_3)
  
  vif(model_3)
  
  ## Removing churn.StreamingTVYes 
  
  model_4 <- glm(formula = Churn ~ tenure + churn.MultipleLinesNo.phone.service + 
                   churn.MultipleLinesYes + churn.InternetServiceFiber.optic + 
                   churn.InternetServiceNo + churn.OnlineSecurityYes + churn.TechSupportYes + 
                   churn.StreamingMoviesYes + churn.SeniorCitizen1 + 
                   churn.ContractOne.year + churn.ContractTwo.year + churn.PaperlessBillingYes + 
                   churn.PaymentMethodElectronic.check, family = "binomial", 
                 data = train)
  
  summary(model_4)
  
  vif(model_4)

 ## all the variables seem to be significant.
  
  # Hence, the final_model is considered as model_4
  final_model <- model_4
  
  # II) Logistic Regression - Model Evaluation
  
  #A) #### C-statistic Computation: Training Dataset
  # The churning is predicted by the final model and stored in "predChurn" in "train" dataset.
  
  train$predChurn <- predict(final_model, type = "response")
  
  # Computing C-statistic:
  
  rcorr.cens(train$predChurn, train$Churn)
  
  # C-statistic is 8.427534e-01 (approx. 84%) in the training dataset.
  # Hence, it can be concluded that the model has higher discriminative power with the 
  # training dataset.
  
  ##### C-statistic Computation: Testing Dataset
  # The churning is predicted by the final model and stored in "predChurn" in "test" dataset.
  
  test$predChurn <- predict(final_model, type = "response", newdata = test)
  
  # Computing C-statistic:
  rcorr.cens(test$predChurn, test$Churn)
  # C-statistic is 8.496090e-01 (approx. 85%) in the testing dataset.
  # Hence, it can be concluded that the model has higher discriminative power with the 
  # testing dataset.
  
  
  #B) #### KS-statistic Computation: Training Dataset
  
  model_score <- prediction(train$predChurn,train$Churn)
  model_perf <- performance(model_score, "tpr", "fpr")
  ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])
  ks = max(ks_table)
  ks
  # 0.530151 is the value of the KS-Statistic obtained with the train dataset.
  # Calculation of Decile Number:
  which(ks_table == ks)
  1279/nrow(train) # = 0.259432 
  # This implies that the KS-statistic lies in the "Third" decile.
  
  # Test Dataframe:
  model_score_test <- prediction(test$predChurn,test$Churn)
  model_perf_test <- performance(model_score_test, "tpr", "fpr")
  ks_table_test <- attr(model_perf_test, "y.values")[[1]] - 
    (attr(model_perf_test, "x.values")[[1]])
  ks_test = max(ks_table_test)
  ks_test
  # 0.5411821 is the value of the KS-Statistic obtained with the test dataset.
  # The KS-statistic lies in the
  which(ks_test == ks_table_test)
  634 / nrow(test) # = 0.3000473
  # This implies that the KS-statistic lies in the "third" decile for the test dataframe.
  
  #C) #### ROC curve plotting
  
  plot(model_perf, colorize = T, lwd = 5,print.cutoffs.at=seq(0,1, by = 0.25))
  
  
  #D) #### Calculating AUC (Area Under Curve)
  auc(train$Churn, train$predChurn)
  # Area under the curve is 0.8426. This indicates that the model has good predictive \
  # ability with the training data.
  
  auc(test$Churn, test$predChurn)
  # Area under the curve is 0.849. This indicates that the model has good predictive \
  # ability with the testing data.
  
  # As the difference between training and testing data is negligible thus it can be 
  # concluded that the predictive ability of the model is 84.5%.
  
  
  #E) #### Creating Confusion Matrix
  train$Churn <- ifelse(train$Churn == "Yes", 1, 0)
  # Threshold: 0.3
  confusionMatrix(as.numeric(train$predChurn > 0.3), train$Churn, positive = "1")
    # Accuracy    :   0.7667
    # Specificity :   0.7718
    # Sensitivity :   0.7479
 
  ## #E) #### Creating Confusion Matrix
  test$Churn <- ifelse(test$Churn == "Yes", 1, 0)
  # Threshold: 0.3
  confusionMatrix(as.numeric(test$predChurn > 0.3), test$Churn, positive = "1")
  # Accuracy : 0.77
  # Sensitivity : 0.7500
  # Specificity : 0.7775
  
## we choose threshold 0.3
  
  ###### SVM ##########
  
  ## Scaling Continuous Variables  
  
  # The continuous variables are scaled to reduce the effect of the magnitude of the variables 
  # affecting the model. Thus, they are standardized.
  
  # 1) tenure:
  churn_SVM$tenure <- scale(churn_SVM$tenure)
  
  # 2) MonthlyCharges
  
  churn_SVM$MonthlyCharges <- scale(churn_SVM$MonthlyCharges)
  
  # 3) 
  churn_SVM$TotalCharges <- scale(churn_SVM$TotalCharges)
  
  set.seed(100)
  indices <- sample(1:nrow(churn_SVM),0.7*nrow(churn_SVM))
  # Creating training dataset:
  train_SVM <- churn_SVM[indices, ]
  View(train_SVM)
  # Creating testing dataset:
  test_SVM <- churn_SVM[-indices, ]
  View(test_SVM)
  # Creating class of true labels:
  response_SVM <- train_SVM[, 4]
  response1_SVM <- test_SVM[,4]
 
  # finding the optimal value of cost using cross-validation
  # svm cross-validation is done using the tune function
  # result: higher costs yield lower error rates
  
  ## tune.svm = tune(svm, Churn ~., data = train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 10, 100,200,500)))
  ## summary(tune.svm)
  
  # After running the tune function we find that the optimal function has a cost =  200
  # so we create new model with cost =  200 
  
  model.svm.1 = svm(Churn ~., data = train_SVM, kernel = "linear", cost =  200, scale = F)  
  plot(model.svm.1, test_SVM)
  summary(model.svm.1)
  
  ## best parameters:
  ## cost =  200
  
  
  # predicting test classes using the best model and analyzing the table
  # best.model is the one with cost =  200

  
  ypred <- predict(model.svm.1, newdata = test_SVM)
  
  test_SVM$PC <- ypred
  
  table(predicted = ypred, truth = test_SVM$Churn)
  
  confusionMatrix(ypred, test_SVM$Churn, positive = "Yes")
  

  ## Observed performance parameters 
  ## Sensitivity : 0.5017          
  ## Specificity : 0.9226 
  ## Accuracy : 0.8079
  

  
  
  