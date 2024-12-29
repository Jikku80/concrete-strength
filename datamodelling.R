install.packages("mice")
library('mice')
install.packages("ranger")
library('ranger')
install.packages("psych")
library(psych)

train <- read.csv("C:/Users/Dell/Downloads/concrete_strength_train.csv")
test <- read.csv("C:/Users/Dell/Downloads/concrete_strength_test.csv")

train$isTrain <- "yes"
test$isTrain <- "no"

combined <- rbind(train, test)
#View(concrete_strength_datasets)  # View the combined dataset
summary(combined)  # Summary statistics of the combined dataset
describe(combined)  # Descriptive statistics of the combined dataset

# Check for missing values in the concrete_strength_datasets dataframe
is.na(combined)
# Summarize the total number of missing values in the dataset
sum(is.na(combined))
sum(is.na(combined$Cement))

# Calculate the proportion of missing values for each column
miss <- apply(combined, 2, 
              function(x) sum(is.na(x)) / nrow(combined))
# Calculate the average missingness across all columns
missingness <- mean(miss)
missingness

md.pattern(combined)

#Q5
#****************************************data imputation using mice***************************

#data imputaion using mean method
imputed_mean<-mice(data=combined,m=1,method="mean",maxit=10)
mean_imputed_dataset<-complete(imputed_mean)
sum(is.na(mean_imputed_dataset))
md.pattern(mean_imputed_dataset)
summary(mean_imputed_dataset)  
describe(mean_imputed_dataset) 

# using rf method
imputed_data <- mice(data = combined, m = 1, method="rf", maxit=10)
concreteImputed <- complete(imputed_data)
View(concreteImputed)
missingSum <- sum(is.na(concreteImputed))
missingSum
md.pattern(concreteImputed)

sum(is.na(concreteImputed$Cement))

# Create empty data frame to store outliers
outliers <- data.frame()
# Loop through each column
for (i in 1:9) {
  # Calculate quartiles and IQR for each column
  Q1 <- quantile(mean_imputed_dataset[[i]], 0.25)
  Q3 <- quantile(mean_imputed_dataset[[i]], 0.75)
  IQR <- Q3 - Q1
  # Calculate lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Subset outliers for each column and bind them to the 'outliers' data frame
  outliers <- rbind(outliers, mean_imputed_dataset[mean_imputed_dataset[[i]] < lower_bound | mean_imputed_dataset[[i]] > upper_bound, ])
}
# Remove duplicate rows from the outliers data frame
outliers <- unique(outliers)
outliers
#**********************remove outlier******************************

outlier_removed_dataset <- mean_imputed_dataset[!(rownames(mean_imputed_dataset) %in% rownames(outliers)), ]
describe(outlier_removed_dataset)
summary(outlier_removed_dataset)

View(outlier_removed_dataset)
summary(mean_imputed_dataset)
sum(is.na(mean_imputed_dataset))

cleaned_dataset<-outlier_removed_dataset
str(cleaned_dataset)

#******************************** Q6. Data Modeling
#*******************************KNN
# Remove the last column from the dataset
KnnDataset <- cleaned_dataset[, -ncol(cleaned_dataset)]

# Load necessary libraries
install.packages("caret")
library(caret)

# Define normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Assuming `cleaned_dataset` is your dataset

# Normalize columns 1 to n-1 (excluding the target variable)
normalizedKnnDataset <- as.data.frame(lapply(KnnDataset[, -ncol(KnnDataset)], normalize))

# Add the target variable to the normalized dataset
normalizedKnnDataset$Strength <- KnnDataset$Strength

# Split data into features (X) and target variable (y)
X <- normalizedKnnDataset[, -ncol(normalizedKnnDataset)]  # Features
y <- normalizedKnnDataset$Strength  # Target variable

# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Train KNN regression model using cross-validation (5-fold)
set.seed(123)  # For reproducibility
knnModel <- train(
  x = X_train,                       # Features
  y = y_train,                       # Target variable
  method = "knn",                    # KNN regression method
  trControl = trainControl(method = "cv", number = 5),  # 5-fold cross-validation
  tuneGrid = expand.grid(k = 1:30)   # Search grid for K values from 1 to 30
)

# Get the optimal tuning parameters
best_k <- knnModel$bestTune$k

# Print the optimal k value
cat("Optimal k value:", best_k, "\n")

# Predict on test set
y_pred <- predict(knnModel, newdata = X_test)

# Calculate residuals
residuals <- y_test - y_pred

# Calculate Root Mean Squared Error (RMSE)
RMSE <- sqrt(mean(residuals^2))

# Calculate R-squared
R_squared <- 1 - sum(residuals^2) / sum((y_test - mean(y_test))^2)

# Print RMSE and R-squared
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")

# Create a data frame to store performance metrics
performance_metrics <- data.frame(
  Metric = c("RMSE", "R-squared"),
  Value = c(RMSE, R_squared)
)

# Print the table of performance metrics
print(performance_metrics)

# Plot predicted vs. original values
plot(y_test, y_pred, main = "Predicted vs. Original Values",
     xlab = "Original Values", ylab = "Predicted Values")
abline(0, 1, col = "red")

# Plot residuals
plot(residuals, main = "Residuals Plot", xlab = "Index", ylab = "Residuals")

#*************************Random Forest****************

#checking for any missing values if yes gives true
any(is.na(cleaned_dataset))

install.packages("caret")
library(caret)

# Splitting into train and test
ind <-createDataPartition(cleaned_dataset$Strength, p=0.7, list=F)
rf_train<-cleaned_dataset[ind,]
rf_test<-cleaned_dataset[-ind,]

## Model Development using randomForest function
install.packages("randomForest")
library(randomForest)
cleaned_dataset.rf=randomForest(Strength ~ ., data = rf_train)
cleaned_dataset.rf

plot(cleaned_dataset.rf)

pred <- predict(cleaned_dataset.rf, rf_test)
metrics_rmse = RMSE(pred,rf_test$Strength)
metrics_r2 = R2(pred, rf_test$Strength)
metrics_MAE = MAE(pred, rf_test$Strength)
c(metrics_rmse,metrics_r2,metrics_MAE)

# Plot predicted vs observed 
df <-data.frame(pred=pred, obs=rf_test$Strength)
ggplot(df, aes(x=obs, y=pred))+geom_point()


# Model Optimisation
oob.err = double(9)
test.err = double(9)

for(mtry in 1:9) {
  rf=randomForest(Strength~ ., rf_train, mtry=mtry,ntree=500)
  oob.err[mtry] = rf$mse[500] #Error of all Trees fitted 
  pred<-predict(rf,rf_test) #Predictions on Test Set for each Tree 
  test.err[mtry]= mean( (rf_test$Strength - pred)**2) #Mean Squared Test Error
}

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("green","orange"),type="b",
        ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("green","orange"))

test.err
oob.err


#########
# Residual analysis
pred <- predict(cleaned_dataset.rf, rf_test)
residuals <- rf_test$Strength - pred

plot(x = pred, y = residuals, main = "Residuals vs. Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

qqnorm(residuals)
qqline(residuals)

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
for (col in colnames(rf_test)[-ncol(rf_test)]) {
  plot(x = rf_test[[col]], y = residuals, main = paste("Residuals vs.", col), xlab = col, ylab = "Residuals")
}


# Plot of predicted vs. original values
plot(rf_test$Strength, pred, main = "Predicted vs. Original Values", xlab = "Original Values", ylab = "Predicted Values")
abline(a = 0, b = 1, col = "red")  # Add a diagonal line for perfect predictions

#***************************************Decision tree*******************
#Descision Tree Modelling
install.packages("rpart.plot")
library(rpart)
library(caret)
set.seed(123)
index <- createDataPartition(cleaned_dataset$Strength, p = 0.8, list = FALSE)

dt_train <- cleaned_dataset[index, ]
dt_test <- cleaned_dataset[-index, ]

cart_fit <- rpart(Strength ~ . , data = dt_train)
summary(cart_fit)

library(rpart.plot)
rpart.plot(cart_fit)

#model evaluation
filtered_data_pred <- predict(cart_fit,dt_test,type="vector")
#confusionMatrix(data=filtered_data_pred,reference = test$Strength)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((filtered_data_pred - dt_test$Strength)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

plot(dt_test$Strength, filtered_data_pred, main = "Actual vs Predicted Strength",
     xlab = "Actual Strength", ylab = "Predicted Strength")
abline(0, 1, col = "red")  # Add a diagonal line for reference

#modeltuning
cart_fit$cptable
plotcp(cart_fit)

opt_filtered_data<- which.min(cart_fit$cptable[,'xerror'])
cp_filtered_data <- cart_fit$cptable[opt_filtered_data,'CP']
cp_filtered_data

pruned_fit<-prune(cart_fit,cp_filtered_data)
rpart.plot(pruned_fit)

pruned_predict<-predict(pruned_fit,dt_test,type = "vector")
pruned_rmse <- sqrt(mean((pruned_predict - dt_test$Strength)^2))
pruned_rmse


# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(pruned_predict - dt_test$Strength))
print(paste("Mean Absolute Error (MAE) for pruned model:", mae))

# Calculate R-squared (coefficient of determination)
SST <- sum((test$Strength - mean(dt_test$Strength))^2)
SSE <- sum((pruned_predict - dt_test$Strength)^2)
rsquared <- 1 - SSE/SST
print(paste("R-squared (R2) for pruned model:", rsquared))

#residual analysis
residuals <- dt_test$Strength - pruned_predict
plot(residuals, main = "Residuals Plot", xlab = "Observation", ylab = "Residuals")
plot(pruned_predict, residuals, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals)
qqline(residuals)
hist(residuals, prob = TRUE, main = "Histogram of Residuals", xlab = "Residuals")
lines(density(residuals), col = "red")


#original vs predicted plot
# Plot original vs predicted strength
plot(dt_test$Strength, pruned_predict, 
     main = "Original vs Predicted Strength",
     xlab = "Original Strength", 
     ylab = "Predicted Strength")
abline(0, 1, col = "red")  # Add a diagonal line for reference





#**************************************logistic**********************

#logistic regression
library(mlbench)

train_data <- cleaned_dataset[cleaned_dataset$isTrain == "yes", ]
test_data <- cleaned_dataset[cleaned_dataset$isTrain == "no", ]
library(mlbench)

#considering the threshold of high and low strength

q1 <- quantile(train_data$Strength, 0.25)
q3 <- quantile(train_data$Strength, 0.75)
print(q3)


filtered_df <- train_data[train_data$Strength < q3 | train_data$Strength > q1, ]
#View(filtered_df)


filtered_df$Strength <- ifelse(filtered_df$Strength > q3, 1, 0)


datasets_for_regression = filtered_df
datasets_for_regression$Strength = filtered_df$Strength

data_to_be_trained = datasets_for_regression
#View(datasets_for_regression)

# Assign filtered Strength values to datasets_for_regression



# Fit logistic regression model
logit <- glm(Strength ~ Cement, family = binomial, data = datasets_for_regression)
summary(logit)

library(tidyverse)
# Create dataset for plotting
train2 <- mutate(datasets_for_regression, prob = ifelse(Strength == 1, 1, 0))

# Plot logistic regression model
ggplot(train2, aes(Cement, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model",
    x = "Cement Concentration",
    y = "Probability of High Strength"
  )



#checking the probability for test data
newdata <- data.frame(Cement = test_data$Cement)
probabilities <- predict(logit, newdata, type = "response")
probabilities


# Load required libraries
library(pROC)

# Compute predicted probabilities for the test data
#newdata <- data.frame(Cement = test_data$Cement)
#probabilities <- predict(logit, newdata, type = "response")

# Create a ROC curve object
roc_obj <- roc(ifelse(test_data$Strength > q3, 1, 0), probabilities)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "green")

# Compute the AUC
auc_result <- auc(roc_obj)
print(paste("AUC:", auc_result))

# Residual plot
plot(logit, which = 1, col = "green", main = "Residual Plot")

# Original vs. Predicted plot
# Original vs. Predicted plot
predicted <- predict(logit, newdata, type = "response")
original <- ifelse(test_data$Strength > q3, 1, 0)  # Assuming your original dataset contains the Strength column
plot(original ~ predicted, col = "green", main = "Original vs. Predicted", xlab = "Predicted Probabilities", ylab = "Original Strength")


# Additional evaluation metrics
library(caret)
# Confusion matrix
# Confusion matrix
predicted_classes <- ifelse(predicted > 0.5, 1, 0)
actual_classes <- ifelse(test_data$Strength > q3, 1, 0)
confusionMatrix(table(predicted_classes, actual_classes))


# Accuracy
accuracy <- sum(predicted_classes == actual_classes) / length(predicted_classes)
print(paste("Accuracy:", accuracy))


# Sensitivity and Specificity
conf_matrix <- confusionMatrix(table(predicted_classes, actual_classes))
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))




#*************************************NAIVE BAYES

install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)

nb_cleaned_data <- cleaned_dataset

# Calculate threshold value
threshold_value <- quantile(nb_cleaned_data$Strength, 0.75)

# Create binary outcome variable
nb_cleaned_data$HighStrength <- ifelse(nb_cleaned_data$Strength >= threshold_value, "High", "Low")

set.seed(123)  # For reproducibility
train_index <- createDataPartition(nb_cleaned_data$HighStrength, p = 0.8, list = FALSE)  # 80% train, 20% test
train_data <- nb_cleaned_data[train_index, ]
test_data <- nb_cleaned_data[-train_index, ]

# Train the Naive Bayes model using caret with cross-validation
nb_fit <- train(
  HighStrength ~ .,  # Use your binary outcome variable as the dependent variable
  data = train_data,
  method = "naive_bayes",  # Specify the method as "naive_bayes" for Naive Bayes
  trControl = trainControl(method = "cv", number = 5),  # Cross-validation with 5 folds
  tuneLength = 10  # Tune the model with 10 parameter combinations
)
nb_fit

# Generate predicted probabilities for the test set
nb_test_pred_prob <- predict(nb_fit, newdata = test_data, type = "prob")

# Extract predicted probabilities for the "High" class
nb_test_pred_prob_high <- nb_test_pred_prob[, "High"]
#ROC

# Convert HighStrength to factor with levels "Low" and "High"
test_data$HighStrength <- factor(test_data$HighStrength, levels = c("Low", "High"))
# Create ROC curve
roc_obj <- roc(test_data$HighStrength, nb_test_pred_prob[, "High"], levels = c("High", "Low"), direction = "<")

plot(roc_obj, main = "ROC Curve for Naive Bayes Model")
auc_value <- auc(roc_obj)


#*****************************************

# Make predictions on the test set using the trained model
nb_predictions <- predict(nb_fit, newdata = test_data)

# Convert predicted values to factor with levels "Low" and "High"
nb_predictions <- factor(nb_predictions, levels = c("Low", "High"))

# Create confusion matrix
conf_matrix <- confusionMatrix(nb_predictions, test_data$HighStrength)

# Print confusion matrix
print(conf_matrix)

# Plot predicted vs. original values
plot(test_data$Strength, nb_test_pred_prob[, "High"], xlab = "Original Values", ylab = "Predicted Values", main = "Predicted vs. Original Values")
abline(0, 1, col = "red")  # Add a diagonal line for comparison

# Create a table with model performance metrics
performance_table <- data.frame(
  AUC = auc_value,
  Accuracy = conf_matrix$overall["Accuracy"],
  Sensitivity = conf_matrix$byClass["Sensitivity"],
  Specificity = conf_matrix$byClass["Specificity"],
  Balanced_Accuracy = conf_matrix$byClass["Balanced Accuracy"]
)

# Print the performance table
print(performance_table)
