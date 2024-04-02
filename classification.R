library(e1071)
library(tidyverse)
library(caret)
library(glmnet)
library(naivebayes)
library(rpart)
library(nnet)
library(neuralnet)
library(adabag)
library(randomForest)
library(xgboost)
library(devtools)
library(performanceEstimation)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

# ==============================================================================
# Models
# ==============================================================================

# Support Vector Machines
# 86.8% accuracy
model_svm <- function(ds) {
  model <- svm(questionable_domain ~ ., train, type = 'C-classification', kernel = 'radial')
  return(model)
}

# Artificial Neural Network
# 86.5% accuracy
model_nn <- function(train) {
  # remove factor variables
  train <- train[-c(3,6)]
  # scale numeric variables
  train[, -1] <- scale(train[, -1])
  model <- nnet(questionable_domain ~ ., train, size=10, rang = 0.1,
                decay = 1e-2, maxit = 100)
  return(model)
}

# XGBoost
# 89.3% accuracy
model_xgboost <- function(train) {
  model <- xgboost(data = data.matrix(train[,-1]),
                label = as.numeric(train[,1][[1]])-1,
                max.depth = 16, eta = 0.2, nthread = 2,
                nrounds = 170,
                objective = "binary:logistic",
                eval_metric = "logloss")
  return(model)
}

# Random Forest
# 88.9% accuracy
model_randomForest <- function(train) {
  model <- randomForest(questionable_domain ~ ., train, do.trace=TRUE, importance = T)
  return(model)
}

# ==============================================================================
# Evaluation Metrics
# ==============================================================================

# Analyse the effect of scale parameter on the support vector machine model's performance
# using 10-fold cross validation
svm_performance_scale <- function(ds){
  performance <- performanceEstimation(PredTask(questionable_domain ~ ., ds), 
                                             workflowVariants(learner = "svm",learner.pars = list(scale = c(TRUE, FALSE))), 
                                             EstimationTask(metrics = c("F"),method = CV()))
  return(performance)
}

# Analyse the effect of different kernels on the support vector machine model's performance
# using 10-fold cross validation
svm_performance_kernels <- function(ds){
  performance <- performanceEstimation(PredTask(questionable_domain ~ ., ds), 
                                       workflowVariants(learner = "svm",learner.pars = list(kernel = c("linear", "polynomial", "radial", "sigmoid"))), 
                                       EstimationTask(metrics = c("F"),method = CV()))
  return(performance)
}

# Analyse the effect of cost and gamma parameters on the support vector machine model's performance
# using 10-fold cross validation
svm_performance_kernels <- function(ds){
  performance <- performanceEstimation(PredTask(questionable_domain ~ ., ds), 
                                       workflowVariants(learner = "svm",learner.pars = list(cost = 1:5, gamma = c(0.1, 0.01))), 
                                       EstimationTask(metrics = c("F"),method = CV()))
  return(performance)
}

# Analyse the effect of scale parameter on the artificial neural network model's performance
# using 10-fold cross validation
nn_performance_scale <- function(ds){
  performance <- performanceEstimation(PredTask(questionable_domain ~ ., ds), 
                                       workflowVariants(learner = "nnet",
                                                        learner.pars = list(size=c(1,5), rang=c(0.5,0.1,0.05), decay=c(0.001,0.1)),
                                                        predictor.pars = list(type = "class")), 
                                       EstimationTask(metrics = c("F"),method = CV()))
  return(performance)
}

# show statistics for support vector machine's model
evaluate_svm <- function(model, test){
  prediction <- predict(model, test[, -1])
  return(confusionMatrix(prediction, test$questionable_domain))
}

# show statistics for artificial neural network's model
evaluate_nn <- function(model, test){
  test <- test[-c(3,6)]
  test[, -1] <- scale(test[, -1])
  prediction <- predict(model, test[, -1], type = "class")
  plot.nnet(model)
  return(confusionMatrix(as.factor(as.numeric(prediction > 0.5)), test$questionable_domain))
}

# show statistics for xgboost's model
evaluate_xgboost <- function(model, test){
  prediction <- predict(model, data.matrix(test[, -1]), reshape=T)
  xgb.plot.importance(xgb.importance(model=model))
  return(confusionMatrix(factor(as.numeric(prediction > 0.5)), test$questionable_domain))
}

# show statistics for random forest's model
evaluate_randomForest <- function(model, test) {
  prediction <- predict(model, test[, -1])
  varImpPlot(model, main = "Feature Relevance Scores")
  return(confusionMatrix(prediction, test$questionable_domain))
}

# Obtain the modified dataset
altered_dataset <- read_csv("altered_dataset.csv")
altered_dataset <- altered_dataset %>%
  mutate(across(c("questionable_domain", "user_verified", "contains_profanity"), ~as.factor(.)))

# Split the dataset into train and test partitions
set.seed(123)
train_pos <- createDataPartition(altered_dataset$questionable_domain, p=0.7)
train <- altered_dataset[train_pos[[1]], ]
test <- altered_dataset[-train_pos[[1]], ]

# Train the 4 models
svm_model <- model_svm(train)
nn_model <- model_nn(train)
randomForest_model <- model_randomForest(train)
xgboost_model <- model_xgboost(train)

# Templates to evaluate each model
#svm_results <- evaluate_svm(svm_model, test)
#nn_results <- evaluate_nn(nn_model, test)
#xgboost_results <- evaluate_xgboost(xgboost_model, test)
#randomForest_results <- evaluate_randomForest(randomForest_model, test)