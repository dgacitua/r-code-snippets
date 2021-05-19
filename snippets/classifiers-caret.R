# Install libraries
if(!require(here)) install.packages("here")
if(!require(import)) install.packages("import")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(parallel)) install.packages("parallel")
if(!require(doSNOW)) install.packages("doSNOW")
if(!require(ROSE)) install.packages("ROSE")
if(!require(C50)) install.packages("C50")
if(!require(naivebayes)) install.packages("naivebayes")
if(!require(randomForest)) install.packages("randomForest")
if(!require(e1071)) install.packages("e1071")
if(!require(kernlab)) install.packages("kernlab")
if(!require(nnet)) install.packages("nnet")

# Import libraries
library(here)
library(import)
library(dplyr)
library(caret)
library(parallel)
library(doSNOW)
library(ROSE)
library(C50)
library(naivebayes)
library(randomForest)
library(e1071)
library(kernlab)
library(nnet)

# Set working directory
setwd(here())
cat("Working Directory:", getwd(),"\n")

# Start parallel cluster with (SystemLogicalCores - 1) threads
cores <- detectCores(all.tests = FALSE, logical = TRUE) - 1
cl <- makeCluster(cores)
registerDoSNOW(cl)
cat("Running with", cores, "cores\n")

# Example dataset
ds <- read.csv("datasets/breastCancerWisconsin.csv")
ds$class <- as.factor(ds$class)
head(ds)

# Set training to 10-fold Cross Validation
ds.cv <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE, allowParallel = TRUE)


# Machine Learning Classifiers (Work In Progress)

## Logistic Regression
model.glm <- c()
run.glm <- TRUE

if (run.glm) {
  model.glm$clsf <- train(class ~ ., data = ds, method = "glm", family = "binomial", trControl = ds.cv)
  model.glm$data <- model.glm$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.glm$conf <- confusionMatrix(model.glm$data, ds$class, positive = "B")
  model.glm$roc <- roc.curve(ds$class, model.glm$data, plotit = FALSE)
  print(model.glm$conf)
  print(model.glm$roc)
}

## Decision tree (C5.0)
model.c50 <- c()
run.c50 <- TRUE

if (run.c50) {
  model.c50$clsf <- train(class ~ ., data = ds, method = "C5.0", trControl = ds.cv)
  model.c50$data <- model.c50$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.c50$conf <- confusionMatrix(model.c50$data, ds$class, positive = "B")
  model.c50$roc <- roc.curve(ds$class, model.c50$data, plotit = FALSE)
  print(model.c50$conf)
  print(model.c50$roc)
}

## Naïve Bayes
model.nb <- c()
run.nb <- TRUE

if (run.nb) {
  model.nb$clsf <- train(class ~ ., data = ds, method = "naive_bayes", trControl = ds.cv)
  model.nb$data <- model.nb$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.nb$conf <- confusionMatrix(model.nb$data, ds$class, positive = "B")
  model.nb$roc <- roc.curve(ds$class, model.nb$data, plotit = FALSE)
  print(model.nb$conf)
  print(model.nb$roc)
}

## Random Forest
model.rf <- c()
run.rf <- TRUE

if (run.rf) {
  model.rf$grid <- expand.grid(mtry = c(1:10))
  model.rf$clsf <- train(class ~ ., data = ds, method = "rf", trControl = ds.cv, tuneGrid = model.rf$grid)
  model.rf$data <- model.rf$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.rf$conf <- confusionMatrix(model.rf$data, ds$class, positive = "B")
  model.rf$roc <- roc.curve(ds$class, model.rf$data, plotit = FALSE)
  print(model.rf$conf)
  print(model.rf$roc)
}

## Support Vector Machine (linear)
model.svm <- c()
run.svm <- TRUE

if (run.svm) {
  model.svm$grid <- expand.grid(C = 2^seq(-4, 10, 1))
  model.svm$clsf <- train(class ~ ., data = ds, method = "svmLinear", trControl = ds.cv, tuneGrid = model.svm$grid)
  model.svm$data <- model.svm$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.svm$conf <- confusionMatrix(model.svm$data, ds$class, positive = "B")
  model.svm$roc <- roc.curve(ds$class, model.svm$data, plotit = FALSE)
  print(model.svm$conf)
  print(model.svm$roc)
}

## Support Vector Machine (radial)
model.svm2 <- c()
run.svm2 <- TRUE

if (run.svm2) {
  model.svm2$grid <- expand.grid(C = 2^seq(-4, 10, 1), sigma = 10^seq(-5, 5, 1))
  model.svm2$clsf <- train(class ~ ., data = ds, method = "svmRadial", trControl = ds.cv, tuneGrid = model.svm2$grid)
  model.svm2$data <- model.svm2$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.svm2$conf <- confusionMatrix(model.svm2$data, ds$class, positive = "B")
  model.svm2$roc <- roc.curve(ds$class, model.svm2$data, plotit = FALSE)
  print(model.svm2$conf)
  print(model.svm2$roc)
}

## Neural Network
model.nn <- c()
run.nn <- TRUE

if (run.nn) {
  model.nn$grid <- expand.grid(decay = c(0.5, 0.1), size = c(5, 6, 7))
  model.nn$clsf <- train(class ~ ., data = ds, method = "nnet", trControl = ds.cv, tuneGrid = model.nn$grid)
  model.nn$data <- model.nn$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.nn$conf <- confusionMatrix(model.nn$data, ds$class, positive = "B")
  model.nn$roc <- roc.curve(ds$class, model.nn$data, plotit = FALSE)
  print(model.nn$conf)
  print(model.nn$roc)
}

# Stop parallel cluster
stopCluster(cl)