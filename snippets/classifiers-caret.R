# Install libraries
if(!require(here)) install.packages("here")
if(!require(import)) install.packages("import")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(parallel)) install.packages("parallel")
if(!require(doSNOW)) install.packages("doSNOW")
if (!require(C50)) install.packages('C50')
if(!require(naivebayes)) install.packages("naivebayes")
if(!require(randomForest)) install.packages("randomForest")
if(!require(e1071)) install.packages("e1071")
if(!require(kernlab)) install.packages("kernlab")

# Import libraries
library(here)
library(import)
library(dplyr)
library(caret)
library(parallel)
library(doSNOW)
library(C50)
library(naivebayes)
library(randomForest)
library(e1071)
library(kernlab)

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

## Decision tree (C5.0)
model.c50 <- c()
run.c50 <- TRUE

if (run.c50) {
  model.c50$clsf <- train(class ~ ., data = ds, method = "C5.0", trControl = ds.cv)
  model.c50$data <- model.c50$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.c50$conf <- confusionMatrix(model.c50$data, ds$class, positive = "B")
  print(model.c50$conf)
}

## Naïve Bayes
model.nb <- c()
run.nb <- TRUE

if (run.nb) {
  model.nb$clsf <- train(class ~ ., data = ds, method = "naive_bayes", trControl = ds.cv)
  model.nb$data <- model.nb$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.nb$conf <- confusionMatrix(model.nb$data, ds$class, positive = "B")
  print(model.nb$conf)
}

## Random Forest
model.rf <- c()
run.rf <- TRUE

if (run.rf) {
  model.rf$clsf <- train(class ~ ., data = ds, method = "rf", trControl = ds.cv)
  model.rf$data <- model.rf$clsf$pred %>% arrange(rowIndex) %>% pull(pred)
  model.rf$conf <- confusionMatrix(model.rf$data, ds$class, positive = "B")
  print(model.rf$conf)
}

# Stop parallel cluster
stopCluster(cl)