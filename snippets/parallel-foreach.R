# Install libraries
if(!require(here)) install.packages("here")
if(!require(foreach)) install.packages("foreach")
if(!require(parallel)) install.packages("parallel")
if(!require(doSNOW)) install.packages("doSNOW")

# Import libraries
library(here)
library(foreach)
library(parallel)
library(doSNOW)

# Set working directory
setwd(here())
cat("Working Directory:", getwd(),"\n")

# Start parallel cluster with (SystemLogicalCores - 1) threads
cores <- detectCores(all.tests = FALSE, logical = TRUE) - 1
cl <- makeCluster(cores)
registerDoSNOW(cl)
cat("Running with", cores, "cores\n")

# Example dataset
df <- read.csv("datasets/iris.csv")
head(df)

# Progress bar
par <- c()
par$pb <- txtProgressBar(max = nrow(df), style = 3)
par$progress <- function(n) setTxtProgressBar(par$pb, n)
par$opts <- list(progress = par$progress)

# Parallel foreach (results stored on "output" dataframe)
output <- data.frame(foreach(i = 1:nrow(df), .combine = rbind, .inorder = FALSE, .packages = c("foreach"), .options.snow = par$opts) %dopar% {
  result <- c()
  
  result$id <-i
  result$meanSL <- mean(df$SepalLengthCm)
  result$meanSW <- mean(df$SepalWidthCm)
  result$meanPL <- mean(df$PetalLengthCm)
  result$meanPW <- mean(df$PetalWidthCm)
  
  return(result)
})

# Close progress bar
close(par$pb)

# Stop parallel cluster
stopCluster(cl)

# Show results
head(output)