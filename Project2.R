rm(list = ls())

library(caret)
library(e1071)
library(jpeg)
library(ggplot2)

# dev.off()

setwd("D:/2015Fall/AIL_Machine_Learning/project2")

source("utils.R");



println <- function(msg) {
  cat("\n", msg, "\n")
}

# load data from files
loadData <- function(path, len = 2688) {
  f <- list.files(path, full.name=TRUE, pattern="jpg");
  len <- min(len, length(f))
  
  #dataFrame <- sapply(f[1:len], colorAvg123);
  dataFrame <- sapply(f[1:len], colorHist)
  
  category <- c()
  
  for (i in 1:len) {
    tmp <- unlist(strsplit(f[i], "_"))
    tmp <- unlist(strsplit(tmp, "/"))
    ct <- tmp[[2]]
    category <- c(category, ct)
  }
  
  
  dataFrame <- t(dataFrame)
  
  dataFrame <- data.frame(dataFrame, category)
  
  return(dataFrame)
}


# dataTable <- loadData("data", 2688)
# write.csv(dataTable, "hist2688.csv")

# training data with output column is "category"
svm_train <- function(train_data) 
{ 
  x <- subset(train_data, select=-c(category));
  y <- subset(train_data, select=c(category));
  
  println("Model fitting...")
  model <- svm(x, y, type='C', kernel='polynomial', degree=3);
  
  print(model);
  
  println("Testing train data")
  y <- train_data[, "category"];
  y1 <- predict (model, x); 
  e1 <- abs(y1 != y);
  cat("\n"); cat("Training error:", sum(e1)/nrow(train_data), "\n");
  
  return(model)
}

svm_test <- function(model, test_data) {
  println("Testing new data")
  x <- subset(test_data, select=-c(category));
  y <- test_data[, "category"]; 
  y1 <- predict (model, x); 
  e1 <- abs(y1 != y);
  cat("\n"); cat("Testing error:", sum(e1)/nrow(test_data) , "\n");
}

random_forest_train <- function(train_data) {
  println("Model fitting...")
  model <- train(category ~ ., method = "rf", data = train_data)
  print(model)
  
  println("Testing train data...")
  y <- train_data$category
  y1 <- predict(model, newdata = train_data)
  e1 <- abs(y != y1)
  cat("\n"); cat("Training error:", sum(e1)/nrow(train_data), "\n");
  
  return(model)
}

random_forest_test <- function(model, test_data) {
  println("Testing new data...")
  y <- test_data$category
  y1 <- predict(model, newdata = test_data)
  e1 <- abs(y != y1)
  cat("\n"); cat("Testing error:", sum(e1)/nrow(test_data), "\n");
}

dectree_train <- function(train_data) {
  println("Model fitting...")
  model <- train(category ~ ., method = "rpart", data = train_data)
  
  println("Testing train data...")
  y <- train_data$category
  y1 <- predict(model, newdata = train_data)
  e1 <- abs(y != y1)
  cat("\n"); cat("Training error:", sum(e1)/nrow(train_data), "\n");
  
  return(model)
}

dectree_test <- function(model, test_data) {
  println("Testing new data...")
  y <- test_data$category
  y1 <- predict(model, newdata = test_data)
  e1 <- abs(y != y1)
  cat("\n"); cat("Testing error:", sum(e1)/nrow(test_data), "\n");
}

boost_train <- function(train_data) {
  println("Model fitting...")
  model <- train(category ~ ., method="gbm", data=train_data, verbose=TRUE);
  
  println("Testing train data...")
  y <- train_data$category
  y1 <- predict(model, train_data)
  e1 <- abs(y != y1)
  cat("\n"); cat("Training error:", sum(e1)/nrow(train_data), "\n");
  
  return(model)
}

boost_test <- function(model, test_data) {
  println("Testing new data...")
  y <- test_data$category
  y1 <- predict(model, newdata = test_data)
  e1 <- abs(y != y1)
  cat("\n"); cat("Testing error:", sum(e1)/nrow(test_data), "\n");
}

printOutput <- function(test_data, y1, e1, seedNum) {
  names <- c("coast", "forest", "mountain", "opencountry", "insidecity", "tallbuilding", "street", "highway")
  
  sink(file = paste("outputSVM", seedNum, ".html"), type="output");
  
  images <- test_data[, "X"]
  images <- as.vector(images)
  output <- as.vector(y1)
  
  cat("<h1>This is the output for scene classification</h1>\n\n")
  cat("\n\n<h2> set.seed(", seedNum, ")</h2>\n\n")
  cat("\n"); cat("<br>Testing error:", sum(e1)/nrow(test_data) , "</br>\n");
  cat("<style>\n.cell { display:inline-block; border: solid 1px #aeaeae; padding: 5px; margin: 3px; border-radius: 3px; background-color: lavender; }\n
      .small { width:64px; }\n
      .tag { color: red; }\n
      </style>\n
      ")
  
  for (i in 1:8) {
    indexes <- which(output %in% c(names[i]))
    cat("\n<div class=cell>\n")
    for (j in indexes) {
      cat("<img class=small src='",images[j], "'> " )
    }
    cat("\n<br><span class=tag>", names[i],"</span>\n")
    cat("</div>\n")
  }
  
  cat("\n\n<br> Phuong D. Pham </br>\n")
  sink(file = NULL)
}

svm_test_output <- function(model, test_data, seedNum) {
  println("Testing new data")
  x <- subset(test_data, select=-c(X, category));
  y <- test_data[, "category"]; 
  y1 <- predict (model, x); 
  e1 <- abs(y1 != y);
  
  printOutput(test_data, y1, e1, seedNum)
}

rf_test_output <- function(model, test_data, seedNum) {
  x <- subset(test_data, select=-c(X, category));
  y <- test_data$category
  y1 <- predict(model, newdata = test_data)
  e1 <- abs(y != y1)
  
  printOutput(test_data, y1, e1, seedNum)
}

explore <- function() {
  svm_train_test <- function(train_data, test_data) {
    model <- svm_train(train_data)
    svm_test(model, test_data)
  }
  
  rf_train_test <- function(train_data, test_data) {
    model <- random_forest_train(train_data)
    random_forest_test(model, test_data)
  }
  
  dectree_train_test <- function(train_data, test_data) {
    model <- dectree_train(train_data)
    dectree_test(model, test_data)
  }
  
  boost_train_test <- function(train_data, test_data) {
    model <- boost_train(train_data)
    boost_test(model, test_data)
  }
  
  # dataTable <- read.csv("mydata_hist50each.csv", header = TRUE)
  dataTable <- read.csv("mydata_colorAvg50each.csv", header = TRUE)
  
  S <- createDataPartition(dataTable$category, p = 0.7, list = FALSE)
  
  train_data <- dataTable[S, ]
  test_data <- dataTable[-S, ]
  
  train_data <- subset(train_data, select = -c(X))
  test_data <- subset(test_data, select = -c(X))
  
  # you can choose one of them to explore
  # I explored and report result in report file.
  
  # model <- svm_train(train_data)
  # svm_test(test_data)
}



main <- function(seedNum) {
  dataTable <- read.csv("coloravg2688.csv", header = TRUE)
  
  S <- createDataPartition(dataTable$category, p = 0.7, list = FALSE)
  
  train_data <- dataTable[S, ]
  test_data <- dataTable[-S, ]
  
  
  train_data <- subset(train_data, select = -c(X))
  
  model <- svm_train(train_data)
  svm_test_output(model, test_data, seedNum)
}

# explore()
set.seed(4321)
main(4321)

set.seed(432)
main(432)

set.seed(125)
main(125)

set.seed(5555)
main(5555)