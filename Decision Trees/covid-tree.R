# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(e1071)

# Read data from CSV
filename = "./data/covid-19-symptoms.tab"
data <- read.table(file = filename, sep =" ", header = TRUE)

# Percentaje os training examples
training_p <- 0.80
mejor_modelo <- list()
mejor_prec <- 0
for(i in 1:15){
  training_indexes <- createDataPartition(y = data$TARGET, p = training_p, list = FALSE)
  # Split training and test data
  training_data <- data[training_indexes, ]  # Extract training data using training_indexes
  test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 
  
  # Create Linear Model using training data. Formula = all the columns except Salary
  #Jugar y elegir los mejores minsplit, minbucket (maxDepth no cambia mucho en nuestra caso)
  model <- rpart(formula = TARGET ~., data = training_data, 
                 minsplit = 150,
                 minbucket = 75, 
                 maxdepth = 25)
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$TARGET, prediction)
  res <- confusionMatrix(prediction_results)  
  if(res$overall["Accuracy"] > mejor_prec) {
    mejor_prec <- res$overall["Accuracy"]
    mejor_modelo <- model
  }
  print(res)
  #print(res$overall["Accuracy"])
}
print(paste("Best Accuracy: " ,round(mejor_prec, 4)))
# Plot tree (this method is slow, wait until pot is completed)
fancyRpartPlot(mejor_modelo)