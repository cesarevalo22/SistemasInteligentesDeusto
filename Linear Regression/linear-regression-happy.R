# Clear plots
if (!is.null(dev.list()))
  dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list = ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(caret)
library(ggplot2)
library(RKEEL)
library(DAAG)
library(leaps)
library(car)
library(purrr)
library(tidyverse)
# Read data from CSV
data <-
  read.csv(
    "./data/happiness.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

# Create 10 data partitions
training_p <- 0.8

# Create data partition 80% training, 20% test
training_samples <-
  createDataPartition(y = data$Score, p = training_p, list = FALSE)

# Split training and test data
training_data    <- data[training_samples,]
test_data        <- data[-training_samples,]


models <- list()
# , 
df <- tibble(Factor = "", Mean.Average.Error = NA, Ris.Stand.Err = NA, Adj.R.Squared = NA, Ris.Min =NA, Ris.1q =NA, Ris.median =NA, Ris.3q=NA, Ris.max=NA)[0,]
  #data.frame(Factor = c(""), Mean.Average.Error= NA)[0,]

#Just the worst function in the world to compute the model 10 times, then calculate several different statistical values about 
#said model. Returns the results appended to the passed dataframe. 
run.n.samples <- function(elm, formula, df, data){
  
  mean_avg_error = list()
  sigma = list()
  adj_r = list()
  ris.min = list()
  ris.1q = list()
  ris.median = list()
  ris.3q = list()
  ris.max = list()
  for(i in 1:10){
    training_samples <-
      createDataPartition(y = data$Score, p = training_p, list = FALSE)
    training_data    <- data[training_samples, ]
    test_data        <- data[-training_samples, ]
    lm(formula = formula, data = training_data)
    model <- lm(formula = formula, data = training_data)
    prediction       <- predict(model, test_data)
    mean_avg_error <-
      append(mean_avg_error, mean(abs(prediction - test_data$Score)))
    sum <- summary(model)
    sigma <- append(sigma, sum$sigma)
    adj_r <- append(adj_r, sum$adj.r.squared)
    ris.min <- append(ris.min, min(model$residuals))
    ris.1q <-
      append(ris.1q, quantile(model$residuals, c(0.25), type = 7))
    ris.median <- append(ris.median, median(model$residuals))
    ris.3q <-
      append(ris.3q, quantile(model$residuals, c(0.75), type = 7))
    ris.max <- append(ris.max, max(model$residuals))
    if(i == 1){
      print(paste0(
        paste("- Mean average error for Score ~", elm,  ": "),
        mean_avg_error[[i]]
      ))
      summary(model)
      par(mfrow = c(2, 2))
      temp <- chartr(".", " ", elm)
      plot(model, main = paste0("Score ~ ", temp))
      models <- append(models, list(model))
    }
  }
  sigma <- mean(unlist(sigma))
  adj_r <- mean(unlist(adj_r))
  ris.min <- mean(unlist(ris.min))
  ris.1q <- mean(unlist(ris.1q))
  ris.median <- mean(unlist(ris.median))
  ris.3q <- mean(unlist(ris.3q))
  ris.max <- mean(unlist(ris.max))
  df <- df %>% add_row(Factor = elm, Mean.Average.Error = mean(unlist(mean_avg_error)), Ris.Stand.Err = sigma, Adj.R.Squared = adj_r, Ris.Min = ris.min,
                       Ris.1q = ris.1q, Ris.median = ris.median, Ris.3q = ris.3q, Ris.max = ris.max)
  return(df)
}

#Individual Models
for (elm in colnames(data)[-1:-3]) {
  df <- run.n.samples(as.formula(paste("Score ~ ", elm)), df = df, data = data, elm = elm)
}

#Run on using 6 factors 
df <- run.n.samples(as.formula(Score ~ GDP.per.capita + Social.support + Healthy.life.expectancy +
                                 Freedom.to.make.life.choices + Generosity +
                                 Perceptions.of.corruption), df = df, data = data, elm = "All")
# eval(parse(text = paste(
#   "anova(",
#   paste(
#     "models[[",
#     1:length(models),
#     "]]",
#     sep = "",
#     collapse = ","
#   ),
#   ")"
# )))
