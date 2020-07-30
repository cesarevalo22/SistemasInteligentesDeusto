# Clear environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# Include algorithm functions
#We modified this file to be slightly faster
source("../algorithms/blind/expand-node.R")
#We removed all the print statements from this file
source("../algorithms/informed/hill-climbing-search.R")
#These are our algorithms
source("../algorithms/informed/random-restart-hill-climbing.R")
source("../algorithms/informed/local-beam-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/more-pizza-problem-complete.R")


analyze.execs <- function(res, v, nombre, param1, param2){
  df <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
  colnames(df) <- c("time", "cost", "count","slices")
  before <- nrow(df)
  df <- df[!(df$cost > df$slices[1]),]
  after <- nrow(df)
  if(nombre == "Local Beam Search"){
    cat(paste0(nombre, " ran ", v, " times with ", param1," beams. \nRemoved ", before-after, " invalid results from analysis \n"))
  }else{
    cat(paste0(nombre, " ran ", v, " times with ", param1, " restarts and ", param2, " max iterations. \nRemoved ", before-after, " invalid results from analysis \n"))
  }
  cat(paste("Mean Time:", round(mean(df$time),4),  " Time Standard Dev:", round(sd(df$time), 3), sep="\t"), "\n")
  cat(paste("Mean Slices:", round(mean(df$cost), 2), " Slices Standard Dev:", round(sd(df$cost), 3), sep="\t"),"\n")
  cat(paste("Mean Iterr:", round(mean(df$count), 4),  " Itrr Standard Dev:", round(sd(df$count), 3), sep="\t"),"\n")
}

execute.local.beam2 <- function(fn, k, v = 10){
  resultados <- list()
  count <- 0
  while(count < v){
    problem <- initialize.problem(filename = fn)
    res <- local.beam.search2(problem, beams = k)
    resultados <- append(resultados, list(res))
    count <- count + 1
  }
  analyze.execs(resultados, v, "Local Beam Search", k, NULL)
}

execute.random.restart <- function(fn, restarts, itrr, veces){
  resultados <- list()
  count <- 0
  while(count < veces){
    resultados <- append(resultados,list(random.restart.search(fn, restarts, itrr)))
    count <- count + 1
  }
  analyze.execs(resultados, veces, "Random Restart Hill Climbing", restarts, itrr)
}

execute.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  res <- hill.climbing.search(problem, max_iterations = 100, count_print = 50)
  browser()
  return(res)
}

execute.local.beam <- function(filename, k, v){
  results <- list()
  for(i in 1:v){
    results <- append(results, list(local.beam.search(filename = filename, beams = k)))
  }
  analyze.execs(results, v, "Local Beam Search", k, NULL)
  
}
# Clear console
cat("\014")
graphics.off()
# execute.local.beam("../data/2-medium.txt", 1, 1000)
# execute.local.beam("../data/2-medium.txt", 3, 1000)
# execute.local.beam("../data/2-medium.txt", 5, 1000)
# execute.local.beam("../data/2-medium.txt", 10, 1000)
#  
 execute.random.restart("../data/2-medium.txt", 500, 500, 50)
 execute.random.restart("../data/2-medium.txt", 10, 500, 50)
 execute.random.restart("../data/2-medium.txt", 500, 10, 50)

