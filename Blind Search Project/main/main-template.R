# =======================================================================
# Group Name:
# Students: Ian Saucy, Cesar Arevalo
# =======================================================================

# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Install required packages
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/uniform-cost-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")
source("../algorithms/results-analysis/plot-results.R")

#Custom diles
source("../problem/towers-hanoi.r");

# ADD YOUR CODE HERE TO INITIALIZE YOUR PROBLEM AND INCLUDE THE DEFINITION FILE
problem <- initialize.problem(3,6,T) 
bfs_ts <- breadth.first.search(problem, max_iterations = 2500)  
bfs_gs <- breadth.first.search(problem, graph_search = T)
dfs_ts <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls6_ts <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000)
dls6_gs <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000, graph_search = TRUE)
dls10_ts <- depth.limited.search(problem, depth_limit = 10, max_iterations = 3500, count_print = 1000)
dls10_gs <- depth.limited.search(problem, depth_limit = 10, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
ids_ts <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000)
ids_gs <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

output <- "Actions in order"
if(problem$actions_random){
  output <- "Actions are random"
}
analyze.results(list(bfs_ts,bfs_gs,dls10_ts, dls10_gs, ids_ts, ids_gs , dfs_ts, dls6_ts, dls6_gs), problem)
print(output, quote = FALSE)








