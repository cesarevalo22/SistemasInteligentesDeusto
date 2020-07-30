analyze.results <- function(algorithm_results, problem) {
  analyzed_results <- data.frame()
  
  for (i in 1:length(algorithm_results)) {
    result <- algorithm_results[[i]]
    print(result$name)

    solution_found  <- any(!is.na(result$state_final))
    solution_length <- -1
    solution_cost   <- -1
        
    if (any(solution_found)) {
      # Checking the solution
      solution_length <- length(result$state_final$actions)
      solution_cost   <- result$state_final$cost
      print(paste0(" * Solution found after ", solution_length, " actions! :)"), quote = FALSE)
    } else {
      print(" * No Solution Found :(", quote = FALSE)
    }
    
    iterations       <- length(result$report$iteration)
    maximum_depth    <- max(result$report$depth_of_expanded)
    maximum_frontier <- max(result$report$nodes_frontier)
    analyzed_results <- rbind(analyzed_results, data.frame(name = result$name,
                                                           solution = solution_found,
                                                           runtime = round(result$runtime, digits = 2),
                                                           actions = solution_length,
                                                           cost = solution_cost,
                                                           iterations = iterations,
                                                           max_depth = maximum_depth,
                                                           max_frontier = maximum_frontier))
  }
  
  return(analyzed_results)
}

local.analyze.results <- function(algorithm_results, problem) {
  analyzed_results <- data.frame()
  
  for (i in 1:length(algorithm_results)) {
    result <- algorithm_results[[i]]
    print(result$name, quote = FALSE)
    
    print("Initial State: ", quote = FALSE)
    to.string(state = problem$state_initial, problem = problem)
    
    print("Final State: ", quote = FALSE)
    to.string(state = result$state_final$state, problem = problem)
    
    iterations       <- length(result$report$iteration)
    maximum_depth    <- result$state_final$depth
    analyzed_results <- rbind(analyzed_results, data.frame(name = result$name,
                                                           cost = get.cost(state = result$state_final$state, problem = problem),
                                                           runtime = round(result$runtime, digits = 2),
                                                           iterations = iterations,
                                                           max_depth = maximum_depth))
  }
  
  return(analyzed_results)
}