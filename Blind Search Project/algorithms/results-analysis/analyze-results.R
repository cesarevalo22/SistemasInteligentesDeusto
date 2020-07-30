analyze.results <- function(algorithm_results, problem) {
  analyzed_results <- data.frame()
  
  for (i in 1:length(algorithm_results)) {
    name <- algorithm_results[[i]]$name
    print(name)
    solution_found <- any(!is.na(algorithm_results[[i]]$state_final))
    
    if (any(solution_found)) {
      # Checking the solution
      solution_length <- length(algorithm_results[[i]]$state_final$actions)
      solution_cost   <- algorithm_results[[i]]$state_final$cost
      print(" * Solution Found! :)", quote = FALSE)
      actions <- algorithm_results[[i]]$state_final$actions
      state_current <- problem$state_initial
      print("Initial State: ")
      to.string(state_current)
      
      for (a in 1:length(actions)) {
        action <- actions[a,]
        state_current <- effect(state_current, action)  
        print(paste0("After applying action: ", action), quote = FALSE)
        to.string(state_current)
      }
    } else {
      solution_length <- -1
      solution_cost   <- -1
      print(" * No Solution Found :(")
    }
    
    number_expanded <- length(algorithm_results[[i]]$report$iteration)
    maximum_depth <- max(algorithm_results[[i]]$report$depth_of_expanded)
    maximum_frontier <- max(algorithm_results[[i]]$report$nodes_frontier)
    analyzed_results <- rbind(analyzed_results, data.frame(name = name,
                                                           solution = solution_found,
                                                           runtime = round(algorithm_results[[i]]$runtime, digits = 2),
                                                           length = solution_length,
                                                           cost = solution_cost,
                                                           expanded = number_expanded,
                                                           max_depth = maximum_depth,
                                                           max_frontier = maximum_frontier))
  }
  
  return(analyzed_results)
}