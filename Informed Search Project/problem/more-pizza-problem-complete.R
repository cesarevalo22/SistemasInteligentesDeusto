# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(filename = "../data/3-big.txt") {
  problem <- list() # Default value is an empty list.

  # Read data from the file
  input_data                <- readLines(filename, n = 2, warn = FALSE)
  
  # Needed slices
  problem$needed_slices      <- as.numeric(strsplit(x = input_data[1], split =" ")[[1]][1])
  # Number of different types of pizza
  problem$different_pizzas  <- as.numeric(strsplit(x = input_data[1], split =" ")[[1]][2])
  # List with the number of slices of each pizza
  problem$pizzas            <- as.numeric(strsplit(x = input_data[2], split =" ")[[1]])
  
  # This attributes are compulsory
  problem$name             <- paste0("More pizza - slices=", problem$needed_slices, 
                                                ", pizzas=", problem$different_pizzas)
  # Initial state is a random vector of 0 and 1. 
  # Each position corresponds to one pizza. It indicates whether it is purchased (1) or not (0).
  problem$state_initial    <- as.numeric(runif(length(problem$pizzas)) > 0.5)

  # In this problem final state is unknown
  problem$state_final      <- NULL
  # Action is: Changing the "purchase" value of a pizza
  problem$actions_possible <- data.frame(action = c(1:length(problem$pizzas)), stringsAsFactors = FALSE)
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  return(TRUE)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action) {
  result <- state
  
  # Change the value (1 to 0 | 0 to 1) of the position defined by the action
  result[action] <- !result[action]
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  return(get.evaluation(state, problem) == 0)
}

# Transforms a state into a string
to.string = function (state, problem) {
  if (any(state > 0)) {
    print_state <- state * problem$pizzas
    print(paste0("Best node=", get.cost(state = state, problem = problem), " / needed=", problem$needed_slices), quote = FALSE)
    print(print_state[which(print_state > 1)])
  } else {
    print("NO pizza has been ordered", quote = FALSE)
  }
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(get.ordered.pizzas(state = state, problem = problem))
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  return(abs(problem$needed_slices - get.ordered.pizzas(state, problem)))
}

# Calculate the total number of pizza slices ordered.
get.ordered.pizzas <- function(state, problem) {
  # The vector of ordered pizzas (state) is multiplied by the vector containing
  # the slices of each pizza; and finally the sum of all the slices is calculated.
  return(sum(state * problem$pizzas))
}