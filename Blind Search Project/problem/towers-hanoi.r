# =======================================================================
# Group Name:
# Students:
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
library(sets)
library(gtools)
#Must pass in rods and disks, random is optional
#Random specifies if the actions performed are put in a random order, or if they're left in order (1,2, 1,3 ....)
initialize.problem <- function(rods, disks, random = F) {
  problem <- list() # Default value is an empty list.
  problem$name              <- "Towers Hanoi"
  problem$state_initial     <- rep(c(1), times = disks)
  problem$state_final       <- rep(c(rods), times = disks)
  problem$actions_possible  <- findActions(rods)
  problem$actions_random <- random
  #Used to randomize the actions, or not
  if(random){
      #Randomize actions
    problem$actions_possible <- problem$actions_possible[sample(nrow(problem$actions_possible)),]
  }
  return(problem)
}


#A simple fucntions to find all the possible actions and return a named data frame 
findActions <- function(rods) {
  combs <- permutations(rods, 2, v = 1:rods, repeats.allowed = FALSE)
  colnames(combs) = c("origin", "dest")
  return(data.frame(combs))
  
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  #Find the location of the "top" disk if it exists
  # origin destino
  # 1         3
  possibleLoc <- selectTop(state, action[1])
  if (!is.na(possibleLoc)) {
    #A disk exists here
    if (isSmallest(state, action[2], possibleLoc)) {
      return(T)
    }
  }
  return(F)
}

#Returns T/F if the specifieds disk would be the smaller than the current disk on the tower
isSmallest <- function(state, tower, disk) {
  currTop <- selectTop(state, tower)
  if (is.na(currTop)) {
    #Nothing is on this tower
    return (T)
  }
  else if (currTop > disk) {
    #The current top is smaller than what we're trying to place
    return(F)
  } else{
    return(T)
  }
  
}

#Returns the top disk of the specified tower
selectTop <- function(state, tower) {
  #Search for to top disk, we start from the back of the vector since larger positions relate to samller disks
  res <- NA
  counter <- length(state)
  for (val in rev(state)) {
    if (val == tower) {
      #We found the disk we're looking for
      res <- as.numeric(counter)
      break
    }
    counter <- counter - 1
  }
  return(res)
}


# Returns the state resulting on applying the action over the state
effect <- function (state, action) {
  res <- c()
  #Verify that this is a valid action first
  if (is.applicable(state, action)) {
    #Now apply the acction to the state
    topDisk <- selectTop(state, action[1])
    state[topDisk] <- as.numeric(action[2])
    res <- state
  } else{
    res <- state
  }
  return(res)
  
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate) {
  
  res <- sum(state) == sum(final_satate)
  return(res)
}

# Transforms a state into a string
to.string = function (state) {
  return(paste(state, collapse = " "))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state) {
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE>
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
  return(1) # Default value is 1.
}