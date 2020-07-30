expand.node = function(current_node, actions_possible, problem) {
  successors <- list()
  
  for (i in 1 : nrow(actions_possible)) {
    action <- actions_possible[i, ]
    
    if (is.applicable(current_node$state, action, problem)) {
      successor               <- list()
      successor$parent        <- current_node
      successor$state         <- effect(current_node$state, action)    
      successor$actions       <- rbind(current_node$actions, action)
      successor$depth         <- current_node$depth + 1
      successor$cost          <- current_node$cost + get.cost(action = action, state = current_node$state)
      successor$evaluation    <- get.evaluation(successor$state, problem)
      
      successors <- append(successors, list(successor))
    }
  }
  
  return(successors)
}

#Little helper function to make the expansion easier 
local.gen.node = function(current_node, action, problem){
  #print(action)
  successor               <- list()
  successor$parent        <- current_node
  successor$state         <- effect(current_node$state, action)   
  successor$actions       <- rbind(current_node$actions, action)
  successor$depth         <- current_node$depth + 1
  #original                                       ▼▼▼▼▼▼▼▼▼
  #successor$cost          <- get.cost(action, current_node$state, problem)
  successor$cost          <- get.cost(action, successor$state, problem)
  successor$evaluation    <- get.evaluation(successor$state, problem)
  return(successor)
}

local.expand.node = function(current_node, actions_possible, problem) {
  
  #We changed this code to be slightly more faster during the expansion process 
  successors <-
    sapply(actions_possible[, ], function (x) {
      ifelse(is.applicable(current_node$state, x, problem),
             list(local.gen.node(current_node, x, problem)))
    })
  
  #Original
  # for (i in 1 : nrow(actions_possible)) {
  #   action <- actions_possible[i, ]
  #
  #   if (is.applicable(current_node$state, action, problem)) {
  #     successor               <- list()
  #     successor$parent        <- current_node
  #     successor$state         <- effect(current_node$state, action)
  #     successor$actions       <- rbind(current_node$actions, action)
  #     successor$depth         <- current_node$depth + 1
  #     successor$cost          <- get.cost(action, current_node$state, problem)
  #     successor$evaluation    <- get.evaluation(successor$state, problem)
  #
  #     successors <- append(successors, list(successor))
  #   }
  # }
  # print(identical(successors, successors2))
  
  return(successors)
}