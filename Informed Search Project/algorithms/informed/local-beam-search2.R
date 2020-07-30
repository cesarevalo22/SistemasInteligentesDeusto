# #Autor
# #Ian Saucy
  #Version no usada 
# 
# 
# local.beam.search2 = function(problem,
#                               max_iterations = 1000,
#                               beams = 2,
#                               count_print = 100,
#                               trace = FALSE) {
#   name_method      <- paste0("Local Beam Searh")
#   state_initial    <- problem$state_initial
#   actions_possible <- problem$actions_possible
#   
#   # Get Start time
#   start_time       <- Sys.time()
#   nodes_current <- list(list(parent = c(),
#                        state = state_initial,
#                        actions = c(),
#                        depth = 1,
#                        cost = get.cost(state = state_initial, problem = problem),
#                        evaluation = get.evaluation(state_initial, problem)))
#   count <- 1
#   end_reason <- 0
# 
#   while (count <= max_iterations) {
#     sucessor_nodes <- sapply(nodes_current, function(x) local.expand.node(x, data.frame(action = c(1:length(problem$pizzas)), stringsAsFactors = FALSE), problem))
#     # Successor nodes are sorted ascending order of the evaluation function
#     sucessor_nodes <-
#       sucessor_nodes[order(sapply(sucessor_nodes, function (x)
#         x$evaluation))]
#     
#     # Select best k successors
#     nodes_best_successors <- sucessor_nodes[1:beams]
#     
#     #First time, current nodes is empty, just fill it with best successor nodes
#     if (count == 1) {
#       nodes_current <- nodes_best_successors
#     } else{
#       #Otherwise, put both lists together and choose the the best
#       nodes_temp <- append(nodes_current, nodes_best_successors)
#       nodes_temp <-
#         nodes_temp[order(sapply(nodes_temp, function (x)
#           x$evaluation))]
#       #Check if the new best is the same as the old best, local min/max thus we should stop
#       #Otherwise we set the current best nodes to new temp list we just generated
#       if(nodes_temp[[1]]$evaluation == 0){
#         nodes_current <- nodes_temp
#         break
#       }
#       allSame <- rep(FALSE, beams)
#       for (i in 1:beams) {
#         allSame[i] <-
#           nodes_temp[[i]]$evaluation == nodes_current[[i]]$evaluation
#       }
#       if (!all(allSame)) {
#         nodes_current <- nodes_best_successors
#       } else{
#         break
#       }
#       
#       
#     }
#     
#     
#     count <- count + 1
#   }
#   end_time <- Sys.time()
#   # Get runtime
#   res <- list()
#   res$time <- end_time - start_time
#   res$evaluation <- nodes_current[[1]]$evaluation
#   res$count <- count
#   return(res)
# }