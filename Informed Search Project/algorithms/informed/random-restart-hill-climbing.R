#Autor
#Cesar


random.restart.search = function(filename,
                                 max_restarts = 1000,
                                 max_iterations = 500,
                                 count_print = 100,
                                 trace = FALSE) {
  start_time <- Sys.time()
  problem <- initialize.problem(filename = filename)
  mejor_nodo <- hill.climbing.search(problem, max_iterations = max_iterations)$state_final
  
  count <- 0
  
  while (count < max_restarts) {
    problem <- initialize.problem(filename = filename)
    temp_nodo <-
      hill.climbing.search(
        problem,
        max_iterations = max_iterations,
        count_print = count_print
      )$state_final
    # browser()
    if (temp_nodo$evaluation  <= mejor_nodo$evaluation) {
      mejor_nodo <- temp_nodo
    }
    if (is.final.state(temp_nodo$state, NULL, problem)) {
      break
    }
    count <- count + 1
  }
  finish_time <- Sys.time()
  resultados <- list()
  resultados$duracion <- finish_time - start_time
  resultados$evaluation <- mejor_nodo$cost
  resultados$restarts <-  count
  resultados$slices <- problem[[1]]
  return(resultados)
}
