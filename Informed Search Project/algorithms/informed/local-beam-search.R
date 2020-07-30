#Autor
#Ian Saucy
#V2, 


#Inicializar la lista de estados actuales con K estados iniciales generados de forma aleatoria.
local.beam.search <-
  function(filename,
           beams = 3,
           max_iterations = 1000) {
    problems <-
      sapply(1:beams, function(x)
        initialize.problem(filename = filename))
    name_method      <- paste0("Local Beam Searh")
    
    
    local_best <- rep(FALSE, beams)
    # Get Start time
    start_time       <- Sys.time()
    
    nodes_current <- lapply(1:beams, function (x)
      list(
        parent = c(),
        state = problems[, x]$state_initial,
        actions = c(),
        depth = 1,
        cost = get.cost(state = problems[, x]$state_initial, problem = problems[, x]),
        evaluation = get.evaluation(problems[, x]$state_initial, problems[, x])
      ))
   # browser()
    #Para cada uno de los esados actuales expandir y seleccionar su mejor sucesor.
    gen.select.best <-
      function(current_node,
               axtions_possible,
               problem) {
        sucessors <-
          local.expand.node(current_node, axtions_possible, problem)
        sucessors <-
          sucessors[order(sapply(sucessors, function (x)
            x$evaluation))]
        return(sucessors[[1]])
      }
    
    count <- 0
    #Continue while less tham max iterations and all beams not at local max
    while (count < max_iterations && !all(local_best)) {
      #Generate successors for all the nodes_current that have not arrived at a local max
      successors <-
        lapply(which(!local_best), function(x)
          gen.select.best(nodes_current[[x]], problems[, x]$actions_possible, problems[, x]))
      #Para cada estado actual su mejor sucesor. V
      #uestra implementación expande todos los estados actuales, junta todos los sucesores y luego los ordena.
      #Esta implementación es una variante de beam search,
      
      #pero la que yo os he pedido es la que procesa los estados actuales de manera independiente y no mezcla los sucesores.
      #Además de la lista de estados actuales, os hace falta un vector para representar
      #si por algún "beam" se ha llegado a un local_best. De esta forma, la búsqueda sólo avanzará por los "beams" que no hayan llegado al "local_best"
      
      #Cycle through all ones not at local max to take their best successor and compare it with the currnet node
      for (i in which(!local_best)) {
          problem <- problems[, i]
          #Have to do a little trickery here since i is the position of nodes_current array, but we need the position in the 
          #successors list, which might be less(if some number of beams have arrived at a local max)
          #so we find the index in the array of which() and that is the location in the successors list of all nodes that have not reached 
          # a local max
          node_best_successor <- successors[[match(i, which(!local_best))]]
          node_current <- nodes_current[[i]]
          if (node_best_successor$evaluation <= node_current$evaluation) {
            # Current node is updated
            nodes_current[[i]] <- node_best_successor
            # Local best found
          } else {
            local_best[i] = TRUE
            break
          }
          
          #Por último, antes de finalizar cada iteración tenéis que usar el metodo is.final.state() para ver si alguno de los nuevos estados actuales es un estado final. En ese caso, el algoritmo se detiene.
          #This is a little weird since we always use the same probloem, but since the values it uses for comparisons
          #remain the same for all problems it does not matter. The values used are, the state(which is based on the apply), the pizzas themselves
          #and the slices needed -- which is the same for all instances of the problem.
          
          if (any(sapply(nodes_current, function(x)
            is.final.state(x$state, NULL, problems[, 1])))) {
            #Found a solution
            break
          }
        }
        
      
      
  
        count <- count + 1
    }
    
    end_time <- Sys.time()
    res <- nodes_current[order(sapply(nodes_current,function (x) x$evaluation))]
    results <- list()
    results$time <- end_time - start_time
    results$cost <- res[[1]]$cost
    results$count <- count
    results$slices <- problems[[1]]
    return(results)

    
    
  }