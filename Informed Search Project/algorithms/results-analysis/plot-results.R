plot.results <- function(report, name_method, problem) {
  #Plot size of frontier list in each iteration
  plot_frontier_size <- ggplot(report, aes(x = iteration, y = nodes_frontier)) +
           geom_line(col = "dodgerblue", size = 2) +
           labs(x = "Iteration Number", y = "Nodes stored in the frontier", title = "Size of Frontier", caption = "University of Deusto") + 
           theme_minimal()
  #Plot frontier list growth in each iteration
  plot_frontier_growth <- ggplot(report, aes(x = nodes_added_frontier)) +
           geom_histogram(fill = "dodgerblue", bins = length(unique(report$depth_of_expanded)), binwidth = 0.1) +
           labs(x = "Number nodes added to frontier", y = "Frequency", title = "Frontier growth", caption = "University of Deusto") +
           theme_minimal()
  #Plot max depth of the nodes in frontier list in each iteration
  plot_nodes_depth <- ggplot(report, aes(x = depth_of_expanded)) +
           geom_histogram(fill = "dodgerblue", bins = length(unique(report$depth_of_expanded)), binwidth = 0.1) +
           labs(x = "depth", y = "Number of Nodes", title = "Depth of nodes in Frontier", caption = "University of Deusto") +
           theme_minimal()
  
  grid.arrange(plot_frontier_size, plot_frontier_growth, plot_nodes_depth, ncol = 1, top = paste0(name_method, ": ", problem$name))
}