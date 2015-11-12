library(igraph)

com_algs = c(
  edge.betweenness.community,
  fastgreedy.community,
  label.propagation.community,
  leading.eigenvector.community,
  multilevel.community,
  optimal.community,
  spinglass.community,
  walktrap.community,
  infomap.community)

com <- edge.betweenness.community(karate)
length(com)
com[1]

length(V(karate))

getValues <- function(graph){
  for(i in 1 : length(com_algs)){
    n <- length(V(graph))
    com_alg <- com_algs[[i]]
    com = com_alg(graph)
    nComs <- length(com)
    avg_modularity <- 0
    avg_conductance <- 0
    avg_cut_ratio <- 0
    avg_expansion <- 0
    
    for(c in 1:nComs){
      mc <- length(com[c])
      fc <- 0 #todo
      conductance <- fc / (2*mc + fc)
      expansion <- fc/nc
      cut_ratio <- fc/(nc*(n-nc))
      avg_conductance <- avg_conductance + conductance*(mc/n)
      avg_expansino <- avg_expansion + expansion*(mc/n)
      avg_cut_ratio <- avg_cut_ratio + cut_ratio*(mc/n)
    }
    modularity(wc)
    
    membership(wc) #dice cada nodo a que cluster pertenede
    
    
  }
  plot(wc, graph)
  plot(graph, vertex.color=membership(wc))
  #dendPlot(fc)
  return(0)
}

karate <- graph.famous("Zachary")

getValues(karate)