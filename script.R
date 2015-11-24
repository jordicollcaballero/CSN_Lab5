library(igraph)
library(xtable)

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

com_algs_names= c(
  "edge_betweenness",
  "fastgreedy",
  "label_propagation",
  "leading_eigenvector",
  "multilevel",
  "optimal",
  "spinglass",
  "walktrap",
  "infomap")

calculate_mcfc <- function(graph, clusters, idCluster) {
  m <- 0
  f <- 0
  vlist <- unlist(clusters[idCluster]) #List of members
  for(v in vlist){
    n <- unlist(neighbors(graph,unlist(v))) #List of neigbours
    ocn <- length(setdiff(n,vlist))
    f <- f + ocn #Extract the n-v (in the set sense)
    m <- m + length(n) - ocn
  }
  return(data.frame(m=m/2,f=f))
}

getValues <- function(graph){
  dfres <- NULL
  algCommunities <- list()
  for(i in 1 : length(com_algs)){
    n <- length(V(graph))
    com_alg <- com_algs[[i]]
    com = com_alg(graph)
    algCommunities[[length(algCommunities)+1]] <- com
    nComs <- length(com)
    avg_conductance <- 0
    avg_cut_ratio <- 0
    avg_expansion <- 0
    for(c in 1:nComs){
      nc <- length(com[c])
      mcfc <- calculate_mcfc(graph, com, c) #nr. edges in the frontier of C
      fc <- mcfc$f
      mc <- mcfc$m
      conductance <- fc / (2*mc + fc)
      expansion <- fc/nc
      cut_ratio <- fc/(nc*(n-nc))
      avg_conductance <- avg_conductance + conductance*(nc/n)
      avg_expansion <- avg_expansion + expansion*(nc/n)
      avg_cut_ratio <- avg_cut_ratio + cut_ratio*(nc/n)
    }
    df <- data.frame(rowname=com_algs_names[i],
                     modularity=modularity(com),
               conductance=avg_conductance,
               expansion=avg_expansion,
               cut_ratio=avg_cut_ratio)
    dfres <- rbind(dfres, df)
    
    
  }
  #plot(wc, graph)
  #plot(graph, vertex.color=membership(wc))
  #dendPlot(fc)
  return(list(dfres, algCommunities))
}


karate <- graph.famous("Zachary")

#com <- edge.betweenness.community(karate)
#length(com)
#com[1]

#length(V(karate))


#Generated Dummy
g <- graph.full(10) + graph.full(10)
g <- g + edges(sample(V(g), 10, replace=TRUE))
g <- simplify(g)
gValues<-getValues(g)
xtable(gValues[[1]], digits=5)
save(dummyValues, file="data/out/dummyValues.dat")


#Zachary
karateValues<-getValues(karate)
xtable(karateValues[[1]], digits=5)
save(karateValues, file="data/out/karateValues.dat")

#Les miserables
lesmis <- read.graph("data/lesmis.gml", format="gml")
lesmisValues <- getValues(lesmis)
xtable(lesmisValues[[1]], digits=5)
save(lesmisValues, file="data/out/lesmisResult.dat")

#Dolphins
dolphin <- read.graph("data/dolphins.gml", format="gml")
dolphinValues <- getValues(dolphin)
xtable(dolphinValues[[1]], digits=5)
save(dolphinValues, file="data/out/dolphinValues.dat")

#Wiki
wiki <- read.graph("data/wikipedia.gml", format="gml")
wikiValues <- getValues(wiki)
xtable(wikiValues[[1]], digits=5)
save(wikiValues, file="data/out/wikiValues.dat")