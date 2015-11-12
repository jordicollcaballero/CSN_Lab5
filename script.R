library(igraph)
karate <- graph.famous("Zachary")
auz <- walktrap.community
wc<-auz(karate)
wc <- walktrap.community(karate)
modularity(wc)

membership(wc)
plot(wc, karate)
plot(karate, vertex.color=membership(wc))
fc <- fastgreedy.community(karate)
dendPlot(fc)

getValues <- function(graph){
  com_algs = c(
    edge.betweenness.community,
    fastgreedy.community,
    label.propagation.comunity,
    leading.eigenvector.community,
    multilevel.community,
    optimal.community,
    spinglass.community,
    walktrap.community,
    infomap.community);
  statements
  return(object)
}