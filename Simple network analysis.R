library(igraph)
library(base)
library(stats)
g1<-graph(c(1,2),directed = F)
plot(g1,vertex=T)

plot(graph(c(1,2,3,4), directed = T))
g1 <- graph(c(1,2, 2,3, 3,1), n=3, directed=T)
g7 <- graph(c(1,2, 3,4, 3,1,2,3),  directed=T)
g8 <- graph(c(1,2, 3,4, 3,1,2,3), directed=T)
plot(g7,vertex.color="red",vertex.size=50,edge.color="black",vertex=F)

#Networ Measure and basic graph properties
g7<-graph(c("a","b","a","c","c","d","b","d","d","e"),directed = T)
plot(g7,vertex.color="green",vertex.size=50,edge.color="red")

#defee
degree(g7,mode = "all")
degree(g7,mode="in")
degree(g7,mode="out")

#diameter
diameter(g7,weight=NA)
diameter(g8,weight=NA)
#radius
radius(g7)
radius(g8)

#networs

networkx<- graph(c("A","B","A","C","B","C","C","D","D","E","D","F","E","F"), directed=F)
plot(networkx)


# Create an undirected graph
edges <- c("A", "B", "A", "C", "B", "C", "C", "D", "D", "E", "D", "F", "E", "F")
networkx <- graph(edges, directed = FALSE)

# Plot the graph
plot(networkx, margin = 0.1)
