# load data
edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(users)
str(edges)
table(users$locale)
# install igraph
install.packages("igraph")
library(igraph)
?graph.data.frame
g = graph.data.frame(edges, F, users)
plot(g,vertex.size = 5, vertex.label = NA)
degree(g)
table(degree(g)>= 10)
V(g)$size = degree(g)/2 +2
plot(g, vertex.label=NA)
table(degree(g))
# Coloring vertices
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g)
# Color based on school
V(g)$color[V(g)$school == "A"] = "blue"
V(g)$color[V(g)$school == "AB"] = "yellow"
plot(g, vertex.label = NA)
# Color based on locale
V(g)$color[V(g)$locale == "A"] = "pink"
V(g)$color[V(g)$locale == "B"] = "green"
plot(g, vertex.label = NA)
# Other plotting options
?igraph.plotting
