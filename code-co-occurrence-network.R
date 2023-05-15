
library(igraph) 
library(psych)

#"group1" is the OTU table, has been filtered to include >1% and presence frequency >2 samples.
occor.group1 = corr.test(t(group1),use="pairwise",method="spearman",adjust="fdr",alpha=.05)
occor.r.group1= occor.group1$r 
occor.p.group1= occor.group1$p 
occor.r.group1[occor.p.group1>0.05|abs(occor.r.group1)<0.6] = 0 #remove those r<0.6, p>0.05.
igraph.group1 = graph_from_adjacency_matrix(occor.r.group1,mode="undirected",weighted=TRUE,diag=FALSE)
igraph.group1.weight = E(igraph.group1)$weight
E(igraph.group1)$weight = NA
# set edge color postive correlation pink color, negative blue.
E.color.group1 = igraph.group1.weight
E.color.group1 = ifelse(E.color.group1>0, "pink",ifelse(E.color.group1<0, "blue","grey")) 
E(igraph.group1)$color = as.character(E.color.group1)
#change edge width
E(igraph.group1)$width = abs(igraph.group1.weight)
#change node size if need
#igraph.size.group1 = group1[V(igraph.group1)$name,] 
#igraph.size1.group1 = rowMeans(igraph.size.group1[,2:4])*100
#V(igraph.group1)$size = igraph.size1.group1
# set vertices color, modularity
fc.group1 = cluster_fast_greedy(igraph.group1,weights =NULL)
modularity.group1 = modularity(igraph.group1,membership(fc.group1))
comps.group1 = membership(fc.group1)
colbar.group1 = rainbow(max(comps.group1))
V(igraph.group1)$color = colbar.group1[comps.group1]
#===============group2
occor.group2 = corr.test(t(group2),use="pairwise",method="spearman",adjust="fdr",alpha=.05)
occor.r.group2= occor.group2$r 
occor.p.group2= occor.group2$p 
occor.r.group2[occor.p.group2>0.05|abs(occor.r.group2)<0.6] = 0
igraph.group2 = graph_from_adjacency_matrix(occor.r.group2,mode="undirected",weighted=TRUE,diag=FALSE)
igraph.group2.weight = E(igraph.group2)$weight
E(igraph.group2)$weight = NA
E.color.group2 = igraph.group2.weight
E.color.group2 = ifelse(E.color.group2>0, "pink",ifelse(E.color.group2<0, "blue","grey")) 
E(igraph.group2)$color = as.character(E.color.group2)
E(igraph.group2)$width = abs(igraph.group2.weight)
fc.group2 = cluster_fast_greedy(igraph.group2,weights =NULL)
modularity.group2 = modularity(igraph.group2,membership(fc.group2))
comps.group2 = membership(fc.group2)
colbar.group2 = rainbow(max(comps.group2))
V(igraph.group2)$color = colbar.group2[comps.group2]
#===============group3
occor.group3 = corr.test(t(group3),use="pairwise",method="spearman",adjust="fdr",alpha=.05)
occor.r.group3= occor.group3$r 
occor.p.group3= occor.group3$p 
occor.r.group3[occor.p.group3>0.05|abs(occor.r.group3)<0.6] = 0
igraph.group3 = graph_from_adjacency_matrix(occor.r.group3,mode="undirected",weighted=TRUE,diag=FALSE)
igraph.group3.weight = E(igraph.group3)$weight
E(igraph.group3)$weight = NA
E.color.group3 = igraph.group3.weight
E.color.group3 = ifelse(E.color.group3>0, "pink",ifelse(E.color.group3<0, "blue","grey")) 
E(igraph.group3)$color = as.character(E.color.group3)
E(igraph.group3)$width = abs(igraph.group3.weight)

fc.group3 = cluster_fast_greedy(igraph.group3,weights =NULL)
modularity.group3 = modularity(igraph.group3,membership(fc.group3))
comps.group3 = membership(fc.group3)
colbar.group3 = rainbow(max(comps.group3))
V(igraph.group3)$color = colbar.group3[comps.group3]
#===============group4
occor.group4 = corr.test(t(group4),use="pairwise",method="spearman",adjust="fdr",alpha=.05)
occor.r.group4= occor.group4$r 
occor.p.group4= occor.group4$p 
occor.r.group4[occor.p.group4>0.05|abs(occor.r.group4)<0.6] = 0
igraph.group4 = graph_from_adjacency_matrix(occor.r.group4,mode="undirected",weighted=TRUE,diag=FALSE)
igraph.group4.weight = E(igraph.group4)$weight
E(igraph.group4)$weight = NA
E.color.group4 = igraph.group4.weight
E.color.group4 = ifelse(E.color.group4>0, "pink",ifelse(E.color.group4<0, "blue","grey")) 
E(igraph.group4)$color = as.character(E.color.group4)
E(igraph.group4)$width = abs(igraph.group4.weight)

fc.group4 = cluster_fast_greedy(igraph.group4,weights =NULL)
modularity.group4 = modularity(igraph.group4,membership(fc.group4))
comps.group4 = membership(fc.group4)
colbar.group4 = rainbow(max(comps.group4))
V(igraph.group4)$color = colbar.group4[comps.group4]
#===============group5
occor.group5 = corr.test(t(group5),use="pairwise",method="spearman",adjust="fdr",alpha=.05)
occor.r.group5= occor.group5$r 
occor.p.group5= occor.group5$p 
occor.r.group5[occor.p.group5>0.05|abs(occor.r.group5)<0.6] = 0
igraph.group5 = graph_from_adjacency_matrix(occor.r.group5,mode="undirected",weighted=TRUE,diag=FALSE)
igraph.group5.weight = E(igraph.group5)$weight
E(igraph.group5)$weight = NA
E.color.group5 = igraph.group5.weight
E.color.group5 = ifelse(E.color.group5>0, "pink",ifelse(E.color.group5<0, "blue","grey")) 
E(igraph.group5)$color = as.character(E.color.group5)
E(igraph.group5)$width = abs(igraph.group5.weight)

fc.group5 = cluster_fast_greedy(igraph.group5,weights =NULL)
modularity.group5 = modularity(igraph.group5,membership(fc.group5))
comps.group5 = membership(fc.group5)
colbar.group5 = rainbow(max(comps.group5))
V(igraph.group5)$color = colbar.group5[comps.group5]


#################plot the graphs
par(mfrow=c(1,5),mar=c(2,2,2,2))
set.seed(123)
plot(igraph.group1,main="Ambient",vertex.frame.color=NA,vertex.label=NA,edge.width=0.2,
     vertex.size=5, edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))
set.seed(123)
plot(igraph.group2,main="Mesophilic Low-solids",vertex.frame.color=NA,vertex.label=NA,edge.width=0.2,
     vertex.size=5, edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))#,layout=layout.graphopt
set.seed(123)
plot(igraph.group3,main="Mesophilic",vertex.frame.color=NA,vertex.label=NA,edge.width=0.2,
     vertex.size=5, edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))
set.seed(123)
plot(igraph.group4,main="Mesophilic Co-digest",vertex.frame.color=NA,vertex.label=NA,edge.width=0.2,
     vertex.size=5, edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))#,layout=layout.graphopt
set.seed(123)
plot(igraph.group5,main="Thermophilic",vertex.frame.color=NA,vertex.label=NA,edge.width=0.2,
     vertex.size=5, edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))

##############random networks
igraph.random.group5=erdos.renyi.game(length(V(igraph.group5)),length(E(igraph.group5)),type="gnm")
igraph.random.group4=erdos.renyi.game(length(V(igraph.group4)),length(E(igraph.group4)),type="gnm")
igraph.random.group3=erdos.renyi.game(length(V(igraph.group3)),length(E(igraph.group3)),type="gnm")
igraph.random.group2=erdos.renyi.game(length(V(igraph.group2)),length(E(igraph.group2)),type="gnm")
igraph.random.group1=erdos.renyi.game(length(V(igraph.group1)),length(E(igraph.group1)),type="gnm")

##############characteristics of network
#group 1 as example
igraph.group1.charac=c( transitivity(igraph.group1), #clustering.coefficient
                        transitivity(igraph.random.group1), #clustering coefficient of random networks
                        modularity(igraph.group1,membership(fc.group1)),#modularity
                        sum(igraph.group1.weight>0)/(sum(igraph.group1.weight>0)+sum(igraph.group1.weight<0)),#positive.ratio
                        mean(igraph::degree(igraph.group1)),#ave.degree
                        average.path.length(igraph.group1), #ave.path.length
                        diameter(igraph.group1, directed = FALSE, unconnected = TRUE, weights = NULL),#diameter
                        length(E(igraph.group1)),#size
                        length(V(igraph.group1)),#order
                        edge_density(igraph.group1,loops=FALSE),#edge.density
                        edge_connectivity(igraph.group1),#edge.connectivity
                        no.clusters(igraph.group1),#number.cluster
                        mean(degree(igraph.group1)/length(V(igraph.group1))),#normalized degree
                        mean(betweenness(igraph.group1,normalized=T)),#normalized betweenness
                        centralization.betweenness(igraph.group1)$centralization) #betweenness.centralization

###############Normalized betweenness and normalized degree of individual genera
degree(igraph.group1)/length(V(igraph.group1))#normalized degree
betweenness(igraph.group1,normalized=T)#normalized betweenness



