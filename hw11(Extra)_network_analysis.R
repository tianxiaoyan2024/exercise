# --------------------------------------------
# Script Name: network_analysis.R
# Purpose: This scribes how to construct a network and  
#          conduct an analysis of the network properties.

# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-05
#
# --------------------------------------------

cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# Step01 - Construct a network and save it as edgelist

library(igraph)
library(RCurl)

# C) Microbiome sequencing network networks as graphs and exploration
# Download and check the data
download.file(url="http://ieg4.rccc.ou.edu/MENA/download/Control.txt",destfile="D:/中科大研一第二学期/数据驱动生态学/Control.txt")
otu_c <- read.table("D:/中科大研一第二学期/数据驱动生态学/Control.txt", head=T, 
                    row.names = 1, sep = "\t")
head(otu_c)
dim(otu_c) # check dataframe
sum(!is.na(otu_c)) 

# Clean data by removing rows with 5 or more NA values

cnt_na1 <- apply(otu_c, 1, function(current_row) sum(is.na(current_row)))
otu_cd <- otu_c[cnt_na1 < 5,]
head(otu_wd)

# Represent relative abundance of OTUs in all samples

otu_cd_tran <- otu_cd |>
  replace(is.na(otu_cd), 0) |>
  t()
view(otu_cd)
view(otu_cd_tran)

otu_cd_rel <- prop.table(as.matrix(otu_cd_tran), 
                         margin = 1)*100
head(otu_cd_rel)

# Calculate correlation coefficient

library(psych) # for correlation coefficient
occor_c <- corr.test(otu_cd_rel, use = "pairwise",
                     method = "spearman", adjust = "fdr",
                     alpha = 0.05)
occor_c_r <- occor_c$r # extracting r values
occor_c_r
occor_c_p <- occor_c$p # extracting p valutes
occor_c_p
occor_c_r[occor_c_p >0.5 | abs(occor_c_r)<0.79] <-0

view(occor_c_r)

# construct co-occurrence network and save it as edgelist

library(igraph)
igraph_c_r <- graph_from_adjacency_matrix(occor_c_r,
                                          mode = "undirected",
                                          weighted = TRUE,
                                          diag = FALSE)
c_bad_vs <- V(igraph_c_r)[degree(igraph_c_r) == 0]
igraph_c_final <- delete.vertices(igraph_c_r, c_bad_vs)

set.seed(233)
plot(igraph_c_final, main ="co-occurrence network of OUTs in the control treatment",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=6,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)

adj_matrix <- get.adjacency(igraph_c_final, sparse = TRUE)

write_graph(igraph_c_final, "D:/中科大研一第二学期/数据驱动生态学/igraph_c.txt", "edgelist")

# Step02 -  an analysis of the network properties

# A) degree and degree distribution

library(igraph)
g <-read_graph("D:/中科大研一第二学期/数据驱动生态学/igraph_c.txt", "edgelist") 
g <- as.undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)
deg <- degree(g, mode="all") # calculate degree
deg

hist(deg, breaks=1:vcount(g)-1) # degree distribution

# B) closeness and betweenness centrality

deg=degree(g) 
lay <- layout.fruchterman.reingold(g) # fix layout
lay
fine = 500 # increase fine regulation
palette = colorRampPalette(c('blue','red')) # set color
degCol = palette(fine)[as.numeric(cut(deg,breaks = fine))]
plot(g, layout=lay, vertex.color=degCol, 
     vertex.size=deg*1.5, vertex.label=NA)

betw <- betweenness(g) # betweenness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=betw*0.8, vertex.label=NA)

clos <- closeness(g) # closeness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=clos*15000,vertex.label=NA)
ev <- evcent(g)
ev <- evcent(g)$vector
ev
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=ev*10, vertex.label=NA)

# C) network connectance

library(igraph)
g<-read_graph("D:/中科大研一第二学期/数据驱动生态学/igraph_c.txt", "edgelist")
g <- as.undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)
connectance = edge_density(g,loops=FALSE)# connectance
connectance

# D) modularity

library(igraph)
g<-read_graph("D:/中科大研一第二学期/数据驱动生态学/igraph_c.txt", "edgelist")
g <- as.undirected(g, mode = "collapse")
ceb <- cluster_edge_betweenness(g)
modularity(ceb)
plot(ceb, g)
