library(readr)
library(igraph)
library(tidyverse)
library(writexl)
library(visNetwork)

#remove environment objects
rm(list=ls())

#reading csv file
authors_net <- read_csv("ShinyApp/authors_net.csv")

#subset -> take rows with this condition only
authors_n <- authors_net[grep("--", authors_net$Authors), ]

# data wrangling from https://stackoverflow.com/questions/57487704/how-to-split-a-string-of-author-names-by-comma-into-a-data-frame-and-generate-an
SplitAuthors <- sapply(authors_n$Authors, strsplit, split = "--", fixed = TRUE)
AuthorCombinations <- sapply(SplitAuthors,function(x){combn(unlist(x),m = 2)})
AuthorEdges <- rapply(AuthorCombinations,unlist)
names(AuthorEdges) <- NULL
AuthorEdges <- trimws(AuthorEdges)
AuthorGraph <- graph(AuthorEdges, directed = FALSE)

#first plot: non interactive plot
plot(AuthorGraph,layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.cex = 0.7, mode = "circle")

#visNetwork package code
Nodes <- data.frame(id=as_ids(V(AuthorGraph)), label=as_ids(V(AuthorGraph)), 
                    title = paste0("<p><b>", as_ids(V(AuthorGraph)),"</b><br></p>"))
Edges <- data.frame(matrix(AuthorEdges, ncol=2, byrow = T))
colnames(Edges) <- c("from", "to")

visNetwork(Nodes, Edges) %>%
  visIgraphLayout(layout = "layout_with_fr") 
    #%>% visEdges(arrows = "middle")
