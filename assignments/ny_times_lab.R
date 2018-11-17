# MSiA-490 Class
# Lab 1


######################################################################################
#
# Intro (DON'T INCLUDE IN REPORT)
#
######################################################################################

# Lines that start with a hashtag/pound symbol, like this one, are comment lines.
# Comment lines are ignored by R when it is running the code.

# To run a non-commented line in RStudio, click the "Run" button above
# or press Ctrl+Enter on Windows and Cmd+Enter on macOS.
# Any output will be printed in the Console pane (placed below) and all plots
# will be displayed in the Plots pane (placed bottom right).
# Try running the lines below and observing what happens:

print("Hello, R!")
plot(1:10)

# Complete the lab work by following the instructions and running the provided code.
# You may be required to edit some lines before running them to achieve the desired result.

######################################################################################
#
# Import the necessary libraries (DON'T INCLUDE IN REPORT)
#
######################################################################################

# First time you run this file, you will need to install several packages.
# To do that, run code lines 36-41. It may take up a copule of minutes.
# You only need to install packages once, next time you should skip those lines.
# install.packages('magrittr', repos = "https://cran.rstudio.com")
# install.packages('igraph', repos = "https://cran.rstudio.com")
# install.packages('httr', repos = "https://cran.rstudio.com")
# install.packages('data.table', repos = "https://cran.rstudio.com")
# install.packages('dplyr', repos = "https://cran.rstudio.com")
# install.packages('xml2', repos = "https://cran.rstudio.com")

# Now run the lines below to load the packages you have installed.
# You need to load packages every time you run the script or restart R.
library(magrittr)
library(httr)
library(data.table)
library(igraph)
library(dplyr)
library(xml2)

######################################################################################
#
# Set current directory (DON'T INCLUDE IN REPORT)
#
######################################################################################

# In this step you tell R where to look for your files.
# From the menu, select "Session > Set Working Directory... > To Source File Location".

# Alternatively, if you know the filename, you can uncomment the line below and run it.
# setwd("replace this with path to your directory")

######################################################################################
#
# Part I: Import data and create graph objects
#
######################################################################################

# Import your word list
name_of_file <- "ListOfWords.txt" # Creates a variable called name_of_file that will contain the name
    # of your text file. Change it if it's someting other than "ListOfWords.txt".
word_list <- read.table(name_of_file, sep = "\n", stringsAsFactors = F) %>% unlist %>% as.vector 
    # Reads the content of your file into a variable.
num_words <- length(word_list) # Creates a variable with the number of words in your list.
url_base <- "https://api.nytimes.com/svc/search/v2/articlesearch.json"
# When you receive the email with your API key, paste it below between the quotation marks.
api <- '155e80dce8bc47459c6e7c3a9427f171' #Insert your key here. Visit https://developer.nytimes.com/signup 

# Our first function will gather all of the search terms and their number of hits
# to be placed in a table. All lines of a function should be run together.
Get_hits_one <- function(keyword1) {
  Sys.sleep(time=3)
  url <- paste0(url_base, "?api-key=", api, "&q=", URLencode(keyword1),"&begin_date=","20160101") # Begin date is in format YYYYMMDD; you can change it if you want only more recent results, for example.
  print(keyword1)
  hits <- content(GET(url))$response$meta$hits %>% as.numeric
  print(hits) # The number of results
  c(SearchTerm=keyword1,ResultsTotal=hits) # Put results in a table
}

# Now we will invoke our function to put information from the API into our global environment.
total_table <- t(sapply(word_list,Get_hits_one)) # Create a table of your words and their number of results.
total_table <- as.data.frame(total_table, stringsAsFactors = F)
total_table$ResultsFactor <- total_table$ResultsTotal
total_table$ResultsTotal <- as.numeric(as.character(total_table$ResultsTotal))

# If you get zero hits for any of these terms, you should substitute that term
# for somethign else and rerun the lab up to this point.

# Next, we will define the function that will collect the term co-occurences network.
Get_hits_two <- function(row_input) {
  keyword1 <- row_input[1]
  keyword2 <- row_input[2]
  url <- paste0(url_base, "?api-key=", api, "&q=", URLencode(keyword1), "+", URLencode(keyword2),"&begin_date=","20160101") #match w/ Begin Date in Get_hits_one.
  print(paste0(keyword1," ",keyword2))
  hits <- content(GET(url))$response$meta$hits %>% as.numeric
  print(hits) # The number of results
  Sys.sleep(time=3)
  c(SearchTerm1=keyword1,SearchTerm2=keyword2,CoOccurrences=hits) # Put results in table
} 

# Grab some coffee, it's going to be a while while we calculate the pair relationships.
pairs_list <- expand.grid(word_list,word_list) %>% filter(Var1 != Var2)
pairs_list <- t(combn(word_list,2))
network_table <- t(apply(pairs_list,1,Get_hits_two)) # Hold on, this takes a while
network_table <- as.data.frame(network_table, stringsAsFactor = F)
network_table$CoOccurrences <- as.numeric(as.character(network_table$CoOccurrences))
total_table <- as.data.table(total_table)
network_table <- as.data.table(network_table)

# You should save your data at this point by clicking the floppy disk icon under the "Environment" tab.

network_table <- read.table("nyt_term_data.RData")

# Remove zero edges from your network
network_table <- network_table[!CoOccurrences==0] 

# Create a graph object with your data
g_valued <- graph_from_data_frame(d = network_table[,1:3,with=FALSE],directed = FALSE,vertices = total_table)

### Dichotomize values in the network

# Mean cut
network_table$mean.cut <- ifelse(network_table$CoOccurrences >= mean(network_table$CoOccurrences),yes = 1,no = 0)

# Quartile cuts - get the 25% percentile, 50% percentile, and 75% percentile
quartiles <- quantile(network_table$CoOccurrences)
network_table$per25.cut <- ifelse(network_table$CoOccurrences >= quartiles[2],yes = 1,no = 0)
network_table$per50.cut <- ifelse(network_table$CoOccurrences >= quartiles[3],yes = 1,no = 0)
network_table$per75.cut <- ifelse(network_table$CoOccurrences >= quartiles[4],yes = 1,no = 0)

# Create the Valued Network
g_valued <- graph_from_data_frame(d = network_table[,1:3,with=FALSE],directed = FALSE,vertices = total_table)
# A network cut at the mean value of ties between pairs of nodes
g_mean  <- graph_from_data_frame(d = network_table[mean.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)
# A network cut at the 25th %ile, ranked by number of ties between pairs
g_25per <- graph_from_data_frame(d = network_table[per25.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)
# A network cut at the 50th %ile, ranked by number of ties between pairs
g_50per <- graph_from_data_frame(d = network_table[per50.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)
# A network cut at the 75th %ile, ranked by number of ties between pairs
g_75per <- graph_from_data_frame(d = network_table[per75.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)

# Count the number of nodes/edges in all of your graphs. Here's an example:
numVertices <- vcount(g_valued)
numVertices
numEdges <- ecount(g_valued)
numEdges

# Maximum possible number of edges
numVertices*(numVertices-1)/2

# nodes/edges for g_mean
numVertices_gmean <- vcount(g_mean)
numVertices_gmean
numEdges_gmean <- ecount(g_mean)
numEdges_gmean

# nodes/edges for g_25per
numVertices_g25per <- vcount(g_25per)
numVertices_g25per
numEdges_g25per <- ecount(g_25per)
numEdges_g25per

# nodes/edges for g_50per
numVertices_g50per <- vcount(g_50per)
numVertices_g50per
numEdges_g50per <- ecount(g_50per)
numEdges_g50per

# nodes/edges for g_75per
numVertices_g75per <- vcount(g_75per)
numVertices_g75per
numEdges_g75per <- ecount(g_75per)
numEdges_g75per

# What is the density of your networks? 
graphDensity1 <- graph.density(g_valued)
graphDensity2 <- graph.density(g_mean)
graphDensity3 <- graph.density(g_25per)
graphDensity4 <- graph.density(g_50per)
graphDensity5 <- graph.density(g_75per)

graphDensity1
graphDensity2
graphDensity3
graphDensity4
graphDensity5

## Sanity check - do these values equal 2 * numEdges / (numVertices * (numVertices-1))? Same as, numEdges / (numVertices * (numVertices-1) / 2 )

######################################################################################
#
# Part II: Visualize your networks
#
######################################################################################

colbar = rainbow(length(word_list)) ## we are selecting different colors to correspond to each 
V(g_valued)$color = colbar
V(g_mean)$color = colbar
V(g_25per)$color = colbar
V(g_50per)$color = colbar
V(g_75per)$color = colbar

# Set layout here 
L = layout_with_fr(g_valued)  # Fruchterman-Reingold

# Plot graph
plot(g_25per,vertex.color=V(g_mean)$color, layout = L, vertex.size=6) 
plot(g_75per,vertex.color=V(g_75per)$color, layout = L,vertex.size=6) #vertex.label=NA

# Experiment with other layouts by setting L to a different setting, then running a plot function above.
# L = layout_with_...(g_...)
# For that, you can pick one of the options below.
# Note that whatever you define L as last will remain for future uses of L.
L = layout_with_dh(g_valued) # Davidson and Harel
L = layout_with_drl(g_valued) # Force-directed
L = layout_with_kk(g_valued) # Spring

# You can play with the vertex sizes and/or edge sizes to get a better vizualization for the graphs below
plot(g_valued,vertex.color=V(g_valued)$color, layout = L, vertex.label=NA,vertex.size=6) 
plot(g_mean,vertex.color=V(g_mean)$color, layout = L, vertex.label=NA,vertex.size=6)
plot(g_25per,vertex.color=V(g_25per)$color, layout = L, vertex.label=NA,vertex.size=6)
plot(g_50per,vertex.color=V(g_50per)$color, layout = L, vertex.label=NA,vertex.size=6)
plot(g_75per,vertex.color=V(g_75per)$color, layout = L, vertex.label=NA,vertex.size=6)

######################################################################################
#
# Part III: Independent Metrics
#
######################################################################################

# Plot the number of clusters in the graph and their size
# there are also other algorithms for this you may want to explore
cluster <- cluster_walktrap(g_25per)

# Find the number of clusters
membership(cluster)   # affiliation list
length(sizes(cluster)) # number of clusters

# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster) 

# Visualize clusters - that puts colored blobs around the nodes in the same community.
# You may want to remove vertex.label=NA to figure out what terms are clustered.
plot(cluster, g_50per, col = V(g_50per)$color, layout = L, vertex.size=6)

# Repeat with other four networks (add your own code, substituting g_mean, etc. for g_50per)
plot(cluster, g_mean, col = V(g_mean)$color, layout = L, vertex.label=NA, vertex.size=6)
plot(cluster, g_valued, col = V(g_valued)$color, layout = L, vertex.label=NA, vertex.size=6)
plot(cluster, g_25per, col = V(g_25per)$color, layout = L, vertex.label=NA, vertex.size=6)
plot(cluster, g_75per, col = V(g_75per)$color, layout = L, vertex.label=NA, vertex.size=6)


# Centrality calculations
# ---------------------------------------

# Compute centralities; swap g_25per for different networks
totalDegree <- degree(g_mean,mode="all")
sort(totalDegree,decreasing=TRUE)[1:10]

b <- betweenness(g_mean,directed=TRUE)
sort(b,decreasing=TRUE)[1:10]

c <- closeness(g_mean)
sort(c,decreasing=TRUE)[1:10]

eigc <- eigen_centrality(g_mean,directed=TRUE)
sort(eigc$vector,decreasing=TRUE)[1:10]

eigc <- eigen_centrality(g_25per,directed=TRUE)
sort(eigc$vector,decreasing=TRUE)[1:10]

# For undirected matrices the adjacency matrix is symmetric and the hub scores are the same as authority scores,
a <- authority_score(g_mean, scale = TRUE)
sort(a$vector,decreasing=TRUE)[1:10]

h <- hub_score(g_25per, scale = TRUE)
sort(h$vector,decreasing=TRUE)[1:10]

# Centrality Visualization
# ---------------------------------------

# Plot based on the centrality
g2 <- g_valued
V(g2)$size <- totalDegree*1 # adjust the number if nodes are too big
plot(g2, layout = L, vertex.label=NA)

# What is the betweenness centrality score for each vertex? 
g4 <- g_25per
V(g4)$size <- b*1  # adjust the number
plot(g4, layout = L)

# What is the closeness centrality score for each vertex? 
g5 <- g_valued
V(g5)$size <- c*200  # adjust the number
plot(g5, layout = L, vertex.label=NA)

# What is the eigenvector centrality score for each vertex? 
g6 <- g_valued
V(g6)$size <- eigc$vector*25 # can adjust the number
plot(g6, layout = L, vertex.label=NA)

# What is the authority score for each vertex? 
g7 <- g_25per
V(g7)$size <- a$vector*40 # can adjust the number
plot(g7, layout = L)

# What is the hub score for each vertex? 
g8 <- g_valued
V(g8)$size <- h$vector*55 # can adjust the number
plot(g8, layout = L)

# Weights coming out of each vertex
graph.strength(g_valued,weights = E(g_valued)$CoOccurrences)

# Weighted betweenness centrality, normalized
btw <- betweenness(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences,normalized = TRUE)
sort(btw,decreasing=TRUE)[1:10]

# Weighted closeness centrality, normalized
close <- closeness(g_valued,mode = "all",weights = E(g_valued)$CoOccurrences,normalized = TRUE)
sort(close,decreasing=TRUE)[1:10]


# Weighted eigenvector centrality, normalized
evcent(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences)

######################################################################################
#
# Part IV: Valued Network Centrality and Centralization
#
######################################################################################

# Degree centralization of split graphs
centralization.degree(g_50per,normalized = TRUE)
# Repeat for other split graphsL g_25per, g_50per, g_75per

# Betweenness centralization of split graphs
centralization.betweenness(g_50per,normalized = TRUE)
# Repeat for other split graphsL g_25per, g_50per, g_75per

# Closeness centralization of split graphs
centralization.closeness(g_50per,normalized = TRUE)
# Repeat for other split graphsL g_25per, g_50per, g_75per

# Eigenvector centralization of split graphs
centralization.evcent(g_50per,normalized = TRUE)
# Repeat for other split graphsL g_25per, g_50per, g_75per

# Calculate degree distribution
deg <- degree(g_50per,v=V(g_50per), mode="all")

# Degree distribution is the cumulative frequency of nodes with a given degree
deg_distr <-degree.distribution(g_50per, cumulative=T, mode="all")

plot(deg_distr)

# Fit a power law to the degree distribution
# The output of the power.law.fit() function tells us what the exponent of the power law is ($alpha)
# and the log-likelihood of the parameters used to fit the power law distribution ($logLik)
# Also, it performs a Kolmogov-Smirnov test to test whether the given degree distribution could have
# been drawn from the fitted power law distribution.
# The function thus gives us the test statistic ($KS.stat) and p-vaule ($KS.p) for that test
power <- power.law.fit(deg_distr)
power

# Plot the degree distribution
plot(deg_distr, log="xy", ylim=c(.01,10), bg="black",pch=21, xlab="Degree", ylab="Cumulative Frequency")


# Average clustering coefficient (ACC)
transitivity(g_50per, type = c("average"))

# Characteristic path length (CPL)
average.path.length(g_50per)

# Generate 1 random networks & compute ACC & CPL
g <- erdos.renyi.game(29, 117, type = "gnm")
transitivity(g, type = c("average"))
average.path.length(g)

# Generate 100 and take the average.
accSum <- 0
cplSum <- 0
for (i in 1:100){
  grph <- erdos.renyi.game(29, 117, type = "gnm")
  accSum <- accSum + transitivity(grph, type = c("average"))
  cplSum <- cplSum + average.path.length(grph)
}

accSum/100
cplSum/100
