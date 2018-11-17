# Install the NetData package & SNA package 
# Note: you need to install packages only ONCE
install.packages('NetData')
install.packages('sna')
install.packages('intergraph')

# Load libraries
# Note: need to load these packages every time
library('intergraph')
library('igraph')
library('network')
library('sna')

library('NetData')

# Make sure you've set your directory to the folder where the "krackhardt_css_data.RData" is saved
# Load the Krackhardt advice/friendship data
# For more info see: https://cran.r-project.org/web/packages/NetData/NetData.pdf
load('krackhardt_css_data.RData')

# ---------------------------------------------------------------------------------
######################## PART I: Make aggregated networks ########################
# 
# These networks combine respondent self report data in different ways
# Read each line of code and try to understand how it is combining the individual networks
# into a single network (e.g. by taking the the union, intersection, etc.)
# ---------------------------------------------------------------------------------

# Read the consensus function documentation so you understand what the code below is doing
?consensus 

# Make single networks from the friendship responses
fr_column <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="OR.col")
fr_row    <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="OR.row")
fr_intersection <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="LAS.intersection")
fr_union  <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="LAS.union")
fr_median <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="central.graph")

# Make single networks from the advice responses
ad_column <- consensus(advice_nets, mode="digraph", diag=FALSE, method="OR.col")
ad_row    <- consensus(advice_nets, mode="digraph", diag=FALSE, method="OR.row")
ad_intersection <- consensus(advice_nets, mode="digraph", diag=FALSE, method="LAS.intersection")
ad_union  <- consensus(advice_nets, mode="digraph", diag=FALSE, method="LAS.union")
ad_median <- consensus(advice_nets, mode="digraph", diag=FALSE, method="central.graph")

# ----------------------------------------------------------------------------------------------------
######################## PART II: Visualization and Inspection ########################
#
# TIP: Use the arrows in the Plots tab to navigate the plots you generate.
# ----------------------------------------------------------------------------------------------------
# Remove the 'sna' and 'network' packages from your environment. 
# They share some function names with igraph so doing this is advised
detach(package:sna)
detach(package:network)
library('igraph')
library('intergraph')

# Set default plot options
igraph_options(vertex.size = 15, vertex.color = 'lightsteelblue', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.3)  # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads

# Plot a single aggregated network
fr_intersection_net <- graph.adjacency(fr_union)                 # make an igraph network object from the friend union adjacency matrix
net_layout   <- layout.fruchterman.reingold(fr_union_net) # this makes a spring embedded layout
net_layout2  <- layout.circle(fr_intersection_net)               # this makes a circle layout
plot(fr_intersection_net, layout=net_layout, edge.color='purple')  # plots the friendship union network, look at the 'Plots' tab

ad_union_net <- graph.adjacency(ad_union)  
net_layout3  <- layout.fruchterman.reingold(ad_union_net) 
plot(ad_union_net, layout=net_layout3, edge.color='green')

# Note: if you want to change the layout of your visualization, change plot(... layout=net_layout2,...)
# Note: your default layout is a spring embedded layout

# Plot a single user's self reported network
respondent_1_net <- asIgraph(advice_nets[[1]]) 
plot(respondent_1_net, layout=net_layout2, edge.color='green') 

respondent_1_net <- asIgraph(friendship_nets[[1]]) # this makes an igraph network of the respondent's network from the indexed # in the [[]]
plot(respondent_1_net, layout=net_layout2, edge.color='purple') 

respondent_21_net <- asIgraph(advice_nets[[21]]) 
plot(respondent_21_net, layout=net_layout2, edge.color='green') 

respondent_21_net <- asIgraph(friendship_nets[[21]]) # this makes an igraph network of the respondent's network from the indexed # in the [[]]
plot(respondent_21_net, layout=net_layout2, edge.color='purple')      # plots respondent 1's self reported advice network

# Plot the intersection of two networks
fr_union_net <- graph.adjacency(fr_union)
ad_union_net <- graph.adjacency(ad_union)                           # makes a network object from the union of the advice networks
union_intersection <- graph.intersection(fr_union_net,ad_union_net) # this makes an igraph network object from two matrices
plot(union_intersection, layout=net_layout, edge.color='darkgreen')     # plot the intersection of the friendship & advice union networks


# ----------------------------------------------------------------------------------------------------
######################## PART III: STRUCTURAL EQUIVALENCE ########################
# ----------------------------------------------------------------------------------------------------
# Remove the 'igraph' package from your environment. 
detach(package:igraph) 
library('sna') # Load the 'sna' package

?sedist # read the documentation on sedist so that you understand what the below code is doing

ad_sem <- sedist(ad_intersection, g=c(1:dim(ad_union)[1]), method="euclidean") # compute structural equivalence in the advice network
fr_sem <- sedist(fr_intersection, g=c(1:dim(fr_union)[1]), method="euclidean") # compute structural equivalence in the friendship network

View(ad_sem) # View a matrix
View(fr_sem)

### This is a helper code
# Run this script after you have calculated the structural equivalence matrices

# Advice SEM Summary Statistics
diag(ad_sem) <- NA # disregards diagonal elements
ad_sem_max <- max(ad_sem,na.rm=TRUE)
ad_sem_min <- min(ad_sem,na.rm=TRUE)   
ad_sem_mean <- mean(ad_sem,na.rm=TRUE)

# Friendship SEM Summary Statistics
diag(fr_sem) <- NA # disregards diagonal elements
fr_sem_max <- max(fr_sem,na.rm=TRUE)
fr_sem_min <- min(fr_sem,na.rm=TRUE)   
fr_sem_mean <- mean(fr_sem,na.rm=TRUE)

# Return exact location
# Example: max element of fr_sem
which(fr_sem==fr_sem_min,arr.ind = TRUE)[1,]
which(fr_sem==fr_sem_max,arr.ind = TRUE)[1,]
which(ad_sem==ad_sem_min,arr.ind = TRUE)[1,]
which(ad_sem==ad_sem_max,arr.ind = TRUE)[1,]


# ----------------------------------------------------------------------------------------------------
######################## PART IV: DIFFERENCES & CORRELATION ########################
# ----------------------------------------------------------------------------------------------------
?qaptest                # read the documentation on qaptest
?plot.qaptest           # read the documentation to help you interpret the qaptest plot. 

# Compare each respondent's advice network with the median network
ad_median_net <- network(ad_median, directed=TRUE)      # create a median advice network

ad_correlations <- vector("numeric",21)           # initialize vector to store correlations
for (i in 1:21){       # loop over each advice network and analyze individually;
  print(i)                
  ad_qap <- qaptest(list(advice_nets[[i]],ad_median_net),gcor,g1=1,g2=2,reps=1000)   # use QAP to determine significance
  s <- summary(ad_qap)                                  # summary of current individual's QAP test.
  print(s)                                             
  ad_correlations[i] <- s$testval           # save the correlation for individual i
}

plot.qaptest(ad_qap)     # plots qaptest
                         # Note this is just the plot for the final ad_qap from the above loop!

message(paste("Proportion of draws which were >= observed value:", ad_qap$pgreq))
message(paste("Proportion of draws which were <= observed value:", ad_qap$pleeq))

# Compare each respondent's friendship network with the median network
fr_median_net <- network(fr_median, directed=TRUE)   # create a median friendship network to compare against

fr_correlations <- vector("numeric",21)           # initialize vector to store correlations
for (i in 1:21){       # loop over each friendship network and analyze individually
  print(i)                    
  fr_qap <- qaptest(list(friendship_nets[[i]],fr_median_net),gcor,g1=1,g2=2,reps=1000)    # use QAP to determine significance
  s <- summary(fr_qap)                                  # summary of current individual's QAP test.
  print(s)                                             
  fr_correlations[i] <- s$testval           # save the correlation for individual i
}

plot.qaptest(fr_qap) 
message(paste("Proportion of draws which were >= observed value:", fr_qap$pgreq))
message(paste("Proportion of draws which were <= observed value:", fr_qap$pleeq))

# Calculate centralities for the advice union consensus network
g.adj <- igraph::graph.adjacency(ad_union) 
d <- igraph::degree(g.adj)
b <- igraph::betweenness(g.adj,directed=TRUE)
c <- igraph::closeness(g.adj)
e <- igraph::eigen_centrality(g.adj,directed=TRUE)$vector

# Calculate the linear correlation between each centrality measure and the accuracy in predicting the median network
cor(ad_correlations, d, method="kendall") 
cor(ad_correlations, b, method="kendall") 
cor(ad_correlations, c, method="kendall") 
cor(ad_correlations, e, method="kendall") 

# Optional: repeat the same procedure for the friendship network. 
# Note: you must first adapt the code for the advice network to store the correlations

# Compare each respondent's friendship network with their advice network
for (i in 1:21){
  print(i)                                             
  fr_ad_qap <- qaptest(list(friendship_nets[[i]],advice_nets[[i]]),gcor,g1=1,g2=2,reps=1000) # does a qap test comparing that individual's friendship & advice networks
  print(summary(fr_ad_qap))                                                            
}

plot.qaptest(fr_ad_qap)  # plots qaptest
message(paste("Proportion of draws which were >= observed value:", fr_ad_qap$pgreq))
message(paste("Proportion of draws which were <= observed value:", fr_ad_qap$pleeq))


