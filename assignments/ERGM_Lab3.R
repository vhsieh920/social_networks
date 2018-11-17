# Lab 3
#
# Install packages below if you do not have them:
# -------------------------------------------------
 install.packages("statnet")
 install.packages("coda")
 install.packages("latticeExtra")
# install.packages("igraph")

library(statnet)

# -------------------------------------------------------------------------------------------------
# Set the working directory
# Session > Set Working Directory > To Source File Location
# -------------------------------------------------------------------------------------------------
list.files() # List the files in the current working directory to see if you're in the right directory


# ----------------------------------------------------------------------------------------------------
######################## PART I: Building and Visualizing the Networks ########################
# ----------------------------------------------------------------------------------------------------

# Load the network file (CRIeq)
A     <- matrix(scan("CRIeq.txt", n=17*17), 17, 17, byrow = TRUE)    # Make an R matrix from the CRIeq file
CRIeq <- as.network.matrix(A, matrix.type="adjacency")               # Turn that matrix into a network

# Load the attribute file (EXeq_cons)
att <- read.table("EXeq_cons.txt", header = T)    # This reads the attribute table and saves in in a variable called 'att'
att <- att$expertise                             # This converts those attributes into an R vector
set.vertex.attribute(CRIeq, "EX", att,v = 1:17)    # This sets those attributes as vertex attributes in the network you created above

# Load the covariate network (CAIeq)
B     <- matrix(scan("CAIeq.txt", n =17*17), 17, 17, byrow =TRUE)  # Reads in the CAIeq file and saves it as an R matrix
CAIeq <- as.network.matrix(B, matrix.type="adjacency")             # Converts that matrix into a network


# To save objects which can be reloaded later, uncomment the 2 lines below:
# save.image("Lab3_files.RData")
# load("Lab3_files.RData")


# ---------------------------------------------------------------------------------------
# Basic information
# ---------------------------------------------------------------------------------------
summary(CRIeq)                              # summarize the CRIeq network
network.size(CRIeq)                         # print out the network size
betweenness(CRIeq)                          # calculate betweenness for the network
isolates(CRIeq)                             # find the isolates in the network

summary(CAIeq)                              # summarize the CAIeq network

# ---------------------------------------------------------------------------------------
# Visualize the networks
# ---------------------------------------------------------------------------------------
library('igraph')

# Set default plot options
igraph_options(vertex.size = 11, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.4)  # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads

# Plot the base network - H1
CRIeq_net <- graph.adjacency(A) # make an igraph network object from the CRIeq adjacency matrix
net_layout <- layout_with_fr(CRIeq_net) # spring-embedded layout
plot(CRIeq_net, layout=net_layout, edge.color='red')

# Size nodes via expertise - H3a
plot(CRIeq_net, layout=net_layout, edge.color='darkgreen',vertex.size=att*20)

# Plot the covariate network - H4
CAIeq_net <- graph.adjacency(B) # make an igraph network object from the CAIeq adjacency matrix
plot(CAIeq_net,layout=net_layout,edge.color="purple")

# -------------------------------------------------------------------------------------------------
######################## PART II: Build the ERGM models ########################
#
# R vignette for more details: https://cran.r-project.org/web/packages/ergm/ergm.pdf
# -------------------------------------------------------------------------------------------------
# Remove the 'igraph' package from your environment. 
detach(package:igraph)
library(statnet)

help('ergm-terms')
model1 <- ergm(CRIeq ~ edges 
               + mutual           # H1
               + transitive       # H2a: Transitive triads ( type 120D, 030T, 120U, or 300)
               + nodeicov("EX")   # H3a
               + nodeocov("EX")   # H3b
               + edgecov(CAIeq)   # H4
) 
summary(model1) 


model2 <- ergm(CRIeq ~ edges 
               + mutual                           # H1
               + dgwesp(0.5, fixed=T, type="OTP")  # H2b: OTP "transitive shared partner" ordered pair (i,j) iff i->k->j.
               + nodeicov("EX")                   # H3a
               + nodeocov("EX")                   # H3b
               + edgecov(CAIeq)                   # H4
) 
summary(model2) 


# -------------------------------------------------------------------------------------------------
######################## PART III: Model diagnostics ########################
# 
# Change 'file_name' and 'model' variables to refer to the desired model (model1, or model2)
# -------------------------------------------------------------------------------------------------
file_name  <- 'model_diagnostics_2.pdf'
model      <- model2            # Insert one of the models here! (model1 or model2)

pdf('model_diagnostics_2.pdf')  # Saves the model diagnostic as a PDF - look for this in your current working directory
mcmc.diagnostics(model)        # Performs the markov chain monte carlo diagnostics

dev.off()

# -------------------------------------------------------------------------------------------------
# Goodness of fit test
# Check how well the estimated model captures certain features of the observed network, for example triangles in the network.
# -------------------------------------------------------------------------------------------------
sim <- simulate(model, burnin=100000, interval=100000, nsim=100, verbose=T)  # Uses the ergm model to simulate a null model

# Plot the first of the simulated networks
sim1_net <- igraph::graph.adjacency(as.matrix.network(sim[[1]]))
igraph::plot.igraph(sim1_net,layout=net_layout,edge.color="brown",  
                    vertex.color = 'grey',edge.arrow.size=.2)                                                               

# Plot the 10th simulated network
sim10_net <- igraph::graph.adjacency(as.matrix.network(sim[[10]]))
igraph::plot.igraph(sim10_net,layout=net_layout,edge.color="steelblue",  
                    vertex.color = 'grey',edge.arrow.size=.4)  


# -------------------------------------------------------------------------------------------------
# Extract the number of triangles from each of the 100 samples and
# compare the distribution of triangles in the sampled networks with the observed network
# -------------------------------------------------------------------------------------------------
model.tridist <- sapply(1:100, function(x) summary(sim[[x]] ~triangle)) # Extracts the tiangle data from the simulated networks
hist(model.tridist,xlim=c(0,140),breaks=10)                             # Plots that triangle distribution as a histogram
CRIeq.tri <- summary(CRIeq ~ triangle)                                  # Saves the CRIeq triangle data from the summary to the CRI.eq variable
arrows(CRIeq.tri,20, CRIeq.tri, 5, col="red", lwd=3)                    # Adds an arrow to the plotted histogram
c(obs=CRIeq.tri,mean=mean(model.tridist),sd=sd(model.tridist),
  tstat=abs(mean(model.tridist)-CRIeq.tri)/sd(model.tridist))

hist(model.tridist,xlim=c(0,140),breaks=10) 

par(mfrow=c(1,1))
model.stat <- sapply(1:100, function(x) summary(sim[[x]] ~ nodemix)) # Extracts the k-star data from the simulated networks
hist(model.stat,xlim=c(0,140),breaks=10)                             # Plots that k-star distribution as a histogram
CRIeq.triple <- summary(CRIeq ~ ctriple)                                  # Saves the CRIeq k-star data from the summary to the CRI.eq variable
arrows(CRIeq.triple,20, CRIeq.triple, 5, col="red", lwd=3)                    # Adds an arrow to the plotted histogram
c(obs=CRIeq.triple,mean=mean(model.stat),sd=sd(model.stat),
  tstat=abs(mean(model.stat)-CRIeq.triple)/sd(model.stat))

hist(model.tridist,xlim=c(0,140),breaks=10) 

# -------------------------------------------------------------------------------------------------
# Test the goodness of fit of the model
# Compiles statistics for these simulations as well as the observed network, and calculates p-values 
# -------------------------------------------------------------------------------------------------
gof <- gof(model ~ idegree + odegree + espartners + distance, verbose=T, burnin=1e+5, interval=1e+5) 

#par(mfrow=c(2,2))   # Separate the plot window into a 2 by 2 orientation; This line may cause an error depending on screen resolution. Do not recommend using. Reset using par(mfrow=c(1,1)) command
plot(gof)           # Plot the goodness of fit

gof                 # Display the goodness of fit info in the console
