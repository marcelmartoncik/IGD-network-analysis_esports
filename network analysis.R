library(bootnet)
library(qgraph)
library(mgm)

# Network analysis --------------------------------------------------------

# netIGD <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", tuning = .50) ##W/o threshold
netIGD <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(netIGD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(netIGD, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# denseNetIGD <- netIGD <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor", tuning = 0)
# plot(denseNetIGD, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# cor(as.vector(netIGD$graph), as.vector(denseNetIGD$graph))  
stabIGD <- bootnet(netIGD, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabIGD)
plot(stabIGD, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootIGD <- bootnet(netIGD, 
                   nBoots = 1000, 
                   statistics = c("Strength","edge", "EI"),
                   type = "nonparametric",
                   verbose = FALSE) 

#plot centrality CIs
plot_boot_dybocs_centrality <- plot(boot_dybocs, c("Strength"))
plot_boot_dybocs_centrality
simIGD <- netSimulator(netIGD,
                        default = "EBICglasso",
                        nCases = c(250,500, 1000, 2000),
                        nReps = 100,
                        nCores = 8)
plot(simIGD)
repIGD <- replicationSimulator(netIGD,
                                default = "EBICglasso",
                                nCases = c(250,500, 1000, 2000),
                                nReps = 100,
                                nCores = 8)
plot(repIGD, statistics = "correlation")

igdTypes <- rep("g", 9)
igdLevels <- rep(1, 9)

mgmNetIGD <- mgm(select(esports, IGDS9SF_1:IGDS9SF_9), 
                 type = igdTypes,
                 level = igdLevels,
                 lambdaSel = "EBIC", 
                 lambdaGam = .5, 
                 ruleReg = "AND")
predMgmNetIGD <- predict(mgmNetIGD, select(esports, IGDS9SF_1:IGDS9SF_9))
predMgmNetIGD$errors

plotMgmNetIGD <- qgraph(mgmNetIGD$pairwise$wadj, 
                          layout = "spring",
                          repulsion = 1, 
                          title = "IGD net",
                          pie = predMgmNetIGD$errors[,3],
                          labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                          label.cex = 1,
                          label.scale = FALSE)


corMat <- cor_auto(select(esports, IGDS9SF_1:IGDS9SF_9))
Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0,
                      sampleSize = nrow(esports))
Graph_lasso2 <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.5,
                      sampleSize = nrow(esports))

