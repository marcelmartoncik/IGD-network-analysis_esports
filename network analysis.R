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
                   statistics = c("Strength","edge"),
                   type = "nonparametric",
                   verbose = FALSE) 
plot(bootIGD, c("Strength"))
plot(bootIGD, c("edge"))
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

qgraph(mgmNetIGD$pairwise$wadj, 
       layout = "spring",
       repulsion = 1, 
       title = "IGD net",
       pie = predMgmNetIGD$errors[,3],
       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
       label.cex = 1,
       label.scale = FALSE)

# corMat <- cor_auto(select(esports, IGDS9SF_1:IGDS9SF_9))
# Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0,
#                       sampleSize = nrow(esports))
# Graph_lasso2 <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.5,
#                       sampleSize = nrow(esports))


esportsP <- subset(esports, subset = playing_esports == 1)
esportsNP <- subset(esports, subset = playing_esports == 0)

pIGD <- estimateNetwork(select(esportsP, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
npIGD <- estimateNetwork(select(esportsNP, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(pIGD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
centralityPlot(pIGD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)

L <- averageLayout(pIGD, npIGD)
plot(pIGD, theme = "classic", title = "Group1", layout = L)
plot(npIGD, theme = "classic", title = "Group2", layout = L)

centralityPlot(
  list(group1 = pIGD,
       group2 = npIGD), 
  include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
  decreasing = TRUE)


pVSnp <- NetworkComparisonTest::NCT(select(esportsP, IGDS9SF_1:IGDS9SF_9), select(esportsNP, IGDS9SF_1:IGDS9SF_9), it = 100, weighted = TRUE, 
                                   test.edges = TRUE, edges = "all", p.adjust.methods = "bonferroni", gamma = 0.5,
                                   test.centrality = TRUE, centrality = "all",
                                   progressbar = TRUE)

pVSnp$glstrinv.pval # global strength invariance p-value 
pVSnp$nwinv.pval # maximum difference in any of the edge weights of the observed networks p-value
pVSnp$einv.pvals # the Holm-Bonferroni corrected p-values per edge from the permutation test concerning differences in edges weights
plot(pVSnp, what = "network")
plot(pVSnp, what = "strength")
