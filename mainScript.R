# Libraries ---------------------------------------------------------------

library(bootnet)
library(qgraph)
library(mgm)
library(psych)
library(networktree)
library(magrittr)
library(psychonetrics)
library(NetworkComparisonTest)

# Sourcing auxiliary scripts ----------------------------------------------
rm(list = ls())
# source("data wrangling.R")
# source("codebook.R")
# source("careless responding.R")
# source("recoding variables.R")
# source("data imputation.R")
gamersImp <- readRDS("gamersImputed.Rds")
esportsImp <- readRDS("esportsImputed.Rds")
data <- readRDS("data.Rds")

bootstrapIterations <- 2000

# Estimate the structure of GD together with additional 
# IGD symptoms (n. 1, 2, 3, 5, 7, 8) measured with the highest content validity
# and with two additional items focused on craving and neglecting health 

# Find splits -------------------------------------------------------------
igdStructures <- list(
  igds9sfVarNames = select(esports, IGDS9SF_1:IGDS9SF_9) %>% names(),
  bestVarNames = select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, IGDS9SF_7:IGDS9SF_9) %>% names(),
  bestCravingHealthVarNames = select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health) %>% names(),
  gdtVarNames = select(esports, GDT_1:GDT_4) %>% names(),
  gdtBestCravingHealthVarNames = select(esports, GDT_1:GDT_4, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGD_alternative_criterion5, IGDS9SF_7, IGDS9SF_8) %>% names()
)

mods <- select(esportsImp[[1]], c(gender, gaming_time, MOGQ_social, MOGQ_escape, MOGQ_competition, MOGQ_coping, IGCQ, BFRS, BSCS, neuroticism, DJGLS, harm_avoidance, BAS_reward)) %>% names()

splitsMatrixGamers <- splitsMatrixEsports <- matrix(data = NA, nrow = length(igdStructures), ncol = length(mods),
                                                    dimnames = list(names(igdStructures), mods))

# Split matrices for gamers
# List elements represent split matrices per nIteration iterations
splitMatrixGamersList <- list(NA)
for(d in 1:length(gamersImp)) {
  for(i in 1:length(igdStructures)) {
    for(j in 1:length(mods)) {
      splitGamers <- tryCatch(networktree(nodevars=gamersImp[[d]] %>% select(eval(substitute(igdStructures[[i]]))), 
                                          splitvars=gamersImp[[d]] %>% select(eval(substitute(mods[j]))), method = "ctree")$node$split$breaks,
                              error = function(e) NULL)
      splitsMatrixGamers[i,j] <- if(is.null(splitGamers)) {NA} else {splitGamers}
    }
  }
  splitMatrixGamersList[[d]] <- splitsMatrixGamers
}

# Split matrices for esports
# List elements represent split matrices per nIteration iterations
splitMatrixEsportsList <- list(NA)
for(d in 1:length(esportsImp)) {
  for(i in 1:length(igdStructures)) {
    for(j in 1:length(mods)) {
      splitEsports <- tryCatch(networktree(nodevars=esportsImp[[d]] %>% select(eval(substitute(igdStructures[[i]]))), 
                                           splitvars=esportsImp[[d]] %>% select(eval(substitute(mods[j]))), method = "ctree")$node$split$breaks,
                               error = function(e) NULL)
      splitsMatrixEsports[i,j] <- if(is.null(splitEsports)) {NA} else {splitEsports}
    }
  }
  splitMatrixEsportsList[[d]] <- splitsMatrixEsports
}

# Compute mean matrices of moderator splits
meanSplitMatrices <- lapply(list(meanMatrixGamers = splitMatrixGamersList, meanMatrixEsports = splitMatrixEsportsList), function(m){
  dims <- sapply(m, dim)
  meanMatrix <- matrix(rowMeans(matrix(unlist(m), dims[1, 1]*dims[2, 1], length(m)), na.rm = T), dims[1, 1], dims[2, 1])
  dimnames(meanMatrix) <- dimnames(m[[1]])
  return(meanMatrix)
})

# Add split values for dichotomic variables
meanSplitMatrices <- lapply(meanSplitMatrices, transform, gender = .5)


# Generic network function ------------------------------------------------

# Generic Network (moderator) analysis function
network <- function(data = NA, structureName = NA, moderator = NULL, estimator = "mgm", nBoot = bootstrapIterations, cores = 8){
  set.seed(1)
  nodesVect <- igdStructures[[eval(substitute(structureName))]]
  if(is.null(moderator)) {
    netObj <- estimateNetwork(data %>% select(eval(substitute(nodesVect))), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5, verbose = FALSE)
    centralityPlot <- suppressMessages(centralityPlot(netObj, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"), orderBy = "Strength", print = FALSE))
    netPlot <- plot(netObj, theme = "gray", labels = paste(1:length(nodesVect)))
    netPlot <- recordPlot()
    dev.off()
    stability <- suppressMessages(bootnet(netObj, nBoots = bootstrapIterations, default = "EBICglasso", type = "case", nCores = cores, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"), verbose = FALSE))
    out <- list(
      "Network object" = netObj,
      "Centrality plot" = centralityPlot,
      "Network plot" = netPlot,
      "Stability" = corStability(stability, verbose = FALSE),
      "Stability plot" = plot(stability, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
    )
  } else {
    netMod1 <- tryCatch(estimateNetwork(data %>% filter(.[[eval(substitute(moderator))]] >= meanSplitMatrices[[ifelse(is.null(data$preffered_esports_game_name), 1, 2)]][structureName, moderator]) %>% select(eval(substitute(nodesVect))),
                                        default = estimator, corMethod = "cor_auto", verbose = FALSE),
                        error = function(e) NULL)
    netMod2 <- tryCatch(estimateNetwork(data %>% filter(.[[eval(substitute(moderator))]] < meanSplitMatrices[[ifelse(is.null(data$preffered_esports_game_name), 1, 2)]][structureName, moderator]) %>% select(eval(substitute(nodesVect))),
                                        default = estimator, corMethod = "cor_auto", verbose = FALSE),
                        error = function(e) NULL)
    set.seed(1)
    netComparison <- suppressMessages(NCT(netMod1, netMod2, test.edges = T, test.centrality = T, progressbar = T, it = bootstrapIterations))
    return(netComparison)
  }
}

# Network analysis --------------------------------------------------------

# Networks for individual IGD structures
igdStructuresNets <- rep(list(rep(list(NA), 2)), 5)
for(i in names(igdStructures)) {
  for(d in names(data)) {
    igdStructuresNets[[i]][[d]] <- tryCatch(network(data = data[[d]], structureName = names(igdStructures[i]), estimator = "EBICglasso"),
                                            error = function(e) NULL)
  }
}
# igdStructuresNets

start_time <- Sys.time()
# Invariance of individual network structures across gamers and esports
igdStructuresDiff <- list(NA)
for(i in names(igdStructures)) {
  set.seed(1)
  nodesVect <- igdStructures[[eval(substitute(i))]]
  igdStructuresGamers <- tryCatch(estimateNetwork(data = data[[1]] %>% select(eval(substitute(nodesVect))),
                                                  default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE),
                                  error = function(e) NULL)
  igdStructuresEsports <- tryCatch(estimateNetwork(data = data[[2]] %>% select(eval(substitute(nodesVect))),
                                                   default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE),
                                   error = function(e) NULL)
  igdStructuresDiff[[i]] <- suppressMessages(NCT(igdStructuresGamers, igdStructuresEsports, test.edges = T, test.centrality = T, progressbar = F, it = 1000))
}
# igdStructuresDiff

# Invariance of igds9sfVarNames and bestVarNames structures across gamers and esports
# First plot is based on the gamers sample, second plot on esports players
structInvariance <- list(NA)
for(d in names(data)){
  struct1 <- data[[d]] %>% select(igdStructures$igds9sfVarNames)
  struct2 <- data[[d]] %>% select(igdStructures$bestVarNames)
  names(struct1) <- names(struct2) <- 1:9
  netIGD9sf <- estimateNetwork(struct1, default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE)
  netBestVar <- estimateNetwork(struct2, default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE)
  set.seed(1)
  structInvariance[[d]][[1]] <- suppressMessages(NCT(netIGD9sf, netBestVar, test.edges = T, test.centrality = T, progressbar = F, it = bootstrapIterations))
  structInvariance[[d]][[2]] <- centralityPlot(list(IGDSF9 = netIGD9sf, BestVar = netBestVar), include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"), decreasing = TRUE)
}
# structInvariance

# Network invariance across levels of moderators
bootstrapIterations <- 1000
# 1 == gamers, 2 = esports players
netInvariance <- strengthInvariance <- proportionSignEdges <- proportionSignInfluence <- list(matrix(NA, nrow = length(igdStructures), ncol = length(mods), dimnames = list(names(igdStructures), mods)),
                                                                                              matrix(NA, nrow = length(igdStructures), ncol = length(mods), dimnames = list(names(igdStructures), mods)))

for(d in 1:length(data)) {
  for(i in 1:length(igdStructures)) {
    for(j in 1:length(mods)) {
      netObj <- tryCatch(network(data = data[[d]], structureName = names(igdStructures[i]), moderator = mods[j], estimator = "mgm"),
                         error = function(e) NULL)
      netInvariance[[d]][i, j] <- ifelse(is.numeric(netObj$nwinv.pval), netObj$nwinv.pval, NA)
      strengthInvariance[[d]][i, j] <- ifelse(is.numeric(netObj$glstrinv.pval), netObj$glstrinv.pval, NA)
      proportionSignEdges[[d]][i, j] <- ifelse(is.numeric(netObj$einv.pvals$`p-value`), sum(netObj$einv.pvals$`p-value` < .05)/length(netObj$einv.pvals$`p-value`), NA)
      proportionSignInfluence[[d]][i, j] <- ifelse(is.numeric(netObj$diffcen.pval[,2]), sum(netObj$diffcen.pval[,2] < .05)/length(netObj$diffcen.pval[,2]), NA)
    }
  }
  modsResults <<- list("Net Invariance" = netInvariance, 
                       "Strength Invariance" = strengthInvariance,
                       "Proportion of significantly different edges" = proportionSignEdges,
                       "Proportion of nodes having significantly different expected influence" = proportionSignInfluence)
}
# modsResults

# Centrality of core vs peripheral IGD symptoms ---------------------------

# Centrality of the core (2, 4, 7, 9) and peripheral IGD symptoms (1, 3, 8) (Charlton & Danforth, 2007; Brunborg et al., 2015) in the population of gamers and esports players

bootCentrality <- rep(list(data.frame(NA)), 2)
set.seed(1)
nIterations <- 10000
for(i in 1:nIterations){
  for(d in 1:length(data)) {
    # Compute correlation matrix for both samples
    mat <- data[[d]] %>% select(IGDS9SF_1:IGDS9SF_9) %>% cor()
    diag(mat) <- NA
    mat[upper.tri(mat)] <- NA
    mat <- c(mat[!is.na(mat)])
    # Reshuffle the correlations and recreate the matrix
    mat <- sample(mat, replace = FALSE)
    class(mat) <- 'dist'
    attr(mat,'Size') <- 9
    mat <- as.matrix(mat)
    diag(mat) <- 1
    
    # Estimate the network for reshuffled matrices, compute raw centrality estimates, and create output object bootCentrality. Done in nIterations number of iterations
    net <- suppressMessages(centralityTable(qgraph(mat, graph = "EBICglasso", sampleSize = nrow(data[[d]]), tuning = 0.5, DoNotPlot = T), standardized = FALSE))
    bootCentrality[[d]][i, "Betweenness"] <- net %>% filter(measure == "Betweenness") %>% select(value) %>% slice(1:4) %>% unlist() %>% mean() - net %>% filter(measure == "Betweenness") %>% select(value) %>% slice(5:7) %>% unlist() %>% mean()
    bootCentrality[[d]][i, "Closeness"] <- net %>% filter(measure == "Closeness") %>% select(value) %>% slice(1:4) %>% unlist() %>% mean() - net %>% filter(measure == "Closeness") %>% select(value) %>% slice(5:7) %>% unlist() %>% mean()
    bootCentrality[[d]][i, "Strength"] <- net %>% filter(measure == "Strength") %>% select(value) %>% slice(1:4) %>% unlist() %>% mean() - net %>% filter(measure == "Strength") %>% select(value) %>% slice(5:7) %>% unlist() %>% mean()
    bootCentrality[[d]][i, "ExpectedInfluence"] <- net %>% filter(measure == "ExpectedInfluence") %>% select(value) %>% slice(1:4) %>% unlist() %>% mean() - net %>% filter(measure == "ExpectedInfluence") %>% select(value) %>% slice(5:7) %>% unlist() %>% mean()
  }
}

# Estimate network based on empirical data from gamers and esports players, extract centrality indices and compare means of those centrality indices for core and peripheral symptoms. 
# Lastly, compute one-tailed permutation p-values for the difference of those centrality means, where the iterated set of reshuffled matrices act as the H0 distribution.
empiricalCentrality <- list(rep(data.frame(NA), 2))
centralityPval <- list(rep(NA, 2))
for(d in 1:length(data)){
  netEmpirical <- centralityTable(estimateNetwork(data[[d]] %>% select(IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", verbose = FALSE, tuning = 0.5), standardized = FALSE)
  empiricalCentrality[[d]] <- c("Betweenness" = netEmpirical %>% filter(measure == "Betweenness") %>% select(value) %>% slice(2, 4, 7, 9) %>% unlist() %>% mean() - netEmpirical %>% filter(measure == "Betweenness") %>% select(value) %>% slice(1, 3, 8) %>% unlist() %>% mean(),
                                "Closeness" = netEmpirical %>% filter(measure == "Closeness") %>% select(value) %>% slice(2, 4, 7, 9) %>% unlist() %>% mean() - netEmpirical %>% filter(measure == "Closeness") %>% select(value) %>% slice(1, 3, 8) %>% unlist() %>% mean(),
                                "Strength" = netEmpirical %>% filter(measure == "Strength") %>% select(value) %>% slice(2, 4, 7, 9) %>% unlist() %>% mean() - netEmpirical %>% filter(measure == "Strength") %>% select(value) %>% slice(1, 3, 8) %>% unlist() %>% mean(),
                                "ExpectedInfluence" = netEmpirical %>% filter(measure == "ExpectedInfluence") %>% select(value) %>% slice(2, 4, 7, 9) %>% unlist() %>% mean() - netEmpirical %>% filter(measure == "ExpectedInfluence") %>% select(value) %>% slice(1, 3, 8) %>% unlist() %>% mean())
  centralityPval[[d]] <- apply(bootCentrality[[d]][,-1] >= empiricalCentrality[[d]], 2, function(x)table(x)[2])/rep(nIterations, length(empiricalCentrality[[1]]))
  names(centralityPval)[[d]] <- names(data)[[d]]
}
# centralityPval

end_time <- Sys.time()
end_time - start_time
#nrow(data)

save.image(file = "allResults")



# Results -----------------------------------------------------------------


meanSplitMatrices

igdStructuresNets$igds9sfVarNames$gamers$`Network plot`
igdStructuresNets$igds9sfVarNames$gamers$`Centrality plot`
igdStructuresNets$igds9sfVarNames$gamers$Stability
igdStructuresNets$igds9sfVarNames$esports$`Network plot`
igdStructuresNets$igds9sfVarNames$esports$`Centrality plot`
igdStructuresNets$igds9sfVarNames$esports$Stability

igdStructuresNets$bestVarNames$gamers$`Network plot`
igdStructuresNets$bestVarNames$gamers$`Centrality plot`
igdStructuresNets$bestVarNames$gamers$Stability
igdStructuresNets$bestVarNames$esports$`Network plot`
igdStructuresNets$bestVarNames$esports$`Centrality plot`
igdStructuresNets$bestVarNames$esports$Stability

igdStructuresNets$bestCravingHealthVarNames$gamers$`Network plot`
igdStructuresNets$bestCravingHealthVarNames$gamers$`Centrality plot`
igdStructuresNets$bestCravingHealthVarNames$gamers$Stability
igdStructuresNets$bestCravingHealthVarNames$esports$`Network plot`
igdStructuresNets$bestCravingHealthVarNames$esports$`Centrality plot`
igdStructuresNets$bestCravingHealthVarNames$esports$Stability

igdStructuresNets$gdtVarNames$gamers$`Network plot`
igdStructuresNets$gdtVarNames$gamers$`Centrality plot`
igdStructuresNets$gdtVarNames$gamers$Stability
igdStructuresNets$gdtVarNames$esports$`Network plot`
igdStructuresNets$gdtVarNames$esports$`Centrality plot`
igdStructuresNets$gdtVarNames$esports$Stability

igdStructuresNets$gdtBestCravingHealthVarNames$gamers$`Network plot`
igdStructuresNets$gdtBestCravingHealthVarNames$gamers$`Centrality plot`
igdStructuresNets$gdtBestCravingHealthVarNames$gamers$Stability
igdStructuresNets$gdtBestCravingHealthVarNames$esports$`Network plot`
igdStructuresNets$gdtBestCravingHealthVarNames$esports$`Centrality plot`
igdStructuresNets$gdtBestCravingHealthVarNames$esports$Stability


igdStructuresDiff$igds9sfVarNames$glstrinv.pval
igdStructuresDiff$igds9sfVarNames$nwinv.pval
igdStructuresDiff$igds9sfVarNames$diffcen.pval

igdStructuresDiff$bestVarNames$glstrinv.pval
igdStructuresDiff$bestVarNames$nwinv.pval
igdStructuresDiff$bestVarNames$diffcen.pval

igdStructuresDiff$bestCravingHealthVarNames$glstrinv.pval
igdStructuresDiff$bestCravingHealthVarNames$nwinv.pval
igdStructuresDiff$bestCravingHealthVarNames$diffcen.pval

igdStructuresDiff$gdtVarNames$glstrinv.pval
igdStructuresDiff$gdtVarNames$nwinv.pval
igdStructuresDiff$gdtVarNames$diffcen.pval


structInvariance$gamers
structInvariance$esports

modsResults$`Net Invariance`[[1]][,1]



bootCentrality
empiricalCentrality
centralityPval






# Graveyard ---------------------------------------------------------------


# network(data = data[[1]], structureName = names(igdStructures[1]), moderator = mods[2])
# aa <- network(data = gamers, structureName = "bestVarNames", moderator = "neuroticism")
# aa

# # Network analysis - esports ----------------------------------------------
# 
# ##IGDSF
# 
# mod0 <- ggm(esports %>% filter(gender == 0) %>% select(IGDS9SF_1:IGDS9SF_9), estimator = "FIML") %>% 
#   runmodel %>% prune(adjust = "fdr", alpha = 0.05, recursive = FALSE)
# 
# mod1 <- ggm(esports %>% filter(gender == 1) %>% select(IGDS9SF_1:IGDS9SF_9), estimator = "FIML") %>% 
#   runmodel %>% prune(adjust = "fdr", alpha = 0.05, recursive = FALSE)
# 
# 
# mod <- ggm(select(esports, IGDS9SF_1:IGDS9SF_9), estimator = "FIML") %>% 
#   runmodel %>% prune(adjust = "fdr", alpha = 0.05, recursive = FALSE)
# 
# psychonetrics::compare(mod, mod1)
# 
# esportsIGDSF <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(mod, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(esportsIGDSF, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# # denseEsportsIGD <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor", tuning = 0)
# # plot(denseEsportsIGD, theme = "gray",
# #      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# # cor(as.vector(denseEsportsIGD$graph), as.vector(denseEsportsIGD$graph))  
# stabEsportsIGDSF <- bootnet(esportsIGDSF, nBoots = 500, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabEsportsIGDSF)
# plot(stabEsportsIGDSF, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootEsportsIGDSF <- bootnet(esportsIGDSF, nBoots = 500, statistics = c("ExpectedInfluence","edge"),
#                             type = "nonparametric", verbose = FALSE) 
# plot(bootEsportsIGDSF, c("ExpectedInfluence"))
# plot(bootEsportsIGDSF, c("edge"))
# plot(bootEsportsIGDSF, c("edge"), plot = "difference")
# 
# 
# # denseEsportsIGD <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor", tuning = 0)
# # plot(denseEsportsIGD, theme = "gray",
# #      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# # cor(as.vector(denseEsportsIGD$graph), as.vector(denseEsportsIGD$graph))  
# stabEsportsIGDSF <- bootnet(esportsIGDSF, nBoots = 500, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabEsportsIGDSF)
# plot(stabEsportsIGDSF, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootEsportsIGDSF <- bootnet(esportsIGDSF, nBoots = 500, statistics = c("ExpectedInfluence","edge"),
#                             type = "nonparametric", verbose = FALSE) 
# plot(bootEsportsIGDSF, c("ExpectedInfluence"))
# plot(bootEsportsIGDSF, c("edge"))
# plot(bootEsportsIGDSF, c("edge"), plot = "difference")
# 
# simEsportsIGDSF <- netSimulator(esportsIGDSF, default = "EBICglasso", corMethod = "cor_auto",
#                                 nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simEsportsIGDSF)
# repEsportsIGDSF <- replicationSimulator(esportsIGDSF, default = "EBICglasso", corMethod = "cor_auto",
#                                nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repEsportsIGDSF)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmEsportsIGDSF <- mgm(select(esports, IGDS9SF_1:IGDS9SF_9), 
#                        type = typesIGD, level = levelsIGD,
#                        lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmEsportsIGDSF <- predict(mgmEsportsIGDSF, select(esports, IGDS9SF_1:IGDS9SF_9))
# predMgmEsportsIGDSF$errors
# qgraph(mgmEsportsIGDSF$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsIGDSF$errors[,3],
#        title = "Esports - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# ##IGD - best items
# 
# esportsIGDSFalt <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                           IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                           IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(esportsIGDSFalt, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(esportsIGDSFalt, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# stabEsportsIGDSFalt <- bootnet(esportsIGDSFalt, nBoots = 200, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabEsportsIGDSFalt)
# plot(stabEsportsIGDSFalt, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootEsportsIGDSFalt <- bootnet(esportsIGDSFalt, nBoots = 200, statistics = c("ExpectedInfluence","edge"),
#                                type = "nonparametric", verbose = FALSE) 
# plot(bootEsportsIGDSFalt, c("ExpectedInfluence"))
# plot(bootEsportsIGDSFalt, c("edge"))
# plot(bootEsportsIGDSFalt, c("edge"), plot = "difference")
# 
# simEsportsIGDSFalt <- netSimulator(esportsIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
#                                    nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simEsportsIGDSFalt)
# repEsportsIGDSFalt <- replicationSimulator(esportsIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
#                                            nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repEsportsIGDSFalt)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmEsportsIGDSFalt <- mgm(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                  IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                  IGDS9SF_7:IGDS9SF_9), 
#                           type = typesIGD, level = levelsIGD,
#                           lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmEsportsIGDSFalt <- predict(mgmEsportsIGDSFalt, select(esports, IGDS9SF_1:IGDS9SF_9))
# predMgmEsportsIGDSFalt$errors
# qgraph(mgmEsportsIGDSFalt$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsIGDSFalt$errors[,3],
#        title = "Esports - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# ##IGD - best items + craving and health
# 
# esportsIGDSFaltCH <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                           IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                           IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(esportsIGDSFaltCH, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(esportsIGDSFaltCH, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# stabEsportsIGDSFaltCH <- bootnet(esportsIGDSFaltCH, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabEsportsIGDSFaltCH)
# plot(stabEsportsIGDSFaltCH, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootEsportsIGDSFaltCH <- bootnet(esportsIGDSFaltCH, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
#                                type = "nonparametric", verbose = FALSE) 
# plot(bootEsportsIGDSFaltCH, c("ExpectedInfluence"))
# plot(bootEsportsIGDSFaltCH, c("edge"))
# plot(bootEsportsIGDSFaltCH, c("edge"), plot = "difference")
# 
# simEsportsIGDSFaltCH <- netSimulator(esportsIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
#                                    nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simEsportsIGDSFaltCH)
# repEsportsIGDSFaltCH <- replicationSimulator(esportsIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
#                                            nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repEsportsIGDSFaltCH)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmEsportsIGDSFaltCH <- mgm(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                  IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                  IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), 
#                           type = typesIGD, level = levelsIGD,
#                           lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmEsportsIGDSFaltCH <- predict(mgmEsportsIGDSFaltCH, select(esports, IGDS9SF_1:IGDS9SF_9))
# predMgmEsportsIGDSFaltCH$errors
# qgraph(mgmEsportsIGDSFaltCH$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsIGDSFaltCH$errors[,3],
#        title = "Esports - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# ##GDT
# 
# esportsGDT <- estimateNetwork(select(esports, GDT_1:GDT_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(esportsGDT, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(esportsGDT, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# stabEsportsGDT <- bootnet(esportsGDT, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabEsportsGDT)
# plot(stabEsportsGDT, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootEsportsGDT <- bootnet(esportsGDT, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
#                             type = "nonparametric", verbose = FALSE) 
# plot(bootEsportsGDT, c("ExpectedInfluence"))
# plot(bootEsportsGDT, c("edge"))
# plot(bootEsportsGDT, c("edge"), plot = "difference")
# 
# simEsportsGDT <- netSimulator(esportsGDT, default = "EBICglasso", corMethod = "cor_auto",
#                                 nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simEsportsGDT)
# repEsportsGDT <- replicationSimulator(esportsGDT, default = "EBICglasso", corMethod = "cor_auto",
#                                         nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repEsportsGDT)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmEsportsGDT <- mgm(select(esports, IGDS9SF_1:IGDS9SF_9), 
#                        type = typesIGD, level = levelsIGD,
#                        lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmEsportsGDT <- predict(mgmEsportsGDT, select(esports, GDT_1:GDT_4))
# predMgmEsportsGDT$errors
# qgraph(mgmEsportsGDT$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsGDT$errors[,3],
#        title = "Esports - GDT", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# # Network analysis - Gamers -----------------------------------------------
# 
# ##IGDSF
# 
# gamersIGDSF <- estimateNetwork(select(gamers, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(gamersIGDSF, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(gamersIGDSF, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# # denseGamersIGD <- estimateNetwork(select(gamers, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor", tuning = 0)
# # plot(denseGamersIGD, theme = "gray",
# #      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# # cor(as.vector(denseGamersIGD$graph), as.vector(denseGamersIGD$graph))  
# stabGamersIGDSF <- bootnet(gamersIGDSF, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabGamersIGDSF)
# plot(stabGamersIGDSF, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootGamersIGDSF <- bootnet(gamersIGDSF, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
#                            type = "nonparametric", verbose = FALSE) 
# plot(bootGamersIGDSF, c("ExpectedInfluence"))
# plot(bootGamersIGDSF, c("edge"))
# plot(bootGamersIGDSF, c("edge"), plot = "difference")
# 
# simGamersIGDSF <- netSimulator(gamersIGDSF, default = "EBICglasso", corMethod = "cor_auto",
#                                nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simGamersIGDSF)
# repGamersIGDSF <- replicationSimulator(gamersIGDSF, default = "EBICglasso", corMethod = "cor_auto",
#                                        nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repGamersIGDSF)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmGamersIGDSF <- mgm(select(gamers, IGDS9SF_1:IGDS9SF_9), 
#                        type = typesIGD, level = levelsIGD,
#                        lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmGamersIGDSF <- predict(mgmGamersIGDSF, select(gamers, IGDS9SF_1:IGDS9SF_9))
# predMgmGamersIGDSF$errors
# qgraph(mgmGamersIGDSF$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersIGDSF$errors[,3],
#        title = "Gamers - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# ##IGD - best items
# 
# gamersIGDSFalt <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                           IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                           IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(gamersIGDSFalt, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(gamersIGDSFalt, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# stabGamersIGDSFalt <- bootnet(gamersIGDSFalt, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabGamersIGDSFalt)
# plot(stabGamersIGDSFalt, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootGamersIGDSFalt <- bootnet(gamersIGDSFalt, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
#                                type = "nonparametric", verbose = FALSE) 
# plot(bootGamersIGDSFalt, c("ExpectedInfluence"))
# plot(bootGamersIGDSFalt, c("edge"))
# plot(bootGamersIGDSFalt, c("edge"), plot = "difference")
# 
# simGamersIGDSFalt <- netSimulator(gamersIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
#                                   nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simGamersIGDSFalt)
# repGamersIGDSFalt <- replicationSimulator(gamersIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
#                                           nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repGamersIGDSFalt)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmGamersIGDSFalt <- mgm(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                 IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                 IGDS9SF_7:IGDS9SF_9), 
#                          type = typesIGD, level = levelsIGD,
#                          lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmGamersIGDSFalt <- predict(mgmGamersIGDSFalt, select(gamers, IGDS9SF_1:IGDS9SF_9))
# predMgmGamersIGDSFalt$errors
# qgraph(mgmGamersIGDSFalt$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersIGDSFalt$errors[,3],
#        title = "Gamers - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# ##IGD - best items + craving and health
# 
# gamersIGDSFaltCH <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                            IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                            IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(gamersIGDSFaltCH, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(gamersIGDSFaltCH, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# stabGamersIGDSFaltCH <- bootnet(gamersIGDSFaltCH, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabGamersIGDSFaltCH)
# plot(stabGamersIGDSFaltCH, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootGamersIGDSFaltCH <- bootnet(gamersIGDSFaltCH, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
#                                 type = "nonparametric", verbose = FALSE) 
# plot(bootGamersIGDSFaltCH, c("ExpectedInfluence"))
# plot(bootGamersIGDSFaltCH, c("edge"))
# plot(bootGamersIGDSFaltCH, c("edge"), plot = "difference")
# 
# simGamersIGDSFaltCH <- netSimulator(gamersIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
#                                     nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simGamersIGDSFaltCH)
# repGamersIGDSFaltCH <- replicationSimulator(gamersIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
#                                             nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repGamersIGDSFaltCH)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmGamersIGDSFaltCH <- mgm(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                   IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                   IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), 
#                            type = typesIGD, level = levelsIGD,
#                            lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmGamersIGDSFaltCH <- predict(mgmGamersIGDSFaltCH, select(gamers, IGDS9SF_1:IGDS9SF_9))
# predMgmGamersIGDSFaltCH$errors
# qgraph(mgmGamersIGDSFaltCH$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersIGDSFaltCH$errors[,3],
#        title = "Gamers - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# ##GDT
# 
# gamersGDT <- estimateNetwork(select(gamers, GDT_1:GDT_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(gamersGDT, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(gamersGDT, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# stabGamersGDT <- bootnet(gamersGDT, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# corStability(stabGamersGDT)
# plot(stabGamersGDT, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
# bootGamersGDT <- bootnet(gamersGDT, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
#                           type = "nonparametric", verbose = FALSE) 
# plot(bootGamersGDT, c("ExpectedInfluence"))
# plot(bootGamersGDT, c("edge"))
# plot(bootGamersGDT, c("edge"), plot = "difference")
# 
# simGamersGDT <- netSimulator(gamersGDT, default = "EBICglasso", corMethod = "cor_auto",
#                               nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(simGamersGDT)
# repGamersGDT <- replicationSimulator(gamersGDT, default = "EBICglasso", corMethod = "cor_auto",
#                                       nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
# plot(repGamersGDT)
# 
# typesIGD <- rep("g", 9)
# levelsIGD <- rep(1, 9) ##Treated as continuous
# mgmGamersGDT <- mgm(select(gamers, IGDS9SF_1:IGDS9SF_9), 
#                      type = typesIGD, level = levelsIGD,
#                      lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
# predMgmGamersGDT <- predict(mgmGamersGDT, select(gamers, GDT_1:GDT_4))
# predMgmGamersGDT$errors
# qgraph(mgmGamersGDT$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersGDT$errors[,3],
#        title = "Gamers - GDT", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)
# 
# 
# 
# # Compare esports players and regular gamers ------------------------------
# 
# ##IGDSF
# esportsIGDSF <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# gamersIGDSF <- estimateNetwork(select(gamers, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# 
# centralityPlot(
#   list(esports = esportsIGDSF,
#        gamers = gamersIGDSF), 
#   include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#   decreasing = TRUE)
# 
# ##IGD - best items
# esportsIGDSFalt <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                           IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                           IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# gamersIGDSFalt <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                          IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                          IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(
#   list(esports = esportsIGDSFalt,
#        gamers = gamersIGDSFalt), 
#   include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#   decreasing = TRUE)
# 
# ##IGD - best items + craving and health
# esportsIGDSFaltCH <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                             IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                             IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# gamersIGDSFaltCH <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                                            IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
#                                            IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(
#   list(esports = esportsIGDSFaltCH,
#        gamers = gamersIGDSFaltCH), 
#   include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#   decreasing = TRUE)
# 
# ##IGCQ
# esportsGDT <- estimateNetwork(select(esports, GDT_1:GDT_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# gamersGDT <- estimateNetwork(select(gamers, GDT_1:GDT_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(
#   list(esports = esportsGDT,
#        gamers = gamersGDT), 
#   include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#   decreasing = TRUE)
# 
# 
# # Other analyses ----------------------------------------------------------
# 
# ##Compare professional and non-professional esports players
# 
# esportsP <- subset(esports, subset = playing_esports == 1)
# esportsNP <- subset(esports, subset = playing_esports == 0)
# 
# pIGD <- estimateNetwork(dplyr::select(esportsP, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# npIGD <- estimateNetwork(dplyr::select(esportsNP, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(pIGD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# centralityPlot(pIGD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# 
# L <- averageLayout(pIGD, npIGD)
# plot(pIGD, theme = "classic", title = "Group1", layout = L)
# plot(npIGD, theme = "classic", title = "Group2", layout = L)
# 
# centralityPlot(
#   list(group1 = pIGD,
#        group2 = npIGD), 
#   include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#   decreasing = TRUE)
# 
# 
# pVSnp <- NetworkComparisonTest::NCT(dplyr::select(esportsP, IGDS9SF_1:IGDS9SF_9), dplyr::select(esportsNP, IGDS9SF_1:IGDS9SF_9), it = 100, weighted = TRUE, 
#                                    test.edges = TRUE, edges = "all", p.adjust.methods = "bonferroni", gamma = 0.5,
#                                    test.centrality = TRUE, centrality = "all",
#                                    progressbar = TRUE)
# pVSnp
# pVSnp$glstrinv.pval # global strength invariance p-value 
# pVSnp$nwinv.pval # maximum difference in any of the edge weights of the observed networks p-value
# pVSnp$einv.pvals # the Holm-Bonferroni corrected p-values per edge from the permutation test concerning differences in edges weights
# plot(pVSnp, what = "strength")
# plot(pVSnp, what = "network")
# plot(pVSnp, what = "edge")
# plot(pVSnp, what = "centrality")
# 
# ## Check whether the R2s are equal
# predMgmNetIGD$errors
# a <- lm(IGDS9SF_1 ~ IGDS9SF_2 + IGDS9SF_3 + IGDS9SF_4 + IGDS9SF_5 + 
#           IGDS9SF_6 + IGDS9SF_7 + IGDS9SF_8 + IGDS9SF_9, data = esports)
# summary(a)
# 
# 
# 
# 
# # ICD-based IGD -----------------------------------------------------------
# 
# netICD <- estimateNetwork(select(esports, IGCQ_1:IGCQ_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(netICD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(netICD, theme = "gray")
# 
# 
# # Best items --------------------------------------------------------------
# 
# bestIGD <- select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
#                   IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, 
#                   IGDS9SF_7, IGDS9SF_8, IGDS9SF_9)
# names(bestIGD) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
# netBest <- estimateNetwork(bestIGD, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(netBest, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(netBest, theme = "gray")
# 
# dsmIGD <- select(esports, IGDS9SF_1:IGDS9SF_9)
# names(dsmIGD) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
# netIGD <- estimateNetwork(dsmIGD, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
# centralityPlot(netIGD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#                decreasing = TRUE)
# plot(netIGD, theme = "gray")
# 
# centralityPlot(
#   list(group1 = netBest,
#        group2 = netIGD), 
#   include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
#   decreasing = TRUE)


# Reliabilities -----------------------------------------------------------

#Internet Gaming Disorder Scale – Short-Form (IGDS9-SF)
omega(select(esports, IGDS9SF_1 : IGDS9SF_9), nfactors = 1, poly = TRUE)
#Gaming Disorder Test
omega(select(esports, GDT_1 : GDT_4), nfactors = 1, poly = TRUE)
#Brief Family Relationship Scale, Cohesion subscale
omega(select(esports, BFRS_1 : BFRS_7), nfactors = 1, poly = TRUE)
#Motives for Online Gaming Questionnaire, subscale Social
omega(select(esports, MOGQ_social1 : MOGQ_social4), nfactors = 1, poly = TRUE)
#Motives for Online Gaming Questionnaire, subscale Competition
omega(select(esports, MOGQ_competition1 : MOGQ_competition4), nfactors = 1, poly = TRUE)
#Motives for Online Gaming Questionnaire, subscale Escape
omega(select(esports, MOGQ_escape1 : MOGQ_escape4), nfactors = 1, poly = TRUE)
#Motives for Online Gaming Questionnaire, subscale Coping
omega(select(esports, MOGQ_coping1 : MOGQ_coping4), nfactors = 1, poly = TRUE)
#IPIP neuroticism
omega(select(esports, neuroticism_1 : neuroticism_10), nfactors = 1, poly = TRUE)
#IPIP harm avoidance
omega(select(esports, harm_avoidance_1 : harm_avoidance_10), nfactors = 1, poly = TRUE)
#De Jong Gierveld Loneliness Scale
omega(select(esports, DJGLS_1 : DJGLS_6), nfactors = 1, poly = TRUE)
#Brief Self-Control Scale
omega(select(esports, BSCS_1 : BSCS_13), nfactors = 1, poly = TRUE)
#BAS Reward Responsiveness
omega(select(esports, BAS_reward_1 : BAS_reward_5), nfactors = 1, poly = TRUE)
