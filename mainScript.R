# Libraries ---------------------------------------------------------------

list.of.packages <- c("bootnet", "qgraph", "magrittr", "mgm", "NetworkComparisonTest", "networktree", "psych", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE))


# Sourcing auxiliary scripts ----------------------------------------------
# rm(list = ls())
# source("data wrangling.R")
# source("codebook.R")
# source("careless responding.R")
# source("recoding variables.R")
# source("data imputation.R")
gamersImp <- readRDS("gamersImputed.Rds")
esportsImp <- readRDS("esportsImputed.Rds")
data <- readRDS("data.Rds")


# Sensitivity analysis ----------------------------------------------------

# Uncomment the following lines to remove participants with average gaming time > 18 hours (3 participants from regular gamers and 5 participants from esports players)
# for(i in 1:length(data)) {
#   data[[i]] <- subset(data[[i]], gaming_time < 19)
# }

# Uncomment the following lines to include only those participants from the esports players sample who identified themselves as esports players
# for(i in 1:length(esportsImp)) {
#   esportsImp[[i]] <- subset(esportsImp[[i]], playing_esports == 1)
# }
# data$esports <- subset(data$esports, playing_esports == 1)


# Set the number of iterations for bootstrapping --------------------------

bootstrapIterations <- 20


# Estimate the structure of GD together with additional 
# IGD symptoms (n. 1, 2, 3, 5, 7, 8) measured with the highest content validity
# and with two additional items focused on craving and neglecting health 


# Define network structures -----------------------------------------------

igdStructures <- list(
  igds9sfVarNames = select(data$esports, IGDS9SF_1:IGDS9SF_9) %>% names(),
  bestVarNames = select(data$esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, IGDS9SF_7:IGDS9SF_9) %>% names(),
  bestCravingHealthVarNames = select(data$esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health) %>% names(),
  gdtVarNames = select(data$esports, GDT_1:GDT_4) %>% names(),
  gdtBestCravingHealthVarNames = select(data$esports, GDT_1:GDT_4, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGD_alternative_criterion5, IGDS9SF_7, IGDS9SF_8) %>% names()
)

# Define the structures to test how each variable with the highest content validity performs alone
bestVarsStructures <- list(
  bestVarNames2 = select(data$esports, IGDS9SF_1, IGD_alternative_criterion2, IGDS9SF_3:IGDS9SF_9) %>% names(),
  bestVarNames3 = select(data$esports, IGDS9SF_1:IGDS9SF_2, IGD_alternative_criterion3, IGDS9SF_4:IGDS9SF_9) %>% names(),
  bestVarNames5 = select(data$esports, IGDS9SF_1:IGDS9SF_4, IGD_alternative_criterion5, IGDS9SF_6:IGDS9SF_9) %>% names(),
  bestVarNames6 = select(data$esports, IGDS9SF_1:IGDS9SF_5, IGD_alternative_criterion6, IGDS9SF_7:IGDS9SF_9) %>% names()
)


# Find splits -------------------------------------------------------------

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
  igdStructuresDiff[[i]] <- suppressMessages(NCT(igdStructuresGamers, igdStructuresEsports, test.edges = T, test.centrality = T, progressbar = F, it = bootstrapIterations))
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

structInvarianceBestVars <- list(NA)
for(d in names(data)){
  for(i in names(bestVarsStructures)) {
    struct1 <- data[[d]] %>% select(igdStructures$igds9sfVarNames)
    struct2 <- data[[d]] %>% select(eval(substitute(bestVarsStructures[[i]])))
    names(struct1) <- names(struct2) <- 1:9
    netIGD9sf <- estimateNetwork(struct1, default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE)
    netBestVar <- estimateNetwork(struct2, default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE)
    set.seed(1)
    structInvarianceBestVars[[d]][[i]][[1]] <- suppressMessages(NCT(netIGD9sf, netBestVar, test.edges = T, test.centrality = T, progressbar = F, it = bootstrapIterations))
    structInvarianceBestVars[[d]][[i]][[2]] <- centralityPlot(list(IGDSF9 = netIGD9sf, BestVar = netBestVar), include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"), decreasing = FALSE, orderBy = "Strength")
  }
}
# structInvarianceBestVars

# Network invariance across levels of moderators
bootstrapIterations <- 2000
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

# GD network moderated by neuroticism

neuroticismInvariance <- list(NA)
for(d in 1:length(data)) {
  set.seed(1)
  neuroticismInvariance[[d]] <- tryCatch(network(data = data[[d]], structureName = names(igdStructures[4]), moderator = mods[10], estimator = "mgm"),
                                             error = function(e) NULL)}
neuroticismInvariancePlots <- list(neuroticismInvariance,
lowNeuroticismGamers = centralityPlot(estimateNetwork(data = data[[1]] %>% subset(neuroticism < meanSplitMatrices$meanMatrixGamers$neuroticism[4]) %>% select(GDT_1 : GDT_4), 
                default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE), orderBy = "Strength", decreasing = FALSE, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness")),
highNeuroticismGamers = centralityPlot(estimateNetwork(data = data[[1]] %>% subset(neuroticism >= meanSplitMatrices$meanMatrixGamers$neuroticism[4]) %>% select(GDT_1 : GDT_4), 
                               default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE), orderBy = "Strength", decreasing = FALSE, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness")),
lowNeuroticismEsports = centralityPlot(estimateNetwork(data = data[[2]] %>% subset(neuroticism < meanSplitMatrices$meanMatrixGamers$neuroticism[4]) %>% select(GDT_1 : GDT_4), 
                                                      default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE), orderBy = "Strength", decreasing = FALSE, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness")),
highNeuroticismEsports = centralityPlot(estimateNetwork(data = data[[2]] %>% subset(neuroticism >= meanSplitMatrices$meanMatrixGamers$neuroticism[4]) %>% select(GDT_1 : GDT_4), 
                                                      default = "EBICglasso", corMethod = "cor_auto", verbose = FALSE), orderBy = "Strength", decreasing = FALSE, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
)
neuroticismInvariancePlots

# Centrality of core vs peripheral IGD symptoms
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

# save.image(file = "allResults")


# Results -----------------------------------------------------------------

igdStructuresNets$gdtBestCravingHealthVarNames$esports$`Centrality plot`
igdStructuresDiff$gdtBestCravingHealthVarNames$diffcen.pval
structInvariance
modsResults 
centralityPval


# Descriptives and reliabilities ------------------------------------------

scalesStructures <- list(
  gamingTime = select(data[[1]], gaming_time) %>% names(),
  igds9sf = select(data[[1]], IGDS9SF_1:IGDS9SF_9) %>% names(),
  gdt = select(data[[1]], GDT_1:GDT_4) %>% names(),
  bfrs = select(data[[1]], BFRS_1 : BFRS_7) %>% names(),
  mogqSocial = select(data[[1]], MOGQ_social1 : MOGQ_social4) %>% names(),
  mogqCompetition = select(data[[1]], MOGQ_competition1 : MOGQ_competition4) %>% names(),
  mogqEscape = select(data[[1]], MOGQ_escape1 : MOGQ_escape4) %>% names(),
  mogqCoping = select(data[[1]], MOGQ_coping1 : MOGQ_coping4) %>% names(),
  neuroticism = select(data[[1]], neuroticism_1 : neuroticism_10) %>% names(),
  harmAvoidance = select(data[[1]], harm_avoidance_1 : harm_avoidance_10) %>% names(),
  loneliness = select(data[[1]], DJGLS_1 : DJGLS_6) %>% names(),
  selfControl = select(data[[1]], BSCS_1 : BSCS_13) %>% names(),
  rewardResponse = select(data[[1]], BAS_reward_1 : BAS_reward_5) %>% names()
)

m <- matrix(data = NA, nrow = length(scalesStructures), ncol = 8) 
colnames(m) <- c("mean", "sd", "median", "min", "max", "skew", "kurt", "omega")
rownames(m) <- names(scalesStructures)
scalesDescriptives <- list(m, m)
for(d in 1:length(data)) {
  for(i in 1:length((scalesStructures))) {
    scalesDescriptives[[d]][i, 1:7] <- round(as.numeric(describe(rowMeans(select(data[[d]], eval(substitute(scalesStructures[[i]]))))))[c(3:5,8:9,11:12)], 2)
    if(names(scalesStructures[i]) == "gamingTime") {scalesDescriptives[[d]][i, 8] <- NA} else {
      scalesDescriptives[[d]][i, 8] <- round(suppressMessages(suppressWarnings(omega(select(data[[d]], eval(substitute(scalesStructures[[i]]))), nfactors = 1, poly = TRUE)$omega.tot)), 2)
    }
  }
}
scalesDescriptives



