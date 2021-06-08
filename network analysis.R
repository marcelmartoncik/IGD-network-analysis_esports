# Libraries ---------------------------------------------------------------

library(bootnet)
library(qgraph)
library(mgm)


# Load data - original dataset --------------------------------------------

esports <- read.csv2("esports_withoutCarelessRecoded.csv")
gamers <- read.csv2("gamers_withoutCarelessRecoded.csv")


# Network analysis - esports ----------------------------------------------

##IGDSF

esportsIGDSF <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(esportsIGDSF, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(esportsIGDSF, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# denseEsportsIGD <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor", tuning = 0)
# plot(denseEsportsIGD, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# cor(as.vector(denseEsportsIGD$graph), as.vector(denseEsportsIGD$graph))  
stabEsportsIGDSF <- bootnet(esportsIGDSF, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabEsportsIGDSF)
plot(stabEsportsIGDSF, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootEsportsIGDSF <- bootnet(esportsIGDSF, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                            type = "nonparametric", verbose = FALSE) 
plot(bootEsportsIGDSF, c("ExpectedInfluence"))
plot(bootEsportsIGDSF, c("edge"))
plot(bootEsportsIGDSF, c("edge"), plot = "difference")

simEsportsIGDSF <- netSimulator(esportsIGDSF, default = "EBICglasso", corMethod = "cor_auto",
                                nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simEsportsIGDSF)
repEsportsIGDSF <- replicationSimulator(esportsIGDSF, default = "EBICglasso", corMethod = "cor_auto",
                               nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repEsportsIGDSF)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmEsportsIGDSF <- mgm(select(esports, IGDS9SF_1:IGDS9SF_9), 
                       type = typesIGD, level = levelsIGD,
                       lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmEsportsIGDSF <- predict(mgmEsportsIGDSF, select(esports, IGDS9SF_1:IGDS9SF_9))
predMgmEsportsIGDSF$errors
qgraph(mgmEsportsIGDSF$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsIGDSF$errors[,3],
       title = "Esports - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)


##IGD - best items

esportsIGDSFalt <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                          IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                          IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(esportsIGDSFalt, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(esportsIGDSFalt, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
stabEsportsIGDSFalt <- bootnet(esportsIGDSFalt, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabEsportsIGDSFalt)
plot(stabEsportsIGDSFalt, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootEsportsIGDSFalt <- bootnet(esportsIGDSFalt, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                               type = "nonparametric", verbose = FALSE) 
plot(bootEsportsIGDSFalt, c("ExpectedInfluence"))
plot(bootEsportsIGDSFalt, c("edge"))
plot(bootEsportsIGDSFalt, c("edge"), plot = "difference")

simEsportsIGDSFalt <- netSimulator(esportsIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
                                   nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simEsportsIGDSFalt)
repEsportsIGDSFalt <- replicationSimulator(esportsIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
                                           nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repEsportsIGDSFalt)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmEsportsIGDSFalt <- mgm(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                 IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                 IGDS9SF_7:IGDS9SF_9), 
                          type = typesIGD, level = levelsIGD,
                          lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmEsportsIGDSFalt <- predict(mgmEsportsIGDSFalt, select(esports, IGDS9SF_1:IGDS9SF_9))
predMgmEsportsIGDSFalt$errors
qgraph(mgmEsportsIGDSFalt$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsIGDSFalt$errors[,3],
       title = "Esports - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)


##IGD - best items + craving and health

esportsIGDSFaltCH <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                          IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                          IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(esportsIGDSFaltCH, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(esportsIGDSFaltCH, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
stabEsportsIGDSFaltCH <- bootnet(esportsIGDSFaltCH, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabEsportsIGDSFaltCH)
plot(stabEsportsIGDSFaltCH, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootEsportsIGDSFaltCH <- bootnet(esportsIGDSFaltCH, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                               type = "nonparametric", verbose = FALSE) 
plot(bootEsportsIGDSFaltCH, c("ExpectedInfluence"))
plot(bootEsportsIGDSFaltCH, c("edge"))
plot(bootEsportsIGDSFaltCH, c("edge"), plot = "difference")

simEsportsIGDSFaltCH <- netSimulator(esportsIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
                                   nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simEsportsIGDSFaltCH)
repEsportsIGDSFaltCH <- replicationSimulator(esportsIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
                                           nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repEsportsIGDSFaltCH)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmEsportsIGDSFaltCH <- mgm(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                 IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                 IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), 
                          type = typesIGD, level = levelsIGD,
                          lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmEsportsIGDSFaltCH <- predict(mgmEsportsIGDSFaltCH, select(esports, IGDS9SF_1:IGDS9SF_9))
predMgmEsportsIGDSFaltCH$errors
qgraph(mgmEsportsIGDSFaltCH$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsIGDSFaltCH$errors[,3],
       title = "Esports - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)


##GDT

esportsGDT <- estimateNetwork(select(esports, GDT_1:GDT_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(esportsGDT, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(esportsGDT, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
stabEsportsGDT <- bootnet(esportsGDT, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabEsportsGDT)
plot(stabEsportsGDT, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootEsportsGDT <- bootnet(esportsGDT, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                            type = "nonparametric", verbose = FALSE) 
plot(bootEsportsGDT, c("ExpectedInfluence"))
plot(bootEsportsGDT, c("edge"))
plot(bootEsportsGDT, c("edge"), plot = "difference")

simEsportsGDT <- netSimulator(esportsGDT, default = "EBICglasso", corMethod = "cor_auto",
                                nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simEsportsGDT)
repEsportsGDT <- replicationSimulator(esportsGDT, default = "EBICglasso", corMethod = "cor_auto",
                                        nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repEsportsGDT)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmEsportsGDT <- mgm(select(esports, IGDS9SF_1:IGDS9SF_9), 
                       type = typesIGD, level = levelsIGD,
                       lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmEsportsGDT <- predict(mgmEsportsGDT, select(esports, GDT_1:GDT_4))
predMgmEsportsGDT$errors
qgraph(mgmEsportsGDT$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmEsportsGDT$errors[,3],
       title = "Esports - GDT", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)


# Network analysis - Gamers -----------------------------------------------

##IGDSF

gamersIGDSF <- estimateNetwork(select(gamers, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(gamersIGDSF, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(gamersIGDSF, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# denseGamersIGD <- estimateNetwork(select(gamers, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor", tuning = 0)
# plot(denseGamersIGD, theme = "gray",
#      labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
# cor(as.vector(denseGamersIGD$graph), as.vector(denseGamersIGD$graph))  
stabGamersIGDSF <- bootnet(gamersIGDSF, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabGamersIGDSF)
plot(stabGamersIGDSF, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootGamersIGDSF <- bootnet(gamersIGDSF, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                           type = "nonparametric", verbose = FALSE) 
plot(bootGamersIGDSF, c("ExpectedInfluence"))
plot(bootGamersIGDSF, c("edge"))
plot(bootGamersIGDSF, c("edge"), plot = "difference")

simGamersIGDSF <- netSimulator(gamersIGDSF, default = "EBICglasso", corMethod = "cor_auto",
                               nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simGamersIGDSF)
repGamersIGDSF <- replicationSimulator(gamersIGDSF, default = "EBICglasso", corMethod = "cor_auto",
                                       nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repGamersIGDSF)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmGamersIGDSF <- mgm(select(gamers, IGDS9SF_1:IGDS9SF_9), 
                       type = typesIGD, level = levelsIGD,
                       lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmGamersIGDSF <- predict(mgmGamersIGDSF, select(gamers, IGDS9SF_1:IGDS9SF_9))
predMgmGamersIGDSF$errors
qgraph(mgmGamersIGDSF$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersIGDSF$errors[,3],
       title = "Gamers - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)


##IGD - best items

gamersIGDSFalt <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                          IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                          IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(gamersIGDSFalt, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(gamersIGDSFalt, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
stabGamersIGDSFalt <- bootnet(gamersIGDSFalt, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabGamersIGDSFalt)
plot(stabGamersIGDSFalt, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootGamersIGDSFalt <- bootnet(gamersIGDSFalt, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                               type = "nonparametric", verbose = FALSE) 
plot(bootGamersIGDSFalt, c("ExpectedInfluence"))
plot(bootGamersIGDSFalt, c("edge"))
plot(bootGamersIGDSFalt, c("edge"), plot = "difference")

simGamersIGDSFalt <- netSimulator(gamersIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
                                  nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simGamersIGDSFalt)
repGamersIGDSFalt <- replicationSimulator(gamersIGDSFalt, default = "EBICglasso", corMethod = "cor_auto",
                                          nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repGamersIGDSFalt)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmGamersIGDSFalt <- mgm(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                IGDS9SF_7:IGDS9SF_9), 
                         type = typesIGD, level = levelsIGD,
                         lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmGamersIGDSFalt <- predict(mgmGamersIGDSFalt, select(gamers, IGDS9SF_1:IGDS9SF_9))
predMgmGamersIGDSFalt$errors
qgraph(mgmGamersIGDSFalt$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersIGDSFalt$errors[,3],
       title = "Gamers - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)


##IGD - best items + craving and health

gamersIGDSFaltCH <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                           IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                           IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(gamersIGDSFaltCH, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(gamersIGDSFaltCH, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
stabGamersIGDSFaltCH <- bootnet(gamersIGDSFaltCH, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabGamersIGDSFaltCH)
plot(stabGamersIGDSFaltCH, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootGamersIGDSFaltCH <- bootnet(gamersIGDSFaltCH, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                                type = "nonparametric", verbose = FALSE) 
plot(bootGamersIGDSFaltCH, c("ExpectedInfluence"))
plot(bootGamersIGDSFaltCH, c("edge"))
plot(bootGamersIGDSFaltCH, c("edge"), plot = "difference")

simGamersIGDSFaltCH <- netSimulator(gamersIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
                                    nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simGamersIGDSFaltCH)
repGamersIGDSFaltCH <- replicationSimulator(gamersIGDSFaltCH, default = "EBICglasso", corMethod = "cor_auto",
                                            nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repGamersIGDSFaltCH)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmGamersIGDSFaltCH <- mgm(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                  IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                  IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), 
                           type = typesIGD, level = levelsIGD,
                           lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmGamersIGDSFaltCH <- predict(mgmGamersIGDSFaltCH, select(gamers, IGDS9SF_1:IGDS9SF_9))
predMgmGamersIGDSFaltCH$errors
qgraph(mgmGamersIGDSFaltCH$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersIGDSFaltCH$errors[,3],
       title = "Gamers - IGDSF", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)


##GDT

gamersGDT <- estimateNetwork(select(gamers, GDT_1:GDT_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(gamersGDT, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(gamersGDT, theme = "gray",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
stabGamersGDT <- bootnet(gamersGDT, nBoots = 2000, default = "EBICglasso", type = "case", nCores = 8, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
corStability(stabGamersGDT)
plot(stabGamersGDT, statistics = c("Strength","ExpectedInfluence","Closeness", "Betweenness"))
bootGamersGDT <- bootnet(gamersGDT, nBoots = 2000, statistics = c("ExpectedInfluence","edge"),
                          type = "nonparametric", verbose = FALSE) 
plot(bootGamersGDT, c("ExpectedInfluence"))
plot(bootGamersGDT, c("edge"))
plot(bootGamersGDT, c("edge"), plot = "difference")

simGamersGDT <- netSimulator(gamersGDT, default = "EBICglasso", corMethod = "cor_auto",
                              nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(simGamersGDT)
repGamersGDT <- replicationSimulator(gamersGDT, default = "EBICglasso", corMethod = "cor_auto",
                                      nCases = c(250,500, 1000, 2000), nReps = 2000, nCores = 8)
plot(repGamersGDT)

typesIGD <- rep("g", 9)
levelsIGD <- rep(1, 9) ##Treated as continuous
mgmGamersGDT <- mgm(select(gamers, IGDS9SF_1:IGDS9SF_9), 
                     type = typesIGD, level = levelsIGD,
                     lambdaSel = "EBIC", lambdaGam = .5, ruleReg = "AND")
predMgmGamersGDT <- predict(mgmGamersGDT, select(gamers, GDT_1:GDT_4))
predMgmGamersGDT$errors
qgraph(mgmGamersGDT$pairwise$wadj, layout = "spring", repulsion = 1, pie = predMgmGamersGDT$errors[,3],
       title = "Gamers - GDT", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), label.cex = 1, label.scale = FALSE)



# Compare esports players and regular gamers ------------------------------

##IGDSF
esportsIGDSF <- estimateNetwork(select(esports, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
gamersIGDSF <- estimateNetwork(select(gamers, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)

centralityPlot(
  list(esports = esportsIGDSF,
       gamers = gamersIGDSF), 
  include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
  decreasing = TRUE)

##IGD - best items
esportsIGDSFalt <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                          IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                          IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
gamersIGDSFalt <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                         IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                         IGDS9SF_7:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(
  list(esports = esportsIGDSFalt,
       gamers = gamersIGDSFalt), 
  include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
  decreasing = TRUE)

##IGD - best items + craving and health
esportsIGDSFaltCH <- estimateNetwork(select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                            IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                            IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
gamersIGDSFaltCH <- estimateNetwork(select(gamers, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                                           IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6,
                                           IGDS9SF_7:IGDS9SF_9, IGD_alternative_craving, IGD_alternative_health), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(
  list(esports = esportsIGDSFaltCH,
       gamers = gamersIGDSFaltCH), 
  include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
  decreasing = TRUE)

##IGCQ
esportsIGCQ <- estimateNetwork(select(esports, IGCQ_1:IGCQ_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
gamersIGCQ <- estimateNetwork(select(gamers, IGCQ_1:IGCQ_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(
  list(esports = esportsIGCQ,
       gamers = gamersIGCQ), 
  include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
  decreasing = TRUE)


# Other analyses ----------------------------------------------------------

##Compare professional and non-professional esports players

esportsP <- subset(esports, subset = playing_esports == 1)
esportsNP <- subset(esports, subset = playing_esports == 0)

pIGD <- estimateNetwork(dplyr::select(esportsP, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
npIGD <- estimateNetwork(dplyr::select(esportsNP, IGDS9SF_1:IGDS9SF_9), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
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


pVSnp <- NetworkComparisonTest::NCT(dplyr::select(esportsP, IGDS9SF_1:IGDS9SF_9), dplyr::select(esportsNP, IGDS9SF_1:IGDS9SF_9), it = 100, weighted = TRUE, 
                                   test.edges = TRUE, edges = "all", p.adjust.methods = "bonferroni", gamma = 0.5,
                                   test.centrality = TRUE, centrality = "all",
                                   progressbar = TRUE)
pVSnp
pVSnp$glstrinv.pval # global strength invariance p-value 
pVSnp$nwinv.pval # maximum difference in any of the edge weights of the observed networks p-value
pVSnp$einv.pvals # the Holm-Bonferroni corrected p-values per edge from the permutation test concerning differences in edges weights
plot(pVSnp, what = "strength")
plot(pVSnp, what = "network")
plot(pVSnp, what = "edge")
plot(pVSnp, what = "centrality")

## Check whether the R2s are equal
predMgmNetIGD$errors
a <- lm(IGDS9SF_1 ~ IGDS9SF_2 + IGDS9SF_3 + IGDS9SF_4 + IGDS9SF_5 + 
          IGDS9SF_6 + IGDS9SF_7 + IGDS9SF_8 + IGDS9SF_9, data = esports)
summary(a)




# ICD-based IGD -----------------------------------------------------------

netICD <- estimateNetwork(select(esports, IGCQ_1:IGCQ_4), default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(netICD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(netICD, theme = "gray")


# Best items --------------------------------------------------------------

bestIGD <- select(esports, IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, 
                  IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, 
                  IGDS9SF_7, IGDS9SF_8, IGDS9SF_9)
names(bestIGD) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
netBest <- estimateNetwork(bestIGD, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(netBest, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(netBest, theme = "gray")

dsmIGD <- select(esports, IGDS9SF_1:IGDS9SF_9)
names(dsmIGD) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
netIGD <- estimateNetwork(dsmIGD, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
centralityPlot(netIGD, include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
               decreasing = TRUE)
plot(netIGD, theme = "gray")

centralityPlot(
  list(group1 = netBest,
       group2 = netIGD), 
  include = c("Strength","ExpectedInfluence","Closeness", "Betweenness"),
  decreasing = TRUE)
