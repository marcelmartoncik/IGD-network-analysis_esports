# Libraries ---------------------------------------------------------------

library(mice)
library(miceadds)
library(dplyr)
library(psych)

# Data imputation ---------------------------------------------------------

imputations <- 3
iterations <- 3

##Esports

##Check the proportion of missing data
sum(is.na(select(esports, MOGQ_social1:BAS_reward_5))) / (nrow(esports) * ncol(select(esports, MOGQ_social1:BAS_reward_5)))

##To reduce the imputation time, we treated the variables as unordered and the prediction matrix included only correlations above .1
##The imputation was run overnight
##However, please feel free to vary the parameters and check how well the results reproduce
##We run several sensitivity analyses and found that the results are fairly reproducible 
##as early as XY datasets with XY iterations are being imputed
esportsMice <- mice(esports, m = imputations, maxit = iterations, seed = 1, nnet.MaxNWts = 10000,
                    predictorMatrix = quickpred(esports, exclude = c("duration", "Q_RecaptchaScore", "Q_RelevantIDFraudScore",
                                                                     "nationality", "ethnicity", "preferred_game_name", "preffered_esports_game_name",
                                                                     "long1to3", "long1to4", "long1to5", "mahal1to3", "mahal1to4", "mahal1to5",
                                                                     "attention_check_1", "attention_check_2", "ac", "careless"), mincor = .1))

esportsImp <- lapply(complete(esportsMice, action = "all"),
                     function(x){mutate(x, 
                                        MOGQ_social = as.numeric(pca(sapply(data.frame(MOGQ_social1, MOGQ_social2, MOGQ_social3, MOGQ_social4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        MOGQ_escape = as.numeric(pca(sapply(data.frame(MOGQ_escape1, MOGQ_escape2, MOGQ_escape3, MOGQ_escape4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        MOGQ_competition = as.numeric(pca(sapply(data.frame(MOGQ_competition1, MOGQ_competition2, MOGQ_competition3, MOGQ_competition4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        MOGQ_coping = as.numeric(pca(sapply(data.frame(MOGQ_coping1, MOGQ_coping2, MOGQ_coping3, MOGQ_coping4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        IGCQ = as.numeric(pca(sapply(data.frame(IGCQ_1, IGCQ_2, IGCQ_3, IGCQ_4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        IGDS9SF = as.numeric(pca(sapply(data.frame(IGDS9SF_1, IGDS9SF_2, IGDS9SF_3, IGDS9SF_4, IGDS9SF_5, IGDS9SF_6, IGDS9SF_7, IGDS9SF_8, IGDS9SF_9), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        GDT = as.numeric(pca(sapply(data.frame(GDT_1, GDT_2, GDT_3, GDT_4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        IGDS9SF_alt = as.numeric(pca(sapply(data.frame(IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, IGDS9SF_7, IGDS9SF_8, IGDS9SF_9), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        BFRS = as.numeric(pca(sapply(data.frame(BFRS_1, BFRS_2, BFRS_3, BFRS_4, BFRS_5, BFRS_6, BFRS_7), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        BSCS = as.numeric(pca(sapply(data.frame(BSCS_1, BSCS_2, BSCS_3, BSCS_4, BSCS_5, BSCS_6, BSCS_7, BSCS_8, BSCS_9, BSCS_10, BSCS_11, BSCS_12, BSCS_13), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        neuroticism = as.numeric(pca(sapply(data.frame(neuroticism_1, neuroticism_2, neuroticism_3, neuroticism_4, neuroticism_5, neuroticism_6, neuroticism_7, neuroticism_8, neuroticism_9, neuroticism_10), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        DJGLS = as.numeric(pca(sapply(data.frame(DJGLS_1, DJGLS_2, DJGLS_3, DJGLS_4, DJGLS_5, DJGLS_6), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        harm_avoidance = as.numeric(pca(sapply(data.frame(harm_avoidance_1, harm_avoidance_2, harm_avoidance_3, harm_avoidance_4, harm_avoidance_5, harm_avoidance_6, harm_avoidance_7, harm_avoidance_8, harm_avoidance_9, harm_avoidance_10), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        BAS_reward = as.numeric(pca(sapply(data.frame(BAS_reward_1, BAS_reward_2, BAS_reward_3, BAS_reward_4, BAS_reward_5), as.numeric), nfactors = 1, scores = TRUE)$scores))})


##Gamers

##Check the proportion of missing data
sum(is.na(select(gamers, MOGQ_social1:BAS_reward_5))) / (nrow(gamers) * ncol(select(gamers, MOGQ_social1:BAS_reward_5)))

##To reduce the imputation time, we treated the variables as unordered and the prediction matrix included only correlations above .1
##The imputation was run overnight
##However, please feel free to vary the parameters and check how well the results reproduce
##We run several sensitivity analyses and found that the results are fairly reproducible 
##as early as XY datasets with XY iterations are being imputed
gamersMice <- mice(gamers, m = imputations, maxit = iterations, seed = 1, nnet.MaxNWts = 10000,
                    predictorMatrix = quickpred(gamers, exclude = c("duration", "Q_RecaptchaScore", "Q_RelevantIDFraudScore",
                                                                     "nationality", "ethnicity", "preferred_game_name",
                                                                     "long1to3", "long1to4", "long1to5", "mahal1to3", "mahal1to4", "mahal1to5",
                                                                     "attention_check_1", "attention_check_2", "ac", "careless"), mincor = .1))

gamersImp <- lapply(complete(gamersMice, action = "all"),
                     function(x){mutate(x, 
                                        MOGQ_social = as.numeric(pca(sapply(data.frame(MOGQ_social1, MOGQ_social2, MOGQ_social3, MOGQ_social4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        MOGQ_escape = as.numeric(pca(sapply(data.frame(MOGQ_escape1, MOGQ_escape2, MOGQ_escape3, MOGQ_escape4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        MOGQ_competition = as.numeric(pca(sapply(data.frame(MOGQ_competition1, MOGQ_competition2, MOGQ_competition3, MOGQ_competition4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        MOGQ_coping = as.numeric(pca(sapply(data.frame(MOGQ_coping1, MOGQ_coping2, MOGQ_coping3, MOGQ_coping4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        IGCQ = as.numeric(pca(sapply(data.frame(IGCQ_1, IGCQ_2, IGCQ_3, IGCQ_4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        IGDS9SF = as.numeric(pca(sapply(data.frame(IGDS9SF_1, IGDS9SF_2, IGDS9SF_3, IGDS9SF_4, IGDS9SF_5, IGDS9SF_6, IGDS9SF_7, IGDS9SF_8, IGDS9SF_9), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        GDT = as.numeric(pca(sapply(data.frame(GDT_1, GDT_2, GDT_3, GDT_4), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        IGDS9SF_alt = as.numeric(pca(sapply(data.frame(IGDS9SF_1, IGD_alternative_criterion2, IGD_alternative_criterion3, IGDS9SF_4, IGD_alternative_criterion5, IGD_alternative_criterion6, IGDS9SF_7, IGDS9SF_8, IGDS9SF_9), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        BFRS = as.numeric(pca(sapply(data.frame(BFRS_1, BFRS_2, BFRS_3, BFRS_4, BFRS_5, BFRS_6, BFRS_7), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        BSCS = as.numeric(pca(sapply(data.frame(BSCS_1, BSCS_2, BSCS_3, BSCS_4, BSCS_5, BSCS_6, BSCS_7, BSCS_8, BSCS_9, BSCS_10, BSCS_11, BSCS_12, BSCS_13), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        neuroticism = as.numeric(pca(sapply(data.frame(neuroticism_1, neuroticism_2, neuroticism_3, neuroticism_4, neuroticism_5, neuroticism_6, neuroticism_7, neuroticism_8, neuroticism_9, neuroticism_10), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        DJGLS = as.numeric(pca(sapply(data.frame(DJGLS_1, DJGLS_2, DJGLS_3, DJGLS_4, DJGLS_5, DJGLS_6), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        harm_avoidance = as.numeric(pca(sapply(data.frame(harm_avoidance_1, harm_avoidance_2, harm_avoidance_3, harm_avoidance_4, harm_avoidance_5, harm_avoidance_6, harm_avoidance_7, harm_avoidance_8, harm_avoidance_9, harm_avoidance_10), as.numeric), nfactors = 1, scores = TRUE)$scores),
                                        BAS_reward = as.numeric(pca(sapply(data.frame(BAS_reward_1, BAS_reward_2, BAS_reward_3, BAS_reward_4, BAS_reward_5), as.numeric), nfactors = 1, scores = TRUE)$scores))})

gamersMice <- datalist2mids(gamersImp)
esportsMice <- datalist2mids(esportsImp)

gamersLong <- complete(gamersMice, action = "long") 
gamersNoImp<- gamersLong[gamersLong$.imp == 1 , c(2:10, 16:18, 111:119)]
gamersDiscreteImp <- gamersLong[,c(2, 11:15, 19:110)]
gamersContinuousImp <- gamersLong[,c(2, 120:133)]

gamersDiscreteImp <- gamersDiscreteImp %>% group_by(.id) %>%  summarise_all(.funs = mean, digits = 0)
gamersContinuousImp <- gamersContinuousImp %>% group_by(.id) %>%  summarise_all(.funs = mean)
gamers <- cbind(gamersNoImp, gamersDiscreteImp[,-1], gamersContinuousImp[,-1])

esportsLong <- complete(esportsMice, action = "long") 
esportsNoImp<- esportsLong[esportsLong$.imp == 1 , c(2:9, 15:19, 112:120 )]
esportsDiscreteImp <- esportsLong[,c(2, 10:14, 20:111)]
esportsContinuousImp <- esportsLong[,c(2, 121:134)]

esportsDiscreteImp <- esportsDiscreteImp %>% group_by(.id) %>%  summarise_all(.funs = mean, digits = 0)
esportsContinuousImp <- esportsContinuousImp %>% group_by(.id) %>%  summarise_all(.funs = mean)
esports <- cbind(esportsNoImp, esportsDiscreteImp[,-1], esportsContinuousImp[,-1])

data <- list(gamers = gamers, esports = esports)

# Save data ---------------------------------------------------------------

saveRDS(esportsImp, "esportsImputed.Rds", compress = FALSE)
saveRDS(esportsMice, "esportsMice.Rds")
saveRDS(gamersImp, "gamersImputed.Rds", compress = FALSE)
saveRDS(gamersMice, "gamersMice.Rds")
saveRDS(data, "data.Rds", compress = FALSE)
