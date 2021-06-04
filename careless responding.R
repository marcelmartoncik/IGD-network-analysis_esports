# rm(list = ls())

# Libraries ---------------------------------------------------------------

library(careless)
library(dplyr)
library(janitor)
library(psych)


# Load data ---------------------------------------------------------------

esports <- read.csv2("esports_WithCareless.csv")
gamers <- read.csv2("gamers_WithCareless.csv")


# Detection of careless participants --------------------------------------

##A function for calculating longstrings after removing NAs

longstringWoNA <- function(dataCareless) {
  
  library(careless)
  library(dplyr)
  library(janitor)
  
  long <- NULL
  removedNA <- NULL
  for(i in 1:nrow(dataCareless)) {
    if(all(is.na(dataCareless[i, ]))) {
      long[i] <- NA
    } else {
      removedNA <- remove_empty(dataCareless[i, ], which = "cols")
      long[i] <- as.vector(longstring(removedNA, avg = FALSE))
    }
  }
  return(long)
}


##Scale range 1:3 = IGCQ and BFRS
##Scale range 1:4 = BAS_reward
##Scale range 1:5 = MOGQ (all subscales), IGDS9SF, GDT, 
##                  IGD_alternative_criteria, IGD_alternative_craving, IGD_alternative_health,
##                  BSCS, neuroticism, DJGLS, and harm_avoidance


# Esports -----------------------------------------------------------------

##Calculate longstrings
esports$long1to3 <- longstringWoNA(select(esports, IGCQ_1:IGCQ_4, BFRS_1:BFRS_7))
esports$long1to4 <- longstringWoNA(select(esports, BAS_reward_1:BAS_reward_5))
esports$long1to5 <- longstringWoNA(select(esports, MOGQ_social1:MOGQ_coping4, IGDS9SF_1:IGDS9SF_9,
                                          GDT_1:GDT_4, IGD_alternative_criterion2:IGD_alternative_health,
                                          BSCS_1:BSCS_13, neuroticism_1:neuroticism_10,
                                          DJGLS_1:DJGLS_6, harm_avoidance_1:harm_avoidance_10))

##Calculate Mahalanobis distance
esports$mahal1to3 <- mahad(select(esports, IGCQ_1:IGCQ_4, BFRS_1:BFRS_7), flag = FALSE, confidence = .99, plot = FALSE)
esports$mahal1to4 <- mahad(select(esports, BAS_reward_1:BAS_reward_5), flag = FALSE, confidence = .99, plot = FALSE)
esports$mahal1to5 <- mahad(select(esports, MOGQ_social1:MOGQ_coping4, IGDS9SF_1:IGDS9SF_9,
                                  GDT_1:GDT_4, IGD_alternative_criterion2:IGD_alternative_health,
                                  BSCS_1:BSCS_13, neuroticism_1:neuroticism_10,
                                  DJGLS_1:DJGLS_6, harm_avoidance_1:harm_avoidance_10), 
                           flag = FALSE, confidence = .99, plot = FALSE)

##Attention checks
esports$attention_check_1 <- ifelse(esports$attention_check_1 == 1, 0, 1)
esports$attention_check_1[is.na(esports$attention_check_1)] <- 1
esports$attention_check_2 <- ifelse(esports$attention_check_2 == 1, 0, 1)
esports$attention_check_2[is.na(esports$attention_check_2)] <- 1
esports$ac <- esports$attention_check_1 + esports$attention_check_1

whichCareless <- NULL
for(i in 1:nrow(esports)) {
  whichCareless[i] <- isTRUE(sum(is.na(esports[i, ])) / ncol(esports) > .5)
}
sum(whichCareless == TRUE)
which(whichCareless == TRUE)

##Careless participants summary

##About 6% of the participants could be considered careless
esports$careless <- ifelse(esports$ac > 1 | 
                          (scale(esports$long1to3) > 3 | scale(esports$long1to5) > 3) | 
                          (scale(esports$mahal1to3) > 3 | scale(esports$mahal1to5) > 3), 1, 0)
esports$careless[is.na(esports$careless)] <- 0
table(esports$careless)
1-(nrow(subset(esports, careless == 0))/nrow(esports))
esports$careless <- as.numeric(esports$careless)


# Gamers ------------------------------------------------------------------

##Calculate longstrings
gamers$long1to3 <- longstringWoNA(select(gamers, IGCQ_1:IGCQ_4, BFRS_1:BFRS_7))
gamers$long1to4 <- longstringWoNA(select(gamers, BAS_reward_1:BAS_reward_5))
gamers$long1to5 <- longstringWoNA(select(gamers, MOGQ_social1:MOGQ_coping4, IGDS9SF_1:IGDS9SF_9,
                                          GDT_1:GDT_4, IGD_alternative_criterion2:IGD_alternative_health,
                                          BSCS_1:BSCS_13, neuroticism_1:neuroticism_10,
                                          DJGLS_1:DJGLS_6, harm_avoidance_1:harm_avoidance_10))

##Calculate Mahalanobis distance
gamers$mahal1to3 <- mahad(select(gamers, IGCQ_1:IGCQ_4, BFRS_1:BFRS_7), flag = FALSE, confidence = .99, plot = FALSE)
gamers$mahal1to4 <- mahad(select(gamers, BAS_reward_1:BAS_reward_5), flag = FALSE, confidence = .99, plot = FALSE)
gamers$mahal1to5 <- mahad(select(gamers, MOGQ_social1:MOGQ_coping4, IGDS9SF_1:IGDS9SF_9,
                                  GDT_1:GDT_4, IGD_alternative_criterion2:IGD_alternative_health,
                                  BSCS_1:BSCS_13, neuroticism_1:neuroticism_10,
                                  DJGLS_1:DJGLS_6, harm_avoidance_1:harm_avoidance_10), 
                           flag = FALSE, confidence = .99, plot = FALSE)

##Attention checks
gamers$attention_check_1 <- ifelse(gamers$attention_check_1 == 1, 0, 1)
gamers$attention_check_1[is.na(gamers$attention_check_1)] <- 1
gamers$attention_check_2 <- ifelse(gamers$attention_check_2 == 1, 0, 1)
gamers$attention_check_2[is.na(gamers$attention_check_2)] <- 1
gamers$ac <- gamers$attention_check_1 + gamers$attention_check_1

whichCareless <- NULL
for(i in 1:nrow(gamers)) {
  whichCareless[i] <- isTRUE(sum(is.na(gamers[i, ])) / ncol(gamers) > .5)
}
sum(whichCareless == TRUE)
which(whichCareless == TRUE)

##Careless participants summary

##About 5% of the participants could be considered careless
gamers$careless <- ifelse(gamers$ac > 1 | 
                             (scale(gamers$long1to3) > 3 | scale(gamers$long1to5) > 3) | 
                             (scale(gamers$mahal1to3) > 3 | scale(gamers$mahal1to5) > 3), 1, 0)
gamers$careless[is.na(gamers$careless)] <- 0
table(gamers$careless)
1-(nrow(subset(gamers, careless == 0))/nrow(gamers))
gamers$careless <- as.numeric(gamers$careless)


# Write csv files ---------------------------------------------------------

# write.csv2(esports, "esports_withoutCareless.csv", row.names = FALSE)
# write.csv2(gamers, "gamers_withoutCareless.csv", row.names = FALSE)

