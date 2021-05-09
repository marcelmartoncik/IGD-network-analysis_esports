#Reliabilities

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