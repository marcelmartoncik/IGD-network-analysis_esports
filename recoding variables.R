# Recode reverse coded items ----------------------------------------------

##Brief Self Control Scale
esports <- esports %>% mutate_at(c("BSCS_2", "BSCS_3", "BSCS_4", "BSCS_5", "BSCS_7", "BSCS_9", 
                                   "BSCS_10", "BSCS_12", "BSCS_13"), 
                                 funs(recode(., "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1, .default = NaN)))
gamers <- gamers %>% mutate_at(c("BSCS_2", "BSCS_3", "BSCS_4", "BSCS_5", "BSCS_7", "BSCS_9", 
                                 "BSCS_10", "BSCS_12", "BSCS_13"), 
                               funs(recode(., "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1, .default = NaN)))
##Neuroticism and Harm avoidance
esports <- esports %>% mutate_at(c("neuroticism_6", "neuroticism_7", "neuroticism_8", "neuroticism_9", 
                                   "neuroticism_10", "harm_avoidance_4", "harm_avoidance_5", 
                                   "harm_avoidance_6", "harm_avoidance_7", "harm_avoidance_8", 
                                   "harm_avoidance_9", "harm_avoidance_10" ), 
                                 funs(recode(., "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1, .default = NaN)))
gamers <- gamers %>% mutate_at(c("neuroticism_6", "neuroticism_7", "neuroticism_8", "neuroticism_9", 
                                 "neuroticism_10", "harm_avoidance_4", "harm_avoidance_5", 
                                 "harm_avoidance_6", "harm_avoidance_7", "harm_avoidance_8", 
                                 "harm_avoidance_9", "harm_avoidance_10" ), 
                               funs(recode(., "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1, .default = NaN)))
##Loneliness
esports <- esports %>% mutate_at(c("DJGLS_2", "DJGLS_3", "DJGLS_4"), 
                                 funs(recode(., "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1, .default = NaN)))
gamers <- gamers %>% mutate_at(c("DJGLS_2", "DJGLS_3", "DJGLS_4"), 
                               funs(recode(., "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1, .default = NaN)))
