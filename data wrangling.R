# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)


# Load data ---------------------------------------------------------------

esports <- read_excel("IGD_network_esports.xlsx")
gamers <- read_excel("IGD_network_regular_gamers.xlsx")


# Recode text to numeric values -------------------------------------------

##Esports players 
esports <- esports %>% mutate(gender = recode(gender,
                                              `Male`= 0, `Female` = 1, `Non-binary` = 2, 
                                              `Prefer not to say` = 3, .default = NaN))
esports <- esports %>% 
  mutate_at(c("playing_esports","team_membership"), funs(recode(., `No`= 0, `Yes`= 1, .default = NaN)))
esports <- esports %>% mutate(playing_with_offline_friends = recode(playing_with_offline_friends,
                                                                    `Never`= 1, `Rarely` = 2, `Sometimes` = 3,
                                                                    `Often` = 4, `Very often` = 5, .default = NaN))
esports <- esports %>% 
  mutate_at(c("MOGQ_social1", "MOGQ_social2", "MOGQ_social3", "MOGQ_social4",
              "MOGQ_escape1", "MOGQ_escape2", "MOGQ_escape3", "MOGQ_escape4",
              "MOGQ_competition1", "MOGQ_competition2", "MOGQ_competition3", "MOGQ_competition4", 
              "MOGQ_coping1", "MOGQ_coping2", "MOGQ_coping3", "MOGQ_coping4"), funs(recode(., 
                                                                                           `Almost never/never`= 1, `Some of time` = 2, `Half of the time` = 3, `Most of the time` = 4, 
                                                                                           `Almost always/always` = 5, .default = NaN)))
esports <- esports %>% mutate_at(c("IGCQ_1", "IGCQ_2", "IGCQ_3", "IGCQ_4"), funs(recode(.,
                                                                                        `No agreement`= 1, `Agree` = 2, `Strongly Agree` = 3, .default = NaN)))
esports <- esports %>% mutate_at(c("IGDS9SF_1", "IGDS9SF_2", "IGDS9SF_3", "IGDS9SF_4", 
                                   "IGDS9SF_5", "IGDS9SF_6", "IGDS9SF_7", "IGDS9SF_8", 
                                   "IGDS9SF_9", "GDT_1", "GDT_2", "GDT_3", "GDT_4",
                                   "IGD_alternative_criterion2", "IGD_alternative_criterion3", 
                                   "IGD_alternative_criterion5", "IGD_alternative_criterion6",
                                   "IGD_alternative_craving", "IGD_alternative_health"), 
                                 funs(recode(.,`Never`= 1, `Rarely` = 2, 
                                             `Sometimes` = 3, `Often` = 4, `Very often` = 5, .default = NaN)))
esports <- esports %>% mutate_at(c("BFRS_1", "BFRS_2", "BFRS_3", "BFRS_4", "BFRS_5",
                                   "BFRS_6", "BFRS_7"), funs(recode(., `Not at all`= 1, 
                                                                    `Somewhat` = 2, `A lot` = 3, .default = NaN)))
esports <- esports %>% mutate_at(c("BSCS_1", "BSCS_2", "BSCS_3", "BSCS_4", "BSCS_5",
                                   "BSCS_6", "BSCS_7", "BSCS_8", "BSCS_9", "BSCS_10", 
                                   "BSCS_11", "BSCS_12", "BSCS_13"), funs(recode(., `1 Not at all`= 1, 
                                                                                 `2` = 2, `3` = 3, `4` = 4, 
                                                                                 `5       Very much` = 5, .default = NaN)))
esports <- esports %>% mutate_at(c("neuroticism_1", "neuroticism_2", "neuroticism_3", "neuroticism_4",
                                   "neuroticism_5", "neuroticism_6", "neuroticism_7", "neuroticism_8", 
                                   "neuroticism_9", "neuroticism_10", "harm_avoidance_1", "harm_avoidance_2",
                                   "harm_avoidance_3", "harm_avoidance_4", "harm_avoidance_5", "harm_avoidance_6", 
                                   "harm_avoidance_7", "harm_avoidance_8", "harm_avoidance_9", "harm_avoidance_10" ), 
                                 funs(recode(., `Very Inaccurate`= 1, `Moderately Inaccurate` = 2, 
                                             `Neither Accurate Nor Inaccurate` = 3, `Moderately Accurate`= 4, 
                                             `Very Accurate` = 5, .default = NaN)))
esports <- esports %>% mutate_at(c("DJGLS_1", "DJGLS_2", "DJGLS_3", "DJGLS_4", "DJGLS_5",
                                   "DJGLS_6"),
                                 funs(recode(., `Strongly agree`= 1, `Agree` = 2, 
                                             `Neither agree nor disagree` = 3, `Disagree`= 4, 
                                             `Strongly disagree` = 5, .default = NaN)))
esports <- esports %>% mutate_at(c("BAS_reward_1", "BAS_reward_2", "BAS_reward_3", "BAS_reward_4", 
                                   "BAS_reward_5"), 
                                 funs(recode(., `Very true for me`= 1, `Somewhat true for me` = 2, 
                                             `Somewhat false for me` = 3, `Very false for me` = 4,
                                             .default = NaN)))
esports <- esports %>% mutate(attention_check_1 = recode(attention_check_1,
                                                         `Never`= 0, `Rarely` = 0, 
                                                         `Sometimes` = 0, `Often` = 1, `Very often` = 0,
                                                         .default = NaN))
esports <- esports %>% mutate(attention_check_2 = recode(attention_check_2,
                                                         `Very Inaccurate`= 0, `Moderately Inaccurate` = 1, 
                                                         `Neither Accurate Nor Inaccurate` = 0, `Moderately Accurate`= 0, 
                                                         `Very Accurate` = 0, .default = NaN))

##Regular gamers 
gamers <- gamers %>% mutate(gender = recode(gender,
                                            `Male`= 0, `Female` = 1, `Non-binary` = 2, 
                                            `Prefer not to say` = 3, .default = NaN))
gamers <- gamers %>% 
  mutate_at(c("playing_esports","team_membership"), funs(recode(., `No`= 0, `Yes`= 1, .default = NaN)))
gamers <- gamers %>% mutate(playing_with_offline_friends = recode(playing_with_offline_friends,
                                                                  `Never`= 1, `Rarely` = 2, `Sometimes` = 3,
                                                                  `Often` = 4, `Very often` = 5, .default = NaN))
gamers <- gamers %>% 
  mutate_at(c("MOGQ_social1", "MOGQ_social2", "MOGQ_social3", "MOGQ_social4",
              "MOGQ_escape1", "MOGQ_escape2", "MOGQ_escape3", "MOGQ_escape4",
              "MOGQ_competition1", "MOGQ_competition2", "MOGQ_competition3", "MOGQ_competition4", 
              "MOGQ_coping1", "MOGQ_coping2", "MOGQ_coping3", "MOGQ_coping4"), funs(recode(., 
                                                                                           `Almost never/never`= 1, `Some of time` = 2, `Half of the time` = 3, `Most of the time` = 4, 
                                                                                           `Almost always/always` = 5, .default = NaN)))
gamers <- gamers %>% mutate_at(c("IGCQ_1", "IGCQ_2", "IGCQ_3", "IGCQ_4"), funs(recode(.,
                                                                                      `No agreement`= 1, `Agree` = 2, `Strongly Agree` = 3, .default = NaN)))
gamers <- gamers %>% mutate_at(c("IGDS9SF_1", "IGDS9SF_2", "IGDS9SF_3", "IGDS9SF_4", 
                                 "IGDS9SF_5", "IGDS9SF_6", "IGDS9SF_7", "IGDS9SF_8", 
                                 "IGDS9SF_9", "GDT_1", "GDT_2", "GDT_3", "GDT_4",
                                 "IGD_alternative_criterion2", "IGD_alternative_criterion3", 
                                 "IGD_alternative_criterion5", "IGD_alternative_criterion6",
                                 "IGD_alternative_craving", "IGD_alternative_health"), 
                               funs(recode(.,`Never`= 1, `Rarely` = 2, 
                                           `Sometimes` = 3, `Often` = 4, `Very often` = 5, .default = NaN)))
gamers <- gamers %>% mutate_at(c("BFRS_1", "BFRS_2", "BFRS_3", "BFRS_4", "BFRS_5",
                                 "BFRS_6", "BFRS_7"), funs(recode(., `Not at all`= 1, 
                                                                  `Somewhat` = 2, `A lot` = 3, .default = NaN)))
gamers <- gamers %>% mutate_at(c("BSCS_1", "BSCS_2", "BSCS_3", "BSCS_4", "BSCS_5",
                                 "BSCS_6", "BSCS_7", "BSCS_8", "BSCS_9", "BSCS_10", 
                                 "BSCS_11", "BSCS_12", "BSCS_13"), funs(recode(., `1 Not at all`= 1, 
                                                                               `2` = 2, `3` = 3, `4` = 4, 
                                                                               `5       Very much` = 5, .default = NaN)))
gamers <- gamers %>% mutate_at(c("neuroticism_1", "neuroticism_2", "neuroticism_3", "neuroticism_4",
                                 "neuroticism_5", "neuroticism_6", "neuroticism_7", "neuroticism_8", 
                                 "neuroticism_9", "neuroticism_10", "harm_avoidance_1", "harm_avoidance_2",
                                 "harm_avoidance_3", "harm_avoidance_4", "harm_avoidance_5", "harm_avoidance_6", 
                                 "harm_avoidance_7", "harm_avoidance_8", "harm_avoidance_9", "harm_avoidance_10" ), 
                               funs(recode(., `Very Inaccurate`= 1, `Moderately Inaccurate` = 2, 
                                           `Neither Accurate Nor Inaccurate` = 3, `Moderately Accurate`= 4, 
                                           `Very Accurate` = 5, .default = NaN)))
gamers <- gamers %>% mutate_at(c("DJGLS_1", "DJGLS_2", "DJGLS_3", "DJGLS_4", "DJGLS_5",
                                 "DJGLS_6"), 
                               funs(recode(., `Strongly agree`= 1, `Agree` = 2, 
                                           `Neither agree nor disagree` = 3, `Disagree`= 4, 
                                           `Strongly disagree` = 5, .default = NaN)))
gamers <- gamers %>% mutate_at(c("BAS_reward_1", "BAS_reward_2", "BAS_reward_3", "BAS_reward_4", 
                                 "BAS_reward_5"), 
                               funs(recode(., `Very true for me`= 1, `Somewhat true for me` = 2, 
                                           `Somewhat false for me` = 3, `Very false for me` = 4,
                                           .default = NaN)))
gamers <- gamers %>% mutate(attention_check_1 = recode(attention_check_1,
                                                       `Never`= 0, `Rarely` = 0, 
                                                       `Sometimes` = 0, `Often` = 1, `Very often` = 0,
                                                       .default = NaN))
gamers <- gamers %>% mutate(attention_check_2 = recode(attention_check_2,
                                                       `Very Inaccurate`= 0, `Moderately Inaccurate` = 1, 
                                                       `Neither Accurate Nor Inaccurate` = 0, `Moderately Accurate`= 0, 
                                                       `Very Accurate` = 0, .default = NaN))
