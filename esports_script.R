# read necessary packages
library(tidyverse)
library(dataMaid)
library(codebook)
library(labelled)
library(psych)
library(GPArotation)

# read data
esports <- read.csv("/home/z/Dokumenty/VYSKUM/APVV IGD/Clanky/esports a IGD/Data/IGD_network_esports.csv")
esports <- read.csv("/home/z/Dokumenty/VYSKUM/APVV IGD/Clanky/esports a IGD/Data/IGD_network_esports_selection.csv")

# Create labels for codebook
var_label(esports$duration) <- "Duration in seconds of the whole test battery"
var_label(esports$Q_RelevantIDFraudScore) <- "Fraud score assigned by Qualtrics"
var_label(esports$gaming_time) <- "How many hours a day (on average) do you spend on gaming?"
var_label(esports$gaming_related_activities_time) <- "How many hours a day (on average) do you spend on gaming related activities other than gaming (e.g., watching other people play games, consuming other gaming-related media (e.g. videos))?"
var_label(esports$online_gaming_time) <- "Please divide your whole gaming time between playing online and offline games. Your choices must total 100%. - I play online games x percent (of the time)"
var_label(esports$offline_gaming_time) <- "Please divide your whole gaming time between playing online and offline games. Your choices must total 100%. - I play offline games x percent (of the time)"
var_label(esports$playing_esports) <- "Are you an esports player? (i.e., playing esports games on ranked levels)"
var_label(esports$preferred_game_name) <- "What is the name of a game or genre you play mostly?"
var_label(esports$preffered_esports_game_name) <- "What is the name of an esports game or genre you play mostly?"
var_label(esports$rank) <- "What is your rank? (e.g., Gold; Crusader; Distinguished Master Guardian...)"
var_label(esports$team_membership) <- "Are you a member of a gaming team or club?"
var_label(esports$playing_with_offline_friends) <- "Do you play with friends whom you have met outside the game?"
var_label(esports$MOGQ_social1) <- "Motives for online gaming questionnaire; subscale Social; item: because I can get to know new people."
var_label(esports$MOGQ_social2) <- "Motives for online gaming questionnaire; subscale Social; item: because I can meet many different people."
var_label(esports$MOGQ_social3) <- "Motives for online gaming questionnaire; subscale Social; item: because it is a good social experience."
var_label(esports$MOGQ_social4) <- "Motives for online gaming questionnaire; subscale Social; item: because gaming gives me company."
var_label(esports$MOGQ_escape1) <- "Motives for online gaming questionnaire; subscale Escape; item: because gaming helps me to forget about daily hassles."
var_label(esports$MOGQ_escape2) <- "Motives for online gaming questionnaire; subscale Escape; item: because it makes me forget real life."
var_label(esports$MOGQ_escape3) <- "Motives for online gaming questionnaire; subscale Escape; item: because gaming helps me escape reality."
var_label(esports$MOGQ_escape4) <- "Motives for online gaming questionnaire; subscale Escape; item: to forget about unpleasant things or offences."
var_label(esports$MOGQ_competition1) <- "Motives for online gaming questionnaire; subscale Competition; item: because I enjoy competing with others."
var_label(esports$MOGQ_competition2) <- "Motives for online gaming questionnaire; subscale Competition; item: because I like to win."
var_label(esports$MOGQ_competition3) <- "Motives for online gaming questionnaire; subscale Competition; item: because it is good to feel that I am better than others."
var_label(esports$MOGQ_competition4) <- "Motives for online gaming questionnaire; subscale Competition; item: for the pleasure of defeating others."
var_label(esports$MOGQ_coping1) <- "Motives for online gaming questionnaire; subscale Coping; item: because gaming helps me get into a better mood."
var_label(esports$MOGQ_coping2) <- "Motives for online gaming questionnaire; subscale Coping; item: because it helps me get rid of stress."
var_label(esports$MOGQ_coping3) <- "Motives for online gaming questionnaire; subscale Coping; item: because it helps me channel my aggression."
var_label(esports$MOGQ_coping4) <- "Motives for online gaming questionnaire; subscale Coping; item: because it reduces tension."
var_label(esports$IGCQ_1) <- "Internet Gaming Cognition Scale; item: Game rewards are as meaningful to me as anything else in life."
var_label(esports$IGCQ_2) <- "Internet Gaming Cognition Scale; item: It would be a waste to stop playing because I have invested so much time and energy."
var_label(esports$IGCQ_3) <- "Internet Gaming Cognition Scale; item: I can achieve more in a game than I can anywhere else."
var_label(esports$IGCQ_4) <- "Internet Gaming Cognition Scale; item: Non-gamers do not understand an important part of who I am."
var_label(esports$IGDS9SF_1) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Do you feel preoccupied with your gaming behaviour? (Some examples: Do you think about previous gaming activity or anticipate the next gaming session? Do you think gaming has become the dominant activity in your daily life?)"
var_label(esports$IGDS9SF_2) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Do you feel more irritability, anxiety or even sadness when you try to either reduce or stop your gaming activity?"
var_label(esports$IGDS9SF_3) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Do you feel the need to spend increasing amount of time engaged gaming in order to achieve satisfaction or pleasure?"
var_label(esports$IGDS9SF_4) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Do you systematically fail when trying to control or cease your gaming activity?"
var_label(esports$IGDS9SF_5) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Have you lost interests in previous hobbies and other entertainment activities as a result of your engagement with the game?"
var_label(esports$IGDS9SF_6) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Have you continued your gaming activity despite knowing it was causing problems between you and other people?"
var_label(esports$IGDS9SF_7) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Have you deceived any of your family members, therapists or others because of the amount of your gaming activity?"
var_label(esports$IGDS9SF_8) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Do you play in order to temporarily escape or relieve a negative mood (e.g., helplessness, guilt, anxiety)?"
var_label(esports$IGDS9SF_9) <- "Internet Gaming Disorder Scale – Short-Form (IGDS9-SF); item: Have you jeopardised or lost an important relationship, job or an educational or career opportunity because of your gaming activity?"
var_label(esports$GDT_1) <- "The Gaming Disorder Test; item: I have had difficulties controlling my gaming activity."
var_label(esports$GDT_2) <- "The Gaming Disorder Test; item: I have given increasing priority to gaming over other life interests and daily activities."
var_label(esports$GDT_3) <- "The Gaming Disorder Test; item: I have continued gaming despite the occurrence of negative consequences."
var_label(esports$GDT_4) <- "The Gaming Disorder Test; item: I have experienced significant problems in life (e.g., personal, family, social,education, occupational) due to the severity of my gaming behavior."
var_label(esports$IGD_alternative_criterion2) <- "C-IGDS (Sigerson et al., 2017); item: Do you feel irritable, anxious, or sad when gaming is taken away?"
var_label(esports$IGD_alternative_criterion3) <- "C-IGDS (Sigerson et al., 2017); item: Have you experienced loss of interests in previous hobbies and entertainment as a result of, and with the exceptions of, digital games?"
var_label(esports$IGD_alternative_criterion5) <- "C-IGDS (Sigerson et al., 2017); item: Do you continue to use digital games excessively despite knowledge of psychosocial problems?"
var_label(esports$IGD_alternative_criterion6) <- "PIE-9 (Pearcy et al., 2016); item: Do you find an increasing need to spend increasing amounts of time engaged in digital games?"
var_label(esports$IGD_alternative_craving) <- "CVAT 2.0 (van Rooij et al., 2017); item: How often have you had a strong urge (desire) to play digital games?"
var_label(esports$IGD_alternative_health) <- "CVAT 2.0 (van Rooij et al., 2017); item: How often have you neglected your own health because of gaming? (examples: not getting enough sleep, showering less, failing to brush teeth, drinking insufficiently)"
var_label(esports$BFRS_1) <- "The Brief Family Relationship Scale (Fok et al., 2011); item: In our family we really help and support each other."
var_label(esports$BFRS_2) <- "The Brief Family Relationship Scale (Fok et al., 2011); item: In our family we spend a lot of time doing things together at home."
var_label(esports$BFRS_3) <- "The Brief Family Relationship Scale (Fok et al., 2011); item: In our family we work hard at what we do in our home."
var_label(esports$BFRS_4) <- "The Brief Family Relationship Scale (Fok et al., 2011); item: In our family there is a feeling of togetherness."
var_label(esports$BFRS_5) <- "The Brief Family Relationship Scale (Fok et al., 2011); item: My family members really support each other."
var_label(esports$BFRS_6) <- "The Brief Family Relationship Scale (Fok et al., 2011); item: I am proud to be a part of our family."
var_label(esports$BFRS_7) <- "The Brief Family Relationship Scale (Fok et al., 2011); item: In our family we really get along well with each other."
var_label(esports$BSCS_1) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I am good at resisting temptation."
var_label(esports$BSCS_2) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I have a hard time breaking bad habits."
var_label(esports$BSCS_3) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I am lazy."
var_label(esports$BSCS_4) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I say inappropriate things."
var_label(esports$BSCS_5) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I do certain things that are bad for me, if they are fun."
var_label(esports$BSCS_6) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I refuse things that are bad for me."
var_label(esports$BSCS_7) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I wish I had more self-discipline."
var_label(esports$BSCS_8) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: People would say that I have iron self- discipline."
var_label(esports$BSCS_9) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: Pleasure and fun sometimes keep me from getting work done."
var_label(esports$BSCS_10) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I have trouble concentrating."
var_label(esports$BSCS_11) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I am able to work effectively toward long-term goals."
var_label(esports$BSCS_12) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: Sometimes I can’t stop myself from doing something, even if I know it is wrong."
var_label(esports$BSCS_13) <- "The Brief Self-Control Scale (BSCS) (Tangney et al., 2004); item: I often act without thinking through all the alternatives."
var_label(esports$neuroticism_1) <- "IPIP (Goldberg, 1999); neuroticism; item: Often feel blue."
var_label(esports$neuroticism_2) <- "IPIP (Goldberg, 1999); neuroticism; item: Dislike myself."
var_label(esports$neuroticism_3) <- "IPIP (Goldberg, 1999); neuroticism; item: Am often down in the dumps."
var_label(esports$neuroticism_4) <- "IPIP (Goldberg, 1999); neuroticism; item: Have frequent mood swings."
var_label(esports$neuroticism_5) <- "IPIP (Goldberg, 1999); neuroticism; item: Panic easily."
var_label(esports$neuroticism_6) <- "IPIP (Goldberg, 1999); neuroticism; item: Rarely get irritated."
var_label(esports$neuroticism_7) <- "IPIP (Goldberg, 1999); neuroticism; item: Seldom feel blue."
var_label(esports$neuroticism_8) <- "IPIP (Goldberg, 1999); neuroticism; item: Feel comfortable with myself."
var_label(esports$neuroticism_9) <- "IPIP (Goldberg, 1999); neuroticism; item: Am not easily bothered by things."
var_label(esports$neuroticism_10) <- "IPIP (Goldberg, 1999); neuroticism; item: Am very pleased with myself."
var_label(esports$DJGLS_1) <- "De Jong Gierveld Loneliness Scale (De Jong Gierveld, 2006), item: I experience a general sense of emptiness."
var_label(esports$DJGLS_2) <- "De Jong Gierveld Loneliness Scale (De Jong Gierveld, 2006), item: There are plenty of people I can rely on when I have problems."
var_label(esports$DJGLS_3) <- "De Jong Gierveld Loneliness Scale (De Jong Gierveld, 2006), item: There are many people I can trust completely."
var_label(esports$DJGLS_4) <- "De Jong Gierveld Loneliness Scale (De Jong Gierveld, 2006), item: There are enough people I feel close to."
var_label(esports$DJGLS_5) <- "De Jong Gierveld Loneliness Scale (De Jong Gierveld, 2006), item: I miss having people around."
var_label(esports$DJGLS_6) <- "De Jong Gierveld Loneliness Scale (De Jong Gierveld, 2006), item: I often feel rejected."
var_label(esports$harm_avoidance_1) <- "IPIP (Goldberg, 1999); harm avoidance; item: Would never go hang gliding or bungee jumping."
var_label(esports$harm_avoidance_2) <- "IPIP (Goldberg, 1999); harm avoidance; item: Would never make a high risk investment."
var_label(esports$harm_avoidance_3) <- "IPIP (Goldberg, 1999); harm avoidance; item: Avoid dangerous situations."
var_label(esports$harm_avoidance_4) <- "IPIP (Goldberg, 1999); harm avoidance; item: Take risks."
var_label(esports$harm_avoidance_5) <- "IPIP (Goldberg, 1999); harm avoidance; item: Seek danger."
var_label(esports$harm_avoidance_6) <- "IPIP (Goldberg, 1999); harm avoidance; item: Willing to try anything once."
var_label(esports$harm_avoidance_7) <- "IPIP (Goldberg, 1999); harm avoidance; item: Do dangerous things."
var_label(esports$harm_avoidance_8) <- "IPIP (Goldberg, 1999); harm avoidance; item: Know no limits."
var_label(esports$harm_avoidance_9) <- "IPIP (Goldberg, 1999); harm avoidance; item: Let myself go."
var_label(esports$harm_avoidance_10) <- "IPIP (Goldberg, 1999); harm avoidance; item: Enjoy being reckless."
var_label(esports$BAS_reward_1) <- "BAS scale (Carver & White, 1994); item: When I'm doing well at something I love to keep at it."
var_label(esports$BAS_reward_2) <- "BAS scale (Carver & White, 1994); item: When I get something I want, I feel excited and energized."
var_label(esports$BAS_reward_3) <- "BAS scale (Carver & White, 1994); item: When I see an opportunity for something I like I get excited right away."
var_label(esports$BAS_reward_4) <- "BAS scale (Carver & White, 1994); item: When good things happen to me, it affects me strongly."
var_label(esports$BAS_reward_5) <- "BAS scale (Carver & White, 1994); item: It would excite me to win a contest."
var_label(esports$attention_check_1) <- "To answer this question, please choose “Often”."
var_label(esports$attention_check_2) <- "Please answer this question by choosing “Moderately Inaccurate”."

# Create codebook
makeCodebook(esports, reportTitle = NULL, file = NULL)

# Recode text values to numeric values
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
                                   "DJGLS_6", "DJGLS_7"), 
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

#Reliabilities
#Internet Gaming Disorder Scale – Short-Form (IGDS9-SF)
omega(select(esports, IGDS9SF_1 : IGDS9SF_9), nfactors = 1)
#Gaming Disorder Test
omega(select(esports, GDT_1 : GDT_4), nfactors = 1)
#Brief Family Relationship Scale, Cohesion subscale
omega(select(esports, BFRS_1 : BFRS_7), nfactors = 1)
#Motives for Online Gaming Questionnaire, subscale Social
omega(select(esports, MOGQ_social1 : MOGQ_social4), nfactors = 1)
#Motives for Online Gaming Questionnaire, subscale Competition
omega(select(esports, MOGQ_competition1 : MOGQ_competition4), nfactors = 1)
#Motives for Online Gaming Questionnaire, subscale Escape
omega(select(esports, MOGQ_escape1 : MOGQ_escape4), nfactors = 1)
#Motives for Online Gaming Questionnaire, subscale Coping
omega(select(esports, MOGQ_coping1 : MOGQ_coping4), nfactors = 1)
#IPIP neuroticism
omega(select(esports, neuroticism_1 : neuroticism_10), nfactors = 1)
#IPIP harm avoidance
omega(select(esports, harm_avoidance_1 : harm_avoidance_10), nfactors = 1)
#De Jong Gierveld Loneliness Scale
omega(select(esports, DJGLS_1 : DJGLS_6), nfactors = 1)
#Brief Self-Control Scale
omega(select(esports, BSCS_1 : BSCS_13), nfactors = 1)
#BAS Reward Responsiveness
omega(select(esports, BAS_reward_1 : BAS_reward_5), nfactors = 1)
omega(select(data, fl1 : fl8), nfactors = 1, poly = T)