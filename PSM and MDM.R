
# Loading Libraries
library(dplyr)
library(tidyverse)
library(cobalt)
library(AER) #for implementing IV 2SLS
library(sandwich) #for heteroskedasticity corrected standard errors 
library(lmtest) #for coeftest()
library(MatchIt) #for matching
library(readxl)
library(optmatch) #for optimal matching
library(cem)
library(devtools)
library(survey)
library(marginaleffects)
library(haven)
library(stargazer)
library(ggplot2)
library(ggthemr)
library(tableone)

# Loading data 
final_health_data <- read_dta("C:/Users/Rajsi Sah/Desktop/NFHS-5/Data Cleaning/EHE NFHS-5 Data/cleaned_data_stata_vf.dta")
final_health_data <- as.data.frame(final_health_data)

# Ease of use: Converting required variables into factor variable

# Covariates interested

# - health_insur : whether the household has insurance or not
# - type_of_residence : rural and urban
# - ind_high_education : no edu/primary/secondary/higher
# - hh_size : continuous number
# - wealth_index_tot : poorest/poorer/middle/richer/richest
# - birth_order : total live births
# - hh_head_sex : sex of hh head
# - freq_newspaper : not at all/less than once a week/ atleast once a week
# - freq_radio : not at all/less than once a week/ atleast once a week
#n - freq_tv : not at all/less than once a week/ atleast once a week
# - bank_account : No/ yes/ missing (615330)
# - religion : no NAs
# - no_antenal_visits : continuous variable

colnames(final_health_data)
print(sum(is.na(final_health_data$bank_account)))
final_health_data$bank_account <- as.numeric(final_health_data$bank_account)
final_health_data$bank_account[is.na(final_health_data$bank_account)] <- 3


#Renaming the variables 
final_health_data$type_of_residence <- recode_factor(final_health_data$type_of_residence, "1" = "urban", "2" = "rural")

final_health_data$ind_high_education <- recode_factor(final_health_data$ind_high_education, 
                                                      "0" = "no education",
                                                      "1" = "primary",
                                                      "2" = "secondary",
                                                      "3" = "higher")

final_health_data$religion <- recode_factor(final_health_data$religion, 
                                            "1" = "hindu",
                                            "2" = "muslim",
                                            "3" = "christian",
                                            "4" = "sikh",
                                            "5" = "buddhist",
                                            "6" = "jain",
                                            "7" = "jewish",
                                            "8" = "parsi",
                                            "9" = "none")

final_health_data$freq_newspaper <- recode_factor(final_health_data$freq_newspaper,
                                                  "0" = "none",
                                                  "1" = "less than a week",
                                                  "2" = "atleast once a week")


final_health_data$freq_radio2 <- recode_factor(final_health_data$freq_radio2,
                                                  "0" = "none",
                                                  "1" = "less than a week",
                                                  "2" = "atleast once a week")

final_health_data$freq_tv <- recode_factor(final_health_data$freq_tv,
                                                  "0" = "none",
                                                  "1" = "less than a week",
                                                  "2" = "atleast once a week")
final_health_data$bank_account <- recode_factor(final_health_data$bank_account,
                                                "1" = "yes",
                                                "2" = "no",
                                                "3" = "not reported")

final_health_data$wealth_index_tot <- recode_factor(final_health_data$wealth_index_tot,
                                                    "1" = "poorest",
                                                    "2" = "poorer", 
                                                    "3" = "middle",
                                                    "4" = "richer",
                                                    "5" = "richest")

final_health_data$hh_head_sex <- recode_factor(final_health_data$hh_head_sex,
                                               "1" = "male",
                                               "2" = "female",
                                               "3" = "trans")
                                                    


# matching variables
final_health_data$type_of_residence <- as.factor(final_health_data$type_of_residence)
final_health_data$ind_high_education <- as.factor(final_health_data$ind_high_education)
final_health_data$religion <- as.factor(final_health_data$religion)
final_health_data$freq_newspaper <- as.factor(final_health_data$freq_newspaper)
final_health_data$freq_radio2 <- as.factor(final_health_data$freq_radio2)
final_health_data$freq_tv <- as.factor(final_health_data$freq_tv)
final_health_data$bank_account <- as.factor(final_health_data$bank_account)
final_health_data$wealth_index_tot <- as.factor(final_health_data$wealth_index_tot)
final_health_data$health_insur <- as.factor(final_health_data$health_insur)
final_health_data$hh_head_sex <- as.factor(final_health_data$hh_head_sex)
final_health_data$bank_account <- as.factor(final_health_data$bank_account)

# ANC Vairiables
final_health_data$ANC_first_trimester <- as.factor(final_health_data$ANC_first_trimester)
final_health_data$ANC_atleast_four2 <- as.factor(final_health_data$ANC_atleast_four2)
final_health_data$atleast_two_tet <- as.factor(final_health_data$atleast_two_tet)
final_health_data$atleast_five_tet <- as.factor(final_health_data$atleast_five_tet)
final_health_data$atleast_one_tet <- as.factor(final_health_data$atleast_one_tet)
final_health_data$iron_100 <- as.factor(final_health_data$iron_100)
final_health_data$iron_180 <- as.factor(final_health_data$iron_180)
final_health_data$overall_ANC <- as.factor(final_health_data$overall_ANC)

# institutional variables
final_health_data$institutional_birth <- as.factor(final_health_data$institutional_birth)
final_health_data$institutional_birth_public <- as.factor(final_health_data$institutional_birth_public)
final_health_data$skilled_birth_attend <- as.factor(final_health_data$skilled_birth_attend)
final_health_data$caesarean <- as.factor(final_health_data$caesarean)

#PNC Variables
final_health_data$health_check_postdel <- as.factor(final_health_data$health_check_postdel)

#Attach data
attach(final_health_data)

##Total maternal health care

final_health_data$total_maternal_healthcare1 <- ifelse(overall_ANC == 1 & institutional_birth ==1 & health_check_postdel ==1, 1, 0) 
final_health_data$total_maternal_healthcare2 <- ifelse(overall_ANC == 1 & institutional_birth ==1 | overall_ANC == 1& health_check_postdel ==1| 
                                                         institutional_birth ==1 & health_check_postdel ==1, 1, 0) 


final_health_data$total_maternal_healthcare1<- as.factor(final_health_data$total_maternal_healthcare1)
final_health_data$total_maternal_healthcare2<- as.factor(final_health_data$total_maternal_healthcare2)
final_health_data$decision_health<- as.factor(final_health_data$decision_health)

#Attach data (final)
attach(final_health_data)


####################################################################################################################################################

#Pre-Matching Balance Checks 

pre_balance <- matchit(health_insur ~ type_of_residence + ind_high_education + religion + freq_newspaper + freq_radio2 + freq_tv + wealth_index_tot + 
                         hh_head_sex + bank_account, method = NULL, distance = "glm")

summary(pre_balance)

v <- data.frame(old = c('type_of_residence','ind_high_education','religion','freq_newspaper',
                        'freq_radio2','freq_tv','wealth_index_tot', 
                        'hh_head_sex','bank_account'),
                new = c('Residence','Individual Education','Religion','Freq of newspaper',
                        'Freq of radio','Freq of tv','Wealth index', 
                        'HH head sex','Bank account'))

unbalanced_plot <- love.plot(bal.tab(pre_balance), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v, stars = "raw")
print(unbalanced_plot)

#getting stargazer table for pre-match balancing


### MAHALANOBIS MATCHING

match_MDM1 <- matchit(health_insur ~ type_of_residence + ind_high_education + religion + freq_newspaper + freq_radio2 + freq_tv + wealth_index_tot + 
                         hh_head_sex + bank_account, 
                       data = final_health_data, 
                       distance = "mahalanobis", 
                       estimand = "ATT")

summary(match_MDM1)
matched_data_mdm_1 <- match.data(match_MDM1)



balance_logit1_plot <- love.plot(bal.tab(match_MDM1), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v, stars = "raw",
                                  colors = c("red", "blue"),
                                 sample.names = c("Unadjusted", "MD Matched"))
                                 

print(balance_logit1_plot)

mdm_balance_check <- bal.tab(health_insur ~ type_of_residence + ind_high_education + religion + freq_newspaper + freq_radio2 + freq_tv + wealth_index_tot + 
                                  hh_head_sex + bank_account, data = matched_data_mdm_1)
mdm_balance_check

#Finding ATT using the marginaleffects package with cluster robust standard errors as per Abadie and Spiess (2019);  Austin 2009, 2013a; Austin and Small 2014; Gayat et al. 2012; Wan 2019.


#ANC_first_trimester, no_antenal_visits,ANC_atleast_four2, atleast_two_tet, atleast_five_tet, atleast_one_tet, iron_100, iron_180, overall_ANC,
#institutional_birth,institutional_birth_public, skilled_birth_attend,caesarean,  health_check_postdel

#Outcome Variable: Number of ANC Visits
att_mdm_ANC_no <- lm(data = matched_data_mdm_1, no_antenal_visits ~ health_insur)
att_anc_no <- comparisons(att_mdm_ANC_no, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_no)

#Outcome Variable: ANC first trimester Visits
matched_data_mdm_1$ANC_first_trimester <- as.numeric(matched_data_mdm_1$ANC_first_trimester)

att_mdm_ANC_Trimester <- lm(data = matched_data_mdm_1, ANC_first_trimester ~ health_insur)

att_anc_trim <- comparisons(att_mdm_ANC_Trimester, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_trim)

#Outcome Variable: ANC_atleast_four2 Visits
matched_data_mdm_1$ANC_atleast_four2 <- as.numeric(matched_data_mdm_1$ANC_atleast_four2)

att_mdm_ANC_atleast4ANC <- lm(data = matched_data_mdm_1, ANC_atleast_four2 ~ health_insur)

att_anc_atleast4ANC <- comparisons(att_mdm_ANC_atleast4ANC, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_atleast4ANC)


#Outcome Variable: atleast_two_tet Visits
matched_data_mdm_1$atleast_two_tet <- as.numeric(matched_data_mdm_1$atleast_two_tet)

att_mdm_ANC_atleast2tet <- lm(data = matched_data_mdm_1, atleast_two_tet ~ health_insur)

att_anc_atleast2tet <- comparisons(att_mdm_ANC_atleast2tet, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_atleast2tet)

#Outcome Variable: atleast_five_tet Visits---NOT REPORTING THIS!!!!!!!!!!!!!!
matched_data_mdm_1$atleast_five_tet <- as.numeric(matched_data_mdm_1$atleast_five_tet)

att_mdm_ANC_atleast5tet <- lm(data = matched_data_mdm_1, atleast_five_tet ~ health_insur)

att_anc_atleast5tet <- comparisons(att_mdm_ANC_atleast5tet, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_atleast5tet)


#Outcome Variable: atleast_one_tet Visits
matched_data_mdm_1$atleast_one_tet <- as.numeric(matched_data_mdm_1$atleast_one_tet)

att_mdm_ANC_atleast1tet <- lm(data = matched_data_mdm_1, atleast_one_tet ~ health_insur)

att_anc_atleast1tet <- comparisons(att_mdm_ANC_atleast1tet, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_atleast1tet)


#Outcome Variable: iron_100 Visits
matched_data_mdm_1$iron_100 <- as.numeric(matched_data_mdm_1$iron_100)

att_mdm_ANC_iron100 <- lm(data = matched_data_mdm_1, iron_100 ~ health_insur)

att_anc_iron100 <- comparisons(att_mdm_ANC_iron100, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_iron100)

#Outcome Variable: iron_180 Visits
matched_data_mdm_1$iron_180 <- as.numeric(matched_data_mdm_1$iron_180)

att_mdm_ANC_iron180 <- lm(data = matched_data_mdm_1, iron_180 ~ health_insur)

att_anc_iron180 <- comparisons(att_mdm_ANC_iron180, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_iron180)

#Outcome Variable: overall_ANC Visits
matched_data_mdm_1$overall_ANC <- as.numeric(matched_data_mdm_1$overall_ANC)

att_mdm_ANC_overall_ANC <- lm(data = matched_data_mdm_1, overall_ANC ~ health_insur)

att_anc_overall_ANC <- comparisons(att_mdm_ANC_overall_ANC, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_overall_ANC)

############### INSTITUTIONAL BIRTHS
#institutional_birth,institutional_birth_public, skilled_birth_attend,caesarean

#Outcome Variable: institutional_birth
matched_data_mdm_1$institutional_birth <- as.numeric(matched_data_mdm_1$institutional_birth)

att_mdm_ANC_institutional_birth <- lm(data = matched_data_mdm_1, institutional_birth ~ health_insur)

att_anc_institutional_birth <- comparisons(att_mdm_ANC_institutional_birth, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_institutional_birth)

#Outcome Variable: institutional_birth_public
matched_data_mdm_1$institutional_birth_public <- as.numeric(matched_data_mdm_1$institutional_birth_public)

att_mdm_ANC_institutional_birth_public <- lm(data = matched_data_mdm_1, institutional_birth_public ~ health_insur)

att_anc_institutional_birth_public <- comparisons(att_mdm_ANC_institutional_birth_public, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_institutional_birth_public)

#Outcome Variable: skilled_birth_attend
matched_data_mdm_1$skilled_birth_attend <- as.numeric(matched_data_mdm_1$skilled_birth_attend)

att_mdm_ANC_skilled_birth_attend <- lm(data = matched_data_mdm_1, skilled_birth_attend ~ health_insur)

att_anc_skilled_birth_attend <- comparisons(att_mdm_ANC_skilled_birth_attend, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_skilled_birth_attend)

#Outcome Variable: caesarean
matched_data_mdm_1$caesarean <- as.numeric(matched_data_mdm_1$caesarean)

att_mdm_ANC_caesarean <- lm(data = matched_data_mdm_1, caesarean ~ health_insur)

att_anc_caesarean <- comparisons(att_mdm_ANC_caesarean, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_caesarean)

############ POST NATAL CHECK

#Outcome Variable: post delivery check
matched_data_mdm_1$health_check_postdel <- as.numeric(matched_data_mdm_1$health_check_postdel)

att_mdm_ANC_health_check_postdel <- lm(data = matched_data_mdm_1, health_check_postdel ~ health_insur)

att_anc_health_check_postdel <- comparisons(att_mdm_ANC_health_check_postdel, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_anc_health_check_postdel)

##########OVERALL MATERNAL HEALTH CARE

#Outcome variable
matched_data_mdm_1$total_maternal_healthcare1 <- as.numeric(matched_data_mdm_1$total_maternal_healthcare1)

att_mdm_total_maternal_healthcare1 <- lm(data = matched_data_mdm_1, total_maternal_healthcare1 ~ health_insur)

att_mdm_total_maternal_healthcare1 <- comparisons(att_mdm_total_maternal_healthcare1, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_mdm_total_maternal_healthcare1)


#Outcome variable
matched_data_mdm_1$total_maternal_healthcare2 <- as.numeric(matched_data_mdm_1$total_maternal_healthcare2)

att_mdm_total_maternal_healthcare2 <- lm(data = matched_data_mdm_1, total_maternal_healthcare2 ~ health_insur)

att_mdm_total_maternal_healthcare2 <- comparisons(att_mdm_total_maternal_healthcare2, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_mdm_1, health_insur == 1))
summary(att_mdm_total_maternal_healthcare2)

## FINAL SUMMARIES ######################################## FINALYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY########################################
summary(att_anc_no)
summary(att_anc_trim)
summary(att_anc_atleast4ANC)
summary(att_anc_atleast2tet)
summary(att_anc_atleast1tet)
summary(att_anc_iron100)
summary(att_anc_iron180)
summary(att_anc_overall_ANC)
summary(att_anc_institutional_birth)
summary(att_anc_institutional_birth_public)
summary(att_anc_skilled_birth_attend)
summary(att_anc_caesarean)
summary(att_anc_health_check_postdel)
summary(att_mdm_total_maternal_healthcare1)
summary(att_mdm_total_maternal_healthcare2)


## Stargazer Tables
sum1 <- summary(att_anc_no)
sum2 <- summary(att_anc_trim)
sum3 <- summary(att_anc_atleast4ANC)
sum4 <- summary(att_anc_atleast2tet)
sum5 <- summary(att_anc_atleast1tet)
sum6 <- summary(att_anc_iron100)
sum7 <- summary(att_anc_iron180)
sum8 <- summary(att_anc_overall_ANC)
sum9 <- summary(att_anc_institutional_birth)
sum10 <- summary(att_anc_institutional_birth_public)
sum11 <- summary(att_anc_skilled_birth_attend)
sum12 <- summary(att_anc_caesarean)
sum13 <- summary(att_anc_health_check_postdel)
sum14 <- summary(att_mdm_total_maternal_healthcare1)
sum15 <- summary(att_mdm_total_maternal_healthcare2)

stargazer(sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8 ,sum9, sum10, sum11, sum12, sum13, sum14, sum15, type = "text", align =T)


################################################################################################################################################3

# PRPOENSITY SCORE MATCHING

match_PSM_1 <- matchit(health_insur ~ type_of_residence + ind_high_education + religion + freq_newspaper + freq_radio2 + freq_tv + wealth_index_tot + 
                        hh_head_sex + bank_account, 
                      data = final_health_data, 
                      method = "nearest", distance = "logit", caliper = 0.02, estimand = "ATT")

summary(match_PSM_1)
matched_data_psm_1 <- match.data(match_PSM_1)

v2 <- data.frame(old = c('type_of_residence','ind_high_education','religion','freq_newspaper',
                        'freq_radio2','freq_tv','wealth_index_tot', 
                        'hh_head_sex','bank_account'),
                new = c('Residence','Individual Education','Religion','Freq of newspaper',
                        'Freq of radio','Freq of tv','Wealth index', 
                        'HH head sex','Bank account'))


balance_psm_1_plot <- love.plot(bal.tab(match_PSM_1), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v2, stars = "raw",
                                colors = c("red", "blue"),
                                sample.names = c("Unadjusted", "MD Matched"))
print(balance_psm_1_plot)

psm_balance_check <- bal.tab(health_insur ~ type_of_residence + ind_high_education + religion + freq_newspaper + freq_radio2 + freq_tv + wealth_index_tot + 
                               hh_head_sex + bank_account, data = matched_data_psm_1)
psm_balance_check


#Finding ATT using the marginaleffects package with cluster robust standard errors as per Abadie and Spiess (2019);  Austin 2009, 2013a; Austin and Small 2014; Gayat et al. 2012; Wan 2019.

############### ANTENATAL CARE

#Outcome Variable: no_antenatal_visits
matched_data_psm_1$no_antenal_visits <- as.numeric(matched_data_psm_1$no_antenal_visits)

att_psm_ANC_no_antenal_visits <- lm(data = matched_data_psm_1, no_antenal_visits ~ health_insur)

att_anc_no_antenal_visits_psm <- comparisons(att_psm_ANC_no_antenal_visits, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_no_antenal_visits_psm)

#Outcome Variable: no_antenatal_visits
matched_data_psm_1$ANC_atleast_four2 <- as.numeric(matched_data_psm_1$ANC_atleast_four2)

att_psm_ANC_ANC_atleast_four2 <- lm(data = matched_data_psm_1, ANC_atleast_four2 ~ health_insur)

att_anc_ANC_atleast_four2_psm <- comparisons(att_psm_ANC_ANC_atleast_four2, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_ANC_atleast_four2_psm)

#Outcome Variable: ANC_first_trimester
matched_data_psm_1$ANC_first_trimester <- as.numeric(matched_data_psm_1$ANC_first_trimester)

att_psm_ANC_ANC_first_trimester <- lm(data = matched_data_psm_1, ANC_first_trimester ~ health_insur)

att_anc_ANC_trimester_psm <- comparisons(att_psm_ANC_ANC_first_trimester, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_ANC_trimester_psm)

#Outcome Variable: atleast_one_tet
matched_data_psm_1$atleast_one_tet <- as.numeric(matched_data_psm_1$atleast_one_tet)

att_psm_ANC_atleast_one_tet <- lm(data = matched_data_psm_1, atleast_one_tet ~ health_insur)

att_anc_ANC_atleast_one_tet <- comparisons(att_psm_ANC_atleast_one_tet, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_ANC_atleast_one_tet)

#Outcome Variable: atleast_two_tet
matched_data_psm_1$atleast_two_tet <- as.numeric(matched_data_psm_1$atleast_two_tet)

att_psm_ANC_atleast_two_tet <- lm(data = matched_data_psm_1, atleast_two_tet ~ health_insur)

att_anc_ANC_atleast_two_tet <- comparisons(att_psm_ANC_atleast_two_tet, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_ANC_atleast_two_tet)

#Outcome Variable: iron_100
matched_data_psm_1$iron_100 <- as.numeric(matched_data_psm_1$iron_100)

att_psm_ANC_iron_100 <- lm(data = matched_data_psm_1, iron_100 ~ health_insur)

att_anc_ANC_iron_100 <- comparisons(att_psm_ANC_iron_100, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_ANC_iron_100)

#Outcome Variable: iron_180
matched_data_psm_1$iron_180 <- as.numeric(matched_data_psm_1$iron_180)

att_psm_ANC_iron_180 <- lm(data = matched_data_psm_1, iron_180 ~ health_insur)

att_anc_ANC_iron_180 <- comparisons(att_psm_ANC_iron_180, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_ANC_iron_180)

#Outcome Variable: overall_ANC
matched_data_psm_1$overall_ANC <- as.numeric(matched_data_psm_1$overall_ANC)

att_psm_ANC_overall_ANC <- lm(data = matched_data_psm_1, overall_ANC ~ health_insur)

att_anc_ANC_overall_ANC <- comparisons(att_psm_ANC_overall_ANC, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_ANC_overall_ANC)

############### INSTITUTIONAL BIRTHS
#institutional_birth,institutional_birth_public, skilled_birth_attend,caesarean

#Outcome Variable: institutional_birth
matched_data_psm_1$institutional_birth <- as.numeric(matched_data_psm_1$institutional_birth)

att_psm_ANC_institutional_birth <- lm(data = matched_data_psm_1, institutional_birth ~ health_insur)

att_anc_institutional_birth_psm <- comparisons(att_psm_ANC_institutional_birth, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_institutional_birth_psm)

#Outcome Variable: institutional_birth_public
matched_data_psm_1$institutional_birth_public <- as.numeric(matched_data_psm_1$institutional_birth_public)

att_mdm_ANC_institutional_birth_public <- lm(data = matched_data_psm_1, institutional_birth_public ~ health_insur)

att_anc_institutional_birth_public <- comparisons(att_mdm_ANC_institutional_birth_public, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_institutional_birth_public)

#Outcome Variable: skilled_birth_attend
matched_data_psm_1$skilled_birth_attend <- as.numeric(matched_data_psm_1$skilled_birth_attend)

att_psm_ANC_skilled_birth_attend <- lm(data = matched_data_psm_1, skilled_birth_attend ~ health_insur)

att_anc_skilled_birth_attend_psm <- comparisons(att_psm_ANC_skilled_birth_attend, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_skilled_birth_attend_psm)

#Outcome Variable: caesarean
matched_data_psm_1$caesarean <- as.numeric(matched_data_psm_1$caesarean)

att_psm_ANC_caesarean <- lm(data = matched_data_psm_1, caesarean ~ health_insur)

att_anc_caesarean_psm <- comparisons(att_psm_ANC_caesarean, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_anc_caesarean_psm)

#Outcome Variable: post deliver
matched_data_psm_1$health_check_postdel <- as.numeric(matched_data_psm_1$health_check_postdel)

att_psm_health_check_postdel <- lm(data = matched_data_psm_1, health_check_postdel ~ health_insur)

att_health_check_postdel_psm <- comparisons(att_psm_health_check_postdel, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_health_check_postdel_psm)


#Outcome Variable: total 1
matched_data_psm_1$total_maternal_healthcare1 <- as.numeric(matched_data_psm_1$total_maternal_healthcare1)

att_psm_total_maternal_healthcare1 <- lm(data = matched_data_psm_1, total_maternal_healthcare1 ~ health_insur)

att_total_maternal_healthcare1_psm <- comparisons(att_psm_total_maternal_healthcare1, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_total_maternal_healthcare1_psm)

#Outcome Variable: total 2
matched_data_psm_1$total_maternal_healthcare2 <- as.numeric(matched_data_psm_1$total_maternal_healthcare1)

att_psm_total_maternal_healthcare2 <- lm(data = matched_data_psm_1, total_maternal_healthcare2 ~ health_insur)

att_total_maternal_healthcare2_psm <- comparisons(att_psm_total_maternal_healthcare2, variables = "health_insur", vcov = "HC3", newdata = subset(matched_data_psm_1, health_insur == 1))
summary(att_total_maternal_healthcare2_psm)

#### summary allllllllllllllllllll 

summary(att_anc_no_antenal_visits_psm)
summary(att_anc_ANC_atleast_four2_psm)
summary(att_anc_ANC_trimester_psm)
summary(att_anc_ANC_atleast_one_tet)
summary(att_anc_ANC_atleast_two_tet)
summary(att_anc_ANC_iron_100)
summary(att_anc_ANC_iron_180)
summary(att_anc_ANC_overall_ANC)
summary(att_anc_institutional_birth_psm)
summary(att_anc_institutional_birth_public)
summary(att_anc_skilled_birth_attend)
summary(att_anc_caesarean)
summary(att_health_check_postdel_psm)
summary(att_total_maternal_healthcare1_psm)
summary(att_total_maternal_healthcare2_psm)



#######################################################################################################################################
##### CREATE DESCRIPTIVE STATS TABLES

#health insurance total
table(health_insur)

#type of residence
table(type_of_residence, health_insur) ##keep changing the bg variables here

myVars <-  c('type_of_residence','ind_high_education','religion','freq_newspaper',
        'freq_radio2','freq_tv','wealth_index_tot', 
        'hh_head_sex','bank_account')

tab2 <- CreateTableOne(vars = myVars, data = final_health_data)
summary(tab2)

######### Create categ of health insurance plot

final_health_data$type_of_insurance <- as.factor(final_health_data$type_of_insurance)

final_health_data2 <- final_health_data %>% subset(type_of_insurance != "0")

final_health_data2$type_of_insurance <- recode_factor(final_health_data2$type_of_insurance, 
                                            "1" = "CGHS",
                                            "2" = "CHIP",
                                            "3" = "Employer",
                                            "4" = "Employer(Reimbursement)",
                                            "5" = "ESIS",
                                            "6" = "Private",
                                            "7" = "RSBY",
                                            "8" = "State",
                                            "9" = "Other")

ggthemr('pale')

ggplot(data = final_health_data2, aes(x = type_of_insurance)) +
  geom_bar()+ labs(title= "Type of Health Insurance Schemes (n=42228)")+ ylab("Count")+ xlab("")+
  theme(plot.title = element_text(color="black", size=24, face="bold"),
        axis.title.x = element_text(color="black", size=18, face="bold"),
        axis.title.y = element_text(color="black", size=18, face="bold")) +
  theme(axis.text.x = element_text(face = "bold", size = 14)) + theme(axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))  








