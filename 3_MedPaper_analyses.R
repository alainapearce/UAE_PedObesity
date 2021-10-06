# This script was written by Alaina Pearce in 2021
# to analyze data for the paper examining
# medical, family history, and behavior in Emirati children
# by weight status
#
#     Copyright (C) 2020 Alaina L Pearce
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

####    Basic Setup    ####

## load libraries -  uncomment if running separately
# library(stats)
# library(car)
# library(emmeans)

## standard functions - uncomment if running separately
# source('functions.R')

## load data/clean - uncomment if running separately
# source('1_MedBeh_DataOrg.R')


####   Demo Data  Tables     ####

# correlations with IOTF_pOWcutoff 
IOTF_pOWcutoff_demo_cor.varnames <- names(UAE_allDat)[c(7, 42:43, 105)]
IOTF_pOWcutoff_demo_cor.vars <- UAE_allDat[c(7, 42:43, 105)]
IOTF_pOWcutoff_demo_cormat <- data.frame(cor.matrix(IOTF_pOWcutoff_demo_cor.vars, IOTF_pOWcutoff_demo_cor.varnames))

# monthly income ANOVA
IOTF_pOWcutoff_income_mod <- lm(IOTF_pOWcutoff ~ Month_AED, data = UAE_allDat)
IOTF_pOWcutoff_income_anova <- Anova(IOTF_pOWcutoff_income_mod, type = 3, test.statistic = 'F')

####    Medial Data       ####

## table of t-tests for IOTF_pOWcutoff by presence/absence of comorbidities

IOTF_pOWcutoff.VitD_ttest <- t.test(IOTF_pOWcutoff ~ VitDdeficiency, data = UAE_allDat)
IOTF_pOWcutoff.Anemia_ttest <- t.test(UAE_allDat$IOTF_pOWcutoff ~ as.factor(ifelse(is.na(UAE_allDat$Anemia), 'N', 'Y')))
IOTF_pOWcutoff.Thyroid_ttest <- t.test(UAE_allDat$IOTF_pOWcutoff ~ as.factor(ifelse(is.na(UAE_allDat$ThyroidConditions), 'N', 'Y')))
IOTF_pOWcutoff.Glycemic_ttest <- t.test(UAE_allDat$IOTF_pOWcutoff ~ as.factor(ifelse(is.na(UAE_allDat$GlycemicStatus), 'N', 'Y')))

IOTF_pOWcutoff.MedComorbid_ttest_tab <- data.frame(matrix(c(round(IOTF_pOWcutoff.VitD_ttest$statistic, 2), round(IOTF_pOWcutoff.VitD_ttest$parameter, 2),round(IOTF_pOWcutoff.VitD_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.VitD_ttest$estimate[1], 2),round(IOTF_pOWcutoff.VitD_ttest$estimate[2], 2),
                                     round(IOTF_pOWcutoff.Anemia_ttest$statistic, 2), round(IOTF_pOWcutoff.Anemia_ttest$parameter, 2), round(IOTF_pOWcutoff.Anemia_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.Anemia_ttest$estimate[1], 2), round(IOTF_pOWcutoff.Anemia_ttest$estimate[2], 2),
                                     round(IOTF_pOWcutoff.Thyroid_ttest$statistic, 2), round(IOTF_pOWcutoff.Thyroid_ttest$parameter, 2), round(IOTF_pOWcutoff.Thyroid_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.Thyroid_ttest$estimate[1], 2), round(IOTF_pOWcutoff.Thyroid_ttest$estimate[2], 2),
                                     round(IOTF_pOWcutoff.Glycemic_ttest$statistic, 2), round(IOTF_pOWcutoff.Glycemic_ttest$parameter, 2), round(IOTF_pOWcutoff.Glycemic_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.Glycemic_ttest$estimate[1], 2), round(IOTF_pOWcutoff.Glycemic_ttest$estimate[2], 2)), byrow = TRUE, nrow = 4))

names(IOTF_pOWcutoff.MedComorbid_ttest_tab) <- c('t', 'df', 'pvalue', 'AbsentMean', 'PresentMean')
rownames(IOTF_pOWcutoff.MedComorbid_ttest_tab) <- c('VitD Deficiency', 'Anemia', 'Thyroid Dysfunction', 'Glycemic Status')
IOTF_pOWcutoff.MedComorbid_ttest_tab$sig <- c('', '**', '', '')
IOTF_pOWcutoff.MedComorbid_ttest_tab <- IOTF_pOWcutoff.MedComorbid_ttest_tab[c(4:5, 1:3, 6)]

# correlation with total number of comorbidities

IOTF_pOWcutoff_nComorbid_cor <- cor.test(UAE_allDat$IOTF_pOWcutoff, UAE_allDat$nComorbid)
                           
####   Family History       ####

##ttests 
IOTF_pOWcutoff_FamOB_ttest <- t.test(IOTF_pOWcutoff ~ Fam_OB_YN, data = UAE_allDat)
IOTF_pOWcutoff_FamED_ttest <- t.test(IOTF_pOWcutoff ~ Fam_ED_YN, data = UAE_allDat)


IOTF_pOWcutoff_fam_ttest_tab <- data.frame(matrix(c(round(IOTF_pOWcutoff_FamOB_ttest$statistic, 2), round(IOTF_pOWcutoff_FamOB_ttest$parameter, 2), round(IOTF_pOWcutoff_FamOB_ttest$p.value, 4),
                                                        round(IOTF_pOWcutoff_FamOB_ttest$estimate[1], 2), round(IOTF_pOWcutoff_FamOB_ttest$estimate[2], 2),
                                                        round(IOTF_pOWcutoff_FamED_ttest$statistic, 2), round(IOTF_pOWcutoff_FamED_ttest$parameter, 2), round(IOTF_pOWcutoff_FamED_ttest$p.value, 4),
                                                        round(IOTF_pOWcutoff_FamED_ttest$estimate[1], 2), round(IOTF_pOWcutoff_FamED_ttest$estimate[2], 2)), byrow = TRUE, nrow = 2))

names(IOTF_pOWcutoff_fam_ttest_tab) <- c('t', 'df', 'pvalue', 'No', 'Yes')
rownames(IOTF_pOWcutoff_fam_ttest_tab) <- c('Family History of Obesity', 'Family History of Eating Disorder')
IOTF_pOWcutoff_fam_ttest_tab$sig <- c('***', '')
IOTF_pOWcutoff_fam_ttest_tab <- IOTF_pOWcutoff_fam_ttest_tab[c(4:5, 1:3, 6)]


##correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_FamHistory_cor.varnames <- names(UAE_allDat)[c(117, 126, 105)]
IOTF_pOWcutoff_FamHistory_cor.vars <- UAE_allDat[c(117, 126, 105)]
IOTF_pOWcutoff_FamHistory_cormat <- cor.matrix(IOTF_pOWcutoff_FamHistory_cor.vars, IOTF_pOWcutoff_FamHistory_cor.varnames)

##sensitivity test with poisson model 
IOTF_pOWcutoff_nFamOB_mod <- glm(nFam_Obesity ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat, family = poisson(link = 'log'))
IOTF_pOWcutoff_nFamOB_sum <- summary(IOTF_pOWcutoff_nFamOB_mod)
IOTF_pOWcutoff_nFamOB_odds <- exp(coef(IOTF_pOWcutoff_nFamOB_mod))
IOTF_pOWcutoff_nFamOB_oddsCI <- exp(confint(IOTF_pOWcutoff_nFamOB_mod))
IOTF_pOWcutoff_nFamOB_tab <- cbind.data.frame(IOTF_pOWcutoff_nFamOB_sum$coefficients, c('', '', '', '', '.', '', '', '***'), IOTF_pOWcutoff_nFamOB_odds, IOTF_pOWcutoff_nFamOB_oddsCI)
names(IOTF_pOWcutoff_nFamOB_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_nFamOB_tab <- IOTF_pOWcutoff_nFamOB_tab[c(1, 6, 2, 7:8, 3:5)]

#add predicted odds of more nFam to data
IOTF_pOWcutoff_nFamOB_dat <- UAE_allDat[!is.na(UAE_allDat$nFam_Obesity) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
IOTF_pOWcutoff_nFamOB_dat$nFam_Obesity_probLogit <- predict(IOTF_pOWcutoff_nFamOB_mod, type = 'link')
                          
####   Sleep       ####

##correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_CSHQ_cor.varnames <- c(names(UAE_allDat)[c(105, 93)], 'BedResit', 'OnsetDelay', 'Duration', 'Anxiety',
                                      'NightWaking', 'Parasomnias', 'DisorderBreathing', 'DaySleepiness')
IOTF_pOWcutoff_CSHQ_cor.vars <- UAE_allDat[c(105, 93:97, 99:102)]
IOTF_pOWcutoff_CSHQ_cormat <- cor.matrix(IOTF_pOWcutoff_CSHQ_cor.vars, IOTF_pOWcutoff_CSHQ_cor.varnames)

##CSHQ sensitivity tests for IOTF_pOWcutoff

### Sleep onset delay
IOTF_pOWcutoff_sleepdelay_mod <- lm(CSHQ_SleepOnsetDelay ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat)
IOTF_pOWcutoff_sleepdelay_sum <- summary(IOTF_pOWcutoff_sleepdelay_mod)
IOTF_pOWcutoff_sleepdelay_tab <- cbind.data.frame(IOTF_pOWcutoff_sleepdelay_sum$coefficients, c('', '', '', '', '', '*', '', '*'))
names(IOTF_pOWcutoff_sleepdelay_tab) <- c('b', 'se', 't', 'p', ' ')

#add predicted sleep onset delay to data
CHSQ_SleepDelay_dat <- UAE_allDat[!is.na(UAE_allDat$CSHQ_SleepOnsetDelay) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
CHSQ_SleepDelay_dat$CSHQ_SleepOnsetDelay_pred <- predict(IOTF_pOWcutoff_sleepdelay_mod, type = 'response')

### Sleep disordered breathing

IOTF_pOWcutoff_breathing_mod <- lm(CSHQ_SleepDisorderBreathing ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat)
IOTF_pOWcutoff_breathing_sum <- summary(IOTF_pOWcutoff_breathing_mod)
IOTF_pOWcutoff_breathing_tab <- cbind.data.frame(IOTF_pOWcutoff_breathing_sum$coefficients, c('', '', '', '', '', '', '', '**'))
names(IOTF_pOWcutoff_breathing_tab) <- c('b', 'se', 't', 'p', ' ')

#add predicted sleep disordered breathing to data
CHSQ_DisorderedBreathing_dat <- UAE_allDat[!is.na(UAE_allDat$CSHQ_SleepDisorderBreathing) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
CHSQ_DisorderedBreathing_dat$CSHQ_SleepDisorderBreathing_pred <- predict(IOTF_pOWcutoff_breathing_mod, type = 'response')

#### SDQ ####

##SDQ sensitivity tests for IOTF_pOWcutoff

##center for interaction
UAE_allDat$IOTF_pOWcutoff_c100 <- UAE_allDat$IOTF_pOWcutoff - 100

## emotional problems
UAE_allDat$SDQ_EmotProb_Elevated <- ifelse(UAE_allDat$SDQ_ConductProb_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_EmotProb_Elevated <- factor(UAE_allDat$SDQ_EmotProb_Elevated,
                                           levels = c('N', 'Y'))

IOTF_pOWcutoff_EmotProb_Elevated_mod <- glm(SDQ_EmotProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_EmotProb_Elevated_sum <- summary(IOTF_pOWcutoff_EmotProb_Elevated_mod)
IOTF_pOWcutoff_EmotProb_Elevated_odds <- exp(coef(IOTF_pOWcutoff_EmotProb_Elevated_mod))
IOTF_pOWcutoff_EmotProb_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_EmotProb_Elevated_mod))
IOTF_pOWcutoff_EmotProb_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_EmotProb_Elevated_sum$coefficients, c('', '', '', '', '.', '', '', '', ''), IOTF_pOWcutoff_EmotProb_Elevated_odds, IOTF_pOWcutoff_EmotProb_Elevated_oddsCI)
names(IOTF_pOWcutoff_EmotProb_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_EmotProb_Elevated_tab <- IOTF_pOWcutoff_EmotProb_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

## conduct problems
UAE_allDat$SDQ_ConductProb_Elevated <- ifelse(UAE_allDat$SDQ_EmotionProb_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_ConductProb_Elevated <- factor(UAE_allDat$SDQ_ConductProb_Elevated,
                                           levels = c('N', 'Y'))

IOTF_pOWcutoff_ConductProb_Elevated_mod <- glm(SDQ_ConductProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_ConductProb_Elevated_sum <- summary(IOTF_pOWcutoff_ConductProb_Elevated_mod)
IOTF_pOWcutoff_ConductProb_Elevated_odds <- exp(coef(IOTF_pOWcutoff_ConductProb_Elevated_mod))
IOTF_pOWcutoff_ConductProb_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_ConductProb_Elevated_mod))
IOTF_pOWcutoff_ConductProb_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_ConductProb_Elevated_sum$coefficients, c('', '', '', '', '.', '', '', '', ''), IOTF_pOWcutoff_ConductProb_Elevated_odds, IOTF_pOWcutoff_ConductProb_Elevated_oddsCI)
names(IOTF_pOWcutoff_ConductProb_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_ConductProb_Elevated_tab <- IOTF_pOWcutoff_ConductProb_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

## hyperactivity problems
UAE_allDat$SDQ_HypeProb_Elevated <- ifelse(UAE_allDat$SDQ_HyperactivityProb_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_HypeProb_Elevated <- factor(UAE_allDat$SDQ_HypeProb_Elevated,
                                           levels = c('N', 'Y'))

IOTF_pOWcutoff_HypeProb_Elevated_mod <- glm(SDQ_HypeProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_HypeProb_Elevated_sum <- summary(IOTF_pOWcutoff_HypeProb_Elevated_mod)
IOTF_pOWcutoff_HypeProb_Elevated_odds <- exp(coef(IOTF_pOWcutoff_HypeProb_Elevated_mod))
IOTF_pOWcutoff_HypeProb_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_HypeProb_Elevated_mod))
IOTF_pOWcutoff_HypeProb_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_HypeProb_Elevated_sum$coefficients, c('', '', '', '', '.', '', '', '', ''), IOTF_pOWcutoff_HypeProb_Elevated_odds, IOTF_pOWcutoff_HypeProb_Elevated_oddsCI)
names(IOTF_pOWcutoff_HypeProb_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_HypeProb_Elevated_tab <- IOTF_pOWcutoff_HypeProb_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

## peer problems
UAE_allDat$SDQ_PeerProb_Elevated <- ifelse(UAE_allDat$SDQ_PeerProb_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_PeerProb_Elevated <- factor(UAE_allDat$SDQ_PeerProb_Elevated,
                                           levels = c('N', 'Y'))

IOTF_pOWcutoff_peerprob_Elevated_mod <- glm(SDQ_PeerProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_peerprob_Elevated_sum <- summary(IOTF_pOWcutoff_peerprob_Elevated_mod)
IOTF_pOWcutoff_peerprob_Elevated_odds <- exp(coef(IOTF_pOWcutoff_peerprob_Elevated_mod))
IOTF_pOWcutoff_peerprob_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_peerprob_Elevated_mod))
IOTF_pOWcutoff_peerprob_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_peerprob_Elevated_sum$coefficients, c('', '', '.', '', '.', '', '*', '*', '*'), IOTF_pOWcutoff_peerprob_Elevated_odds, IOTF_pOWcutoff_peerprob_Elevated_oddsCI)
names(IOTF_pOWcutoff_peerprob_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_peerprob_Elevated_tab <- IOTF_pOWcutoff_peerprob_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

IOTF_pOWcutoff_peerprob_Elevated_emtrends <- emtrends(IOTF_pOWcutoff_peerprob_Elevated_mod, ~ IOTF_pOWcutoff_c100 | sex , var = 'IOTF_pOWcutoff_c100')

SDQ_PeerProb_dat <- UAE_allDat[!is.na(UAE_allDat$SDQ_PeerProb_raw) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
SDQ_PeerProb_dat$SDQ_PeerProb_Elevated_predLogit <- predict(IOTF_pOWcutoff_peerprob_Elevated_mod, type = 'link')

## pro-social problems
UAE_allDat$SDQ_Prosocial_Low <- ifelse(UAE_allDat$SDQ_Prosocial_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_Prosocial_Low <- factor(UAE_allDat$SDQ_Prosocial_Low,
                                           levels = c('N', 'Y'))

IOTF_pOWcutoff_prosocial_Low_mod <- glm(SDQ_Prosocial_Low ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_prosocial_Low_sum <- summary(IOTF_pOWcutoff_prosocial_Low_mod)
IOTF_pOWcutoff_prosocial_Low_odds <- exp(coef(IOTF_pOWcutoff_prosocial_Low_mod))
IOTF_pOWcutoff_prosocial_Low_oddsCI <- exp(confint(IOTF_pOWcutoff_prosocial_Low_mod))
IOTF_pOWcutoff_prosocial_Low_tab <- cbind.data.frame(IOTF_pOWcutoff_prosocial_Low_sum$coefficients, c('', '', '', '.', '', '', '*', '', '.'), IOTF_pOWcutoff_prosocial_Low_odds, IOTF_pOWcutoff_prosocial_Low_oddsCI)
names(IOTF_pOWcutoff_prosocial_Low_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_prosocial_Low_tab <- IOTF_pOWcutoff_prosocial_Low_tab[c(1, 6, 2, 7:8, 3:5)]
