# This script was written by Alaina Pearce in 2022
# to analyze data for the paper examining
# cognitive function in Emirati children by weight status
#
#     Copyright (C) 2022 Alaina L Pearce
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

# t-test for sex and IOTF_pOWcutoff 
IOTF_pOWcutoff.sex_ttest <- t.test(IOTF_pOWcutoff ~ sex, data = UAE_allDat)

# correlations with IOTF_pOWcutoff 
IOTF_pOWcutoff_demo_cor.varnames <- names(UAE_allDat)[c(5, 18:19, 68)]
IOTF_pOWcutoff_demo_cor.vars <- UAE_allDat[c(5, 18:19, 68)]
IOTF_pOWcutoff_demo_cormat <- data.frame(cor.matrix(IOTF_pOWcutoff_demo_cor.vars, IOTF_pOWcutoff_demo_cor.varnames))
IOTF_pOWcutoff_demo_cormat_ps <- data.frame(cor.matrix_ps(IOTF_pOWcutoff_demo_cor.vars, IOTF_pOWcutoff_demo_cor.varnames))

# monthly income ANOVA
IOTF_pOWcutoff_income_mod <- lm(IOTF_pOWcutoff ~ Month_AED, data = UAE_allDat)
IOTF_pOWcutoff_income_anova <- Anova(IOTF_pOWcutoff_income_mod, type = 3, test.statistic = 'F')

##sensitivity test with linear model 
IOTF_age_pOWcutoff_mod <- lm(IOTF_pOWcutoff ~ Month_AED + Mother_ed + Age_yr + sex, data = UAE_allDat)
IOTF_age_pOWcutoff_sum <- summary(IOTF_age_pOWcutoff_mod)
IOTF_age_pOWcutoff_tab <- cbind.data.frame(IOTF_age_pOWcutoff_sum$coefficients, c('', '', '', '', '', '**', ''))
names(IOTF_age_pOWcutoff_tab) <- c('b', 'se', 't', 'p', ' ')

####   CSHQ       ####
## correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_CSHQ_cor.varnames <- c('IOTF_pOWcutoff', 'BedResit', 'OnsetDelay', 'Duration', 'Anxiety', 'NightWaking', 'Parasomnias', 'DisorderBreathing', 'DaySleepiness', 'Total')
IOTF_pOWcutoff_CSHQ_cor.vars <- UAE_allDat[c(68, 36:39, 41:44, 46)]
IOTF_pOWcutoff_CSHQ_cormat <- cor.matrix(IOTF_pOWcutoff_CSHQ_cor.vars, IOTF_pOWcutoff_CSHQ_cor.varnames)
IOTF_pOWcutoff_CSHQ_cormat_ps <- cor.matrix_ps(IOTF_pOWcutoff_CSHQ_cor.vars, IOTF_pOWcutoff_CSHQ_cor.varnames)

##CSHQ sensitivity tests for IOTF_pOWcutoff

### Sleep onset delay
IOTF_pOWcutoff_sleepdelay_mod <- lm(CSHQ_SleepOnsetDelay ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat)
IOTF_pOWcutoff_sleepdelay_sum <- summary(IOTF_pOWcutoff_sleepdelay_mod)
IOTF_pOWcutoff_sleepdelay_tab <- cbind.data.frame(IOTF_pOWcutoff_sleepdelay_sum$coefficients, c('', '', '', '', '', '.', '', '*'))
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
UAE_allDat$SDQ_EmotProb_Elevated <- factor(UAE_allDat$SDQ_EmotProb_Elevated, levels = c('N', 'Y'))

## conduct problems
UAE_allDat$SDQ_ConductProb_Elevated <- ifelse(UAE_allDat$SDQ_EmotionProb_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_ConductProb_Elevated <- factor(UAE_allDat$SDQ_ConductProb_Elevated,  levels = c('N', 'Y'))


## hyperactivity problems
UAE_allDat$SDQ_HypeProb_Elevated <- ifelse(UAE_allDat$SDQ_HyperactivityProb_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_HypeProb_Elevated <- factor(UAE_allDat$SDQ_HypeProb_Elevated,  levels = c('N', 'Y'))

## peer problems
UAE_allDat$SDQ_PeerProb_Elevated <- ifelse(UAE_allDat$SDQ_PeerProb_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_PeerProb_Elevated <- factor(UAE_allDat$SDQ_PeerProb_Elevated, levels = c('N', 'Y'))

## pro-social problems
UAE_allDat$SDQ_Prosocial_Low <- ifelse(UAE_allDat$SDQ_Prosocial_cat == 'CloseToAverage', 'N', 'Y')
UAE_allDat$SDQ_Prosocial_Low <- factor(UAE_allDat$SDQ_Prosocial_Low,  levels = c('N', 'Y'))

## emotional problems
IOTF_pOWcutoff_EmotProb_Elevated_mod <- glm(SDQ_EmotProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_EmotProb_Elevated_sum <- summary(IOTF_pOWcutoff_EmotProb_Elevated_mod)
IOTF_pOWcutoff_EmotProb_Elevated_odds <- exp(coef(IOTF_pOWcutoff_EmotProb_Elevated_mod))
IOTF_pOWcutoff_EmotProb_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_EmotProb_Elevated_mod))
IOTF_pOWcutoff_EmotProb_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_EmotProb_Elevated_sum$coefficients, c('', '', '', '', '', '.', '', '', ''), IOTF_pOWcutoff_EmotProb_Elevated_odds, IOTF_pOWcutoff_EmotProb_Elevated_oddsCI)
names(IOTF_pOWcutoff_EmotProb_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_EmotProb_Elevated_tab <- IOTF_pOWcutoff_EmotProb_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

## conduct problems
IOTF_pOWcutoff_ConductProb_Elevated_mod <- glm(SDQ_ConductProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_ConductProb_Elevated_sum <- summary(IOTF_pOWcutoff_ConductProb_Elevated_mod)
IOTF_pOWcutoff_ConductProb_Elevated_odds <- exp(coef(IOTF_pOWcutoff_ConductProb_Elevated_mod))
IOTF_pOWcutoff_ConductProb_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_ConductProb_Elevated_mod))
IOTF_pOWcutoff_ConductProb_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_ConductProb_Elevated_sum$coefficients, c('', '', '', '', '', '', '', '', ''), IOTF_pOWcutoff_ConductProb_Elevated_odds, IOTF_pOWcutoff_ConductProb_Elevated_oddsCI)
names(IOTF_pOWcutoff_ConductProb_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_ConductProb_Elevated_tab <- IOTF_pOWcutoff_ConductProb_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

## hyperactivity problems
IOTF_pOWcutoff_HypeProb_Elevated_mod <- glm(SDQ_HypeProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_HypeProb_Elevated_sum <- summary(IOTF_pOWcutoff_HypeProb_Elevated_mod)
IOTF_pOWcutoff_HypeProb_Elevated_odds <- exp(coef(IOTF_pOWcutoff_HypeProb_Elevated_mod))
IOTF_pOWcutoff_HypeProb_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_HypeProb_Elevated_mod))
IOTF_pOWcutoff_HypeProb_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_HypeProb_Elevated_sum$coefficients, c('', '', '', '', '', '', '', '', ''), IOTF_pOWcutoff_HypeProb_Elevated_odds, IOTF_pOWcutoff_HypeProb_Elevated_oddsCI)
names(IOTF_pOWcutoff_HypeProb_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_HypeProb_Elevated_tab <- IOTF_pOWcutoff_HypeProb_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

## peer problems
IOTF_pOWcutoff_peerprob_Elevated_mod <- glm(SDQ_PeerProb_Elevated ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_peerprob_Elevated_sum <- summary(IOTF_pOWcutoff_peerprob_Elevated_mod)
IOTF_pOWcutoff_peerprob_Elevated_odds <- exp(coef(IOTF_pOWcutoff_peerprob_Elevated_mod))
IOTF_pOWcutoff_peerprob_Elevated_oddsCI <- exp(confint(IOTF_pOWcutoff_peerprob_Elevated_mod))
IOTF_pOWcutoff_peerprob_Elevated_tab <- cbind.data.frame(IOTF_pOWcutoff_peerprob_Elevated_sum$coefficients, c('', '', '.', '', '*', '', '', '*', '*'), IOTF_pOWcutoff_peerprob_Elevated_odds, IOTF_pOWcutoff_peerprob_Elevated_oddsCI)
names(IOTF_pOWcutoff_peerprob_Elevated_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_peerprob_Elevated_tab <- IOTF_pOWcutoff_peerprob_Elevated_tab[c(1, 6, 2, 7:8, 3:5)]

IOTF_pOWcutoff_peerprob_Elevated_emtrends <- emtrends(IOTF_pOWcutoff_peerprob_Elevated_mod, ~ IOTF_pOWcutoff_c100 | sex , var = 'IOTF_pOWcutoff_c100')

SDQ_PeerProb_dat <- UAE_allDat[!is.na(UAE_allDat$SDQ_PeerProb_raw) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
SDQ_PeerProb_dat$SDQ_PeerProb_Elevated_predLogit <- predict(IOTF_pOWcutoff_peerprob_Elevated_mod, type = 'link')

## pro-social problems
IOTF_pOWcutoff_prosocial_Low_mod <- glm(SDQ_Prosocial_Low ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_prosocial_Low_sum <- summary(IOTF_pOWcutoff_prosocial_Low_mod)
IOTF_pOWcutoff_prosocial_Low_odds <- exp(coef(IOTF_pOWcutoff_prosocial_Low_mod))
IOTF_pOWcutoff_prosocial_Low_oddsCI <- exp(confint(IOTF_pOWcutoff_prosocial_Low_mod))
IOTF_pOWcutoff_prosocial_Low_tab <- cbind.data.frame(IOTF_pOWcutoff_prosocial_Low_sum$coefficients, c('', '', '', '*', '', '', '', '', '.'), IOTF_pOWcutoff_prosocial_Low_odds, IOTF_pOWcutoff_prosocial_Low_oddsCI)
names(IOTF_pOWcutoff_prosocial_Low_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_prosocial_Low_tab <- IOTF_pOWcutoff_prosocial_Low_tab[c(1, 6, 2, 7:8, 3:5)]