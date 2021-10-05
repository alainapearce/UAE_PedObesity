# This script was written by Alaina Pearce in 2021
# to set up/organize data for the paper examining
# medical, famility history, and behavior in Emirati children
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
library(stats)
library(car)
library(gtsummary)
library(ggplot2)
library(emmeans)

#gtsummary table functions
my_ttest <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
}

my_chifisher <- function(data, variable, by, ...) {
  tab <- xtabs(~data[[variable]] + data[[by]])
  if (min(tab) <= 5){
    fisher.test(tab)$p.value
  } else {
    chisq.test(tab)$p.value
  }
}

my_anova <- function(data, variable, by, ...) {
  mod <- anova(lm(data[[variable]] ~ as.factor(data[[by]])))
  return(mod$`Pr(>F)`[1])
}

## standard functions
source('functions.R')

## load data/clean
source('1_DataOrg_MedBeh.R')


####   Demo Data  Tables     ####


# table by sex ####
sum_tab_sex <- UAE_allDat[c(5, 7, 12, 105:106, 17, 42:43, 64, 2:3)]
UAE_demo_sex <-
  tbl_summary(
    data=sum_tab_sex,
    by = sex, 
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_ttest) %>%
  add_stat(fns = all_categorical() ~ my_chifisher) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

# table by weight status ####
sum_tab_OB <- UAE_allDat[c(107, 7, 12, 105:106, 42:43, 64, 2:3)]
UAE_demo_OB <-
  tbl_summary(
    data=sum_tab_OB,
    by = IOTF_3class, 
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_anova) %>%
  add_stat(fns = all_categorical() ~ my_chifisher) %>%
  modify_header(
    list(
      add_stat_1 ~ "**ANOVA**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

# correlations with IOTF_pOWcutoff 
IOTF_pOWcutoff_demo_cor.varnames <- names(UAE_allDat)[c(7, 42:43, 105:106)]
IOTF_pOWcutoff_demo_cor.vars <- UAE_allDat[c(7, 42:43, 105:106)]
IOTF_pOWcutoff_demo_cormat <- data.frame(cor.matrix(IOTF_pOWcutoff_demo_cor.vars, IOTF_pOWcutoff_demo_cor.varnames))

                 
####    Medial Data       ####

# by sex ####
med_tab_sex <- UAE_allDat[c(5, 108, 65:69, 71:73, 75:76)]
UAE_med_sex <-
  tbl_summary(
    data=med_tab_sex,
    by = sex, 
    value = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    label = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    type = list(nComorbid ~ "continuous"), 
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_ttest) %>%
  add_stat(fns = list(VitDdeficiency ~ my_chifisher, Anemia ~ my_chifisher,
                      Hyperlipidemia ~ my_chifisher, ThyroidConditions ~ my_chifisher,
                      GlycemicStatus ~ my_chifisher, Growth.Stature ~ my_chifisher)) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

# by weight status ####
med_tab_OB <- UAE_allDat[c(107, 108, 65:69, 71:73, 75:76)]
UAE_med_OB <-
  tbl_summary(
    data=med_tab_OB,
    by = IOTF_3class, 
    value = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    label = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    type = list(nComorbid ~ "continuous"), 
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_anova) %>%
  add_stat(fns = list(VitDdeficiency ~ my_chifisher, Anemia ~ my_chifisher,
                      Hyperlipidemia ~ my_chifisher, ThyroidConditions ~ my_chifisher,
                      GlycemicStatus ~ my_chifisher, Growth.Stature ~ my_chifisher)) %>%
  modify_header(
    list(
      add_stat_1 ~ "**ANOVA**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )


## table of t-tests for IOTF_pOWcutoff by presence/absence of comorbidite
#VitD
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

                           
####   Family History       ####

# by sex ####
fam_tab_sex <- UAE_allDat[c(5, 49, 117, 109:116, 51, 126, 118:125)]
UAE_fam_sex <-
  tbl_summary(
    data=fam_tab_sex,
    by = sex, 
    value = list(Mother_OBhistory ~ "Mother", Father_OBhistory ~ "Father",
                 Sister_OBhistory ~ "Sister", Brother_OBhistory ~ "Brother",
                 Aunt_OBhistory ~ "Aunt", Uncle_OBhistory ~ "Uncle",
                 Grandmother_OBhistory ~ "Grandmother", Grandfather_OBhistory ~ "Grandfather",
                 Mother_EDhistory ~ "Mother", Father_EDhistory ~ "Father",
                 Sister_EDhistory ~ "Sister", Brother_EDhistory ~ "Brother",
                 Aunt_EDhistory ~ "Aunt", Uncle_EDhistory ~ "Uncle",
                 Grandmother_EDhistory ~ "Grandmother", Grandfather_EDhistory ~ "Grandfather"),
    label = list(Mother_OBhistory ~ "Mother", Father_OBhistory ~ "Father",
                 Sister_OBhistory ~ "Sister", Brother_OBhistory ~ "Brother",
                 Aunt_OBhistory ~ "Aunt", Uncle_OBhistory ~ "Uncle",
                 Grandmother_OBhistory ~ "Grandmother", Grandfather_OBhistory ~ "Grandfather",
                 Mother_EDhistory ~ "Mother", Father_EDhistory ~ "Father",
                 Sister_EDhistory ~ "Sister", Brother_EDhistory ~ "Brother",
                 Aunt_EDhistory ~ "Aunt", Uncle_EDhistory ~ "Uncle",
                 Grandmother_EDhistory ~ "Grandmother", Grandfather_EDhistory ~ "Grandfather"),
    type = list(nFam_Obesity ~ "continuous", nFam_EatingDisorder ~ "continuous", Fam_OB_YN ~ 'categorical', Fam_ED_YN ~ 'categorical'), 
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_ttest) %>%
  add_stat(fns = list(Fam_OB_YN ~ my_chifisher, Fam_ED_YN ~ my_chifisher)) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

# by weight status ####
fam_tab_OB <- UAE_allDat[c(107, 49, 117, 109:116, 51, 126, 118:125)]
UAE_fam_OB <-
  tbl_summary(
    data=fam_tab_OB,
    by = IOTF_3class, 
    value = list(Mother_OBhistory ~ "Mother", Father_OBhistory ~ "Father",
                 Sister_OBhistory ~ "Sister", Brother_OBhistory ~ "Brother",
                 Aunt_OBhistory ~ "Aunt", Uncle_OBhistory ~ "Uncle",
                 Grandmother_OBhistory ~ "Grandmother", Grandfather_OBhistory ~ "Grandfather",
                 Mother_EDhistory ~ "Mother", Father_EDhistory ~ "Father",
                 Sister_EDhistory ~ "Sister", Brother_EDhistory ~ "Brother",
                 Aunt_EDhistory ~ "Aunt", Uncle_EDhistory ~ "Uncle",
                 Grandmother_EDhistory ~ "Grandmother", Grandfather_EDhistory ~ "Grandfather"),
    label = list(Mother_OBhistory ~ "Mother", Father_OBhistory ~ "Father",
                 Sister_OBhistory ~ "Sister", Brother_OBhistory ~ "Brother",
                 Aunt_OBhistory ~ "Aunt", Uncle_OBhistory ~ "Uncle",
                 Grandmother_OBhistory ~ "Grandmother", Grandfather_OBhistory ~ "Grandfather",
                 Mother_EDhistory ~ "Mother", Father_EDhistory ~ "Father",
                 Sister_EDhistory ~ "Sister", Brother_EDhistory ~ "Brother",
                 Aunt_EDhistory ~ "Aunt", Uncle_EDhistory ~ "Uncle",
                 Grandmother_EDhistory ~ "Grandmother", Grandfather_EDhistory ~ "Grandfather"),
    type = list(nFam_Obesity ~ "continuous", nFam_EatingDisorder ~ "continuous", Fam_OB_YN ~ 'categorical', Fam_ED_YN ~ 'categorical'), 
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_anova) %>%
  add_stat(fns = list(Fam_OB_YN ~ my_chifisher, Fam_ED_YN ~ my_chifisher)) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )


##correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_FamHistory_cor.varnames <- names(UAE_allDat)[c(117, 126, 105)]
IOTF_pOWcutoff_FamHistory_cor.vars <- UAE_allDat[c(117, 126, 105)]
IOTF_pOWcutoff_FamHistory_cormat <- cor.matrix(IOTF_pOWcutoff_FamHistory_cor.vars, IOTF_pOWcutoff_FamHistory_cor.varnames)

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

##sensitivity test with poisson model 
IOTF_pOWcutoff_nFamOB_mod <- glm(nFam_Obesity ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat, family = poisson(link = 'log'))
IOTF_pOWcutoff_nFamOB_sum <- summary(IOTF_pOWcutoff_nFamOB_mod)
IOTF_pOWcutoff_nFamOB_odds <- exp(coef(IOTF_pOWcutoff_nFamOB_mod))
IOTF_pOWcutoff_nFamOB_oddsCI <- exp(confint(IOTF_pOWcutoff_nFamOB_mod))
IOTF_pOWcutoff_nFamOB_tab <- cbind.data.frame(IOTF_pOWcutoff_nFamOB_sum$coefficients, c('', '', '', '', '.', '', '', '***'), IOTF_pOWcutoff_nFamOB_odds, IOTF_pOWcutoff_nFamOB_oddsCI)
names(IOTF_pOWcutoff_nFamOB_tab) <- c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
IOTF_pOWcutoff_nFamOB_tab <- IOTF_pOWcutoff_nFamOB_tab[c(1, 6, 2, 7:8, 3:5)]


IOTF_pOWcutoff_nFamOB_dat <- UAE_allDat[!is.na(UAE_allDat$nFam_Obesity) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
IOTF_pOWcutoff_nFamOB_dat$nFam_Obesity_probLogit <- predict(IOTF_pOWcutoff_nFamOB_mod, type = 'link')

IOTF_pOWcutoff_nFamOB_adjOdds_plot <- ggplot(IOTF_pOWcutoff_nFamOB_dat, aes(y = exp(nFam_Obesity_probLogit), x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
   ggtitle('Corrleation between Percent of Overweight Cutoff cuttoff and Number of Family Members with History of Obesity (adjusted for covariates') +
  scale_y_continuous(name='Adjusted Odds of having an adiition Family with OB (income, mother ed, age, and sex)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

IOTF_pOWcutoff_nFamOB_raw_plot <- ggplot(UAE_allDat, aes(y = nFam_Obesity, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
   ggtitle('Corrleation between Percent of Overweight Cutoff cuttoff and Number of Family Members with History of Obesity') +
  scale_y_continuous(name='# Family with OB') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())
                          
####   Behavioral/Psychological       ####

# media use and sleep ####

# by sex ####
beh_tab_sex <- UAE_allDat[c(5, 59, 61, 63, 128, 93:97, 99:102, 104)]
UAE_beh_sex <-
  tbl_summary(
    data=beh_tab_sex,
    by = sex, 
    type = list(CSHQ_BedtimeResit ~ 'continuous', CSHQ_SleepOnsetDelay ~ 'continuous', 
                CSHQ_SleepDuration ~ 'continuous', CSHQ_SleepAnxiety ~ 'continuous', 
                CSHQ_NightWaking_no16 ~ 'continuous', CSHQ_Parasomnias ~ 'continuous', 
                CSHQ_SleepDisorderBreathing ~ 'continuous', CSHQ_DaytimeSleepiness ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_ttest) %>%
  add_stat(fns = all_categorical() ~ my_chifisher) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

# by weight status ####
beh_tab_OB <- UAE_allDat[c(107, 59, 61, 63, 128, 93:97, 99:102)]
UAE_beh_OB <-
  tbl_summary(
    data=beh_tab_OB,
    by = IOTF_3class, 
    type = list(CSHQ_BedtimeResit ~ 'continuous', CSHQ_SleepOnsetDelay ~ 'continuous', 
                CSHQ_SleepDuration ~ 'continuous', CSHQ_SleepAnxiety ~ 'continuous', 
                CSHQ_NightWaking_no16 ~ 'continuous', CSHQ_Parasomnias ~ 'continuous', 
                CSHQ_SleepDisorderBreathing ~ 'continuous', CSHQ_DaytimeSleepiness ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_anova) %>%
  add_stat(fns = all_categorical() ~ my_chifisher) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

##correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_CSHQ_cor.varnames <- names(UAE_allDat)[c(105, 127, 93:97, 99:102)]
IOTF_pOWcutoff_CSHQ_cor.vars <- UAE_allDat[c(105, 127, 93:97, 99:102)]
IOTF_pOWcutoff_CSHQ_cormat <- cor.matrix(IOTF_pOWcutoff_CSHQ_cor.vars, IOTF_pOWcutoff_CSHQ_cor.varnames)

##CSHQ sensitivity tests for IOTF_pOWcutoff
IOTF_pOWcutoff_sleepdelay_mod <- lm(CSHQ_SleepOnsetDelay ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat)
IOTF_pOWcutoff_sleepdelay_sum <- summary(IOTF_pOWcutoff_sleepdelay_mod)
IOTF_pOWcutoff_sleepdelay_tab <- cbind.data.frame(IOTF_pOWcutoff_sleepdelay_sum$coefficients, c('', '', '', '', '', '*', '', '*'))
names(IOTF_pOWcutoff_sleepdelay_tab) <- c('b', 'se', 't', 'p', ' ')

CHSQ_SleepDelay_dat <- UAE_allDat[!is.na(UAE_allDat$CSHQ_SleepOnsetDelay) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
CHSQ_SleepDelay_dat$CSHQ_SleepOnsetDelay_pred <- predict(IOTF_pOWcutoff_sleepdelay_mod, type = 'response')

IOTF_pOWcutoff_SleepDelay_adj_plot <- ggplot(CHSQ_SleepDelay_dat, aes(y = CSHQ_SleepOnsetDelay_pred, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Association between Percent of Overweight Cutoff and Sleep Onset Delay (adjust for covariates)') +
  scale_y_continuous(name='Adjusted Sleep Onset Delay (income, mother ed, age, and sex)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

IOTF_pOWcutoff_SleepDelay_raw_plot <- ggplot(UAE_allDat, aes(y = CSHQ_SleepOnsetDelay, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Association between Percent of Overweight Cutoff and Sleep Onset Delay') +
  scale_y_continuous(name='Sleep Onset Delay') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

IOTF_pOWcutoff_breathing_mod <- lm(CSHQ_SleepDisorderBreathing ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat)
IOTF_pOWcutoff_breathing_sum <- summary(IOTF_pOWcutoff_breathing_mod)
IOTF_pOWcutoff_breathing_tab <- cbind.data.frame(IOTF_pOWcutoff_breathing_sum$coefficients, c('', '', '', '', '', '', '', '**'))
names(IOTF_pOWcutoff_breathing_tab) <- c('b', 'se', 't', 'p', ' ')

CHSQ_DisorderedBreathing_dat <- UAE_allDat[!is.na(UAE_allDat$CSHQ_SleepDisorderBreathing) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
CHSQ_DisorderedBreathing_dat$CSHQ_SleepDisorderBreathing_pred <- predict(IOTF_pOWcutoff_breathing_mod, type = 'response')

IOTF_pOWcutoff_DisorderedBreathing_adj_plot <- ggplot(CHSQ_DisorderedBreathing_dat, aes(y = CSHQ_SleepDisorderBreathing_pred, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Association between Percent of Overweight Cutoff and Sleep Disordered Breathing (adjusted for covariates)') +
  scale_y_continuous(name='Adjusted Sleep Disordered Breathing (income, mother ed, age, and sex)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

IOTF_pOWcutoff_DisorderedBreathing_raw_plot <- ggplot(UAE_allDat, aes(y = CSHQ_SleepDisorderBreathing, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Association between Percent of Overweight Cutoff and Sleep Disordered Breathing') +
  scale_y_continuous(name='Sleep Disordered Breathing') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())


# SDQ ####

# by sex ####
sdq_tab_sex <- UAE_allDat[c(5, 79:90)]
UAE_sdq_sex <-
  tbl_summary(
    data=sdq_tab_sex,
    by = sex, 
    type = list(SDQ_EmotionProb_raw ~ 'continuous', SDQ_ConductProb_raw ~ 'continuous',
                SDQ_HyperactiveProb_raw ~ 'continuous', SDQ_PeerProb_raw ~ 'continuous',
                SDQ_Prosocial_raw ~ 'continuous', SDQ_TotalProb_raw ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_ttest) %>%
  add_stat(fns = all_categorical() ~ my_chifisher) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

# by weight status ####
sdq_tab_OB <- UAE_allDat[c(107, 79:90)]
UAE_sdq_OB <-
  tbl_summary(
    data=sdq_tab_OB,
    by = IOTF_3class, 
    type = list(SDQ_EmotionProb_raw ~ 'continuous', SDQ_ConductProb_raw ~ 'continuous',
                SDQ_HyperactiveProb_raw ~ 'continuous', SDQ_PeerProb_raw ~ 'continuous',
                SDQ_Prosocial_raw ~ 'continuous', SDQ_TotalProb_raw ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})   [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_anova) %>%
  add_stat(fns = all_categorical() ~ my_chifisher) %>%
  modify_header(
    list(
      add_stat_1 ~ "**t-test**",
      add_stat_2 ~ "**chi/fisher**",
      all_stat_cols() ~ "**{level}**"
    )
  )

##correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_SDQ_cor.varnames <- names(UAE_allDat)[c(105, 79:84)]
IOTF_pOWcutoff_SDQ_cor.vars <- UAE_allDat[c(105, 79:84)]
IOTF_pOWcutoff_SDQ_cormat <- cor.matrix(IOTF_pOWcutoff_SDQ_cor.vars, IOTF_pOWcutoff_SDQ_cor.varnames)

##SDQ sensitivity tests for IOTF_pOWcutoff

##center for interaction
UAE_allDat$IOTF_pOWcutoff_c100 <- UAE_allDat$IOTF_pOWcutoff - 100
  
## peer problems
IOTF_pOWcutoff_peerprob_mod <- lm(SDQ_PeerProb_raw ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, data = UAE_allDat)
IOTF_pOWcutoff_peerprob_sum <- summary(IOTF_pOWcutoff_peerprob_mod)
IOTF_pOWcutoff_peerprob_tab <- cbind.data.frame(IOTF_pOWcutoff_peerprob_sum$coefficients, c('', '', '', '', '.', '', '', '.', ''))
names(IOTF_pOWcutoff_peerprob_tab) <- c('b', 'se', 't', 'p', ' ')

SDQ_PeerProb_dat <- UAE_allDat[!is.na(UAE_allDat$SDQ_PeerProb_raw) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
SDQ_PeerProb_dat$SDQ_PeerProb_pred <- predict(IOTF_pOWcutoff_peerprob_mod, type = 'response')

IOTF_pOWcutoff_peerprob_adj_plot <- ggplot(SDQ_PeerProb_dat, aes(y = SDQ_PeerProb_pred, x = (IOTF_pOWcutoff_c100 + 100), color = sex)) +
  geom_smooth(method = 'lm', formula = y~x, aes(group = sex, linetype = sex)) +
  geom_point(aes(shape = sex), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Peer Problems Raw (adjusted for covariates)') +
  scale_y_continuous(name='Adjusted Peer Problems (income, mother ed, age, and sex)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

IOTF_pOWcutoff_peerprob_raw_plot <- ggplot(UAE_allDat, aes(y = SDQ_PeerProb_raw, x = (IOTF_pOWcutoff_c100 + 100), color = sex)) +
  geom_smooth(method = 'lm', formula = y~x, aes(group = sex, linetype = sex)) +
  geom_point(aes(color = sex, shape = sex), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Peer Problems Raw') +
  scale_y_continuous(name='Peer Problems') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

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

SDQ_PeerProb_dat$SDQ_PeerProb_Elevated_predLogit <- predict(IOTF_pOWcutoff_peerprob_Elevated_mod, type = 'link')

IOTF_pOWcutoff_peerprob_ElevatedCat_adjOdds_plot <- ggplot(SDQ_PeerProb_dat, aes(y = exp(SDQ_PeerProb_Elevated_predLogit), x = (IOTF_pOWcutoff_c100 + 100), color = sex)) +
  geom_smooth(method = 'lm', formula = y~x, aes(group = sex, linetype = sex)) +
  geom_point(aes(shape = sex), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Peer Problems Elevated vs Not (adjusted for covariates)') +
  scale_y_continuous(name='Adjusted Odds for Elevated Peer Problems (income, mother ed, age, and sex)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())



IOTF_pOWcutoff_prosocial_mod <- lm(SDQ_Prosocial_raw ~ Month_AED + Mother_ed + Age_yr + sex*IOTF_pOWcutoff_c100, data = UAE_allDat)
IOTF_pOWcutoff_prosocial_sum <- summary(IOTF_pOWcutoff_prosocial_mod)
IOTF_pOWcutoff_prosocial_tab <- cbind.data.frame(IOTF_pOWcutoff_prosocial_sum$coefficients, c('', '', '', '', '', '*', '**', '', '***'))
names(IOTF_pOWcutoff_prosocial_tab) <- c('b', 'se', 't', 'p', ' ')

IOTF_pOWcutoff_prosocial_emtrends <- emtrends(IOTF_pOWcutoff_prosocial_mod, ~ IOTF_pOWcutoff_c100 | sex , var = 'IOTF_pOWcutoff_c100')


SDQ_Prosocial_dat <- UAE_allDat[!is.na(UAE_allDat$SDQ_Prosocial_raw) & !is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed), ]
SDQ_Prosocial_dat$SDQ_Prosocial_pred <- predict(IOTF_pOWcutoff_prosocial_mod, type = 'response')

IOTF_pOWcutoff_prosocial_adj_plot <- ggplot(SDQ_Prosocial_dat, aes(y = SDQ_Prosocial_pred, x = (IOTF_pOWcutoff_c100 + 100), color = sex)) +
  geom_smooth(method = 'lm', formula = y~x, aes(group = sex, linetype = sex)) +
  geom_point(aes(shape = sex), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Prosocial Raw (adjusted for covariates)') +
  scale_y_continuous(name='Adjusted Prosocial (income, mother ed, age, and sex)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

IOTF_pOWcutoff_prosocial_raw_plot <- ggplot(UAE_allDat, aes(y = SDQ_Prosocial_raw, x = (IOTF_pOWcutoff_c100 + 100), color = sex)) +
  geom_smooth(method = 'lm', formula = y~x, aes(group = sex, linetype = sex)) +
  geom_point(aes(shape = sex), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Prosocial Raw') +
  scale_y_continuous(name='Prosocial)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

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

SDQ_Prosocial_dat$SDQ_prosocial_Low_predLogit <- predict(IOTF_pOWcutoff_prosocial_Low_mod, type = 'link')

IOTF_pOWcutoff_prosocialLow_Cat_adjOdds_plot <- ggplot(SDQ_Prosocial_dat, aes(y = exp(SDQ_prosocial_Low_predLogit), x = (IOTF_pOWcutoff_c100 + 100), color = sex)) +
  geom_smooth(method = 'lm', formula = y~x, aes(group = sex, linetype = sex)) +
  geom_point(aes(shape = sex), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Prosocial Low vs Not (adjusted for covariates)') +
  scale_y_continuous(name='Adjusted Odds for Low Prosocial (income, mother ed, age, and sex)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())


# ##correlations with IOTF_pOWcutoff
# IOTF_pOWcutoff_SDQ_cor.varnames <- c('EmlProb', 'ConductProb', 'Hyper', 'PeerProb',  'TotalProb','Prosocial', 'IOTF_pOWcutoff')
# IOTF_pOWcutoff_SDQ_cor.vars <- UAE_allDat[c('Emotional.Problems_Raw', 'Conduct.Problems_Raw', 'Hyperactivity_Raw', 'Peer.Problems_Raw',
#                                            'Total.Difficulty.Score_Raw', 'Prosocial_Raw',  'IOTF_pOWcutoff')]
# IOTF_pOWcutoff_SDQ_cormat <- cor.matrix(IOTF_pOWcutoff_SDQ_cor.vars, IOTF_pOWcutoff_SDQ_cor.varnames)
# 
# #Emotional Problems - Elevated vs Not
# IOTF_pOWcutoff_EmProbs.Sex_plot <- ggplot(UAE_allDat, aes(y = Emotional.Problems_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
#   geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
#   scale_y_continuous(name='Emotional Problems') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# UAE_allDat$Emotional.problems.categorization2 <- ifelse(UAE_allDat$Emotional.problems.categorization == 'VeryHigh', 'Elevated', 
#                                                               ifelse(UAE_allDat$Emotional.problems.categorization == 'High', 'Elevated', 
#                                                                      ifelse(UAE_allDat$Emotional.problems.categorization == 'SlightlyRaised', 'Elevated',
#                                                                             ifelse(UAE_allDat$Emotional.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))
# 
# UAE_allDat$Emotional.problems.categorization2 <- factor(UAE_allDat$Emotional.problems.categorization2, 
#                                                                 levels = c('NotElevated', 'Elevated'))
# 
# IOTF_pOWcutoff_EmProbs_glm2_mod <- glm(Emotional.problems.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
# IOTF_pOWcutoff_EmProbs_glm2_sum <- summary(IOTF_pOWcutoff_EmProbs_glm2_mod)
# IOTF_pOWcutoff_EmProbs_glm2_odds <- exp(coef(IOTF_pOWcutoff_EmProbs_glm2_mod))
# IOTF_pOWcutoff_EmProbs_glm2_oddsCI <- exp(confint(IOTF_pOWcutoff_EmProbs_glm2_mod))
# IOTF_pOWcutoff_EmProbs_glm2_tab <- cbind.data.frame(IOTF_pOWcutoff_EmProbs_glm2_sum$coefficients, IOTF_pOWcutoff_EmProbs_glm2_odds, IOTF_pOWcutoff_EmProbs_glm2_oddsCI)
# names(IOTF_pOWcutoff_EmProbs_glm2_tab) <- c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
# 
# #Conduct Problems - elevated vs not
# IOTF_pOWcutoff_CondProbs.Sex_plot <- ggplot(UAE_allDat, aes(y = Conduct.Problems_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
#   geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
#   scale_y_continuous(name='Conduct Problems') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# UAE_allDat$Conduct.problems.categorization2 <- ifelse(UAE_allDat$Conduct.problems.categorization == 'VeryHigh', 'Elevated', 
#                                                               ifelse(UAE_allDat$Conduct.problems.categorization == 'High', 'Elevated', 
#                                                                      ifelse(UAE_allDat$Conduct.problems.categorization == 'SlightlyRaised', 'Elevated',
#                                                                             ifelse(UAE_allDat$Conduct.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))
# 
# UAE_allDat$Conduct.problems.categorization2 <- factor(UAE_allDat$Conduct.problems.categorization2, 
#                                                               levels = c('NotElevated', 'Elevated'))
# 
# IOTF_pOWcutoff_CondProbs_glm2_mod <- glm(Conduct.problems.categorization2 ~ SEScat + Mother_ed+ nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
# IOTF_pOWcutoff_CondProbs_glm2_sum <- summary(IOTF_pOWcutoff_CondProbs_glm2_mod)
# IOTF_pOWcutoff_CondProbs_glm2_odds <- exp(coef(IOTF_pOWcutoff_CondProbs_glm2_mod))
# IOTF_pOWcutoff_CondProbs_glm2_oddsCI <- exp(confint(IOTF_pOWcutoff_CondProbs_glm2_mod))
# IOTF_pOWcutoff_CondProbs_glm2_tab <- cbind.data.frame(IOTF_pOWcutoff_CondProbs_glm2_sum$coefficients, c('', '', '', '', '', '.', '.', '', '', ''), IOTF_pOWcutoff_CondProbs_glm2_odds, IOTF_pOWcutoff_CondProbs_glm2_oddsCI)
# names(IOTF_pOWcutoff_CondProbs_glm2_tab) <- c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
# 
# #Hyperactivity - elevated vs not
# IOTF_pOWcutoff_Hyper.Sex_plot <- ggplot(UAE_allDat, aes(y = Hyperactivity_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
#   geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
#   scale_y_continuous(name='Hyperactivity') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# UAE_allDat$Hyperactivity.categorization2 <- ifelse(UAE_allDat$Hyperactivity.categorization == 'VeryHigh', 'Elevated', 
#                                                               ifelse(UAE_allDat$Hyperactivity.categorization == 'High', 'Elevated', 
#                                                                      ifelse(UAE_allDat$Hyperactivity.categorization == 'SlightlyRaised', 'Elevated',
#                                                                             ifelse(UAE_allDat$Hyperactivity.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))
# 
# UAE_allDat$Hyperactivity.categorization2 <- factor(UAE_allDat$Hyperactivity.categorization2, 
#                                                            levels = c('NotElevated', 'Elevated'))
# 
# IOTF_pOWcutoff_Hyper_glm2_mod <- glm(Hyperactivity.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
# IOTF_pOWcutoff_Hyper_glm2_sum <- summary(IOTF_pOWcutoff_Hyper_glm2_mod)
# IOTF_pOWcutoff_Hyper_glm2_odds <- exp(coef(IOTF_pOWcutoff_Hyper_glm2_mod))
# IOTF_pOWcutoff_Hyper_glm2_oddsCI <- exp(confint(IOTF_pOWcutoff_Hyper_glm2_mod))
# IOTF_pOWcutoff_Hyper_glm2_tab <- cbind.data.frame(IOTF_pOWcutoff_Hyper_glm2_sum$coefficients, IOTF_pOWcutoff_Hyper_glm2_odds, IOTF_pOWcutoff_Hyper_glm2_oddsCI)
# names(IOTF_pOWcutoff_Hyper_glm2_tab) <- c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
# 
# #Peer Problems - elevated vs not
# UAE_allDat$Peer.problems.categorization2 <- ifelse(UAE_allDat$Peer.problems.categorization == 'VeryHigh', 'Elevated', 
#                                                            ifelse(UAE_allDat$Peer.problems.categorization == 'High', 'Elevated', 
#                                                                   ifelse(UAE_allDat$Peer.problems.categorization == 'SlightlyRaised', 'Elevated',
#                                                                          ifelse(UAE_allDat$Peer.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))
# 
# UAE_allDat$Peer.problems.categorization2 <- factor(UAE_allDat$Peer.problems.categorization2, 
#                                                            levels = c('NotElevated', 'Elevated'))
# 
# IOTF_pOWcutoff_PeerProbs_glm2_mod <- glm(Peer.problems.categorization2 ~ SEScat + Mother_ed+ nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
# IOTF_pOWcutoff_PeerProbs_glm2_sum <- summary(IOTF_pOWcutoff_PeerProbs_glm2_mod)
# IOTF_pOWcutoff_PeerProbs_glm2_odds <- exp(coef(IOTF_pOWcutoff_PeerProbs_glm2_mod))
# IOTF_pOWcutoff_PeerProbs_glm2_oddsCI <- exp(confint(IOTF_pOWcutoff_PeerProbs_glm2_mod))
# IOTF_pOWcutoff_PeerProbs_glm2_tab <- cbind.data.frame(IOTF_pOWcutoff_PeerProbs_glm2_sum$coefficients, c('','','','.','*','','','','*','*'), IOTF_pOWcutoff_PeerProbs_glm2_odds, IOTF_pOWcutoff_PeerProbs_glm2_oddsCI)
# names(IOTF_pOWcutoff_PeerProbs_glm2_tab) <- c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
# 
# IOTF_pOWcutoff_PeerProbs_glm2_emtrends <- emtrends(IOTF_pOWcutoff_PeerProbs_glm2_mod, ~ IOTF_pOWcutoff_c100 | Gender , var = 'IOTF_pOWcutoff_c100', infer = c('TRUE', 'TRUE'))
# IOTF_pOWcutoff_PeerProbs_glm2_emtrends_sum <- summary(IOTF_pOWcutoff_PeerProbs_glm2_emtrends)
# 
# PeerProbs_dat <- UAE_allDat[!is.na(UAE_allDat$Peer.problems.categorization2) & !is.na(UAE_allDat$SEScat) & !is.na(UAE_allDat$Mother_ed), 
#                                     c('Gender', 'IOTF_pOWcutoff_c100')]
# PeerProbs_dat$PredProb <- predict(IOTF_pOWcutoff_PeerProbs_glm2_mod, type = 'response')
# PeerProbs_dat$PredLogit <- predict(IOTF_pOWcutoff_PeerProbs_glm2_mod, type = 'link')
# 
# IOTF_pOWcutoff_PeerProbs.Sex_ORplot <- ggplot(PeerProbs_dat, aes(y = exp(PredLogit), x = IOTF_pOWcutoff_c100, group = Gender)) +
#   geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
#   geom_point(aes(y = exp(PredLogit), shape = Gender), size = 3) +
#   scale_y_continuous(name='Predicted Odds Ratio') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# #Prosocial - elevated vs not
# UAE_allDat$Prosocial.categorization2 <- ifelse(UAE_allDat$Prosocial.categorization == 'VeryLow', 'Lowered', 
#                                                            ifelse(UAE_allDat$Prosocial.categorization == 'Low', 'Lowered', 
#                                                                   ifelse(UAE_allDat$Prosocial.categorization == 'SlightlyLowered', 'Lowered',
#                                                                          ifelse(UAE_allDat$Prosocial.categorization == 'CloseToAverage', 'NotLowered', 'Error'))))
# 
# UAE_allDat$Prosocial.categorization2 <- factor(UAE_allDat$Prosocial.categorization2, 
#                                                            levels = c('NotLowered', 'Lowered'))
# 
# IOTF_pOWcutoff_Prosocial_glm2_mod <- glm(Prosocial.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
# IOTF_pOWcutoff_Prosocial_glm2_sum <- summary(IOTF_pOWcutoff_Prosocial_glm2_mod)
# IOTF_pOWcutoff_Prosocial_glm2_odds <- exp(coef(IOTF_pOWcutoff_Prosocial_glm2_mod))
# IOTF_pOWcutoff_Prosocial_glm2_oddsCI <- exp(confint(IOTF_pOWcutoff_Prosocial_glm2_mod))
# IOTF_pOWcutoff_Prosocial_glm2_tab <- cbind.data.frame(IOTF_pOWcutoff_Prosocial_glm2_sum$coefficients, c('','.','','','','','','','','.'), IOTF_pOWcutoff_Prosocial_glm2_odds, IOTF_pOWcutoff_Prosocial_glm2_oddsCI)
# names(IOTF_pOWcutoff_Prosocial_glm2_tab) <- c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
# 
# IOTF_pOWcutoff_Prosocial_glm2_emtrends <- emtrends(IOTF_pOWcutoff_Prosocial_glm2_mod, ~ IOTF_pOWcutoff_c100 | Gender , var = 'IOTF_pOWcutoff_c100', infer = c('TRUE', 'TRUE'))
# IOTF_pOWcutoff_Prosocial_glm2_emtrends_sum <- summary(IOTF_pOWcutoff_Prosocial_glm2_emtrends)
# 
# Prosocial_dat <- UAE_allDat[!is.na(UAE_allDat$Prosocial.categorization2) & !is.na(UAE_allDat$SEScat) & !is.na(UAE_allDat$Mother_ed), 
#                                     c('Gender', 'IOTF_pOWcutoff_c100')]
# Prosocial_dat$PredProb <- predict(IOTF_pOWcutoff_Prosocial_glm2_mod, type = 'response')
# Prosocial_dat$PredLogit <- predict(IOTF_pOWcutoff_Prosocial_glm2_mod, type = 'link')
# 
# IOTF_pOWcutoff_Prosocial.Sex_ORplot <- ggplot(Prosocial_dat, aes(y = exp(PredLogit), x = IOTF_pOWcutoff_c100, group = Gender)) +
#   geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
#   geom_point(aes(y = exp(PredLogit), shape = Gender), size = 3) +
#   scale_y_continuous(name='Predicted Odds Ratio') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# #Total Problems - Elevated vs Not
# IOTF_pOWcutoff_TotalProbs.Sex_plot <- ggplot(UAE_allDat, aes(y = Emotional.Problems_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
#   geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
#   scale_y_continuous(name='Emotional Problems') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# UAE_allDat$Total.difficulty.Categorization2 <- ifelse(UAE_allDat$Total.difficulty.Categorization == 'VeryHigh', 'Elevated', 
#                                                                 ifelse(UAE_allDat$Total.difficulty.Categorization == 'High', 'Elevated', 
#                                                                        ifelse(UAE_allDat$Total.difficulty.Categorization == 'SlightlyRaised', 'Elevated',
#                                                                               ifelse(UAE_allDat$Total.difficulty.Categorization == 'CloseToAverage', 'NotElevated', 'Error'))))
# 
# UAE_allDat$Total.difficulty.Categorization2 <- factor(UAE_allDat$Total.difficulty.Categorization2, 
#                                                                 levels = c('NotElevated', 'Elevated'))
# 
# IOTF_pOWcutoff_TotalProbs_glm2_mod <- glm(Total.difficulty.Categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
# IOTF_pOWcutoff_TotalProbs_glm2_sum <- summary(IOTF_pOWcutoff_TotalProbs_glm2_mod)
# IOTF_pOWcutoff_TotalProbs_glm2_odds <- exp(coef(IOTF_pOWcutoff_TotalProbs_glm2_mod))
# IOTF_pOWcutoff_TotalProbs_glm2_oddsCI <- exp(confint(IOTF_pOWcutoff_TotalProbs_glm2_mod))
# IOTF_pOWcutoff_TotalProbs_glm2_tab <- cbind.data.frame(IOTF_pOWcutoff_TotalProbs_glm2_sum$coefficients, IOTF_pOWcutoff_TotalProbs_glm2_odds, IOTF_pOWcutoff_TotalProbs_glm2_oddsCI)
# names(IOTF_pOWcutoff_TotalProbs_glm2_tab) <- c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')
# 

# #####################################
# ####
# ####   CHSQ -  Models  ####
# ####
# #####################################
# 
# ##Scatter Plots
# ###Bedtime Resistance
# IOTF_pOWcutoff_BedRes_plot <- ggplot(UAE_allDat, aes(y = BedtimeRes, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Bedtime Resistance') +
#   scale_y_continuous(name='Bedtime Resistance') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ###Sleep Delay
# IOTF_pOWcutoff_SleepDelay_plot <- ggplot(UAE_allDat, aes(y = SleepDelay, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Delay') +
#   scale_y_continuous(name='Sleep Delay') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ###Sleep Duration
# IOTF_pOWcutoff_SleepDur_plot <- ggplot(UAE_allDat, aes(y = SleepDuration, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Duration') +
#   scale_y_continuous(name='Sleep Duration Concerns') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ###Sleep Anxiety
# IOTF_pOWcutoff_SleepAnxiety_plot <- ggplot(UAE_allDat, aes(y = SleepAnxiety, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Anxiety') +
#   scale_y_continuous(name='Sleep Anxiety') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ###Night Wake
# IOTF_pOWcutoff_NightWake_plot <- ggplot(UAE_allDat, aes(y = NightWake, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Night Waking') +
#   scale_y_continuous(name='Sleep Anxiety') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ###Parasomnias
# IOTF_pOWcutoff_Parasom_plot <- ggplot(UAE_allDat, aes(y = Parasomnias, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Parasomnias') +
#   scale_y_continuous(name='Parasomnias') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ###Sleep Disordered Breathing
# IOTF_pOWcutoff_SleepDisBreathing_plot <- ggplot(UAE_allDat, aes(y = SleepDisBreathing, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Disordered Breathing') +
#   scale_y_continuous(name='Sleep Disordered Breathing') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ##Daytime Sleepiness
# IOTF_pOWcutoff_DaySleepy_plot <- ggplot(UAE_allDat, aes(y = DaytimeSleepy, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Daytime Sleepiness') +
#   scale_y_continuous(name='Daytime Sleepiness') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())
# 
# ##Total Score
# IOTF_pOWcutoff_Totalgood_plot <- ggplot(UAE_allDat[UAE_allDat$GoodTotalScore_missing16orless == 'Y', ], aes(y = TotalScore, x = IOTF_pOWcutoff_c100)) +
#   geom_smooth(method = 'loess', formula = y~x) +
#   geom_point(aes(color = Gender)) + 
#   #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Daytime Sleepiness') +
#   scale_y_continuous(name='Total Score') +
#   scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.background = element_blank())