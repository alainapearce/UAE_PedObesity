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
library(gtsummary)

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
IOTF_pOWcutoff_demo_cor.varnames = names(UAE_allDat)[c(7, 42:43, 105:106)]
IOTF_pOWcutoff_demo_cor.vars = UAE_allDat[c(7, 42:43, 105:106)]
IOTF_pOWcutoff_demo_cormat = data.frame(cor.matrix(IOTF_pOWcutoff_demo_cor.vars, IOTF_pOWcutoff_demo_cor.varnames))

                 
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
IOTF_pOWcutoff.VitD_ttest = t.test(IOTF_pOWcutoff ~ VitDdeficiency, data = UAE_allDat)
IOTF_pOWcutoff.Anemia_ttest = t.test(UAE_allDat$IOTF_pOWcutoff ~ as.factor(ifelse(is.na(UAE_allDat$Anemia), 'N', 'Y')))
IOTF_pOWcutoff.Thyroid_ttest = t.test(UAE_allDat$IOTF_pOWcutoff ~ as.factor(ifelse(is.na(UAE_allDat$ThyroidConditions), 'N', 'Y')))
IOTF_pOWcutoff.Glycemic_ttest = t.test(UAE_allDat$IOTF_pOWcutoff ~ as.factor(ifelse(is.na(UAE_allDat$GlycemicStatus), 'N', 'Y')))

IOTF_pOWcutoff.MedComorbid_ttest_tab = data.frame(matrix(c(round(IOTF_pOWcutoff.VitD_ttest$statistic, 2), round(IOTF_pOWcutoff.VitD_ttest$parameter, 2),round(IOTF_pOWcutoff.VitD_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.VitD_ttest$estimate[1], 2),round(IOTF_pOWcutoff.VitD_ttest$estimate[2], 2),
                                     round(IOTF_pOWcutoff.Anemia_ttest$statistic, 2), round(IOTF_pOWcutoff.Anemia_ttest$parameter, 2), round(IOTF_pOWcutoff.Anemia_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.Anemia_ttest$estimate[1], 2), round(IOTF_pOWcutoff.Anemia_ttest$estimate[2], 2),
                                     round(IOTF_pOWcutoff.Thyroid_ttest$statistic, 2), round(IOTF_pOWcutoff.Thyroid_ttest$parameter, 2), round(IOTF_pOWcutoff.Thyroid_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.Thyroid_ttest$estimate[1], 2), round(IOTF_pOWcutoff.Thyroid_ttest$estimate[2], 2),
                                     round(IOTF_pOWcutoff.Glycemic_ttest$statistic, 2), round(IOTF_pOWcutoff.Glycemic_ttest$parameter, 2), round(IOTF_pOWcutoff.Glycemic_ttest$p.value, 4),
                                     round(IOTF_pOWcutoff.Glycemic_ttest$estimate[1], 2), round(IOTF_pOWcutoff.Glycemic_ttest$estimate[2], 2)), byrow = TRUE, nrow = 4))

names(IOTF_pOWcutoff.MedComorbid_ttest_tab) = c('t', 'df', 'pvalue', 'AbsentMean', 'PresentMean')
rownames(IOTF_pOWcutoff.MedComorbid_ttest_tab) = c('VitD Deficiency', 'Anemia', 'Thyroid Dysfunction', 'Glycemic Status')
IOTF_pOWcutoff.MedComorbid_ttest_tab$sig = c('', '**', '', '')
IOTF_pOWcutoff.MedComorbid_ttest_tab <- IOTF_pOWcutoff.MedComorbid_ttest_tab[c(4:5, 1:3, 6)]

#####################################
####                            
####   Family History       ####
####                            
#####################################

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
IOTF_pOWcutoff_FamHistory_cor.varnames = names(UAE_allDat)[c(117, 126, 105)]
IOTF_pOWcutoff_FamHistory_cor.vars = UAE_allDat[c(117, 126, 105)]
IOTF_pOWcutoff_FamHistory_cormat = cor.matrix(IOTF_pOWcutoff_FamHistory_cor.vars, IOTF_pOWcutoff_FamHistory_cor.varnames)

##ttests 
IOTF_pOWcutoff_FamOB_ttest = t.test(IOTF_pOWcutoff ~ Fam_OB_YN, data = UAE_allDat)
IOTF_pOWcutoff_FamED_ttest = t.test(IOTF_pOWcutoff ~ Fam_ED_YN, data = UAE_allDat)


IOTF_pOWcutoff_fam_ttest_tab = data.frame(matrix(c(round(IOTF_pOWcutoff_FamOB_ttest$statistic, 2), round(IOTF_pOWcutoff_FamOB_ttest$parameter, 2), round(IOTF_pOWcutoff_FamOB_ttest$p.value, 4),
                                                        round(IOTF_pOWcutoff_FamOB_ttest$estimate[1], 2), round(IOTF_pOWcutoff_FamOB_ttest$estimate[2], 2),
                                                        round(IOTF_pOWcutoff_FamED_ttest$statistic, 2), round(IOTF_pOWcutoff_FamED_ttest$parameter, 2), round(IOTF_pOWcutoff_FamED_ttest$p.value, 4),
                                                        round(IOTF_pOWcutoff_FamED_ttest$estimate[1], 2), round(IOTF_pOWcutoff_FamED_ttest$estimate[2], 2)), byrow = TRUE, nrow = 2))

names(IOTF_pOWcutoff_fam_ttest_tab) = c('t', 'df', 'pvalue', 'No', 'Yes')
rownames(IOTF_pOWcutoff_fam_ttest_tab) = c('Family History of Obesity', 'Family History of Eating Disorder')
IOTF_pOWcutoff_fam_ttest_tab$sig = c('***', '')
IOTF_pOWcutoff_fam_ttest_tab <- IOTF_pOWcutoff_fam_ttest_tab[c(4:5, 1:3, 6)]

##sensitivity test with poisson model 
IOTF_pOWcutoff_nFamOB_mod <- glm(nFam_Obesity ~ Month_AED + Mother_ed + Age_yr + sex + IOTF_pOWcutoff, data = UAE_allDat, family = poisson(link = 'log'))
IOTF_pOWcutoff_nFamOB_sum <- summary(IOTF_pOWcutoff_nFamOB_mod)
IOTF_pOWcutoff_nFamOB_odds = exp(coef(IOTF_pOWcutoff_nFamOB_mod))
IOTF_pOWcutoff_nFamOB_oddsCI = exp(confint(IOTF_pOWcutoff_nFamOB_mod))
IOTF_pOWcutoff_nFamOB_tab = cbind.data.frame(IOTF_pOWcutoff_nFamOB_sum$coefficients, c('', '', '', '', '.', '', '', '***'), IOTF_pOWcutoff_nFamOB_odds, IOTF_pOWcutoff_nFamOB_oddsCI)
names(IOTF_pOWcutoff_nFamOB_tab) = c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

                          
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
IOTF_pOWcutoff_CSHQ_cor.varnames = names(UAE_allDat)[c(105, 127, 93:97, 99:102)]
IOTF_pOWcutoff_CSHQ_cor.vars = UAE_allDat[c(105, 127, 93:97, 99:102)]
IOTF_pOWcutoff_CSHQ_cormat = cor.matrix(IOTF_pOWcutoff_CSHQ_cor.vars, IOTF_pOWcutoff_CSHQ_cor.varnames)


##correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_cog_cor.varnames = c('Block', 'BlockT', 'Matrix', 'MatrixT',
                                 'IQ', 'dsF', 'dsFSS',
                                 'dsB', 'sdBSS', 'Code', 'CodeSS',
                                 'Age', 'BMI25p', 'W2H', 'nComorbid')
IOTF_pOWcutoff_cog_cor.vars = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, c('WASI_BlockRaw', 'WASI_BlockT', 'WASI_MatrixRaw', 'WASI_MatrixT',
                                                  'WASI_PRI_IQ', 'DigitSpan_ForwardRaw', 'DigitSpan_ForwardSS',
                                                 'DigitSpan_BackwardRaw', 'DigitSpan_BackwardSS', 'CodingRaw', 'CodingSS',
                                                 'Age_yr', 'IOTF_pOWcutoff', 'Wasit2Hip', 'nComorbid')]
IOTF_pOWcutoff_cog_cormat = cor.matrix(IOTF_pOWcutoff_cog_cor.vars, IOTF_pOWcutoff_cog_cor.varnames)



# SDQ ####

##correlations with IOTF_pOWcutoff
IOTF_pOWcutoff_SDQ_cor.varnames = c('EmlProb', 'ConductProb', 'Hyper', 'PeerProb',  'TotalProb','Prosocial', 'IOTF_pOWcutoff')
IOTF_pOWcutoff_SDQ_cor.vars = UAE_allDat[c('Emotional.Problems_Raw', 'Conduct.Problems_Raw', 'Hyperactivity_Raw', 'Peer.Problems_Raw',
                                           'Total.Difficulty.Score_Raw', 'Prosocial_Raw',  'IOTF_pOWcutoff')]
IOTF_pOWcutoff_SDQ_cormat = cor.matrix(IOTF_pOWcutoff_SDQ_cor.vars, IOTF_pOWcutoff_SDQ_cor.varnames)

#Emotional Problems - Elevated vs Not
IOTF_pOWcutoff_EmProbs.Sex_plot = ggplot(UAE_allDat, aes(y = Emotional.Problems_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Emotional Problems') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_allDat$Emotional.problems.categorization2 = ifelse(UAE_allDat$Emotional.problems.categorization == 'VeryHigh', 'Elevated', 
                                                              ifelse(UAE_allDat$Emotional.problems.categorization == 'High', 'Elevated', 
                                                                     ifelse(UAE_allDat$Emotional.problems.categorization == 'SlightlyRaised', 'Elevated',
                                                                            ifelse(UAE_allDat$Emotional.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_allDat$Emotional.problems.categorization2 = factor(UAE_allDat$Emotional.problems.categorization2, 
                                                                levels = c('NotElevated', 'Elevated'))

IOTF_pOWcutoff_EmProbs_glm2_mod = glm(Emotional.problems.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_EmProbs_glm2_sum = summary(IOTF_pOWcutoff_EmProbs_glm2_mod)
IOTF_pOWcutoff_EmProbs_glm2_odds = exp(coef(IOTF_pOWcutoff_EmProbs_glm2_mod))
IOTF_pOWcutoff_EmProbs_glm2_oddsCI = exp(confint(IOTF_pOWcutoff_EmProbs_glm2_mod))
IOTF_pOWcutoff_EmProbs_glm2_tab = cbind.data.frame(IOTF_pOWcutoff_EmProbs_glm2_sum$coefficients, IOTF_pOWcutoff_EmProbs_glm2_odds, IOTF_pOWcutoff_EmProbs_glm2_oddsCI)
names(IOTF_pOWcutoff_EmProbs_glm2_tab) = c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

#Conduct Problems - elevated vs not
IOTF_pOWcutoff_CondProbs.Sex_plot = ggplot(UAE_allDat, aes(y = Conduct.Problems_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Conduct Problems') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_allDat$Conduct.problems.categorization2 = ifelse(UAE_allDat$Conduct.problems.categorization == 'VeryHigh', 'Elevated', 
                                                              ifelse(UAE_allDat$Conduct.problems.categorization == 'High', 'Elevated', 
                                                                     ifelse(UAE_allDat$Conduct.problems.categorization == 'SlightlyRaised', 'Elevated',
                                                                            ifelse(UAE_allDat$Conduct.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_allDat$Conduct.problems.categorization2 = factor(UAE_allDat$Conduct.problems.categorization2, 
                                                              levels = c('NotElevated', 'Elevated'))

IOTF_pOWcutoff_CondProbs_glm2_mod = glm(Conduct.problems.categorization2 ~ SEScat + Mother_ed+ nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_CondProbs_glm2_sum = summary(IOTF_pOWcutoff_CondProbs_glm2_mod)
IOTF_pOWcutoff_CondProbs_glm2_odds = exp(coef(IOTF_pOWcutoff_CondProbs_glm2_mod))
IOTF_pOWcutoff_CondProbs_glm2_oddsCI = exp(confint(IOTF_pOWcutoff_CondProbs_glm2_mod))
IOTF_pOWcutoff_CondProbs_glm2_tab = cbind.data.frame(IOTF_pOWcutoff_CondProbs_glm2_sum$coefficients, c('', '', '', '', '', '.', '.', '', '', ''), IOTF_pOWcutoff_CondProbs_glm2_odds, IOTF_pOWcutoff_CondProbs_glm2_oddsCI)
names(IOTF_pOWcutoff_CondProbs_glm2_tab) = c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

#Hyperactivity - elevated vs not
IOTF_pOWcutoff_Hyper.Sex_plot = ggplot(UAE_allDat, aes(y = Hyperactivity_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Hyperactivity') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_allDat$Hyperactivity.categorization2 = ifelse(UAE_allDat$Hyperactivity.categorization == 'VeryHigh', 'Elevated', 
                                                              ifelse(UAE_allDat$Hyperactivity.categorization == 'High', 'Elevated', 
                                                                     ifelse(UAE_allDat$Hyperactivity.categorization == 'SlightlyRaised', 'Elevated',
                                                                            ifelse(UAE_allDat$Hyperactivity.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_allDat$Hyperactivity.categorization2 = factor(UAE_allDat$Hyperactivity.categorization2, 
                                                           levels = c('NotElevated', 'Elevated'))

IOTF_pOWcutoff_Hyper_glm2_mod = glm(Hyperactivity.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_Hyper_glm2_sum = summary(IOTF_pOWcutoff_Hyper_glm2_mod)
IOTF_pOWcutoff_Hyper_glm2_odds = exp(coef(IOTF_pOWcutoff_Hyper_glm2_mod))
IOTF_pOWcutoff_Hyper_glm2_oddsCI = exp(confint(IOTF_pOWcutoff_Hyper_glm2_mod))
IOTF_pOWcutoff_Hyper_glm2_tab = cbind.data.frame(IOTF_pOWcutoff_Hyper_glm2_sum$coefficients, IOTF_pOWcutoff_Hyper_glm2_odds, IOTF_pOWcutoff_Hyper_glm2_oddsCI)
names(IOTF_pOWcutoff_Hyper_glm2_tab) = c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

#Peer Problems - elevated vs not
UAE_allDat$Peer.problems.categorization2 = ifelse(UAE_allDat$Peer.problems.categorization == 'VeryHigh', 'Elevated', 
                                                           ifelse(UAE_allDat$Peer.problems.categorization == 'High', 'Elevated', 
                                                                  ifelse(UAE_allDat$Peer.problems.categorization == 'SlightlyRaised', 'Elevated',
                                                                         ifelse(UAE_allDat$Peer.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_allDat$Peer.problems.categorization2 = factor(UAE_allDat$Peer.problems.categorization2, 
                                                           levels = c('NotElevated', 'Elevated'))

IOTF_pOWcutoff_PeerProbs_glm2_mod = glm(Peer.problems.categorization2 ~ SEScat + Mother_ed+ nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_PeerProbs_glm2_sum = summary(IOTF_pOWcutoff_PeerProbs_glm2_mod)
IOTF_pOWcutoff_PeerProbs_glm2_odds = exp(coef(IOTF_pOWcutoff_PeerProbs_glm2_mod))
IOTF_pOWcutoff_PeerProbs_glm2_oddsCI = exp(confint(IOTF_pOWcutoff_PeerProbs_glm2_mod))
IOTF_pOWcutoff_PeerProbs_glm2_tab = cbind.data.frame(IOTF_pOWcutoff_PeerProbs_glm2_sum$coefficients, c('','','','.','*','','','','*','*'), IOTF_pOWcutoff_PeerProbs_glm2_odds, IOTF_pOWcutoff_PeerProbs_glm2_oddsCI)
names(IOTF_pOWcutoff_PeerProbs_glm2_tab) = c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

IOTF_pOWcutoff_PeerProbs_glm2_emtrends = emtrends(IOTF_pOWcutoff_PeerProbs_glm2_mod, ~ IOTF_pOWcutoff_c100 | Gender , var = 'IOTF_pOWcutoff_c100', infer = c('TRUE', 'TRUE'))
IOTF_pOWcutoff_PeerProbs_glm2_emtrends_sum = summary(IOTF_pOWcutoff_PeerProbs_glm2_emtrends)

PeerProbs_dat = UAE_allDat[!is.na(UAE_allDat$Peer.problems.categorization2) & !is.na(UAE_allDat$SEScat) & !is.na(UAE_allDat$Mother_ed), 
                                    c('Gender', 'IOTF_pOWcutoff_c100')]
PeerProbs_dat$PredProb = predict(IOTF_pOWcutoff_PeerProbs_glm2_mod, type = 'response')
PeerProbs_dat$PredLogit = predict(IOTF_pOWcutoff_PeerProbs_glm2_mod, type = 'link')

IOTF_pOWcutoff_PeerProbs.Sex_ORplot = ggplot(PeerProbs_dat, aes(y = exp(PredLogit), x = IOTF_pOWcutoff_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  geom_point(aes(y = exp(PredLogit), shape = Gender), size = 3) +
  scale_y_continuous(name='Predicted Odds Ratio') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#Prosocial - elevated vs not
UAE_allDat$Prosocial.categorization2 = ifelse(UAE_allDat$Prosocial.categorization == 'VeryLow', 'Lowered', 
                                                           ifelse(UAE_allDat$Prosocial.categorization == 'Low', 'Lowered', 
                                                                  ifelse(UAE_allDat$Prosocial.categorization == 'SlightlyLowered', 'Lowered',
                                                                         ifelse(UAE_allDat$Prosocial.categorization == 'CloseToAverage', 'NotLowered', 'Error'))))

UAE_allDat$Prosocial.categorization2 = factor(UAE_allDat$Prosocial.categorization2, 
                                                           levels = c('NotLowered', 'Lowered'))

IOTF_pOWcutoff_Prosocial_glm2_mod = glm(Prosocial.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_Prosocial_glm2_sum = summary(IOTF_pOWcutoff_Prosocial_glm2_mod)
IOTF_pOWcutoff_Prosocial_glm2_odds = exp(coef(IOTF_pOWcutoff_Prosocial_glm2_mod))
IOTF_pOWcutoff_Prosocial_glm2_oddsCI = exp(confint(IOTF_pOWcutoff_Prosocial_glm2_mod))
IOTF_pOWcutoff_Prosocial_glm2_tab = cbind.data.frame(IOTF_pOWcutoff_Prosocial_glm2_sum$coefficients, c('','.','','','','','','','','.'), IOTF_pOWcutoff_Prosocial_glm2_odds, IOTF_pOWcutoff_Prosocial_glm2_oddsCI)
names(IOTF_pOWcutoff_Prosocial_glm2_tab) = c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

IOTF_pOWcutoff_Prosocial_glm2_emtrends = emtrends(IOTF_pOWcutoff_Prosocial_glm2_mod, ~ IOTF_pOWcutoff_c100 | Gender , var = 'IOTF_pOWcutoff_c100', infer = c('TRUE', 'TRUE'))
IOTF_pOWcutoff_Prosocial_glm2_emtrends_sum = summary(IOTF_pOWcutoff_Prosocial_glm2_emtrends)

Prosocial_dat = UAE_allDat[!is.na(UAE_allDat$Prosocial.categorization2) & !is.na(UAE_allDat$SEScat) & !is.na(UAE_allDat$Mother_ed), 
                                    c('Gender', 'IOTF_pOWcutoff_c100')]
Prosocial_dat$PredProb = predict(IOTF_pOWcutoff_Prosocial_glm2_mod, type = 'response')
Prosocial_dat$PredLogit = predict(IOTF_pOWcutoff_Prosocial_glm2_mod, type = 'link')

IOTF_pOWcutoff_Prosocial.Sex_ORplot = ggplot(Prosocial_dat, aes(y = exp(PredLogit), x = IOTF_pOWcutoff_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  geom_point(aes(y = exp(PredLogit), shape = Gender), size = 3) +
  scale_y_continuous(name='Predicted Odds Ratio') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#Total Problems - Elevated vs Not
IOTF_pOWcutoff_TotalProbs.Sex_plot = ggplot(UAE_allDat, aes(y = Emotional.Problems_Raw, x = IOTF_pOWcutoff_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Emotional Problems') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_allDat$Total.difficulty.Categorization2 = ifelse(UAE_allDat$Total.difficulty.Categorization == 'VeryHigh', 'Elevated', 
                                                                ifelse(UAE_allDat$Total.difficulty.Categorization == 'High', 'Elevated', 
                                                                       ifelse(UAE_allDat$Total.difficulty.Categorization == 'SlightlyRaised', 'Elevated',
                                                                              ifelse(UAE_allDat$Total.difficulty.Categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_allDat$Total.difficulty.Categorization2 = factor(UAE_allDat$Total.difficulty.Categorization2, 
                                                                levels = c('NotElevated', 'Elevated'))

IOTF_pOWcutoff_TotalProbs_glm2_mod = glm(Total.difficulty.Categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_pOWcutoff_c100, family = binomial(link = 'logit'), data = UAE_allDat)
IOTF_pOWcutoff_TotalProbs_glm2_sum = summary(IOTF_pOWcutoff_TotalProbs_glm2_mod)
IOTF_pOWcutoff_TotalProbs_glm2_odds = exp(coef(IOTF_pOWcutoff_TotalProbs_glm2_mod))
IOTF_pOWcutoff_TotalProbs_glm2_oddsCI = exp(confint(IOTF_pOWcutoff_TotalProbs_glm2_mod))
IOTF_pOWcutoff_TotalProbs_glm2_tab = cbind.data.frame(IOTF_pOWcutoff_TotalProbs_glm2_sum$coefficients, IOTF_pOWcutoff_TotalProbs_glm2_odds, IOTF_pOWcutoff_TotalProbs_glm2_oddsCI)
names(IOTF_pOWcutoff_TotalProbs_glm2_tab) = c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')


#####################################
####
####   CHSQ -  Models  ####
####
#####################################

##Scatter Plots
###Bedtime Resistance
IOTF_pOWcutoff_BedRes_plot = ggplot(UAE_allDat, aes(y = BedtimeRes, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Bedtime Resistance') +
  scale_y_continuous(name='Bedtime Resistance') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Delay
IOTF_pOWcutoff_SleepDelay_plot = ggplot(UAE_allDat, aes(y = SleepDelay, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Delay') +
  scale_y_continuous(name='Sleep Delay') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Duration
IOTF_pOWcutoff_SleepDur_plot = ggplot(UAE_allDat, aes(y = SleepDuration, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Duration') +
  scale_y_continuous(name='Sleep Duration Concerns') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Anxiety
IOTF_pOWcutoff_SleepAnxiety_plot = ggplot(UAE_allDat, aes(y = SleepAnxiety, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Anxiety') +
  scale_y_continuous(name='Sleep Anxiety') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Night Wake
IOTF_pOWcutoff_NightWake_plot = ggplot(UAE_allDat, aes(y = NightWake, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Night Waking') +
  scale_y_continuous(name='Sleep Anxiety') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Parasomnias
IOTF_pOWcutoff_Parasom_plot = ggplot(UAE_allDat, aes(y = Parasomnias, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Parasomnias') +
  scale_y_continuous(name='Parasomnias') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Disordered Breathing
IOTF_pOWcutoff_SleepDisBreathing_plot = ggplot(UAE_allDat, aes(y = SleepDisBreathing, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Disordered Breathing') +
  scale_y_continuous(name='Sleep Disordered Breathing') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Daytime Sleepiness
IOTF_pOWcutoff_DaySleepy_plot = ggplot(UAE_allDat, aes(y = DaytimeSleepy, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Daytime Sleepiness') +
  scale_y_continuous(name='Daytime Sleepiness') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Total Score
IOTF_pOWcutoff_Totalgood_plot = ggplot(UAE_allDat[UAE_allDat$GoodTotalScore_missing16orless == 'Y', ], aes(y = TotalScore, x = IOTF_pOWcutoff_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Daytime Sleepiness') +
  scale_y_continuous(name='Total Score') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#####################################
####                            
####         NBack           ####
####                            
#####################################

#sex
Nback_sex_tab = xtabs(~Gender, data = UAE_cogBMI_NBACKDat.merge)
107-nrow(UAE_cogBMI_NBACKDat.merge)

#dataframe for functions
LoadxSex = data.frame(UAE_cogBMI_NBACKDat.merge_long$Load, UAE_cogBMI_NBACKDat.merge_long$Gender)
LoadxSex_IQge70 = data.frame(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Load, 
                             UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Gender)

##Ballance Acc
Nback_BalAcc1_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B1_BalAcc)*100, na.rm = TRUE)
Nback_BalAcc2_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B2_BalAcc)*100, na.rm = TRUE)

Nback_BalAcc1_sex_t = t.test(B1_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge)
Nback_BalAcc2_sex_t = t.test(B2_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge)

Nback_BalAcc_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$BalAcc)*100, LoadxSex)
Nback_BalAcc_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$BalAcc)*100, LoadxSex)
Nback_BalAcc_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$BalAcc)*100, LoadxSex)

#IQ >= 70
Nback_BalAcc1_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B1_BalAcc)*100, na.rm = TRUE)
Nback_BalAcc2_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B2_BalAcc)*100, na.rm = TRUE)

Nback_BalAcc1_IQge70_sex_t = t.test(B1_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])
Nback_BalAcc2_IQge70_sex_t = t.test(B2_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])

Nback_BalAcc_IQge70_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc)*100, LoadxSex_IQge70)
Nback_BalAcc_IQge70_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc)*100, LoadxSex_IQge70)
Nback_BalAcc_IQge70_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc)*100, LoadxSex_IQge70)

##d'
library(psycho)
UAE_cogBMI_NBACKDat.merge$B1_nCorReject = 45 - UAE_cogBMI_NBACKDat.merge$B1_nFA
UAE_cogBMI_NBACKDat.merge$B2_nCorReject = 45 - UAE_cogBMI_NBACKDat.merge$B2_nFA
UAE_cogBMI_NBACKDat.merge_long$nCorReject = 45 - UAE_cogBMI_NBACKDat.merge_long$nFA

B1_dprime_mat = dprime(n_hit = UAE_cogBMI_NBACKDat.merge$B1_nCor, 
                   n_miss = UAE_cogBMI_NBACKDat.merge$B1_nMiss, 
                   n_fa = UAE_cogBMI_NBACKDat.merge$B1_nFA, 
                   n_cr = UAE_cogBMI_NBACKDat.merge$B1_nCorReject)
B2_dprime_mat = dprime(n_hit = UAE_cogBMI_NBACKDat.merge$B2_nCor, 
                   n_miss = UAE_cogBMI_NBACKDat.merge$B2_nMiss, 
                   n_fa = UAE_cogBMI_NBACKDat.merge$B2_nFA, 
                   n_cr = UAE_cogBMI_NBACKDat.merge$B2_nCorReject)

UAE_cogBMI_NBACKDat.merge$B1_dprime = B1_dprime_mat$dprime
UAE_cogBMI_NBACKDat.merge$B2_dprime = B2_dprime_mat$dprime

dprime_long = dprime(n_hit = UAE_cogBMI_NBACKDat.merge_long$nCor, 
                   n_miss = UAE_cogBMI_NBACKDat.merge_long$nMiss, 
                   n_fa = UAE_cogBMI_NBACKDat.merge_long$nFA, 
                   n_cr = UAE_cogBMI_NBACKDat.merge_long$nCorReject)

UAE_cogBMI_NBACKDat.merge_long$dprime = dprime_long$dprime

Nback_dPrime_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, UAE_cogBMI_NBACKDat.merge_long$Load)
Nback_dPrime_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, UAE_cogBMI_NBACKDat.merge_long$Load)

Nback_dPrime1_sex_t = t.test(B1_dprime~Gender, data = UAE_cogBMI_NBACKDat.merge)
Nback_dPrime2_sex_t = t.test(B2_dprime~Gender, data = UAE_cogBMI_NBACKDat.merge)

Nback_dPrime_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, LoadxSex)
Nback_dPrime_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, LoadxSex)
Nback_dPrime_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, LoadxSex)

#IQ >= 70
Nback_dPrime1_IQge70_sum = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$dprime, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$Load)
Nback_dPrime2_IQge70_sum = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$dprime, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$Load)

Nback_dPrime1_IQge70_sex_t = t.test(B1_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])
Nback_dPrime2_IQge70_sex_t = t.test(B2_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])

Nback_dPrime_IQge70_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$dprime, LoadxSex_IQge70)
Nback_dPrime_IQge70_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$dprime, LoadxSex_IQge70)
Nback_dPrime_IQge70_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$dprime, LoadxSex_IQge70)

##RT
Nback_meanRT1_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B1_meanRTcor)*1000, na.rm = TRUE)
Nback_meanRT2_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B2_meanRTcor)*1000, na.rm = TRUE)

Nback_meanRT1_sex_t = t.test(B1_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge)
Nback_meanRT2_sex_t = t.test(B2_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge)

Nback_meanRT_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$meanRT)*1000, LoadxSex)
Nback_meanRT_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$meanRT)*1000, LoadxSex)
Nback_meanRT_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$meanRT)*1000, LoadxSex)

#IQ >= 70
Nback_meanRT1_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B1_meanRTcor)*1000, na.rm = TRUE)
Nback_meanRT2_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B2_meanRTcor)*1000, na.rm = TRUE)

Nback_meanRT1_IQge70_sex_t = t.test(B1_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])
Nback_meanRT2_IQge70_sex_t = t.test(B2_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])

Nback_meanRT_IQge70_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$meanRT)*1000, LoadxSex_IQge70)
Nback_meanRT_IQge70_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$meanRT)*1000, LoadxSex_IQge70)
Nback_meanRT_IQge70_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$meanRT)*1000, LoadxSex_IQge70)

##correlations with IOTF_pOWcutoff
UAE_cogBMI_NBACKDat.merge$Wasit2Hip = UAE_cogBMI_NBACKDat.merge$Waist_cm/UAE_cogBMI_NBACKDat.merge$Hips_cm
UAE_cogBMI_NBACKDat.merge$IOTF_pOWcutoff = UAE_cogBMI_NBACKDat.merge$BMI/UAE_cogBMI_NBACKDat.merge$IOTF_BMI25

UAE_cogBMI_NBACKDat.merge_long$Wasit2Hip = UAE_cogBMI_NBACKDat.merge_long$Waist_cm/UAE_cogBMI_NBACKDat.merge_long$Hips_cm
UAE_cogBMI_NBACKDat.merge_long$IOTF_pOWcutoff = UAE_cogBMI_NBACKDat.merge_long$BMI/UAE_cogBMI_NBACKDat.merge_long$IOTF_BMI25

IOTF_pOWcutoff_Nback_cor.varnames = c('BlockT', 'MatrixT',
                                 'IQ', 'B1acc', 'B2acc',
                                 'B1RT', 'B2RT', 'B1d', 'B2d',
                                 'Age', 'BMI25p', 'W2H')
IOTF_pOWcutoff_Nback_cor.vars = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, c('WASI_BlockT', 'WASI_MatrixT',
                                                                                        'WASI_PRI_IQ', 'B1_BalAcc', 'B2_BalAcc',
                                                                                        'B1_meanRTcor', 'B2_meanRTcor', 'B1_dprime', 'B1_dprime',
                                                                                        'Age_yr', 'IOTF_pOWcutoff', 'Wasit2Hip')]
IOTF_pOWcutoff_Nback_cormat = cor.matrix(IOTF_pOWcutoff_Nback_cor.vars, IOTF_pOWcutoff_Nback_cor.varnames)

#####################################
####
####   Nback Performance - Models  ####
####
#####################################
#merge
UAE_cogBMI_NBACKDat.merge_long = merge(UAE_cogBMI_NBACKDat.merge_long, UAE_allDat[c(1, 105:118, 141:142)], id ='ParID', all.x = TRUE, all.y=FALSE)
UAE_cogBMI_NBACKDat.merge = merge(UAE_cogBMI_NBACKDat.merge, UAE_allDat[c(1, 105:118, 141:142)], id ='ParID', all.x = TRUE, all.y=FALSE)

##Scatter Plots
###Ballanced Accuracy
IOTF_pOWcutoff_BalAcc_plot = ggplot(UAE_cogBMI_NBACKDat.merge_long, aes(y = BalAcc, x = IOTF_pOWcutoff_c100, Group = Load)) +
  geom_smooth(method = 'loess', formula = y~x, aes(linetype = Load)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Ballanced Accuracy') +
  scale_y_continuous(name='Ballanced Accuracy') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###d'
IOTF_pOWcutoff_dprime_plot = ggplot(UAE_cogBMI_NBACKDat.merge_long, aes(y = dprime, x = IOTF_pOWcutoff_c100, Group = Load)) +
  geom_smooth(method = 'loess', formula = y~x, aes(linetype = Load)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and dprime') +
  scale_y_continuous(name='dprime') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###RT
IOTF_pOWcutoff_meanRT_plot = ggplot(UAE_cogBMI_NBACKDat.merge_long, aes(y = meanRT*1000, x = IOTF_pOWcutoff_c100, Group = Load)) +
  geom_smooth(method = 'loess', formula = y~x, aes(linetype = Load)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and mean RT') +
  scale_y_continuous(name='meanRT') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Load x BMI25p

#BalAcc
IOTF_pOWcutoff_BalAcc.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_pOWcutoff_BalAcc.Load_sum = summary(IOTF_pOWcutoff_BalAcc.Load_mod)

IOTF_pOWcutoff_BalAcc.Load_IQge70_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_pOWcutoff_BalAcc.Load_IQge70_sum = summary(IOTF_pOWcutoff_BalAcc.Load_mod)

#dprime
IOTF_pOWcutoff_dprime.Load_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_pOWcutoff_dprime.Load_sum = summary(IOTF_pOWcutoff_dprime.Load_mod)

IOTF_pOWcutoff_dprime.Load_IQge70_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_pOWcutoff_dprime.Load_IQge70_sum = summary(IOTF_pOWcutoff_dprime.Load_IQge70_mod)

#meanRT
IOTF_pOWcutoff_meanRT.Load_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_pOWcutoff_meanRT.Load_sum = summary(IOTF_pOWcutoff_meanRT.Load_mod)

IOTF_pOWcutoff_meanRT.Load_IQge70_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_pOWcutoff_meanRT.Load_IQge70_sum = summary(IOTF_pOWcutoff_meanRT.Load_IQge70_mod)

#nFA
IOTF_pOWcutoff_nFA.Load_mod = lmer(nFA ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_pOWcutoff_nFA.Load_sum = summary(IOTF_pOWcutoff_nFA.Load_mod)

IOTF_pOWcutoff_nFA.Load_IQge70_mod = lmer(nFA ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_pOWcutoff_nFA.Load_IQge70_sum = summary(IOTF_pOWcutoff_nFA.Load_IQge70_mod)

#pMiss
IOTF_pOWcutoff_pMiss.Load_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_pOWcutoff_pMiss.Load_sum = summary(IOTF_pOWcutoff_pMiss.Load_mod)

IOTF_pOWcutoff_pMiss.Load_IQge70_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_pOWcutoff_pMiss.Load_IQge70_sum = summary(IOTF_pOWcutoff_pMiss.Load_IQge70_mod)

##Load x Glucose Impairment
UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN = factor(UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)

#BalAcc
IOTF_BalAcc_Glucose.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BalAcc_Glucose.Load_sum = summary(IOTF_BalAcc_Glucose.Load_mod)
IOTF_BalAcc_Glucose.Load_emmeans = emmeans(IOTF_BalAcc_Glucose.Load_mod, pairwise ~ ImpariedGlucose_YN | Load, var ='ImpariedGlucose_YN')

IOTF_Glucose_BalAcc.Load_IQge70_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_Glucose_BalAcc.Load_IQge70_sum = summary(IOTF_Glucose_BalAcc.Load_IQge70_mod)

Glucose.Load = data.frame(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$ImpariedGlucose_YN, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Load)
Glucose_BalAcc.Load_means = means.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)
Glucose_BalAcc.Load_se = se.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)

IOTF_BalAcc_Glucose.Load_plot = bar_graph.se(Glucose_BalAcc.Load_means, Glucose_BalAcc.Load_se, 'Load', 'BalAcc', 1, 0.5, UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)

#dprime
IOTF_dprime_Glucose.Load_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_dprime_Glucose.Load_sum = summary(IOTF_dprime_Glucose.Load_mod)

IOTF_dprime_Glucose.Load_IQge70_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_dprime_Glucose.Load_IQge70_sum = summary(IOTF_dprime_Glucose.Load_IQge70_mod)

#meanRT
IOTF_meanRT_Glucose.Load_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_meanRT_Glucose.Load_sum = summary(IOTF_meanRT_Glucose.Load_mod)

IOTF_meanRT_Glucose.Load_IQge70_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_meanRT_Glucose.Load_IQge70_sum = summary(IOTF_meanRT_Glucose.Load_IQge70_mod)

##Load x Glucose Impairment x BMI25p
UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN = factor(UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)

#BalAcc
IOTF_BalAcc_Glucose.Load.BMIp25_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BalAcc_Glucose.Load.BMIp25_sum = summary(IOTF_BalAcc_Glucose.Load.BMIp25_mod)
IOTF_BalAcc_Glucose.Load.BMIp25_emmeans = emmeans(IOTF_BalAcc_Glucose.Load.BMIp25_mod, pairwise ~ ImpariedGlucose_YN | Load, var ='ImpariedGlucose_YN')

IOTF_Glucose_BalAcc.Load_IQge70_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_Glucose_BalAcc.Load_IQge70_sum = summary(IOTF_Glucose_BalAcc.Load_IQge70_mod)

Glucose.Load = data.frame(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$ImpariedGlucose_YN, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Load)
Glucose_BalAcc.Load_means = means.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)
Glucose_BalAcc.Load_se = se.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)

IOTF_BalAcc_Glucose.Load_plot = bar_graph.se(Glucose_BalAcc.Load_means, Glucose_BalAcc.Load_se, 'Load', 'BalAcc', 1, 0.5, UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)


# 
# #pMiss
# IOTF_pOWcutoff_pMiss.Load_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_pOWcutoff_pMiss.Load_sum = summary(IOTF_pOWcutoff_pMiss.Load_mod)
# 
# IOTF_Glucose_pMiss.Load_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_Glucose_pMiss.Load_sum = summary(IOTF_Glucose_pMiss.Load_mod)
# IOTF_Glucose_BalAcc.Load_emmeans = emmeans(IOTF_Glucose_pMiss.Load_mod, pairwise ~ ImpariedGlucose_YN | Load, var ='ImpariedGlucose_YN')
# 
# Glucose_BalAcc.Load_means = means.function(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$pMiss, Glucose.Load)
# Glucose_BalAcc.Load_se = se.function(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$pMiss, Glucose.Load)
# 
# bar_graph.se(Glucose_BalAcc.Load_means, Glucose_BalAcc.Load_se, 'Load', 'BalAcc', 0.7, 0, UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)
# 
# 
# IOTF_pOWcutoff_pMiss.Load_IQge70_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
# IOTF_pOWcutoff_pMiss.Load_IQge70_sum = summary(IOTF_pOWcutoff_pMiss.Load_IQge70_mod)
# 
# #other
# IOTF_Anemia_BalAcc.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + Anemia_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_Anemia_BalAcc.Load_sum = summary(IOTF_Anemia_BalAcc.Load_mod)
# 
# IOTF_Thyroid_BalAcc.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_pOWcutoff_c100 + Thyriod_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_Thyroid_BalAcc.Load_sum = summary(IOTF_Thyroid_BalAcc.Load_mod)


# 
# 
# #merge with demo data
# UAE_allDat$ParID = UAE_allDat$Participant.Number
# NBack_goodDat_long = merge(NBack_goodDat_long, UAE_allDat[c(3, 5, 10:12, 19, 99, 100)], by = "ParID", all.x = TRUE)
# 
# xtabs(~CDC_WeightStatus + Load, data = NBack_goodDat_long[NBack_goodDat_long$Load1 == "B1", ])
# 
# NBack_goodDat_noUW_long = NBack_goodDat_long[NBack_goodDat_long$CDC_WeightStatus != "Underweight", ]
# 
# #### Ballance Accuracy x OB status
# Nback_BalAcc_Load.WeightStatus_noUW_IQmod = lmer(BalAcc ~ WASI_PRI_IQ + CDC_WeightStatus*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.WeightStatus_noUW_IQanova = anova(Nback_BalAcc_Load.WeightStatus_noUW_IQmod)
# 
# Nback_BalAcc_Load.WeightStatus_noUW_mod = lmer(BalAcc ~ CDC_WeightStatus*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.WeightStatus_noUW_anova = anova(Nback_BalAcc_Load.WeightStatus_noUW_mod)
# 
# Nback_BalAcc_Load.WeightStatus_mod = lmer(BalAcc ~ CDC_WeightStatus*Load + (1|ParID), data = NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.WeightStatus_anova = anova(Nback_BalAcc_Load.WeightStatus_mod)
# 
# NBack_goodDat_noUW_long$CDC_WeightStatus = factor(NBack_goodDat_noUW_long$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# Nback_Load.WeightStatus_datframe = data.frame(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$CDC_WeightStatus, NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$Load)
# Nback_BalAcc_Load.WeightStatus_noUW_means = means.function(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ], NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$BalAcc, Nback_Load.WeightStatus_datframe)
# Nback_BalAcc_Load.WeightStatus_noUW_se = se.function(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ], NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$BalAcc, Nback_Load.WeightStatus_datframe)
# bar_graph.se(Nback_BalAcc_Load.WeightStatus_noUW_means, Nback_BalAcc_Load.WeightStatus_noUW_se, "Load", "Accuracy", 1, 0.5, NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$CDC_WeightStatus)
# 
# Nback_Load.WeightStatus_datframe = data.frame(NBack_goodDat_noUW_long$CDC_WeightStatus, NBack_goodDat_noUW_long$Load)
# Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_means = means.function(NBack_goodDat_noUW_long, NBack_goodDat_noUW_long$BalAcc, Nback_Load.WeightStatus_datframe)
# Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_se = se.function(NBack_goodDat_noUW_long, NBack_goodDat_noUW_long$BalAcc, Nback_Load.WeightStatus_datframe)
# bar_graph.se(Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_means, Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_se, "Load", "Accuracy", 1, 0.5, NBack_goodDat_noUW_long$CDC_WeightStatus)
# 
# 
# Nback_BalAcc_Load.OBstatus_noUW_mod = lmer(BalAcc ~ CDC_ObesityStatus*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.OBstatus_noUW_anova = anova(Nback_BalAcc_Load.OBstatus_noUW_mod)
# 
# Nback_BalAcc_Load.BMIpercentile_mod = lmer(BalAcc ~ CDC_BMIptile*Load + (1|ParID), data = NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.BMIpercentile_anova = anova(Nback_BalAcc_Load.BMIpercentile_mod)
# Nback_BalAcc_Load.BMIpercentile_sum = summary(lmer(BalAcc ~ CDC_BMIptile +Load + (1|ParID), data = NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ])
# )
# ggplot(NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ], aes(x=CDC_BMIptile, y=BalAcc)) + 
#   geom_point(aes(color = CDC_WeightStatus))+
#   geom_smooth(method=lm)
# 
# Nback_BalAcc_Load.BMIpercentile_noUW_mod = lmer(BalAcc ~ CDC_BMIptile*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.BMIpercentile_noUW_anova = anova(Nback_BalAcc_Load.BMIpercentile_mod)
# Nback_BalAcc_Load.BMIpercentile_noUW_sum = summary(lmer(BalAcc ~ CDC_BMIptile + Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# )
# 
# ggplot(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ], aes(x=CDC_BMIptile, y=BalAcc)) + 
#   geom_point(aes(color = CDC_WeightStatus))+
#   geom_smooth(method=lm)
# 
# #####################################
# ####                            
# ####              SST           ####
# ####                            
# #####################################
# 
# ############## Across Blocks #######################
# UAE_cogBMI_SSTDat_GoodDat = UAE_cogBMI_SSTDat[UAE_cogBMI_SSTDat$Practice == 'Y', ]
# UAE_cogBMI_SSTDat_GoodDat$BothBlocks = ifelse(!is.na(UAE_cogBMI_SSTDat_GoodDat$HighB1_percStop), ifelse(
#   !is.na(UAE_cogBMI_SSTDat_GoodDat$HighB2_percStop), 'Y', 'N'), ifelse(
#     !is.na(UAE_cogBMI_SSTDat_GoodDat$LowB1_percStop), ifelse(
#       !is.na(UAE_cogBMI_SSTDat_GoodDat$LowB2_percStop), 'Y', 'N'), 'N'))
# UAE_cogBMI_SSTDat_GoodDat$BothLoads = ifelse(!is.na(UAE_cogBMI_SSTDat_GoodDat$High_percStop), ifelse(
#   !is.na(UAE_cogBMI_SSTDat_GoodDat$Low_percStop), 'Y', 'N'), 'N')
# 
# UAE_cogBMI_SSTDat_GoodDat = merge(UAE_cogBMI_SSTDat_GoodDat, UAE_allDat[c(1, 3, 5, 10:12, 19, 99)], by.x = "ParID", by.y = "Participant.Number", all.x = TRUE)
# UAE_cogBMI_SSTDat_GoodDat_noUW = UAE_cogBMI_SSTDat_GoodDat[UAE_cogBMI_SSTDat_GoodDat$CDC_WeightStatus != "Underweight", ]
# UAE_cogBMI_SSTDat_GoodDat_noUW$CDC_WeightStatus = factor(UAE_cogBMI_SSTDat_GoodDat_noUW$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# 
# 
# UAE_cogBMI_SSTDat_AvgBlocks_long = melt(UAE_cogBMI_SSTDat_GoodDat[c(1:4, 77:85, 11, 23)], id.vars = names(UAE_cogBMI_SSTDat_GoodDat)[c(1:4, 77:85)])
# UAE_cogBMI_SSTDat_AvgBlocks_long$ssd = UAE_cogBMI_SSTDat_AvgBlocks_long$value
# UAE_cogBMI_SSTDat_AvgBlocks_long$Load = ifelse(UAE_cogBMI_SSTDat_AvgBlocks_long$variable == 'High_ssd', 'High', ifelse(
#   UAE_cogBMI_SSTDat_AvgBlocks_long$variable == 'Low_ssd', 'Low', NA))
#   
# UAE_cogBMI_SSTDat_AvgBlocks_long$Load = factor(UAE_cogBMI_SSTDat_AvgBlocks_long$Load)
# 
# UAE_cogBMI_SSTDat_SSRTmean = melt(UAE_cogBMI_SSTDat_GoodDat[c(12, 24)])
# UAE_cogBMI_SSTDat_SSRTint = melt(UAE_cogBMI_SSTDat_GoodDat[c(13, 25)])
# 
# 
# UAE_cogBMI_SSTDat_AvgBlocks_long = data.frame(UAE_cogBMI_SSTDat_AvgBlocks_long[c(1:13, 16:17)], UAE_cogBMI_SSTDat_SSRTmean[2], UAE_cogBMI_SSTDat_SSRTmean[2])
# names(UAE_cogBMI_SSTDat_AvgBlocks_long) = c(names(UAE_cogBMI_SSTDat_AvgBlocks_long)[1:15], 'SSRTmean', 'SSRTint')
# 
# UAE_cogBMI_SSTDat_AvgBlocks_long = UAE_cogBMI_SSTDat_AvgBlocks_long[!is.na(UAE_cogBMI_SSTDat_AvgBlocks_long$ssd), ]
# UAE_cogBMI_SSTDat_AvgBlocks_noUW_long = UAE_cogBMI_SSTDat_AvgBlocks_long[UAE_cogBMI_SSTDat_AvgBlocks_long$CDC_WeightStatus != "Underweight", ]
# UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$CDC_WeightStatus = factor(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# 
# xtabs(~CDC_WeightStatus + BothBlocks + Load, data = UAE_cogBMI_SSTDat_AvgBlocks_long)
# 
# SST_SSRT_Load.OBOWstatus_mod = lmer(SSRTint ~ CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_SSRT_Load.OBOWstatus_anova = anova(SST_SSRT_Load.OBOWstatus_mod)
# 
# SST_ssd_Load.OBOWstatus_mod = lmer(ssd ~ CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_ssd_Load.OBOWstatus_anova = anova(SST_ssd_Load.OBOWstatus_mod)
# 
# SST_SSRT_Load.Weightstatus_mod = lmer(SSRTint ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_SSRT_Load.Weightstatus_anova = anova(SST_SSRT_Load.Weightstatus_mod)
# 
# SST_ssd_Load.Weightstatus_mod = lmer(ssd ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_ssd_Load.Weightstatus_anova = anova(SST_ssd_Load.Weightstatus_mod)
# 
# SSD_Load.WeightStatus_dataframe = data.frame(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$CDC_WeightStatus, UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$Load)
# SSD_Load.WeightStatus_means = means.function(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ], UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$ssd, SSD_Load.WeightStatus_dataframe)
# SSD_Load.WeightStatus_se = se.function(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ], UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$ssd, SSD_Load.WeightStatus_dataframe)
# bar_graph.se(SSD_Load.WeightStatus_means, SSD_Load.WeightStatus_se, "Load", "SSD",900, 0, UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus)
# 
# SST_SSD_Load.BMIptile_mod = lmer(ssd ~ CDC_BMIptile*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_SSD_Load.BMIptile_anova = anova(SST_SSD_Load.BMIptile_mod)
# 
# ############## Just Block 2 or Block 1 #######################
# UAE_cogBMI_SSTDat_long_prac = UAE_cogBMI_SSTDat_long[UAE_cogBMI_SSTDat_long$Practice == 'Y', ]
# UAE_cogBMI_SSTDat_long_prac = merge(UAE_cogBMI_SSTDat_long_prac, UAE_allDat[c(1, 3, 5, 10:12, 19, 99)], by.x = "ParID", by.y = "Participant.Number", all.x = TRUE)
# UAE_cogBMI_SSTDat_long_prac_noUW = UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$CDC_WeightStatus != "Underweight", ]
# UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus = factor(UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# 
# xtabs(~Load + CDC_WeightStatus, data = UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ])
# 
# SST_SSRT_Load.OBOWstatus.Block_mod = lmer(ssrtInt_cor ~ WASI_PRI_IQ + CDC_ObesityStatus*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# SST_SSRT_Load.OBOWstatus.Block_anova = anova(SST_SSRT_Load.OBOWstatus.Block_mod)
# 
# 
# SSRT_Load.OBOWstatus.Block_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac$Load, UAE_cogBMI_SSTDat_long_prac$CDC_ObesityStatus)
# SSRT_Load.OBOWstatus.Block_means = means.function(UAE_cogBMI_SSTDat_long_prac, UAE_cogBMI_SSTDat_long_prac$ssrtInt_cor, SSRT_Load.OBOWstatus.Block_dataframe)
# SSRT_Load.OBOWstatus.Block_se = se.function(UAE_cogBMI_SSTDat_long_prac, UAE_cogBMI_SSTDat_long_prac$ssrtInt_cor, SSRT_Load.OBOWstatus.Block_dataframe)
# bar_graph.se(SSRT_Load.OBOWstatus.Block_means, SSRT_Load.OBOWstatus.Block_se, "SSRT", "Group",500, 0, UAE_cogBMI_SSTDat_long_prac$Load)
# 
# SST_SSD_Load.OBOWstatus.Block_mod = lmer(ssd ~ WASI_PRI_IQ + CDC_ObesityStatus*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# SST_SSD_Load.WeightStatus_mod = lmer(ssd ~ WASI_PRI_IQ + CDC_WeightStatus*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# SST_SSD_Load.BMIptile.Block_mod = lmer(ssd ~ CDC_BMIptile*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# 
# ## Block 1
# SST_SSRT_Load.WeightStatus_mod = lmer(ssrtInt_cor ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSRT_Load.WeightStatus_anova = anova(SST_SSRT_Load.WeightStatus_mod)
# 
# SST_SSD_Load.OBOWstatusB1_mod = lmer(ssd ~ Load_Order + CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSD_Load.OBOWstatusB1_anova = anova(SST_SSD_Load.OBOWstatusB1_mod)
# 
# SSD_Load.OBOWstatusB1_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$CDC_ObesityStatus, UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$Load)
# SSD_Load.OBOWstatusB1_means = means.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$ssd, SSD_Load.OBOWstatusB1_dataframe)
# SSD_Load.OBOWstatusB1_se = se.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$ssd, SSD_Load.OBOWstatusB1_dataframe)
# bar_graph.se(SSD_Load.OBOWstatusB1_means, SSD_Load.OBOWstatusB1_se, "Weight Status", "Load",900, 0, UAE_cogBMI_SSTDat_long_prac$CDC_ObesityStatus)
# 
# SST_SSD_Load.WeightStatusB1_mod = lmer(ssd ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSD_Load.WeightStatusB1_anova = anova(SST_SSD_Load.WeightStatusB1_mod)
# 
# SSD_Load.WeightStatusB1_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$CDC_WeightStatus, UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$Load)
# SSD_Load.WeightStatusB1_means = means.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$ssd, SSD_Load.WeightStatusB1_dataframe)
# SSD_Load.WeightStatusB1_se = se.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$ssd, SSD_Load.WeightStatusB1_dataframe)
# bar_graph.se(SSD_Load.WeightStatusB1_means, SSD_Load.WeightStatusB1_se, "Load", "SSD",900, 0, UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus)
# 
# SST_SSD_Load.BMIptileB1_mod = lmer(ssd ~ CDC_BMIptile*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSD_Load.BMIptileB1_anova = anova(SST_SSD_Load.BMIptileB1_mod)
# 
# ## Block 2
# SST_SSRT_Load.WeightStatus_mod = lmer(ssrtInt_cor ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSRT_Load.WeightStatus_anova = anova(SST_SSRT_Load.WeightStatus_mod)
# 
# SST_SSD_Load.OBOWstatusB2_mod = lmer(ssd ~ Load_Order + CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSD_Load.OBOWstatusB2_anova = anova(SST_SSD_Load.OBOWstatusB2_mod)
# 
# SSD_Load.OBOWstatusB2_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$CDC_ObesityStatus, UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$Load)
# SSD_Load.OBOWstatusB2_means = means.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$ssd, SSD_Load.OBOWstatusB2_dataframe)
# SSD_Load.OBOWstatusB2_se = se.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$ssd, SSD_Load.OBOWstatusB2_dataframe)
# bar_graph.se(SSD_Load.OBOWstatusB2_means, SSD_Load.OBOWstatusB2_se, "Weight Status", "Load",900, 0, UAE_cogBMI_SSTDat_long_prac$CDC_ObesityStatus)
# 
# SST_SSD_Load.WeightStatusB2_mod = lmer(ssd ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSD_Load.WeightStatusB2_anova = anova(SST_SSD_Load.WeightStatusB2_mod)
# 
# SSD_Load.WeightStatusB2_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$CDC_WeightStatus, UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$Load)
# SSD_Load.WeightStatusB2_means = means.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$ssd, SSD_Load.WeightStatusB2_dataframe)
# SSD_Load.WeightStatusB2_se = se.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$ssd, SSD_Load.WeightStatusB2_dataframe)
# bar_graph.se(SSD_Load.WeightStatusB2_means, SSD_Load.WeightStatusB2_se, "Load", "SSD",900, 0, UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus)
# 
# SST_SSD_Load.BMIptileB2_mod = lmer(ssd ~ CDC_BMIptile*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSD_Load.BMIptileB2_anova = anova(SST_SSD_Load.BMIptileB2_mod)
