# This script was written by Alaina Pearce in 2021
# to set up tables by weight status for the paper examining
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

## load libraries - uncomment if running separately
# library(stats)
# library(gtsummary)
# theme_gtsummary_compact()

## load basic functions and custom gtsummmary table functions - uncomment if running separately
# source('functions.R')

## load data/clean - uncomment if running separately
# source('1_MedBeh_DataOrg.R')


####   Demo Data  Table     ####

sum_tab_OB <- UAE_allDat[c(107, 5, 7, 12, 105, 42:43, 64, 2:3)]
UAE_demo_OB <-
  tbl_summary(
    data=sum_tab_OB,
    by = IOTF_3class, 
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

UAE_demo_OB_stat <-
  tbl_summary(
    data=sum_tab_OB,
    by = IOTF_3class, 
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
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


                 
####    Medial Data       ####

med_tab_OB <- UAE_allDat[c(107, 108, 65:69, 71:73, 75:76)]
UAE_med_OB <-
  tbl_summary(
    data=med_tab_OB,
    by = IOTF_3class, 
    value = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    label = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    type = list(nComorbid ~ "continuous"), 
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

UAE_med_OB_stat <-
  tbl_summary(
    data=med_tab_OB,
    by = IOTF_3class, 
    value = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    label = list(AcanthosisNigricans ~ "Acanthosis Nigricans", MetabolicSyndrome ~ "Metabolic Syndrome"),
    type = list(nComorbid ~ "continuous"), 
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
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

                           
####   Family History       ####

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
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

nfamOB_tab <- UAE_allDat[c(109:116)]
UAE_nFamOB <-
  tbl_summary(
    data=nfamOB_tab,
    value = list(Mother_OBhistory ~ "Mother", Father_OBhistory ~ "Father",
                 Sister_OBhistory ~ "Sister", Brother_OBhistory ~ "Brother",
                 Aunt_OBhistory ~ "Aunt", Uncle_OBhistory ~ "Uncle",
                 Grandmother_OBhistory ~ "Grandmother", Grandfather_OBhistory ~ "Grandfather"),
    label = list(Mother_OBhistory ~ "Mother", Father_OBhistory ~ "Father",
                 Sister_OBhistory ~ "Sister", Brother_OBhistory ~ "Brother",
                 Aunt_OBhistory ~ "Aunt", Uncle_OBhistory ~ "Uncle",
                 Grandmother_OBhistory ~ "Grandmother", Grandfather_OBhistory ~ "Grandfather"),
    missing = "ifany") %>%
  add_n()

nfamED_tab <- UAE_allDat[c(118:125)]
UAE_nFamED <-
  tbl_summary(
    data=nfamED_tab,
    value = list(Mother_EDhistory ~ "Mother", Father_EDhistory ~ "Father",
                 Sister_EDhistory ~ "Sister", Brother_EDhistory ~ "Brother",
                 Aunt_EDhistory ~ "Aunt", Uncle_EDhistory ~ "Uncle",
                 Grandmother_EDhistory ~ "Grandmother", Grandfather_EDhistory ~ "Grandfather"),
    label = list(Mother_EDhistory ~ "Mother", Father_EDhistory ~ "Father",
                 Sister_EDhistory ~ "Sister", Brother_EDhistory ~ "Brother",
                 Aunt_EDhistory ~ "Aunt", Uncle_EDhistory ~ "Uncle",
                 Grandmother_EDhistory ~ "Grandmother", Grandfather_EDhistory ~ "Grandfather"),
    missing = "ifany") %>%
  add_n()

UAE_fam_OB_stat <-
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
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
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
                          
####   Behavioral/Psychological       ####
sleep_tab_OB <- UAE_allDat[c(107, 127:130, 93:97, 99:102)]
UAE_sleep_OB <-
  tbl_summary(
    data=sleep_tab_OB,
    by = IOTF_3class, 
    type = list(CSHQ_BedtimeResit ~ 'continuous', CSHQ_SleepOnsetDelay ~ 'continuous', 
                CSHQ_SleepDuration ~ 'continuous', CSHQ_SleepAnxiety ~ 'continuous', 
                CSHQ_NightWaking_no16 ~ 'continuous', CSHQ_Parasomnias ~ 'continuous', 
                CSHQ_SleepDisorderBreathing ~ 'continuous', CSHQ_DaytimeSleepiness ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

sleep_tab_OB_stat <- UAE_allDat[c(107, 127:130, 93:97, 99:102)]
UAE_sleep_OBstat <-
  tbl_summary(
    data=sleep_tab_OB,
    by = IOTF_3class, 
    type = list(CSHQ_BedtimeResit ~ 'continuous', CSHQ_SleepOnsetDelay ~ 'continuous', 
                CSHQ_SleepDuration ~ 'continuous', CSHQ_SleepAnxiety ~ 'continuous', 
                CSHQ_NightWaking_no16 ~ 'continuous', CSHQ_Parasomnias ~ 'continuous', 
                CSHQ_SleepDisorderBreathing ~ 'continuous', CSHQ_DaytimeSleepiness ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
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

beh_tab_OB <- UAE_allDat[c(107, 59, 61, 63, 128, 93:97, 99:102)]
UAE_beh_OB <-
  tbl_summary(
    data=beh_tab_OB,
    by = IOTF_3class, 
    type = list(CSHQ_BedtimeResit ~ 'continuous', CSHQ_SleepOnsetDelay ~ 'continuous', 
                CSHQ_SleepDuration ~ 'continuous', CSHQ_SleepAnxiety ~ 'continuous', 
                CSHQ_NightWaking_no16 ~ 'continuous', CSHQ_Parasomnias ~ 'continuous', 
                CSHQ_SleepDisorderBreathing ~ 'continuous', CSHQ_DaytimeSleepiness ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
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

# SDQ ####

sdq_tab_OB <- UAE_allDat[c(107, 79:90)]
UAE_sdq_OB <-
  tbl_summary(
    data=sdq_tab_OB,
    by = IOTF_3class, 
    type = list(SDQ_EmotionProb_raw ~ 'continuous', SDQ_ConductProb_raw ~ 'continuous',
                SDQ_HyperactiveProb_raw ~ 'continuous', SDQ_PeerProb_raw ~ 'continuous',
                SDQ_Prosocial_raw ~ 'continuous', SDQ_TotalProb_raw ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

sdq_tab_overall <- UAE_allDat[c(85:90)]
UAE_sdq_overall <-
  tbl_summary(
    data=sdq_tab_overall,
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

UAE_sdq_OB_stat <-
  tbl_summary(
    data=sdq_tab_OB,
    by = IOTF_3class, 
    type = list(SDQ_EmotionProb_raw ~ 'continuous', SDQ_ConductProb_raw ~ 'continuous',
                SDQ_HyperactiveProb_raw ~ 'continuous', SDQ_PeerProb_raw ~ 'continuous',
                SDQ_Prosocial_raw ~ 'continuous', SDQ_TotalProb_raw ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
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

