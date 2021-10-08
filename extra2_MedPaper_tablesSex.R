# This script was written by Alaina Pearce in 2021
# to set up tables by sex for the paper examining
# medical, family history, and behavior in Emerati children
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

## load basic functions and custom gtsummmary table functions - uncomment if running separately
# source('functions.R')

## load data/clean - uncomment if running separately
# source('1_MedBeh_DataOrg.R')



####   Demo Data  Table    ####

sum_tab_sex <- UAE_allDat[c(5, 7, 12, 105, 17, 42:43, 64, 2:3)]
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

####    Medial Data       ####

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

####   Family History       ####

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

####   Behavioral/Psychological       ####

# media use and sleep ####

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

sleep_tab_sex <- UAE_allDat[c(5, 127:130, 93:97, 99:102)]
UAE_sleep_sex <-
  tbl_summary(
    data=sleep_tab_sex,
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

# SDQ ####

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