# This script was written by Alaina Pearce in 2022
# to set up tables by weight status for the paper examining
# cogntivie function in Emirati children
# by weight status
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

## load libraries - uncomment if running separately
# library(stats)
# library(gtsummary)
# theme_gtsummary_compact()

## load basic functions and custom gtsummmary table functions - uncomment if running separately
# source('functions.R')

## load data/clean - uncomment if running separately
# source('1_CogPaper_DataOrg.R')


####   Demo Data  Table     ####

sum_tab_OB <- UAE_allDat[c(70, 3, 5, 10, 68, 18:19, 20, 15:16)]
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

####   Sleep       ####
sleep_tab_OB <- UAE_allDat[c(70, 36:39, 41:44, 46)]
UAE_sleep_OB <-
  tbl_summary(
    data=sleep_tab_OB,
    by = IOTF_3class, 
    value = list(CSHQ_BedtimeResit ~ 'Bedtime Resistance', CSHQ_SleepOnsetDelay ~ 'Sleep Delay', CSHQ_SleepDuration ~ 'Sleep Duration', CSHQ_SleepAnxiety ~ 'Sleep Anxiety', CSHQ_NightWaking_no16 ~ 'Night Waking', CSHQ_Parasomnias ~ 'Parasomnias', CSHQ_SleepDisorderBreathing ~ 'Sleep Disordered Breather', CSHQ_DaytimeSleepiness ~ 'Daytime Sleepiness', CSHQ_Total_no16 ~ 'Total'),
    label = list(CSHQ_BedtimeResit ~ 'Bedtime Resistance', CSHQ_SleepOnsetDelay ~ 'Sleep Delay', CSHQ_SleepDuration ~ 'Sleep Duration', CSHQ_SleepAnxiety ~ 'Sleep Anxiety',  CSHQ_NightWaking_no16 ~ 'Night Waking', CSHQ_Parasomnias ~ 'Parasomnias', CSHQ_SleepDisorderBreathing ~ 'Sleep Disordered Breather', CSHQ_DaytimeSleepiness ~ 'Daytime Sleepiness',  CSHQ_Total_no16 ~ 'Total'),
    type = list(CSHQ_BedtimeResit ~ 'continuous', CSHQ_SleepOnsetDelay ~ 'continuous', CSHQ_SleepDuration ~ 'continuous', CSHQ_SleepAnxiety ~ 'continuous', CSHQ_NightWaking_no16 ~ 'continuous', CSHQ_Parasomnias ~ 'continuous', CSHQ_SleepDisorderBreathing ~ 'continuous', CSHQ_DaytimeSleepiness ~ 'continuous', CSHQ_Total_no16 ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

UAE_sleep_OBstat <-
  tbl_summary(
    data=sleep_tab_OB,
    by = IOTF_3class, 
    value = list(CSHQ_BedtimeResit ~ 'Bedtime Resistance', CSHQ_SleepOnsetDelay ~ 'Sleep Delay', CSHQ_SleepDuration ~ 'Sleep Duration', CSHQ_SleepAnxiety ~ 'Sleep Anxiety', CSHQ_NightWaking_no16 ~ 'Night Waking', CSHQ_Parasomnias ~ 'Parasomnias', CSHQ_SleepDisorderBreathing ~ 'Sleep Disordered Breathing', CSHQ_DaytimeSleepiness ~ 'Daytime Sleepiness', CSHQ_Total_no16 ~ 'Total'),
    label = list(CSHQ_BedtimeResit ~ 'Bedtime Resistance', CSHQ_SleepOnsetDelay ~ 'Sleep Delay', CSHQ_SleepDuration ~ 'Sleep Duration', CSHQ_SleepAnxiety ~ 'Sleep Anxiety',  CSHQ_NightWaking_no16 ~ 'Night Waking', CSHQ_Parasomnias ~ 'Parasomnias', CSHQ_SleepDisorderBreathing ~ 'Sleep Disordered Breathing', CSHQ_DaytimeSleepiness ~ 'Daytime Sleepiness',  CSHQ_Total_no16 ~ 'Total'),
    type = list(CSHQ_BedtimeResit ~ 'continuous', CSHQ_SleepOnsetDelay ~ 'continuous', CSHQ_SleepDuration ~ 'continuous', CSHQ_SleepAnxiety ~ 'continuous', CSHQ_NightWaking_no16 ~ 'continuous', CSHQ_Parasomnias ~ 'continuous', CSHQ_SleepDisorderBreathing ~ 'continuous', CSHQ_DaytimeSleepiness ~ 'continuous', CSHQ_Total_no16 ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n() %>%
  add_stat(fns = all_continuous() ~ my_anova) %>%
  modify_header(
    list(
      add_stat_1 ~ "**ANOVA**",
      all_stat_cols() ~ "**{level}**"
    )
  )

# SDQ ####

sdq_tab_OB <- UAE_allDat[c(70, 21:32)]
UAE_sdq_OB <-
  tbl_summary(
    data=sdq_tab_OB,
    by = IOTF_3class, 
    value = list(SDQ_EmotionProb_raw ~ 'Emotional Problems, raw', SDQ_ConductProb_raw ~ 'Contuct Problems, raw', SDQ_HyperactiveProb_raw ~ 'Hyperactivity, raw', SDQ_PeerProb_raw ~ 'Peer Problems, raw', SDQ_Prosocial_raw ~ 'Prosocial, raw', SDQ_TotalProb_raw ~ 'Total Problems, raw', SDQ_EmotionProb_cat ~ 'Emotional Problems', SDQ_ConductProb_cat ~ 'Contuct Problems', SDQ_HyperactivityProb_cat ~ 'Hyperactivity', SDQ_PeerProb_cat ~ 'Peer Problems', SDQ_Prosocial_cat ~ 'Prosocial', SDQ_TotalProb_cat ~ 'Total Problems'),
    label = list(SDQ_EmotionProb_raw ~ 'Emotional Problems, raw', SDQ_ConductProb_raw ~ 'Contuct Problems, raw', SDQ_HyperactiveProb_raw ~ 'Hyperactivity, raw', SDQ_PeerProb_raw ~ 'Peer Problems, raw', SDQ_Prosocial_raw ~ 'Prosocial, raw', SDQ_TotalProb_raw ~ 'Total Problems, raw', SDQ_EmotionProb_cat ~ 'Emotional Problems', SDQ_ConductProb_cat ~ 'Contuct Problems', SDQ_HyperactivityProb_cat ~ 'Hyperactivity', SDQ_PeerProb_cat ~ 'Peer Problems', SDQ_Prosocial_cat ~ 'Prosocial', SDQ_TotalProb_cat ~ 'Total Problems'),
    type = list(SDQ_EmotionProb_raw ~ 'continuous', SDQ_ConductProb_raw ~ 'continuous', SDQ_HyperactiveProb_raw ~ 'continuous', SDQ_PeerProb_raw ~ 'continuous', SDQ_Prosocial_raw ~ 'continuous', SDQ_TotalProb_raw ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

sdq_tab_overall <- UAE_allDat[c(27:32)]
UAE_sdq_overall <-
  tbl_summary(
    data=sdq_tab_overall,
    missing = "ifany",
    value = list(SDQ_EmotionProb_cat ~ 'Emotional Problems', SDQ_ConductProb_cat ~ 'Contuct Problems', SDQ_HyperactivityProb_cat ~ 'Hyperactivity', SDQ_PeerProb_cat ~ 'Peer Problems', SDQ_Prosocial_cat ~ 'Prosocial', SDQ_TotalProb_cat ~ 'Total Problems'),
    label = list(SDQ_EmotionProb_cat ~ 'Emotional Problems', SDQ_ConductProb_cat ~ 'Contuct Problems', SDQ_HyperactivityProb_cat ~ 'Hyperactivity', SDQ_PeerProb_cat ~ 'Peer Problems', SDQ_Prosocial_cat ~ 'Prosocial', SDQ_TotalProb_cat ~ 'Total Problems'),
    digits = all_continuous() ~ 2) %>%
  add_n()

UAE_sdq_OB_stat <-
  tbl_summary(
    data=sdq_tab_OB,
    by = IOTF_3class, 
    value = list(SDQ_EmotionProb_raw ~ 'Emotional Problems, raw', SDQ_ConductProb_raw ~ 'Contuct Problems, raw', SDQ_HyperactiveProb_raw ~ 'Hyperactivity, raw', SDQ_PeerProb_raw ~ 'Peer Problems, raw', SDQ_Prosocial_raw ~ 'Prosocial, raw', SDQ_TotalProb_raw ~ 'Total Problems, raw', SDQ_EmotionProb_cat ~ 'Emotional Problems', SDQ_ConductProb_cat ~ 'Contuct Problems', SDQ_HyperactivityProb_cat ~ 'Hyperactivity', SDQ_PeerProb_cat ~ 'Peer Problems', SDQ_Prosocial_cat ~ 'Prosocial', SDQ_TotalProb_cat ~ 'Total Problems'),
    label = list(SDQ_EmotionProb_raw ~ 'Emotional Problems, raw', SDQ_ConductProb_raw ~ 'Contuct Problems, raw', SDQ_HyperactiveProb_raw ~ 'Hyperactivity, raw', SDQ_PeerProb_raw ~ 'Peer Problems, raw', SDQ_Prosocial_raw ~ 'Prosocial, raw', SDQ_TotalProb_raw ~ 'Total Problems, raw', SDQ_EmotionProb_cat ~ 'Emotional Problems', SDQ_ConductProb_cat ~ 'Contuct Problems', SDQ_HyperactivityProb_cat ~ 'Hyperactivity', SDQ_PeerProb_cat ~ 'Peer Problems', SDQ_Prosocial_cat ~ 'Prosocial', SDQ_TotalProb_cat ~ 'Total Problems'),
    type = list(SDQ_EmotionProb_raw ~ 'continuous', SDQ_ConductProb_raw ~ 'continuous', SDQ_HyperactiveProb_raw ~ 'continuous', SDQ_PeerProb_raw ~ 'continuous', SDQ_Prosocial_raw ~ 'continuous', SDQ_TotalProb_raw ~ 'continuous'),
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

