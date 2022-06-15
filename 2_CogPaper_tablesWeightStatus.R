# This script was written by Alaina Pearce in 2022
# to set up tables by weight status for the paper examining
# cognitive function in Emirati children
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
sum_tab <- UAE_allDat[c(3, 5, 10, 82, 18:19, 20, 15:16)]

UAE_demo_tab <-
  tbl_summary(
    data=sum_tab,
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

UAE_demo_sex_tab <-
  tbl_summary(
    data = sum_tab,
    by = sex, 
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

demo_tab_merge <- 
  tbl_merge(
    tbls = list(UAE_demo_tab, UAE_demo_sex_tab),
    tab_spanner = c('**All**', '**Sex**')
  )

sum_tab_OB <- UAE_allDat[c(84, 3, 5, 10, 82, 18:19, 20, 15:16)]

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

####   Neuropsych Data       ####
neuropsych_tab <- UAE_allDat[c(48, 50:52, 54:61)]
UAE_neuropsych_tab <-
  tbl_summary(
    data=neuropsych_tab,
    value = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Digit Total, SS'),
    label = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Coding, SS'),
    type = list(WASI_BlockT ~ 'continuous', WASI_MatrixT ~ 'continuous', WASI_PRI_sumT ~ 'continuous', WASI_PRI_IQ ~ 'continuous', DigitSpan_ForwardRaw ~ 'continuous', DigitSpan_ForwardSS ~ 'continuous', DigitSpan_BackwardRaw ~ 'continuous', DigitSpan_BackwardSS ~ 'continuous', DigitSpan_TotalRaw ~ 'continuous', DigitSpan_totalSS ~ 'continuous', CodingRaw ~ 'continuous', CodingSS ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

neuropsych_tab_sex <- UAE_allDat[c(3, 48, 50:52, 54:61)]

UAE_neuropsych_sexstat <-
  tbl_summary(
    data=neuropsych_tab_sex,
    by = sex, 
    value = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Digit Total, SS'),
    label = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Digit Total, SS'),
    type = list(WASI_BlockT ~ 'continuous', WASI_MatrixT ~ 'continuous', WASI_PRI_sumT ~ 'continuous', WASI_PRI_IQ ~ 'continuous', DigitSpan_ForwardRaw ~ 'continuous', DigitSpan_ForwardSS ~ 'continuous', DigitSpan_BackwardRaw ~ 'continuous', DigitSpan_BackwardSS ~ 'continuous', DigitSpan_TotalRaw ~ 'continuous', DigitSpan_totalSS ~ 'continuous', CodingRaw ~ 'continuous', CodingSS ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
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

neuropsych_tab_merge <- 
  tbl_merge(
    tbls = list(UAE_neuropsych_tab, UAE_neuropsych_sexstat),
    tab_spanner = c('**All**', '**Sex**')
  )

neuropsych_tab_OB <- UAE_allDat[c(84, 48, 50:52, 54:61)]

UAE_neuropsych_OBstat <-
  tbl_summary(
    data=neuropsych_tab_OB,
    by = IOTF_3class, 
    value = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Digit Total, SS'),
    label = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Digit Total, SS'),
    type = list(WASI_BlockT ~ 'continuous', WASI_MatrixT ~ 'continuous', WASI_PRI_sumT ~ 'continuous', WASI_PRI_IQ ~ 'continuous', DigitSpan_ForwardRaw ~ 'continuous', DigitSpan_ForwardSS ~ 'continuous', DigitSpan_BackwardRaw ~ 'continuous', DigitSpan_BackwardSS ~ 'continuous', DigitSpan_TotalRaw ~ 'continuous', DigitSpan_totalSS ~ 'continuous', CodingRaw ~ 'continuous', CodingSS ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
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

neuropsych_IQ70_tab_OB <- UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, c(84, 48, 50:52, 54:61)]

UAE_neuropsych_IQ70_OBstat <-
  tbl_summary(
    data=neuropsych_IQ70_tab_OB,
    by = IOTF_3class, 
    value = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Digit Total, SS'),
    label = list(WASI_BlockT ~ 'WASI-Block, T', WASI_MatrixT ~ 'WASI-Matrix, T', WASI_PRI_sumT ~ 'WASI-PRI, T', WASI_PRI_IQ ~ 'WASI-PRI, IQ', DigitSpan_ForwardRaw ~ 'Digit Forward, raw', DigitSpan_ForwardSS ~ 'Digit Forward, SS', DigitSpan_BackwardRaw ~ 'Digit Backward, raw', DigitSpan_BackwardSS ~ 'Digit Backward, SS', DigitSpan_TotalRaw ~ 'Digit Total, raw', DigitSpan_totalSS ~ 'Digit Total, SS', CodingRaw ~ 'Coding, raw', CodingSS ~ 'Digit Total, SS'),
    type = list(WASI_BlockT ~ 'continuous', WASI_MatrixT ~ 'continuous', WASI_PRI_sumT ~ 'continuous', WASI_PRI_IQ ~ 'continuous', DigitSpan_ForwardRaw ~ 'continuous', DigitSpan_ForwardSS ~ 'continuous', DigitSpan_BackwardRaw ~ 'continuous', DigitSpan_BackwardSS ~ 'continuous', DigitSpan_TotalRaw ~ 'continuous', DigitSpan_totalSS ~ 'continuous', CodingRaw ~ 'continuous', CodingSS ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
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


###   Nback Data       ####
nback_tab <- UAE_goodNBackDat[c(101, 112, 105, 116, 108, 119, 122:123, 102, 113, 103, 114)]
UAE_nback_tab <-
  tbl_summary(
    data=nback_tab,
    value = list(B1_pCor ~ '1-Back: Correct, %', B2_pCor ~ '2-Back: Correct, %', B1_pFA ~ '1-Back: False Alarm, %', B2_pFA ~ '2-Back: False Alarm, %', B1_BalAcc ~ '1-Back: Balanced Acc, %', B2_BalAcc ~ '2-Back: Balanced Acc, %', B1_dprime ~ "1-Back: d'", B2_dprime ~ "2-Back: d'", B1_meanRTcor ~ '1-Back: mean RT, ms', B2_meanRTcor ~ '2-Back: mean RT, ms', B1_medRTcor ~ '1-Back: median RT, ms', B2_medRTcor ~ '2-Back: median RT, ms'),
    label = list(B1_pCor ~ '1-Back: Correct, %', B2_pCor ~ '2-Back: Correct, %', B1_pFA ~ '1-Back: False Alarm, %', B2_pFA ~ '2-Back: False Alarm, %', B1_BalAcc ~ '1-Back: Balanced Acc, %', B2_BalAcc ~ '2-Back: Balanced Acc, %', B1_dprime ~ "1-Back: d'", B2_dprime ~ "2-Back: d'", B1_meanRTcor ~ '1-Back: mean RT, ms', B2_meanRTcor ~ '2-Back: mean RT, ms', B1_medRTcor ~ '1-Back: median RT, ms', B2_medRTcor ~ '2-Back: median RT, ms'),
    type = list(B1_pCor ~ 'continuous', B2_pCor ~ 'continuous', B1_pFA ~ 'continuous', B2_pFA ~ 'continuous', B1_BalAcc ~ 'continuous', B2_BalAcc ~ 'continuous', B1_dprime ~ 'continuous', B2_dprime ~ 'continuous', B1_meanRTcor ~ 'continuous', B2_meanRTcor ~ 'continuous', B1_medRTcor ~ 'continuous', B2_medRTcor ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
    missing = "ifany",
    digits = all_continuous() ~ 2) %>%
  add_n()

nback_tab_sex <- UAE_goodNBackDat[c(9, 101, 112, 105, 116, 108, 119, 122:123, 102, 113, 103, 114)]

UAE_nback_sexstat <-
  tbl_summary(
    data=nback_tab_sex,
    by = sex,
    value = list(B1_pCor ~ '1-Back: Correct, %', B2_pCor ~ '2-Back: Correct, %', B1_pFA ~ '1-Back: False Alarm, %', B2_pFA ~ '2-Back: False Alarm, %', B1_BalAcc ~ '1-Back: Balanced Acc, %', B2_BalAcc ~ '2-Back: Balanced Acc, %', B1_dprime ~ "1-Back: d'", B2_dprime ~ "2-Back: d'", B1_meanRTcor ~ '1-Back: mean RT, ms', B2_meanRTcor ~ '2-Back: mean RT, ms', B1_medRTcor ~ '1-Back: median RT, ms', B2_medRTcor ~ '2-Back: median RT, ms'),
    label = list(B1_pCor ~ '1-Back: Correct, %', B2_pCor ~ '2-Back: Correct, %', B1_pFA ~ '1-Back: False Alarm, %', B2_pFA ~ '2-Back: False Alarm, %', B1_BalAcc ~ '1-Back: Balanced Acc, %', B2_BalAcc ~ '2-Back: Balanced Acc, %', B1_dprime ~ "1-Back: d'", B2_dprime ~ "22-Back: d'", B1_meanRTcor ~ '1-Back: mean RT, ms', B2_meanRTcor ~ '2-Back: mean RT, ms', B1_medRTcor ~ '1-Back: median RT, ms', B2_medRTcor ~ '2-Back: median RT, ms'),
    type = list(B1_pCor ~ 'continuous', B2_pCor ~ 'continuous', B1_pFA ~ 'continuous', B2_pFA ~ 'continuous', B1_BalAcc ~ 'continuous', B2_BalAcc ~ 'continuous', B1_dprime ~ 'continuous', B2_dprime ~ 'continuous', B1_meanRTcor ~ 'continuous', B2_meanRTcor ~ 'continuous', B1_medRTcor ~ 'continuous', B2_medRTcor ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
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

nback_tab_merge <- 
  tbl_merge(
    tbls = list(UAE_nback_tab, UAE_nback_sexstat),
    tab_spanner = c('**All**', '**Sex**')
  )

nback_tab_OB <- UAE_goodNBackDat[c(84, 101, 112, 105, 116, 108, 119, 122:123, 102, 113, 103, 114)]

UAE_nback_OBstat <-
  tbl_summary(
    data=nback_tab_OB,
    by = IOTF_3class,
    value = list(B1_pCor ~ '1-Back: Correct, %', B2_pCor ~ '2-Back: Correct, %', B1_pFA ~ '1-Back: False Alarm, %', B2_pFA ~ '2-Back: False Alarm, %', B1_BalAcc ~ '1-Back: Balanced Acc, %', B2_BalAcc ~ '2-Back: Balanced Acc, %', B1_dprime ~ "1-Back: d'", B2_dprime ~ "2-Back: d'", B1_meanRTcor ~ '1-Back: mean RT, ms', B2_meanRTcor ~ '2-Back: mean RT, ms', B1_medRTcor ~ '1-Back: median RT, ms', B2_medRTcor ~ '2-Back: median RT, ms'),
    label = list(B1_pCor ~ '1-Back: Correct, %', B2_pCor ~ '2-Back: Correct, %', B1_pFA ~ '1-Back: False Alarm, %', B2_pFA ~ '2-Back: False Alarm, %', B1_BalAcc ~ '1-Back: Balanced Acc, %', B2_BalAcc ~ '2-Back: Balanced Acc, %', B1_dprime ~ "1-Back: d'", B2_dprime ~ "2-Back: d'", B1_meanRTcor ~ '1-Back: mean RT, ms', B2_meanRTcor ~ '2-Back: mean RT, ms', B1_medRTcor ~ '1-Back: median RT, ms', B2_medRTcor ~ '2-Back: median RT, ms'),
    type = list(B1_pCor ~ 'continuous', B2_pCor ~ 'continuous', B1_pFA ~ 'continuous', B2_pFA ~ 'continuous', B1_BalAcc ~ 'continuous', B2_BalAcc ~ 'continuous', B1_dprime ~ 'continuous', B2_dprime ~ 'continuous', B1_meanRTcor ~ 'continuous', B2_meanRTcor ~ 'continuous', B1_medRTcor ~ 'continuous', B2_medRTcor ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd}) [{min} - {max}]"), 
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
