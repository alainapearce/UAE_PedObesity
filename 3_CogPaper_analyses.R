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

# t-test for sex and pOW 
pOW.sex_ttest <- t.test(pOW ~ sex, data = UAE_allDat)

# correlations with pOW 
pOW_demo_cor.varnames <- names(UAE_allDat)[c(5, 18:19, 82)]
pOW_demo_cor.vars <- UAE_allDat[c(5, 18:19, 82)]
pOW_demo_cormat <- data.frame(cor.matrix(pOW_demo_cor.vars, pOW_demo_cor.varnames))
pOW_demo_cormat_ps <- data.frame(cor.matrix_ps(pOW_demo_cor.vars, pOW_demo_cor.varnames))

# monthly income ANOVA
pOW_income_mod <- lm(pOW ~ Month_AED, data = UAE_allDat)
pOW_income_anova <- Anova(pOW_income_mod, type = 3, test.statistic = 'F')

##sensitivity test with linear model 
IOTF_age_pOWcutoff_mod <- lm(pOW ~ Month_AED + Mother_ed + Age_yr, data = UAE_allDat)
IOTF_age_pOWcutoff_sum <- summary(IOTF_age_pOWcutoff_mod)
IOTF_age_pOWcutoff_tab <- cbind.data.frame(IOTF_age_pOWcutoff_sum$coefficients, c('**', '', '', '', '', '**'))
names(IOTF_age_pOWcutoff_tab) <- c('b', 'se', 't', 'p', ' ')

####   Neuropsych Data       ####

##correlations with IOTF_BMI25p
neuropsych_cor_varnames <- c('blockT', 'matrixT', 'PRI', 'ds_fSS', 'ds_bSS', 'codingSS', 'age', 'pOW', 'nComorbid', 'CSHQ', 'SDQ')
neuropsych_cor_vars = UAE_allDat[c(48, 50, 52, 55, 57, 61, 5, 82, 95, 46, 26)]
neuropsych_cormat <- data.frame(cor.matrix(neuropsych_cor_vars, neuropsych_cor_varnames))
neuropsych_cormat_ps <- data.frame(cor.matrix_ps(neuropsych_cor_vars, neuropsych_cor_varnames))

neuropsych_cor_IQ70_vars = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, c(48, 50, 52, 55, 57, 61, 5, 82, 95, 46, 26)]
neuropsych_IQ70_cormat <- data.frame(cor.matrix(neuropsych_cor_IQ70_vars, neuropsych_cor_varnames))
neuropsych_IQ70_cormat_ps <- data.frame(cor.matrix_ps(neuropsych_cor_IQ70_vars, neuropsych_cor_varnames))


## Models
UAE_allDat$pOW_c100 <- UAE_allDat$pOW - 100
UAE_allDat$pOW_c100_sq <- UAE_allDat$pOW_c100 * UAE_allDat$pOW_c100

#BlockT
pOW_BlockT_mod <- lm(WASI_BlockT ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat)

pOW_BlockT_quadmod <- lm(WASI_BlockT ~ Month_AED + Mother_ed + Age_yr + pOW_c100 + pOW_c100_sq, data = UAE_allDat)

anova(pOW_BlockT_mod, pOW_BlockT_quadmod)

pOW_BlockT_quad_sum <- summary(pOW_BlockT_quadmod)

##find vertex - remember, centered at 100 so add 100 to interpret
pOW_BlockT_quad_vertex <- -pOW_BlockT_quad_sum$coefficients[7, 1]/(2*pOW_BlockT_quad_sum$coefficients[8, 1])
  
#get dataset with full data for predicted value
UAE_allDat_WASI <- UAE_allDat[!is.na(UAE_allDat$Mother_ed) & !is.na(UAE_allDat$Month_AED), ]
UAE_allDat_WASI$BlockT_quad_pred <- predict(pOW_BlockT_quadmod, type = 'response')

#BlockT - IQ 70
pOW_BlockT_IQ70_quadmod <- lm(WASI_BlockT ~ Month_AED + Mother_ed + Age_yr  + pOW_c100 + pOW_c100_sq, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_BlockT_IQ70_quad_sum <- summary(pOW_BlockT_IQ70_quadmod)

#get dataset with full data for predicted value
UAE_allDat_WASI70 <- UAE_allDat[!is.na(UAE_allDat$Mother_ed) & !is.na(UAE_allDat$Month_AED) & UAE_allDat$WASI_PRI_IQ >= 70, ]
UAE_allDat_WASI70$BlockT_quad_pred <- predict(pOW_BlockT_IQ70_quadmod, type = 'response')

#MatrixT
pOW_MatrixT_mod <- lm(WASI_MatrixT ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat)

pOW_MatrixT_quadmod <- lm(WASI_MatrixT ~ Month_AED + Mother_ed + Age_yr + pOW_c100 + pOW_c100_sq, data = UAE_allDat)

anova(pOW_MatrixT_mod, pOW_MatrixT_quadmod)

pOW_MatrixT_sum <- summary(pOW_MatrixT_mod)
UAE_allDat_WASI$MatrixT_pred <- predict(pOW_MatrixT_mod, type = 'response')


pOW_MatrixT_IQ70_mod <- lm(WASI_MatrixT ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_MatrixT_IQ70_sum <- summary(pOW_MatrixT_IQ70_mod)
UAE_allDat_WASI70$MatrixT_pred <- predict(pOW_MatrixT_IQ70_mod, type = 'response')


#PRI
pOW_PRI_mod <- lm(WASI_PRI_IQ ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat)

pOW_PRI_quadmod <- lm(WASI_PRI_IQ ~ Month_AED + Mother_ed + Age_yr + pOW_c100 + pOW_c100_sq, data = UAE_allDat)

anova(pOW_PRI_mod, pOW_PRI_quadmod)

pOW_PRI_quad_sum <- summary(pOW_PRI_quadmod)

pOW_PRI_IQ70_mod <- lm(WASI_PRI_IQ ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_PRI_IQ70_quad_sum <- summary(pOW_PRI_IQ70_mod)

## Digit Span
#forward
pOW_dsforward_mod <- lm(DigitSpan_ForwardSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat)
pOW_dsforward_sum <- summary(pOW_dsforward_mod)
UAE_allDat_WASI$dsforward_pred <- predict(pOW_dsforward_mod, type = 'response')

pOW_IQ70_dsforward_mod <- lm(DigitSpan_ForwardSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_dsforward_sum <- summary(pOW_IQ70_dsforward_mod)
UAE_allDat_WASI70$dsforward_pred <- predict(pOW_IQ70_dsforward_mod, type = 'response')

#### Sleep
##center sleep to put on similar scale
UAE_allDat$CSHQ_Total_no16_cmean <- UAE_allDat$CSHQ_Total_no16 - mean(UAE_allDat$CSHQ_Total_no16, na.rm = TRUE)

pOW_dsforward_sleep_mod <- lm(DigitSpan_ForwardSS ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16_cmean*pOW_c100, data = UAE_allDat)
pOW_dsforward_sleep_sum <- summary(pOW_dsforward_sleep_mod)

pOW_IQ70_dsforward_sleep_mod <- lm(DigitSpan_ForwardSS ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16_cmean*pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_dsforward_sleep_sum <- summary(pOW_IQ70_dsforward_sleep_mod)

#### SDQ
##center SDQ to put on similar scale
UAE_allDat$SDQ_TotalProb_raw_cmean <- UAE_allDat$SDQ_TotalProb_raw - mean(UAE_allDat$SDQ_TotalProb_raw, na.rm = TRUE)

pOW_dsforward_SDQ_mod <- lm(DigitSpan_ForwardSS ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw_cmean*pOW_c100, data = UAE_allDat)
pOW_dsforward_SDQ_sum <- summary(pOW_dsforward_SDQ_mod)


pOW_dsforward_SDQ_slopes <- emmeans::emtrends(pOW_dsforward_SDQ_mod, ~pOW_c100|SDQ_TotalProb_raw_cmean, var = 'pOW_c100', at = list(SDQ_TotalProb_raw_cmean = c(-5, 0, 5.3)))

pOW_IQ70_dsforward_SDQ_mod <- lm(DigitSpan_ForwardSS ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw_cmean*pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_dsforward_SDQ_sum <- summary(pOW_IQ70_dsforward_SDQ_mod)

#backward
pOW_dsbackward_mod <- lm(DigitSpan_BackwardSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat)
pOW_dsbackward_sum <- summary(pOW_dsbackward_mod)
UAE_allDat_WASI$dsbackward_pred <- predict(pOW_dsbackward_mod, type = 'response')

pOW_IQ70_dsbackward_mod <- lm(DigitSpan_BackwardSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_dsbackward_sum <- summary(pOW_IQ70_dsbackward_mod)
UAE_allDat_WASI70$dsbackward_pred <- predict(pOW_IQ70_dsbackward_mod, type = 'response')

### sleep
pOW_dsbackward_sleep_mod <- lm(DigitSpan_BackwardSS ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16_cmean*pOW_c100, data = UAE_allDat)
pOW_dsbackward_sleep_sum <- summary(pOW_dsbackward_sleep_mod)

pOW_IQ70_dsbackward_sleep_mod <- lm(DigitSpan_BackwardSS ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16_cmean*pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_dsbackward_sleep_sum <- summary(pOW_IQ70_dsbackward_sleep_mod)

### SDQ
pOW_dsbackward_SDQ_mod <- lm(DigitSpan_BackwardSS ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw_cmean*pOW_c100, data = UAE_allDat)
pOW_dsbackward_SDQ_sum <- summary(pOW_dsbackward_SDQ_mod)

pOW_IQ70_dsbackward_SDQ_mod <- lm(DigitSpan_BackwardSS ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw_cmean*pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_dsbackward_SDQ_sum <- summary(pOW_IQ70_dsbackward_SDQ_mod)

#total
pOW_dstotal_mod <- lm(DigitSpan_totalSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat)
pOW_dstotal_sum <- summary(pOW_dstotal_mod)

pOW_IQ70_dstotal_mod <- lm(DigitSpan_totalSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_dstotal_sum <- summary(pOW_IQ70_dstotal_mod)

## coding
#total
pOW_coding_mod <- lm(CodingSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat)
pOW_coding_sum <- summary(pOW_coding_mod)
UAE_allDat_WASI$coding_pred <- predict(pOW_coding_mod, type = 'response')

pOW_IQ70_coding_mod <- lm(CodingSS ~ Month_AED + Mother_ed + Age_yr + pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_coding_sum <- summary(pOW_IQ70_coding_mod)
UAE_allDat_WASI70$coding_pred <- predict(pOW_IQ70_coding_mod, type = 'response')

### Sleep
pOW_coding_sleep_mod <- lm(CodingSS ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16_cmean*pOW_c100, data = UAE_allDat)
pOW_coding_sleep_sum <- summary(pOW_coding_sleep_mod)

pOW_coding_sleep_slopes <- emmeans::emtrends(pOW_coding_sleep_mod, ~pOW_c100|CSHQ_Total_no16_cmean, var = 'pOW_c100', at = list(CSHQ_Total_no16_cmean = c(-8, 0, 8)))

UAE_allDat_codingmod <- UAE_allDat[!is.na(UAE_allDat$Month_AED) & !is.na(UAE_allDat$Mother_ed) & !is.na(UAE_allDat$CSHQ_Total_no16), ]
UAE_allDat_codingmod$coding_pred <- predict(pOW_coding_sleep_mod, type = 'response')

pOW_IQ70_coding_sleep_mod <- lm(CodingSS ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_coding_sleep_sum <- summary(pOW_IQ70_coding_sleep_mod)

### SDQ
pOW_coding_SDQ_mod <- lm(CodingSS ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw_cmean*pOW_c100, data = UAE_allDat)
pOW_coding_SDQ_sum <- summary(pOW_coding_SDQ_mod)

pOW_IQ70_coding_SDQ_mod <- lm(CodingSS ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw_cmean*pOW_c100, data = UAE_allDat[UAE_allDat$WASI_PRI_IQ >= 70, ])
pOW_IQ70_coding_SDQ_sum <- summary(pOW_IQ70_coding_SDQ_mod)

## Nback ####
pOW_Nback_varnames <- c("B1_pFA", "B2_pFA", "B1_BalAcc", "B2_BalAcc", "B1_dprime", "B2_dprime", "B1_RT", "B2_RT", "pOW", "nComorbid", "CSHQ", "SDQ", "Age_yr" )
pOW_Nback_vars <- UAE_goodNBackDat[c(105, 116, 108, 119, 122:123, 102, 113, 82, 95, 52, 32, 11)]

nback_cormat <- data.frame(cor.matrix(pOW_Nback_vars, pOW_Nback_varnames))
nback_cormat_ps <- data.frame(cor.matrix_ps(pOW_Nback_vars, pOW_Nback_varnames))

pOW_IQ70_Nback_vars <- UAE_goodNBackDat[UAE_goodNBackDat$WASI_PRI_IQ >= 70, c(105, 116, 108, 119, 122:123, 102, 113, 82, 95, 52, 32, 11)]
nback_IQ70_cormat <- data.frame(cor.matrix(pOW_IQ70_Nback_vars, pOW_Nback_varnames))
nback_IQ70_cormat_ps <- data.frame(cor.matrix_ps(pOW_IQ70_Nback_vars, pOW_Nback_varnames))

#####################################
####
####   Nback Performance - Models  ####
####
#####################################
UAE_goodNBackDat_long$pOW_c100 <- UAE_goodNBackDat_long$pOW - 100

##Load x percent of overweight ####

#BalAcc
pOW_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long)
pOW_BalAccInt_sum <- summary(pOW_BalAccInt_mod)
pOW_BalAccInt_anova <- anova(pOW_BalAccInt_mod, test.statistic = 'F')

UAE_goodNBack_modDat <- UAE_goodNBackDat_long[!is.na(UAE_goodNBackDat_long$Month_AED) & !is.na(UAE_goodNBackDat_long$Mother_ed), ]
UAE_goodNBack_modDat$BalAcc_pOW_pred <- predict(pOW_BalAccInt_mod, type = 'response')

pOW_IQg70_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
pOW_IQg70_BalAccInt_sum <- summary(pOW_IQg70_BalAccInt_mod)
pOW_IQg70_BalAccInt_anova <- anova(pOW_IQg70_BalAccInt_mod, test.statistic = 'F')

#pFA
pOW_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long)
pOW_pFAInt_sum <- summary(pOW_pFAInt_mod)
pOW_pFAInt_anova <- anova(pOW_pFAInt_mod, test.statistic = 'F')

pOW_IQg70_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
pOW_IQg70_pFAInt_sum <- summary(pOW_IQg70_pFAInt_mod)
pOW_IQg70_pFAInt_anova <- anova(pOW_IQg70_pFAInt_mod, test.statistic = 'F')

#dprime
pOW_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long)
pOW_dPrimeInt_sum <- summary(pOW_dPrimeInt_mod)
pOW_dPrimeInt_anova <- anova(pOW_dPrimeInt_mod, test.statistic = 'F')

pOW_IQg70_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
pOW_IQg70_dPrimeInt_sum <- summary(pOW_IQg70_dPrimeInt_mod)
pOW_IQg70_dPrimeInt_anova <- anova(pOW_IQg70_dPrimeInt_mod, test.statistic = 'F')

#meanRT
pOW_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long)
pOW_meanRTInt_sum <- summary(pOW_meanRTInt_mod)
pOW_meanRTInt_anova <- anova(pOW_meanRTInt_mod, test.statistic = 'F')

pOW_IQg70_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
pOW_IQg70_meanRTInt_sum <- summary(pOW_IQg70_meanRTInt_mod)
pOW_IQg70_meanRTInt_anova <- anova(pOW_IQg70_meanRTInt_mod, test.statistic = 'F')

##Exploratory : Percent overweight x N comorbidites ####
#BalAcc
ncomorbid_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long)
ncomorbid_BalAccInt_sum <- summary(ncomorbid_BalAccInt_mod)

ncomorbid_BalAccInt_slopes <- emmeans::emtrends(ncomorbid_BalAccInt_mod, ~pOW_c100|nComorbid, var = 'pOW_c100', at = list(nComorbid = c(0, 1, 2, 3)))

UAE_goodNBack_modDat$BalACC_ncomorbid <- predict(ncomorbid_BalAccInt_mod, type = 'response')

ncomorbid_IQg70_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
ncomorbid_IQg70_BalAccInt_sum <- summary(ncomorbid_IQg70_BalAccInt_mod)

#pFA
ncomorbid_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long)
ncomorbid_pFAInt_sum <- summary(ncomorbid_pFAInt_mod)

ncomorbid_pFA_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100 + nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long)
ncomorbid_pFA_sum <- summary(ncomorbid_pFA_mod)

ncomorbid_IQg70_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
ncomorbid_IQg70_pFAInt_sum <- summary(ncomorbid_IQg70_pFAInt_mod)

#dprime
ncomorbid_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long)
ncomorbid_dPrimeInt_sum <- summary(ncomorbid_dPrimeInt_mod)

ncomorbid_dPrimeInt_slopes <- emmeans::emtrends(ncomorbid_dPrimeInt_mod, ~pOW_c100|nComorbid, var = 'pOW_c100', at = list(nComorbid = c(0, 1, 2, 3)))

UAE_goodNBack_modDat$dPrime_ncomorbid <- predict(ncomorbid_dPrimeInt_mod, type = 'response')

ncomorbid_IQg70_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
ncomorbid_IQg70_dPrimeInt_sum <- summary(ncomorbid_IQg70_dPrimeInt_mod)

#meanRT
ncomorbid_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long)
ncomorbid_meanRTInt_sum <- summary(ncomorbid_meanRTInt_mod)

ncomorbid_IQg70_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*nComorbid + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
ncomorbid_IQg70_meanRTInt_sum <- summary(ncomorbid_IQg70_meanRTInt_mod)


##Exploratory : Percent overweight x sleep ####
UAE_goodNBackDat_long$CSHQ_Total_no16_cmean <- UAE_goodNBackDat_long$CSHQ_Total_no16 - mean(UAE_goodNBackDat_long$CSHQ_Total_no16, na.rm = TRUE)

#BalAcc
sleep_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
sleep_BalAccInt_sum <- summary(sleep_BalAccInt_mod)

sleep_IQg70_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
sleep_IQg70_BalAccInt_sum <- summary(sleep_IQg70_BalAccInt_mod)

#pFA
sleep_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
sleep_pFAInt_sum <- summary(sleep_pFAInt_mod)

sleep_pFAInt_slopes <- emmeans::emtrends(sleep_pFAInt_mod, ~pOW_c100|CSHQ_Total_no16_cmean, var = 'pOW_c100', at = list(CSHQ_Total_no16_cmean = c(-8, 0, 8)))

UAE_goodNBack_sleep_modDat <- UAE_goodNBack_modDat[!is.na(UAE_goodNBack_modDat$CSHQ_Total_no16), ]

UAE_goodNBack_sleep_modDat$pFA_sleep <- predict(sleep_pFAInt_mod, type = 'response')

UAE_goodNBackDat_no109_long <- UAE_goodNBackDat_long[UAE_goodNBackDat_long$ParID != 109, ]

sleep_no109_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_no109_long)
sleep_no109_pFAInt_sum <- summary(sleep_no109_pFAInt_mod)

sleep_no109_pFAInt_slopes <- emmeans::emtrends(sleep_no109_pFAInt_mod, ~pOW_c100|CSHQ_Total_no16_cmean, var = 'pOW_c100', at = list(CSHQ_Total_no16_cmean = c(-8, 0, 8)))

sleep_IQg70_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
sleep_IQg70_pFAInt_sum <- summary(sleep_IQg70_pFAInt_mod)

#dprime
sleep_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
sleep_dPrimeInt_sum <- summary(sleep_dPrimeInt_mod)

sleep_IQg70_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
sleep_IQg70_dPrimeInt_sum <- summary(sleep_IQg70_dPrimeInt_mod)

#meanRT
sleep_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
sleep_meanRTInt_sum <- summary(sleep_meanRTInt_mod)

sleep_IQg70_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*CSHQ_Total_no16_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
sleep_IQg70_meanRTInt_sum <- summary(sleep_IQg70_meanRTInt_mod)

##Exploratory : Percent overweight x SDQ ####
##center sleep to put on similar scale
UAE_goodNBackDat_long$SDQ_TotalProb_raw_cmean <- UAE_goodNBackDat_long$SDQ_TotalProb_raw - mean(UAE_goodNBackDat_long$SDQ_TotalProb_raw, na.rm = TRUE)

UAE_goodNBackDat_no109_long$SDQ_TotalProb_raw_cmean <- UAE_goodNBackDat_no109_long$SDQ_TotalProb_raw - mean(UAE_goodNBackDat_no109_long$SDQ_TotalProb_raw, na.rm = TRUE)

#BalAcc
SDQ_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_BalAccInt_sum <- summary(SDQ_BalAccInt_mod)

SDQ_IQg70_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_BalAccInt_sum <- summary(SDQ_IQg70_BalAccInt_mod)

#pFA
SDQ_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_pFAInt_sum <- summary(SDQ_pFAInt_mod)

SDQ_pFAInt_slopes <- emmeans::emtrends(SDQ_pFAInt_mod, ~pOW_c100|SDQ_TotalProb_raw_cmean, var = 'pOW_c100', at = list(SDQ_TotalProb_raw_cmean = c(-5, 0, 5)))

UAE_goodNBack_SDQ_modDat <- UAE_goodNBack_modDat[!is.na(UAE_goodNBack_modDat$SDQ_TotalProb_raw), ]

UAE_goodNBack_SDQ_modDat$pFA_SDQ <- predict(SDQ_pFAInt_mod, type = 'response')

SDQ_no109_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_no109_long)
SDQ_no109_pFAInt_sum <- summary(SDQ_no109_pFAInt_mod)

SDQ_IQg70_pFAInt_mod <- lmer(pFA ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_pFAInt_sum <- summary(SDQ_IQg70_pFAInt_mod)

#dprime
SDQ_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_dPrimeInt_sum <- summary(SDQ_dPrimeInt_mod)

SDQ_IQg70_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_dPrimeInt_sum <- summary(SDQ_IQg70_dPrimeInt_mod)

#meanRT
SDQ_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_meanRTInt_sum <- summary(SDQ_meanRTInt_mod)

SDQ_IQg70_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + pOW_c100*SDQ_TotalProb_raw_cmean + Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_meanRTInt_sum <- summary(SDQ_IQg70_meanRTInt_mod)

##Load x Total Sleep
#BalAcc
Sleep_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long)
Sleep_BalAccInt_sum <- summary(Sleep_BalAccInt_mod)
Sleep_BalAccInt_anova <- anova(Sleep_BalAccInt_mod, test.statistic = 'F')

Sleep_IQg70_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
Sleep_IQg70_BalAccInt_sum <- summary(Sleep_IQg70_BalAccInt_mod)
Sleep_IQg70_BalAccInt_anova <- anova(Sleep_IQg70_BalAccInt_mod, test.statistic = 'F')

#pFA
Sleep_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long)
Sleep_pFAInt_sum = summary(Sleep_pFAInt_mod)
Sleep_pFAInt_anova <- anova(Sleep_pFAInt_mod, test.statistic = 'F')

Sleep_pFAInt_slopes <- emmeans::emtrends(Sleep_pFAInt_mod, ~CSHQ_Total_no16|Load, var = 'CSHQ_Total_no16')

UAE_goodNBack_sleepmodDat <- UAE_goodNBack_modDat[!is.na(UAE_goodNBack_modDat$CSHQ_Total_no16), ]
UAE_goodNBack_sleepmodDat$pFA_sleep_pred <- predict(Sleep_pFAInt_mod, type = 'response')

#check without outliers
Sleep_no109_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$ParID != 109, ])
Sleep_no109_pFAInt_sum = summary(Sleep_no109_pFAInt_mod)
Sleep_no109_pFAInt_anova <- anova(Sleep_no109_pFAInt_mod, test.statistic = 'F')

Sleep_IQg07_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
Sleep_IQg70_pFAInt_sum = summary(Sleep_IQg07_pFAInt_mod)
Sleep_IQg70_pFAInt_anova <- anova(Sleep_IQg07_pFAInt_mod, test.statistic = 'F')


#dprime
Sleep_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long)
Sleep_dPrimeInt_sum <- summary(Sleep_dPrimeInt_mod)
Sleep_dPrimeInt_anova <- anova(Sleep_dPrimeInt_mod, test.statistic = 'F')

Sleep_dPrimeInt_slopes <- emmeans::emtrends(Sleep_dPrimeInt_mod, ~CSHQ_Total_no16|Load, var = 'CSHQ_Total_no16')

UAE_goodNBack_sleepmodDat$dPrime_sleep_pred <- predict(Sleep_dPrimeInt_mod, type = 'response')

Sleep_IQg70_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
Sleep_IQg70_dPrimeInt_sum <- summary(Sleep_IQg70_dPrimeInt_mod)
Sleep_IQg70_dPrimeInt_anova <- anova(Sleep_IQg70_dPrimeInt_mod, test.statistic = 'F')

#meanRT
Sleep_meanRTInt_mod = lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long)
Sleep_meanRTInt_sum = summary(Sleep_meanRTInt_mod)

Sleep_IQg70_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + CSHQ_Total_no16*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
Sleep_IQg70_meanRTInt_sum <- summary(Sleep_IQg70_meanRTInt_mod)
Sleep_IQg70_meanRTInt_anova <- anova(Sleep_IQg70_meanRTInt_mod, test.statistic = 'F')

##Load x SDQ Total Probs
#BalAcc
SDQ_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_BalAccInt_sum <- summary(SDQ_BalAccInt_mod)
SDQ_BalAccInt_anova <- anova(SDQ_BalAccInt_mod, test.statistic = 'F')

SDQ_IQg70_BalAccInt_mod <- lmer(BalAcc ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_BalAccInt_sum <- summary(SDQ_IQg70_BalAccInt_mod)
SDQ_IQg70_BalAccInt_anova <- anova(SDQ_IQg70_BalAccInt_mod, test.statistic = 'F')

#pFA
SDQ_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_pFAInt_sum = summary(SDQ_pFAInt_mod)
SDQ_pFAInt_anova <- anova(SDQ_pFAInt_mod, test.statistic = 'F')

SDQ_pFAInt_slopes <- emmeans::emtrends(SDQ_pFAInt_mod, ~SDQ_TotalProb_raw|Load, var = 'SDQ_TotalProb_raw')

UAE_goodNBack_SDQmodDat <- UAE_goodNBack_modDat[!is.na(UAE_goodNBack_modDat$SDQ_TotalProb_raw), ]
UAE_goodNBack_SDQmodDat$pFA_sdq_pred <- predict(SDQ_pFAInt_mod, type = 'response')


SDQ_IQg07_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_pFAInt_sum = summary(SDQ_IQg07_pFAInt_mod)
SDQ_IQg70_pFAInt_anova <- anova(SDQ_IQg07_pFAInt_mod, test.statistic = 'F')

### exploratory - subscales
### hyperactivity
SDQhyper_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + SDQ_HyperactiveProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQhyper_pFAInt_sum = summary(SDQhyper_pFAInt_mod)
SDQhyper_pFAInt_anova <- anova(SDQhyper_pFAInt_mod, test.statistic = 'F')

SDQhyper_pFAInt_slopes <- emmeans::emtrends(SDQhyper_pFAInt_mod, ~SDQ_HyperactiveProb_raw|Load, var = 'SDQ_HyperactiveProb_raw')

UAE_goodNBack_SDQhypermodDat <- UAE_goodNBack_modDat[!is.na(UAE_goodNBack_modDat$SDQ_HyperactiveProb_raw), ]
UAE_goodNBack_SDQhypermodDat$pFA_sdqhyper_pred <- predict(SDQhyper_pFAInt_mod, type = 'response')

### emotional
SDQemot_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + SDQ_EmotionProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQemot_pFAInt_sum = summary(SDQemot_pFAInt_mod)
SDQemot_pFAInt_anova <- anova(SDQemot_pFAInt_mod, test.statistic = 'F')

SDQemot_pFAInt_slopes <- emmeans::emtrends(SDQemot_pFAInt_mod, ~SDQ_EmotionProb_raw|Load, var = 'SDQ_EmotionProb_raw')

UAE_goodNBack_SDQemotmodDat <- UAE_goodNBack_modDat[!is.na(UAE_goodNBack_modDat$SDQ_EmotionProb_raw), ]
UAE_goodNBack_SDQemotmodDat$pFA_sdqemot_pred <- predict(SDQemot_pFAInt_mod, type = 'response')

### conduct
SDQconduct_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + SDQ_ConductProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQconduct_pFAInt_sum = summary(SDQconduct_pFAInt_mod)
SDQconduct_pFAInt_anova <- anova(SDQconduct_pFAInt_mod, test.statistic = 'F')

SDQconduct_pFAInt_slopes <- emmeans::emtrends(SDQconduct_pFAInt_mod, ~SDQ_ConductProb_raw|Load, var = 'SDQ_ConductProb_raw')

UAE_goodNBack_SDQconductmodDat <- UAE_goodNBack_modDat[!is.na(UAE_goodNBack_modDat$SDQ_ConductProb_raw), ]
UAE_goodNBack_SDQconductmodDat$pFA_sdqconduct_pred <- predict(SDQconduct_pFAInt_mod, type = 'response')

### peer
SDQpeer_pFAInt_mod = lmer(pFA ~ Month_AED + Mother_ed + Age_yr + SDQ_PeerProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQpeer_pFAInt_sum = summary(SDQpeer_pFAInt_mod)
SDQpeer_pFAInt_anova <- anova(SDQpeer_pFAInt_mod, test.statistic = 'F')


#dprime
SDQ_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_dPrimeInt_sum <- summary(SDQ_dPrimeInt_mod)
SDQ_dPrimeInt_anova <- anova(SDQ_dPrimeInt_mod, test.statistic = 'F')

SDQ_IQg70_dPrimeInt_mod <- lmer(dprime ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_dPrimeInt_sum <- summary(SDQ_IQg70_dPrimeInt_mod)
SDQ_IQg70_dPrimeInt_anova <- anova(SDQ_IQg70_dPrimeInt_mod, test.statistic = 'F')

#meanRT
SDQ_meanRTInt_mod = lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long)
SDQ_meanRTInt_sum = summary(SDQ_meanRTInt_mod)

SDQ_IQg70_meanRTInt_mod <- lmer(meanRTcor ~ Month_AED + Mother_ed + Age_yr + SDQ_TotalProb_raw*Load + (1|ParID), data = UAE_goodNBackDat_long[UAE_goodNBackDat_long$WASI_PRI_IQ >= 70, ])
SDQ_IQg70_meanRTInt_sum <- summary(SDQ_IQg70_meanRTInt_mod)
SDQ_IQg70_meanRTInt_anova <- anova(SDQ_IQg70_meanRTInt_mod, test.statistic = 'F')


