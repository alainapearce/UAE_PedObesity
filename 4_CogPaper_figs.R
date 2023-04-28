# This script was written by Alaina Pearce in 2022
# to set up/organize data for the paper examining
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
# library(ggplot2)
# library(ggridges)

## load data/clean - uncomment if running separately
# source('1_CogPaper_DataOrg.R')

####   Demo Data  Figs     ####
UAE_allDat$all_group <- 1
pOW_plot <- ggplot(UAE_allDat, aes(x = pOW, y = all_group, group = all_group, color = sex)) +
  ggtitle('Distribution of Percent of Overweight Cuttoff') +
  scale_y_continuous(name=paste0('Density')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  annotate("rect", xmin = 0, xmax = 100, ymin = 1, ymax = 1.02, alpha = .3,fill = "#56B4E9") +
  annotate("rect", xmin = 100, xmax = 122, ymin = 1, ymax = 1.02, alpha = .3,fill = "#F0E442") +
  annotate("rect", xmin = 122, xmax = 250, ymin = 1, ymax = 1.02, alpha = .3,fill = "#CC79A7") +
  geom_density_ridges(aes(fill = factor(all_group), point_fill = sex, point_shape = sex), 
                      rel_min_height = 0.03,
                      jittered_points = TRUE, point_size = 1.5) +
  scale_fill_manual(values = alpha(c('white', 'white'), 0)) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 24)) +
  scale_discrete_manual(aesthetics = "point_fill", values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())


#### Neuropsych ####

##Block ####
pOW_BlockT_plot = ggplot(UAE_allDat_WASI, aes(y = BlockT_quad_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x+I(x*x)) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and WASI Block T-score (adjusting for covariates)') +
  scale_y_continuous(name=paste('Adjusted Block T-Score', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

pOW_IQ70_BlockT_plot = ggplot(UAE_allDat_WASI70, aes(y = BlockT_quad_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x+I(x*x)) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and WASI Block T-score (adjusting for covariates) for those with IQ > 70') +
  scale_y_continuous(name=paste('Adjusted Block T-Score', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Matrix ####
pOW_MatrixT_plot = ggplot(UAE_allDat_WASI, aes(y = MatrixT_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and WASI Matrix T-score (adjusting for covariates)') +
  scale_y_continuous(name=paste('Adjusted Matrix T-Score', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

pOW_IQ70_MatrixT_plot = ggplot(UAE_allDat_WASI70, aes(y = MatrixT_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and WASI Matrix T-score (adjusting for covariates) for those with IQ > 70') +
  scale_y_continuous(name=paste('Adjusted Matrix T-Score', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Digit Span - Forward ####
pOW_DSforward_plot = ggplot(UAE_allDat_WASI, aes(y = dsforward_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Digit Span Forward SS (adjusting for covariates)') +
  scale_y_continuous(name=paste('Adjusted Digit Span Forward SS', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

pOW_IQ70_DSforward_plot = ggplot(UAE_allDat_WASI70, aes(y = dsforward_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Digit Span Forward SS (adjusting for covariates) for those with IQ > 70') +
  scale_y_continuous(name=paste('Adjusted Digit Span Forward SS', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Digit Span - Backward ####
pOW_DSbackward_plot = ggplot(UAE_allDat_WASI, aes(y = dsbackward_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Digit Span Backward SS (adjusting for covariates)') +
  scale_y_continuous(name=paste('Adjusted Digit Span Backward SS', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

pOW_IQ70_DSbackward_plot = ggplot(UAE_allDat_WASI70, aes(y = dsbackward_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Digit Span Backward SS (adjusting for covariates) for those with IQ > 70') +
  scale_y_continuous(name=paste('Adjusted Digit Span Backward SS', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Coding ####
pOW_coding_plot = ggplot(UAE_allDat_WASI, aes(y = coding_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Coding SS (adjusting for covariates)') +
  scale_y_continuous(name=paste('Adjusted Coding SS', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

pOW_IQ70_coding_plot = ggplot(UAE_allDat_WASI70, aes(y = coding_pred, x = pOW)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Coding SS (adjusting for covariates) for those with IQ > 70') +
  scale_y_continuous(name=paste('Adjusted Coding SS', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

## Exploratory with sleep ####
sleep_coding_mean <- mean(UAE_allDat_codingmod$CSHQ_Total_no16, na.rm = TRUE)
sleep_coding_sd <- sd(UAE_allDat_codingmod$CSHQ_Total_no16, na.rm = TRUE)
UAE_allDat_codingmod$CSHQ_Total_no16_cat <- ifelse(UAE_allDat_codingmod$CSHQ_Total_no16 <= (sleep_coding_mean - sleep_coding_sd), 'SD-1', ifelse(UAE_allDat_codingmod$CSHQ_Total_no16 >= (sleep_coding_mean + sleep_coding_sd), 'SD+1', 'Center'))

pOW_sleep_coding_plot = ggplot(UAE_allDat_codingmod, aes(y = coding_pred, x = pOW, Group = factor(CSHQ_Total_no16_cat))) +
  geom_smooth(method = 'lm', formula = y~x, aes(color = factor(CSHQ_Total_no16_cat))) +
  geom_point(aes(color = factor(CSHQ_Total_no16_cat)), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Coding performance (adjusting for covariates)') +
  scale_y_continuous(name=paste('Coding', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("mediumpurple1", "purple", "purple4")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##### Nback Plots ####
## pOW ####
pOW_Nback_plot = ggplot(UAE_goodNBack_modDat, aes(y = BalAcc_pOW_pred, x = pOW, Group = Load)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(aes(color = Load), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Nback performance (adjusting for covariates)') +
  scale_y_continuous(name=paste('Ballanced Accuracy', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

## Exploratory with comorbidity ####
pOW_ncomorbid_BalAcc_plot = ggplot(UAE_goodNBack_modDat, aes(y = BalACC_ncomorbid, x = pOW, Group = factor(nComorbid))) +
  geom_smooth(method = 'lm', formula = y~x, aes(color = factor(nComorbid))) +
  geom_point(aes(color = factor(nComorbid)), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Nback performance (adjusting for covariates)') +
  scale_y_continuous(name=paste('Ballanced Accuracy', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("violet", "mediumpurple1", "purple", "purple4")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

pOW_ncomorbid_dprime_plot = ggplot(UAE_goodNBack_modDat, aes(y = dPrime_ncomorbid, x = pOW, Group = factor(nComorbid))) +
  geom_smooth(method = 'lm', formula = y~x, aes(color = factor(nComorbid))) +
  geom_point(aes(color = factor(nComorbid)), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Nback performance (adjusting for covariates)') +
  scale_y_continuous(name=paste("d'", '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("violet", "mediumpurple1", "purple", "purple4")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

## Exploratory with sleep ####
sleep_mean <- mean(UAE_goodNBack_sleep_modDat$CSHQ_Total_no16, na.rm = TRUE)
sleep_sd <- sd(UAE_goodNBack_sleep_modDat$CSHQ_Total_no16, na.rm = TRUE)
UAE_goodNBack_sleep_modDat$CSHQ_Total_no16_cat <- ifelse(UAE_goodNBack_sleep_modDat$CSHQ_Total_no16 <= (sleep_mean - sleep_sd), 'SD-1', ifelse(UAE_goodNBack_sleep_modDat$CSHQ_Total_no16 >= (sleep_mean + sleep_sd), 'SD+1', 'Center'))

pOW_sleep_pFA_plot = ggplot(UAE_goodNBack_sleep_modDat, aes(y = pFA_sleep, x = pOW, Group = factor(CSHQ_Total_no16_cat))) +
  geom_smooth(method = 'lm', formula = y~x, aes(color = factor(CSHQ_Total_no16_cat))) +
  geom_point(aes(color = factor(CSHQ_Total_no16_cat)), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Nback performance (adjusting for covariates)') +
  scale_y_continuous(name=paste('Percent False Alarms', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("mediumpurple1", "purple", "purple4")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

## Exploratory with SDQ ####
SDQ_mean <- mean(UAE_goodNBack_SDQ_modDat$SDQ_TotalProb_raw, na.rm = TRUE)
SDQ_sd <- sd(UAE_goodNBack_SDQ_modDat$SDQ_TotalProb_raw, na.rm = TRUE)
UAE_goodNBack_SDQ_modDat$SDQ_TotalProb_raw_cat <- ifelse(UAE_goodNBack_SDQ_modDat$SDQ_TotalProb_raw <= (SDQ_mean - SDQ_sd), 'SD-1', ifelse(UAE_goodNBack_SDQ_modDat$SDQ_TotalProb_raw >= (SDQ_mean + SDQ_sd), 'SD+1', 'Center'))

pOW_SDQ_pFA_plot = ggplot(UAE_goodNBack_SDQ_modDat, aes(y = pFA_SDQ, x = pOW, Group = factor(SDQ_TotalProb_raw_cat))) +
  geom_smooth(method = 'lm', formula = y~x, aes(color = factor(SDQ_TotalProb_raw_cat))) +
  geom_point(aes(color = factor(SDQ_TotalProb_raw_cat)), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Nback performance (adjusting for covariates)') +
  scale_y_continuous(name=paste('Ballanced Accuracy', '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("mediumpurple1", "purple", "purple4")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())


## sleep ####
Nback_sleep_pFA_plot = ggplot(UAE_goodNBack_sleepmodDat, aes(y = pFA_sleep_pred, x = CSHQ_Total_no16, Group = Load, color = Load)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(aes(color = Load), size = 3) +
  ggtitle("Association between Sleep and Nback False Alarm Percent (adjusting for covariates)") +
  scale_y_continuous(name=paste("Adjusted False Alarm Percent", '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Total Sleep') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

Nback_sleep_dPrime_plot = ggplot(UAE_goodNBack_sleepmodDat, aes(y = dPrime_sleep_pred, x = CSHQ_Total_no16, Group = Load, color = Load)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(aes(color = Load), size = 3) +
  ggtitle("Association between Sleep and Nback d' (adjusting for covariates)") +
  scale_y_continuous(name=paste("Adjusted d'", '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Total Sleep') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

Nback_SDQtotal_pFA_plot = ggplot(UAE_goodNBack_SDQmodDat, aes(y = pFA_sdq_pred, x = SDQ_TotalProb_raw, Group = Load, color = Load)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(aes(color = Load), size = 3) +
  ggtitle("Association between SDQ Total and Nback Percent False Alarms (adjusting for covariates)") +
  scale_y_continuous(name=paste("Adjusted Percent False Alarms", '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Total SDQ Problems') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

Nback_SDQhyper_pFA_plot = ggplot(UAE_goodNBack_SDQhypermodDat, aes(y = pFA_sdqhyper_pred, x = SDQ_HyperactiveProb_raw, Group = Load, color = Load)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(aes(color = Load), size = 3) +
  ggtitle("Association between SDQ Hyperactive and Nback Percent False Alarms (adjusting for covariates)") +
  scale_y_continuous(name=paste("Adjusted Percent False Alarms", '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Hyperactive SDQ Problems') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

Nback_SDQemot_pFA_plot = ggplot(UAE_goodNBack_SDQemotmodDat, aes(y = pFA_sdqemot_pred, x = SDQ_EmotionProb_raw, Group = Load, color = Load)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(aes(color = Load), size = 3) +
  ggtitle("Association between SDQ Emotional and Nback Percent False Alarms (adjusting for covariates)") +
  scale_y_continuous(name=paste("Adjusted Percent False Alarms", '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Emotional SDQ Problems') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

Nback_SDQconduct_pFA_plot = ggplot(UAE_goodNBack_SDQconductmodDat, aes(y = pFA_sdqconduct_pred, x = SDQ_ConductProb_raw, Group = Load, color = Load)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_point(aes(color = Load), size = 3) +
  ggtitle("Association between SDQ Conduct and Nback Percent False Alarms (adjusting for covariates)") +
  scale_y_continuous(name=paste("Adjusted Percent False Alarms", '\n', 
                                '(income, mother ed, and age)')) +
  scale_x_continuous(name='Conduct SDQ Problems') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())
