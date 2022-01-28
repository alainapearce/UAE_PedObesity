# This script was written by Alaina Pearce in 2021
# to set up/organize data for the paper examining
# medical, family history, and behavior in Emirati children
# by weight status
#
#     Copyright (C) 2021 Alaina L Pearce
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
# source('1_MedPaper_DataOrg.R')

####   Demo Data  Figs     ####
UAE_allDat$all_group <- 1
IOTF_pOWcutoff_plot <- ggplot(UAE_allDat, aes(x = IOTF_pOWcutoff, y = all_group, group = all_group, color = sex)) +
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


####   Family History       ####

if(!exists('IOTF_pOWcutoff_nFamOB_dat') || !is.data.frame(get('IOTF_pOWcutoff_nFamOB_dat'))){
 source('3_MedPaper_analyses.R') 
}

IOTF_pOWcutoff_nFamOB_adjOdds_plot <- ggplot(IOTF_pOWcutoff_nFamOB_dat, aes(y = exp(nFam_Obesity_probLogit), x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Corrleation between Percent of Overweight Cutoff cuttoff and Number of Family Members with History of Obesity (adjusted for covariates') +
  scale_y_continuous(name=paste0('Adjusted Odds of having an adiition family relationship', '\n',
                                'category with OB (income, mother ed, age, and sex)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

IOTF_pOWcutoff_nFamOB_raw_plot <- ggplot(UAE_allDat, aes(y = nFam_Obesity, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Corrleation between Percent of Overweight Cutoff cuttoff and Number of Family Members with History of Obesity') +
  scale_y_continuous(name='# Family Relationship Categories with OB') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())
                          
#### Sleep ####

## Sleep Onset Delay
if(!exists('CHSQ_SleepDelay_dat') || !is.data.frame(get('CHSQ_SleepDelay_dat'))){
  source('3_MedPaper_analyses.R') 
}

IOTF_pOWcutoff_SleepDelay_adj_plot <- ggplot(CHSQ_SleepDelay_dat, aes(y = CSHQ_SleepOnsetDelay_pred, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Association between Percent of Overweight Cutoff and Sleep Onset Delay (adjust for covariates)') +
  scale_y_continuous(name=paste('Adjusted Sleep Onset Delay', '\n', 
                                '(income, mother ed, age, and sex)')) +
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

## Sleep Disordered Breathing
if(!exists('CHSQ_DisorderedBreathing_dat') || !is.data.frame(get('CHSQ_DisorderedBreathing_dat'))){
  source('3_MedPaper_analyses.R') 
}

IOTF_pOWcutoff_DisorderedBreathing_adj_plot <- ggplot(CHSQ_DisorderedBreathing_dat, aes(y = CSHQ_SleepDisorderBreathing_pred, x = IOTF_pOWcutoff)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle('Association between Percent of Overweight Cutoff and Sleep Disordered Breathing (adjusted for covariates)') +
  scale_y_continuous(name=paste('Adjusted Sleep Disordered Breathing', '\n', 
                                '(income, mother ed, age, and sex)')) +
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


#### SDQ ####

## peer Problems
if(!exists('SDQ_PeerProb_dat') || !is.data.frame(get('SDQ_PeerProb_dat'))){
  source('3_MedPaper_analyses.R') 
}

IOTF_pOWcutoff_peerprob_ElevatedCat_adjOdds_plot <- ggplot(SDQ_PeerProb_dat, aes(y = exp(SDQ_PeerProb_Elevated_predLogit), x = (IOTF_pOWcutoff_c100 + 100), color = sex)) +
  geom_smooth(method = 'lm', formula = y~x, aes(group = sex, linetype = sex)) +
  geom_point(aes(shape = sex), size = 3) +
  ggtitle('Association between Percent of Overweight Cutoff and Elevated Peer Problems (adjusted for covariates)') +
  scale_y_continuous(name=paste('Adjusted Odds for Elevated Peer Problems', '\n', 
                                '(income, mother ed, age, and sex)')) +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  scale_color_manual(values = c("purple", "darkorange")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())




