# This script was written by Alaina Pearce in 2021
# to set up/organize data for the paper examining
# cognitive function in Emirati children
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
#

####        Basic Data Load/Setup       ####
# load libraries - uncomment if running separately
# library(lubridate)
# library(reshape2)

# source data load and clean ####

# summary data
UAE_sumDat <- read.csv('Data/UAE_cogBMI_summary_10.1.21.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))

# demographic data 
UAE_demoDat <- read.csv('Data/UAE_cogBMI_demo_10.1.21.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
names(UAE_demoDat)[35] <- c('Month_AED')

## may use as moderators to test hypothesis from USA paper

#mental health
UAE_SDQDat <- read.csv('Data/UAE_cogBMI_SDQ_10.1.21.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
names(UAE_SDQDat)[c(5:16)] <- c('SDQ_EmotionProb_raw', 'SDQ_ConductProb_raw', 'SDQ_HyperactiveProb_raw', 'SDQ_PeerProb_raw',
                                'SDQ_Prosocial_raw', 'SDQ_TotalProb_raw', 'SDQ_TotalProb_cat', 'SDQ_EmotionProb_cat',
                                'SDQ_ConductProb_cat', 'SDQ_HyperactivityProb_cat', 'SDQ_PeerProb_cat', 'SDQ_Prosocial_cat') 

#sleep
UAE_CSHQ_RawDat <- read.csv('Data/UAE_cogBMI_CSHQ_rawResponses.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_CSHQDat <- UAE_CSHQ_RawDat[c(1:4, 38:40)]
UAE_CSHQDat$CSHQ_BedtimeResit <- rowSums(UAE_CSHQ_RawDat[c('Question1', 'Question2',
                                                           'Question9', 'Question10',
                                                           'Question11')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_SleepOnsetDelay <- UAE_CSHQ_RawDat$Question3
UAE_CSHQDat$CSHQ_SleepDuration <- rowSums(UAE_CSHQ_RawDat[c('Question13', 'Question4',
                                                            'Question5')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_SleepAnxiety <- rowSums(UAE_CSHQ_RawDat[c('Question11', 'Question14',
                                                           'Question12', 'Question15')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_NightWaking <- rowSums(UAE_CSHQ_RawDat[c('Question16', 'Question17',
                                                          'Question18')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_NightWaking_no16 <- rowSums(UAE_CSHQ_RawDat[c('Question17', 'Question18')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_Parasomnias <- rowSums(UAE_CSHQ_RawDat[c('Question22', 'Question19',
                                                          'Question20', 'Question21', 
                                                          'Question23', 'Question25', 
                                                          'Question24')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_SleepDisorderBreathing <- rowSums(UAE_CSHQ_RawDat[c('Question26', 'Question27',
                                                                     'Question28')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_DaytimeSleepiness <- rowSums(UAE_CSHQ_RawDat[c('Question6', 'Question29',
                                                                'Question30', 'Question31',
                                                                'Question32', 'Question33',
                                                                'Question7', 'Question8')], na.rm = FALSE)
UAE_CSHQDat$CSHQ_Total <- rowSums(UAE_CSHQ_RawDat[5:37], na.rm = FALSE)
UAE_CSHQDat$CSHQ_Total_no16 <- rowSums(UAE_CSHQ_RawDat[c(5:19, 21:37)], na.rm = FALSE)

#nback
UAE_NbackDat <- read.csv('Data/NBack_Database_04-Apr-2020.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_NbackDat_long <- read.csv('Data/NBack_database_LoadLong_04-Apr-2020.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))

# compile ####
UAE_allDat <- merge(UAE_sumDat[c(1:10, 14:15, 18:21)], UAE_demoDat[c(1, 8, 11, 13, 35)], id = 'ParID', all = TRUE) 
UAE_allDat <- merge(UAE_allDat, UAE_SDQDat[c(1, 5:16)], id = 'ParID', all = TRUE) 
UAE_allDat <- merge(UAE_allDat, UAE_CSHQDat[c(1, 5:18)], id = 'ParID', all = TRUE)
UAE_allDat <- merge(UAE_allDat, UAE_sumDat[c(1, 24:30, 32:37, 39:40)], id = 'ParID', all = TRUE) 
UAE_allDat <- merge(UAE_allDat, UAE_NbackDat[c(1:4, 17, 5, 18)], id = 'ParID', all = TRUE)

####        Clean Demographic Data       ####

##sex
names(UAE_allDat)[3] <- 'sex'
UAE_allDat <- UAE_allDat[!is.na(UAE_allDat$sex), ]
UAE_allDat$sex <- factor(UAE_allDat$sex, levels = c('F', 'M'))

##weight class
UAE_allDat$IOTF_WeightStatus <- factor(UAE_allDat$IOTF_WeightStatus, 
                                       levels = c('Thinness2', 'Thinness1', 'HW', 
                                                  'Overweight', 'Obese', 'MorbidlyObese'))

#income
UAE_allDat$Month_AED <- ifelse(is.na(UAE_allDat$Month_AED), NA, 
                               ifelse(UAE_allDat$Month_AED == '<5,000AED' | UAE_allDat$Month_AED == '5,000-15,000AED' | 
                                        UAE_allDat$Month_AED == '15,000-25,000AED', '<25,000 AED', 
                                      ifelse(UAE_allDat$Month_AED == '25,000-35,000AED' | UAE_allDat$Month_AED == '35,000-45,000AED' | 
                                               UAE_allDat$Month_AED == '45,000-55,000AED',  '25,000 - 55,000 AED', 
                                             ifelse(UAE_allDat$Month_AED == '55,000-65,000AED' | 
                                                      UAE_allDat$Month_AED == '65,000-75,000AED', '55,000 - 75,000 AED', '> 75,000 AED'))))

UAE_allDat$Month_AED <- factor(UAE_allDat$Month_AED, 
                               levels = c('<25,000 AED', '25,000 - 55,000 AED', '55,000 - 75,000 AED', '> 75,000 AED'))

##nationality
UAE_allDat$DadNationality <- factor(UAE_allDat$DadNationality, 
                                    levels = c('Emirati', 'Omani', 'Yemeni'))
UAE_allDat$MomNationality <- factor(UAE_allDat$MomNationality, 
                                    levels = c('Emirati', 'Omani', 'Yemeni',
                                               'Moroccan', 'Egyptian', 'Bahrani'))

##IOTF BMI25
UAE_allDat$IOTF_pOWcutoff <- 100*(UAE_allDat$BMI/UAE_allDat$IOTF_BMI25)

##waist:hip ratio
UAE_allDat$hw_ratio <- UAE_allDat$Hips_cm/UAE_allDat$Waist_cm

##reduced weight status
UAE_allDat$IOTF_3class <- ifelse(UAE_allDat$IOTF_WeightStatus == 'MorbidlyObese' | UAE_allDat$IOTF_WeightStatus == 'Obese', 'OB', 
                                 ifelse(UAE_allDat$IOTF_WeightStatus == 'Overweight', 'OW', 'HW'))
UAE_allDat$IOTF_3class <- factor(UAE_allDat$IOTF_3class, 
                                 levels = c('HW', 'OW', 'OB'))

####        Clean Nback Data       ####

UAE_allDat <- UAE_allDat[UAE_allDat$ParID != 1 & UAE_allDat$ParID != 2, ]
UAE_allDat$BothLoads <- ifelse(!is.na(UAE_allDat$B1_nTrials) & !is.na(UAE_allDat$B2_nTrials), 'Y', "N")
UAE_allDat$BothPractices <- ifelse(UAE_allDat$B1_Practice == 'Y' & UAE_allDat$B2_Practice == 'Y', 'Y', "N")

UAE_goodNBackDat <- UAE_allDat[UAE_allDat$BothLoads == 'Y' & UAE_allDat$BothPractices == 'Y', ]

UAE_goodNBackDat_long <- merge(UAE_goodNBackDat[c(1:61, 68:70)], UAE_NbackDat_long, id = 'ParID', all.x = TRUE, all.y = FALSE)