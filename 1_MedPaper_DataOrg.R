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
#

####        Basic Data Load/Setup       ####
# load libraries - uncomment if running separately
# library(lubridate)

# source data load and clean ####
UAE_sumDat <- read.csv('Data/UAE_cogBMI_summary_10.1.21.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))

#demographics/family history
UAE_demoDat <- read.csv('Data/UAE_cogBMI_demo_10.1.21.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
names(UAE_demoDat)[c(9, 15:16, 19:30, 32, 34, 35)] <- c('P_Sex', 'P_Height', 'P_Weight', 'P_WeightStatus', 'Fam_OB_YN', 'Fam_OB_list', 'Fam_ED_YN', 'Fam_ED_list', 'Child_CronicIll_YN', 'Child_CronicIll_list','P_childHeight', 'P_childWeight', 'Child_Hist_OW', 'Child_Hist_ED', 'TV_hrday', 'VG_Comp_hrday', 'Media_hrday', 'Month_AED')

UAE_healthDat <- read.csv('Data/UAE_cogBMI_health_10.1.21.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
names(UAE_healthDat)[c(20:21)] <- c('ThyroidConditions', 'GlycemicStatus')

#mental health
UAE_SDQDat <- read.csv('Data/UAE_cogBMI_SDQ_10.1.21.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
names(UAE_SDQDat)[c(5:16)] <- c('SDQ_EmotionProb_raw', 'SDQ_ConductProb_raw', 'SDQ_HyperactiveProb_raw', 'SDQ_PeerProb_raw', 'SDQ_Prosocial_raw', 'SDQ_TotalProb_raw', 'SDQ_TotalProb_cat', 'SDQ_EmotionProb_cat', 'SDQ_ConductProb_cat', 'SDQ_HyperactivityProb_cat', 'SDQ_PeerProb_cat', 'SDQ_Prosocial_cat') 

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

# compile ####
UAE_allDat <- merge(UAE_sumDat[c(1:22, 24:30, 32:37, 39:40)], UAE_demoDat[c(1, 5:11, 13, 15:35)], id = 'ParID', all = TRUE)
UAE_allDat <- merge(UAE_allDat, UAE_healthDat[c(1, 17:30)], id = 'ParID', all = TRUE) 
UAE_allDat <- merge(UAE_allDat, UAE_SDQDat[c(1, 5:16)], id = 'ParID', all = TRUE) 
UAE_allDat <- merge(UAE_allDat, UAE_CSHQDat[c(1, 5:18)], id = 'ParID', all = TRUE)

####        Clean Demographic Data       ####

##sex
names(UAE_allDat)[5] <- 'sex'
UAE_allDat <- UAE_allDat[!is.na(UAE_allDat$sex), ]
UAE_allDat$sex <- factor(UAE_allDat$sex, levels = c('F', 'M'))

##weight class
UAE_allDat$IOTF_WeightStatus <- factor(UAE_allDat$IOTF_WeightStatus, 
                                       levels = c('Thinness2', 'Thinness1', 'HW', 
                                                  'Overweight', 'Obese', 'MorbidlyObese'))

#income
UAE_allDat$Month_AED <- ifelse(is.na(UAE_allDat$Month_AED), NA,  ifelse(UAE_allDat$Month_AED == '<5,000AED' | UAE_allDat$Month_AED == '5,000-15,000AED' |  UAE_allDat$Month_AED == '15,000-25,000AED', '<25,000 AED', ifelse(UAE_allDat$Month_AED == '25,000-35,000AED' | UAE_allDat$Month_AED == '35,000-45,000AED' | UAE_allDat$Month_AED == '45,000-55,000AED',  '25,000 - 55,000 AED', ifelse(UAE_allDat$Month_AED == '55,000-65,000AED' | UAE_allDat$Month_AED == '65,000-75,000AED', '55,000 - 75,000 AED', '> 75,000 AED'))))

UAE_allDat$Month_AED <- factor(UAE_allDat$Month_AED,  levels = c('<25,000 AED', '25,000 - 55,000 AED', '55,000 - 75,000 AED', '> 75,000 AED'))

##nationality
UAE_allDat$DadNationality <- factor(UAE_allDat$DadNationality, levels = c('Emirati', 'Omani', 'Yemeni'))
UAE_allDat$MomNationality <- factor(UAE_allDat$MomNationality,  levels = c('Emirati', 'Omani', 'Yemeni','Moroccan', 'Egyptian', 'Bahrani'))

##IOTF BMI25
UAE_allDat$IOTF_pOWcutoff <- 100*(UAE_allDat$BMI/UAE_allDat$IOTF_BMI25)

##waist:hip ratio
UAE_allDat$hw_ratio <- UAE_allDat$Hips_cm/UAE_allDat$Waist_cm

##reduced weight status
UAE_allDat$IOTF_3class <- ifelse(UAE_allDat$IOTF_WeightStatus == 'MorbidlyObese' | UAE_allDat$IOTF_WeightStatus == 'Obese', 'OB',  ifelse(UAE_allDat$IOTF_WeightStatus == 'Overweight', 'OW', 'HW'))
UAE_allDat$IOTF_3class <- factor(UAE_allDat$IOTF_3class, levels = c('HW', 'OW', 'OB'))

####        Clean Health Data       ####

UAE_allDat$VitDdeficiency <- factor(UAE_allDat$VitDdeficiency,
                                    levels = c('Y','N'))

UAE_allDat$Anemia <- ifelse(is.na(UAE_allDat$Anemia), NA,ifelse(UAE_allDat$Anemia == 'IronDeficiency', 'Iron Deficiency Anemia (ID)', ifelse(UAE_allDat$Anemia == 'ThalassemiaMinor', 'Thalassemia Minor (TM)',ifelse(UAE_allDat$Anemia == 'IronDeficiency, ThalassemiaMinor', 'ID + TM',  ifelse(UAE_allDat$Anemia == 'IronDeficiency -G6PD', 'ID + G6PD Deficiency', ifelse(UAE_allDat$Anemia == 'G6PD', 'G6PD Deficiency', 'Unspecified Anemia'))))))

UAE_allDat$Anemia <- factor(UAE_allDat$Anemia, levels = c('Iron Deficiency Anemia (ID)', 'Thalassemia Minor (TM)','G6PD  ', 'ID + TM', 'ID + G6PD Deficiency', 'Unspecified Anemia'))

UAE_allDat$Hyperlipidemia <- ifelse(is.na(UAE_allDat$Hyperlipidemia), NA, ifelse(UAE_allDat$Hyperlipidemia == 'mixed Hyperlipidemia', 'Hyperlipidemia - Mixed',ifelse(UAE_allDat$Hyperlipidemia == 'Hyperlipidemia', 'Hyperlipidemia', 'Error')))

UAE_allDat$Hyperlipidemia <- factor(UAE_allDat$Hyperlipidemia, levels = c('Hyperlipidemia', 'Hyperlipidemia - Mixed'))

UAE_allDat$ThyroidConditions <- ifelse(is.na(UAE_allDat$ThyroidConditions), NA,ifelse(UAE_allDat$ThyroidConditions == 'AbnormalThyroidTunctions_notx', 'Abnormal Function',ifelse(UAE_allDat$ThyroidConditions == 'AutoimmuneThyroiditis', 'Autoimmune Thyroiditis', ifelse(UAE_allDat$ThyroidConditions == 'goiter', 'Goiter', ifelse(UAE_allDat$ThyroidConditions == 'Hypothyroidism-autoimmune', 'Autoimmune Thyroiditis', ifelse(UAE_allDat$ThyroidConditions == 'Hypothyroidism-unspecified', 'Unspecified Hypothyroidism','Error'))))))

UAE_allDat$ThyroidConditions <- factor(UAE_allDat$ThyroidConditions, levels = c('Abnormal Function', 'Autoimmune Thyroiditis','Autoimmune Hypothyroidism', 'Unspecified Hypothyroidism','Goiter'))

UAE_allDat$GlycemicStatus <- ifelse(is.na(UAE_allDat$GlycemicStatus), NA,ifelse(UAE_allDat$GlycemicStatus == 'ImpairedFastingGlucose', 'Impaired Fasting Glucose', ifelse(UAE_allDat$GlycemicStatus == 'ImpairedGlucoseToleranceTest', 'Impaired Glucose Tolerance Test', ifelse(UAE_allDat$GlycemicStatus == 'Type1Diabetes', 'Type-1 Diabetes', 'Error'))))

UAE_allDat$GlycemicStatus <- factor(UAE_allDat$GlycemicStatus, levels = c('Impaired Fasting Glucose', 'Impaired Glucose Tolerance Test', 'Type-1 Diabetes'))


UAE_allDat$Hypertension <- ifelse(is.na(UAE_allDat$Hypertension), NA, ifelse(UAE_allDat$Hypertension == 'EssentialPrimaryHypertension', 'Essential Primary Hypertension',  ifelse(UAE_allDat$Hypertension == 'HighBloodPressure', 'High Blood Pressure', 'Error')))

UAE_allDat$Hypertension <- factor(UAE_allDat$Hypertension, levels = c('Essential Primary Hypertension', 'High Blood Pressure'))

UAE_allDat$MetabolicSyndrome <- ifelse(is.na(UAE_allDat$MetabolicSyndrome), NA, ifelse(UAE_allDat$MetabolicSyndrome == 'Y', 'Metabolic Syndrome', 'Error'))

UAE_allDat$MetabolicSyndrome <- factor(UAE_allDat$MetabolicSyndrome, levels = c('Metabolic Syndrome'))

UAE_allDat$AcanthosisNigricans <- ifelse(is.na(UAE_allDat$AcanthosisNigricans), NA, ifelse(UAE_allDat$AcanthosisNigricans == 'AcanthosisNigricans', 'Acanthosis Nigricans', 'Error'))

UAE_allDat$AcanthosisNigricans <- factor(UAE_allDat$AcanthosisNigricans, levels = c('Acanthosis Nigricans'))

UAE_allDat$Growth.Stature <- ifelse(is.na(UAE_allDat$Growth.Stature), NA, ifelse(UAE_allDat$Growth.Stature == 'FailureToThrive', 'Failure To Thrive (FT)', ifelse(UAE_allDat$Growth.Stature == 'GrowthHormoneDeficency', 'Growth Hormone Deficency', ifelse(UAE_allDat$Growth.Stature == 'ShortStature', 'Short Stature', ifelse(UAE_allDat$Growth.Stature == 'ShortStature, FailureToThrive, Underweight', 'FT + ShortStature + Underweight', ifelse(UAE_allDat$Growth.Stature == 'ShortStature, PrecociousPuberty', 'Short Stature + Precocious Puberty', 'Error'))))))

UAE_allDat$Growth.Stature <- factor(UAE_allDat$Growth.Stature, levels = c('Failure To Thrive (FT)', 'Growth Hormone Deficency', 'Short Stature', 'FT + ShortStature + Underweight', 'Short Stature + Precocious Puberty'))

UAE_allDat$PCOS <- ifelse(is.na(UAE_allDat$PCOS), NA, ifelse(UAE_allDat$PCOS == 'PCOS', 'PCOS', ifelse(UAE_allDat$PCOS == 'Hirsutism', 'Hirsutism', ifelse(UAE_allDat$PCOS == 'hirsutism, unspecified ovarian cysts', 'Hirsutism + Unspecified Ovarian Cysts', 'Error'))))

UAE_allDat$PCOS <- factor(UAE_allDat$PCOS, levels = c('PCOS', 'Hirsutism', 'Hirsutism + Unspecified Ovarian Cysts'))

#number of comorbid conditions
UAE_allDat$nComorbid = rowSums(!is.na(UAE_allDat[c(66:69, 71:73, 75:76)])) + as.logical(UAE_allDat$VitDdeficiency == 'Y')

####        Clean Family History Data       ####

##respondent/person completing questionnaires
UAE_allDat$Relationship <- factor(UAE_allDat$Relationship,  levels = c('Mother', 'Father', 'Uncle'))

##Obesity
UAE_allDat$Fam_OB_YN <- factor(UAE_allDat$Fam_OB_YN, levels = c('yes', 'no'))

UAE_allDat$Fam_OB_list <- as.character(UAE_allDat$Fam_OB_list)
UAE_allDat$Mother_OBhistory <- ifelse(grepl('1', UAE_allDat$Fam_OB_list), 'Mother', 'N')
UAE_allDat$Father_OBhistory <- ifelse(grepl('2', UAE_allDat$Fam_OB_list), 'Father', 'N')
UAE_allDat$Grandmother_OBhistory <- ifelse(grepl('3', UAE_allDat$Fam_OB_list), 'Grandmother', 'N')
UAE_allDat$Grandfather_OBhistory <- ifelse(grepl('4', UAE_allDat$Fam_OB_list), 'Grandfather', 'N')
UAE_allDat$Sister_OBhistory <- ifelse(grepl('5', UAE_allDat$Fam_OB_list), 'Sister', 'N')
UAE_allDat$Brother_OBhistory <- ifelse(grepl('6', UAE_allDat$Fam_OB_list), 'Brother', 'N')
UAE_allDat$Aunt_OBhistory <- ifelse(grepl('7', UAE_allDat$Fam_OB_list), 'Aunt', 'N')
UAE_allDat$Uncle_OBhistory <- ifelse(grepl('8', UAE_allDat$Fam_OB_list), 'Uncle', 'N')

##number Obese
UAE_allDat$nFam_Obesity <- rowSums(UAE_allDat[c('Mother_OBhistory', 'Father_OBhistory', 'Grandmother_OBhistory', 'Grandfather_OBhistory', 'Sister_OBhistory', 'Brother_OBhistory',  'Aunt_OBhistory', 'Uncle_OBhistory')] != 'N')

##Eating Disorder
UAE_allDat$Fam_ED_YN <- factor(UAE_allDat$Fam_ED_YN,  levels = c('yes', 'no'))

UAE_allDat$Fam_ED_list <- as.character(UAE_allDat$Fam_ED_list)
UAE_allDat$Mother_EDhistory <- ifelse(grepl('1', UAE_allDat$Fam_ED_list), 'Mother', 'N')
UAE_allDat$Father_EDhistory <- ifelse(grepl('2', UAE_allDat$Fam_ED_list), 'Father', 'N')
UAE_allDat$Grandmother_EDhistory <- ifelse(grepl('3', UAE_allDat$Fam_ED_list), 'Grandmother', 'N')
UAE_allDat$Grandfather_EDhistory <- ifelse(grepl('4', UAE_allDat$Fam_ED_list), 'Grandfather', 'N')
UAE_allDat$Sister_EDhistory <- ifelse(grepl('5', UAE_allDat$Fam_ED_list), 'Sister', 'N')
UAE_allDat$Brother_EDhistory <- ifelse(grepl('6', UAE_allDat$Fam_ED_list), 'Brother', 'N')
UAE_allDat$Aunt_EDhistory <- ifelse(grepl('7', UAE_allDat$Fam_ED_list), 'Aunt', 'N')
UAE_allDat$Uncle_EDhistory <- ifelse(grepl('8', UAE_allDat$Fam_ED_list), 'Uncle', 'N')

##number Obese
UAE_allDat$nFam_EatingDisorder <- rowSums(UAE_allDat[c('Mother_EDhistory', 'Father_EDhistory', 'Grandmother_EDhistory', 'Grandfather_EDhistory', 'Sister_EDhistory', 'Brother_EDhistory', 'Aunt_EDhistory', 'Uncle_EDhistory')] != 'N' & !is.na(UAE_allDat[c('Mother_EDhistory', 'Father_EDhistory', 'Grandmother_EDhistory', 'Grandfather_EDhistory', 'Sister_EDhistory', 'Brother_EDhistory', 'Aunt_EDhistory', 'Uncle_EDhistory')]))



#parent reported weight status
UAE_allDat$P_WeightStatus <- ifelse(UAE_allDat$P_WeightStatus == 'Overweight' | UAE_allDat$P_WeightStatus == 'Overweight ' |  UAE_allDat$P_WeightStatus == 'Overeweight', 'Overweight', as.character(UAE_allDat$P_WeightStatus))
UAE_allDat$P_WeightStatus <- factor(UAE_allDat$P_WeightStatus, levels = c('Healthy Weight', 'Overweight', 'Obese'))

####        Clean Media Use Data       ####

#total media use (including extra parent notes)

#add tv and video games together
UAE_allDat$Media_hrday <- ifelse(UAE_allDat$TV_hrday == 'none' & UAE_allDat$VG_Comp_hrday == 'none', 'less than 1 hr',  ifelse(UAE_allDat$TV_hrday == '<1',  ifelse(is.na(UAE_allDat$VG_Comp_hrday) | UAE_allDat$VG_Comp_hrday == 'none', 'less than 1 hr', ifelse(UAE_allDat$VG_Comp_hrday == '<1', 'less than 1 hr',ifelse(UAE_allDat$VG_Comp_hrday == '1', '1+', ifelse(UAE_allDat$VG_Comp_hrday == '2', '2+', ifelse(UAE_allDat$VG_Comp_hrday == '2.5', '3', ifelse(UAE_allDat$VG_Comp_hrday == '3', '3+', ifelse(UAE_allDat$VG_Comp_hrday == '4', '4+', '5+'))))))), ifelse(UAE_allDat$TV_hrday == '1',  ifelse(is.na(UAE_allDat$VG_Comp_hrday) | UAE_allDat$VG_Comp_hrday == 'none', '1', ifelse(UAE_allDat$VG_Comp_hrday == '<1', '1+',  ifelse(UAE_allDat$VG_Comp_hrday == '1', '2', ifelse(UAE_allDat$VG_Comp_hrday == '2', '3', ifelse(UAE_allDat$VG_Comp_hrday == '2.5', '3.5',  ifelse(UAE_allDat$VG_Comp_hrday == '3', '4', ifelse(UAE_allDat$VG_Comp_hrday == '4', '5', '6+'))))))), ifelse(UAE_allDat$TV_hrday == '2',  ifelse(is.na(UAE_allDat$VG_Comp_hrday) | UAE_allDat$VG_Comp_hrday == 'none', '2', ifelse(UAE_allDat$VG_Comp_hrday == '<1', '2+', ifelse(UAE_allDat$VG_Comp_hrday == '1', '3',  ifelse(UAE_allDat$VG_Comp_hrday == '2', '4',  ifelse(UAE_allDat$VG_Comp_hrday == '2.5', '4.5',  ifelse(UAE_allDat$VG_Comp_hrday == '3', '5', ifelse(UAE_allDat$VG_Comp_hrday == '4', '6', '7+'))))))), ifelse(UAE_allDat$TV_hrday == '2.5', ifelse(is.na(UAE_allDat$VG_Comp_hrday) | UAE_allDat$VG_Comp_hrday == 'none', '2.5', ifelse(UAE_allDat$VG_Comp_hrday == '<1', '3',  ifelse(UAE_allDat$VG_Comp_hrday == '1', '3.5', ifelse(UAE_allDat$VG_Comp_hrday == '2', '4.5', ifelse(UAE_allDat$VG_Comp_hrday == '2.5', '5',  ifelse(UAE_allDat$VG_Comp_hrday == '3', '5.5', ifelse(UAE_allDat$VG_Comp_hrday == '4', '6.5', '7+'))))))),  ifelse(UAE_allDat$TV_hrday == '3',   ifelse(is.na(UAE_allDat$VG_Comp_hrday) | UAE_allDat$VG_Comp_hrday == 'none', '3',  ifelse(UAE_allDat$VG_Comp_hrday == '<1', '3.5', ifelse(UAE_allDat$VG_Comp_hrday == '1', '4', ifelse(UAE_allDat$VG_Comp_hrday == '2', '5', ifelse(UAE_allDat$VG_Comp_hrday == '2.5', '5.5', ifelse(UAE_allDat$VG_Comp_hrday == '3', '6', ifelse(UAE_allDat$VG_Comp_hrday == '4', '7', '8+'))))))), ifelse(UAE_allDat$TV_hrday == '4',  ifelse(is.na(UAE_allDat$VG_Comp_hrday) | UAE_allDat$VG_Comp_hrday == 'none', '4', ifelse(UAE_allDat$VG_Comp_hrday == '<1', '4.5',  ifelse(UAE_allDat$VG_Comp_hrday == '1', '5', ifelse(UAE_allDat$VG_Comp_hrday == '2', '6', ifelse(UAE_allDat$VG_Comp_hrday == '2.5', '6.5', ifelse(UAE_allDat$VG_Comp_hrday == '3', '7', ifelse(UAE_allDat$VG_Comp_hrday == '4', '8', '9+'))))))), ifelse(UAE_allDat$TV_hrday == '5+', ifelse(is.na(UAE_allDat$VG_Comp_hrday) | UAE_allDat$VG_Comp_hrday == 'none', '5+', ifelse(UAE_allDat$VG_Comp_hrday == '<1', '5+', ifelse(UAE_allDat$VG_Comp_hrday == '1', '6+', ifelse(UAE_allDat$VG_Comp_hrday == '2', '7+', ifelse(UAE_allDat$VG_Comp_hrday == '2.5', '7+',  ifelse(UAE_allDat$VG_Comp_hrday == '3', '8+', ifelse(UAE_allDat$VG_Comp_hrday == '4', '9+', '10+'))))))), ifelse(is.na(UAE_allDat$TV_hrday) | UAE_allDat$TV_hrday == 'none', as.character(UAE_allDat$VG_Comp_hrday), 'error')))))))))

#adjust for parent notes
UAE_allDat$Media_hrday <- ifelse(is.na(UAE_allDat$HoursDay_TVnotes), as.character(UAE_allDat$Media_hrday), ifelse(UAE_allDat$HoursDay_TVnotes == '\"ipad all the time\"' | UAE_allDat$HoursDay_TVnotes == '\"on phone all day\"' |  UAE_allDat$HoursDay_TVnotes == '\"social media all day\"', '10+',  ifelse(UAE_allDat$HoursDay_TVnotes == '(phone for 2 hour)',  ifelse(is.na(UAE_allDat$Media_hrday) | UAE_allDat$Media_hrday == 'none', '2', ifelse(UAE_allDat$Media_hrday == 'less than 1 hr', '2+',  ifelse(UAE_allDat$Media_hrday == '1', '3', ifelse(UAE_allDat$Media_hrday == '1+', '3+', ifelse(UAE_allDat$Media_hrday == '2', '4', ifelse(UAE_allDat$Media_hrday == '2.5', '4.5', ifelse(UAE_allDat$Media_hrday == '2+', '4+',  ifelse(UAE_allDat$Media_hrday == '3', '5', ifelse(UAE_allDat$Media_hrday == '3.5', '5.5', ifelse(UAE_allDat$Media_hrday == '3+', '5+', ifelse(UAE_allDat$Media_hrday == '4', '6', ifelse(UAE_allDat$Media_hrday == '4.5', '6.5', ifelse(UAE_allDat$Media_hrday == '4+', '6+', ifelse(UAE_allDat$Media_hrday == '5', '7',  ifelse(UAE_allDat$Media_hrday == '5.5', '7.5', ifelse(UAE_allDat$Media_hrday == '5+', '7+', ifelse(UAE_allDat$Media_hrday == '6+', '8+', ifelse(UAE_allDat$Media_hrday == '7+', '9+', '10+')))))))))))))))))),  ifelse(UAE_allDat$HoursDay_TVnotes == "3-Feb" | UAE_allDat$HoursDay_TVnotes == "he does not watch TV on an average school day, 1, 2", as.character(UAE_allDat$Media_hrday), as.character(UAE_allDat$Media_hrday)))))

#factor into better groups
UAE_allDat$Media_hrday <- ifelse(UAE_allDat$Media_hrday == 'less than 1 hr' | UAE_allDat$Media_hrday == '1' | UAE_allDat$Media_hrday == '1+' , 'less than 2 hr', ifelse(UAE_allDat$Media_hrday == '2' | UAE_allDat$Media_hrday == '2+' | UAE_allDat$Media_hrday == '3' | UAE_allDat$Media_hrday == '3.5' | UAE_allDat$Media_hrday == '3+', '2 - 3 hours',  ifelse(UAE_allDat$Media_hrday == '4' | UAE_allDat$Media_hrday == '4.5' | UAE_allDat$Media_hrday == '4+' | UAE_allDat$Media_hrday == '5' | UAE_allDat$Media_hrday == '5.5' | UAE_allDat$Media_hrday == '5+', '4 - 5 hours', '6+ hours')))

#factor
UAE_allDat$Media_hrday <- factor(UAE_allDat$Media_hrday,  levels = c('less than 2 hr', '2 - 3 hours', '4 - 5 hours', '6+ hours'))

# TV use
UAE_allDat$TV_hrday <- ifelse(is.na(UAE_allDat$TV_hrday), NA,  ifelse(UAE_allDat$TV_hrday == 'none' | UAE_allDat$TV_hrday == '<1', 'less than 1 hr', ifelse(UAE_allDat$TV_hrday == '1' | UAE_allDat$TV_hrday == '2' | UAE_allDat$TV_hrday == '2.5', '1 - 2 hours', ifelse(UAE_allDat$TV_hrday == '3' | UAE_allDat$TV_hrday == '4', '3 - 4 hours', '5+ hours'))))
UAE_allDat$TV_hrday <- factor(UAE_allDat$TV_hrday, levels = c('less than 1 hr', '1 - 2 hours', '3 - 4 hours', '5+ hours'))

# Video Game/Computer use
UAE_allDat$VG_Comp_hrday <- ifelse(is.na(UAE_allDat$VG_Comp_hrday), NA, ifelse(UAE_allDat$VG_Comp_hrday == 'none' | UAE_allDat$VG_Comp_hrday == '<1', 'less than 1 hr', ifelse(UAE_allDat$VG_Comp_hrday == '1' | UAE_allDat$VG_Comp_hrday == '2' | UAE_allDat$VG_Comp_hrday == '2.5', '1 - 2 hours', ifelse(UAE_allDat$VG_Comp_hrday == '3' | UAE_allDat$VG_Comp_hrday == '4', '3 - 4 hours', '5+ hours'))))

UAE_allDat$VG_Comp_hrday <- factor(UAE_allDat$VG_Comp_hrday,levels = c('less than 1 hr', '1 - 2 hours', '3 - 4 hours', '5+ hours'))


####        Clean CSHQ Sleep Data       ####

#add ':00' to times on the hour and set 'any' to NA
UAE_allDat$Bedtime_28hr <- ifelse(UAE_allDat$Bedtime_24hr == 'any' | is.na(UAE_allDat$Bedtime_24hr), NA,  ifelse(UAE_allDat$Bedtime_24hr == '0:00', '00:00', ifelse(grepl(':', UAE_allDat$Bedtime_24hr), as.character(UAE_allDat$Bedtime_24hr),  paste0(UAE_allDat$Bedtime_24hr, ':00'))))

UAE_allDat$Waketime_24hr <- ifelse(is.na(UAE_allDat$Waketime_24), NA,  ifelse(grepl(':', UAE_allDat$Waketime_24), as.character(UAE_allDat$Waketime_24),  paste0(UAE_allDat$Waketime_24, ':00')))

# convert time to hr (hr + min/60)
UAE_allDat$Bedtime_28hr <- strptime(UAE_allDat$Bedtime_28hr, format = "%H:%M")
UAE_allDat$Bedtime_28hr <- hour(UAE_allDat$Bedtime_28hr) + minute(UAE_allDat$Bedtime_28hr)/60

UAE_allDat$Waketime_24hr <- strptime(UAE_allDat$Waketime_24hr, format = "%H:%M")
UAE_allDat$Waketime_24hr <- round((hour(UAE_allDat$Waketime_24hr) + minute(UAE_allDat$Waketime_24hr)/60), digits = 2)

# convert bedtime to 28 hour day so can measure bedtime continuously
UAE_allDat$Bedtime_28hr <- ifelse(is.na(UAE_allDat$Bedtime_28hr), NA, ifelse(UAE_allDat$Bedtime_28hr < 8, UAE_allDat$Bedtime_28hr + 24.00, UAE_allDat$Bedtime_28hr))


# met recommendations
UAE_allDat$Bedtime_Recommendation <- ifelse(UAE_allDat$Bedtime_28hr >= 22, 'N', 'Y')
UAE_allDat$Sleep_Recommendation <- ifelse(UAE_allDat$Age_yr <13,  ifelse(UAE_allDat$Bed_hr >=9 & UAE_allDat$Bed_hr <= 11, 'Y', 'N'), ifelse(UAE_allDat$Bed_hr >=8 & UAE_allDat$Bed_hr <= 10, 'Y', 'N'))

# bed time - categories
UAE_allDat$Bedtime_cat <- ifelse(UAE_allDat$Bedtime_28hr == 24.00, '11 - Midnight', ifelse(UAE_allDat$Bedtime_28hr > 24, 'After Midnight', ifelse(UAE_allDat$Bedtime_28hr < 21, '7 - 8 pm',  ifelse(UAE_allDat$Bedtime_28hr < 22, '9 pm', ifelse(UAE_allDat$Bedtime_28hr < 23, '10 pm', '11 - Midnight')))))


UAE_allDat$Bedtime_cat <- factor(UAE_allDat$Bedtime_cat, levels = c('7 - 8 pm', '9 pm', '10 pm', '11 - Midnight', 'After Midnight'))
