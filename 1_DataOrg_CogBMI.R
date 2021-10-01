############ Basic Data Load/Setup########
library(rstudioapi)

#set working directory to location of script--not needed when called 
#through Rmarkdown doc. Uncomment below if running locally/manually
#this.dir = getActiveDocumentContext()$path
#setwd(dirname(this.dir))


#####################################
####                            
####  Data setup      ####
####                            
#####################################
##load datasets
UAE_cogBMI_sumDat_all = read.csv('Data/UAE_cogBMI_summary_4.12.20.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_cogBMI_sumDat_all$CDC_WeightStatus = factor(UAE_cogBMI_sumDat_all$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW", "Underweight"))
UAE_cogBMI_sumDat_all$CDC_ObesityStatus = factor(UAE_cogBMI_sumDat_all$CDC_ObesityStatus, levels = c("Non-Obese", "Obese"))
UAE_cogBMI_sumDat_all$Gender = factor(UAE_cogBMI_sumDat_all$Gender, levels = c("F", "M"))
UAE_cogBMI_sumDat_all = UAE_cogBMI_sumDat_all[!is.na(UAE_cogBMI_sumDat_all$Visit_date) & 
                                                !is.na(UAE_cogBMI_sumDat_all$CDC_WeightStatus), ]

UAE_cogBMI_demoDat = read.csv('Data/UAE_cogBMI_demo_4.12.20.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_cogBMI_healthDat = read.csv('Data/UAE_cogBMI_health_4.12.20.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_cogBMI_SDQDat = read.csv('Data/UAE_cogBMI_SDQ_4.12.20.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_cogBMI_CSHQDat = read.csv('Data/UAE_cogBMI_CSHQ_4.12.20.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))

UAE_cogBMI_NBACKDat = read.csv('Data/NBack_Database_04-Apr-2020.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_cogBMI_SSTDat = read.csv('Data/SST_Database_04-Apr-2020.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_cogBMI_NBACKDat_long = read.csv('Data/NBack_database_LoadLong_04-Apr-2020.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
UAE_cogBMI_SSTDat_long = read.csv('Data/SST_database_BlockLong_04-Apr-2020.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))

##    WHO Weight Status
# wfawho2007<-read.table('who2007_r/wfawho2007.txt',header=T,sep="",skip=0) 
# hfawho2007<-read.table('who2007_r/hfawho2007.txt',header=T,sep="",skip=0) 
# bfawho2007<-read.table('who2007_r/bfawho2007.txt',header=T,sep="",skip=0)
# 
# source('who2007_r/who2007.R')
# who2007(FilePath = 'Data/', FileLab = 'UAE_cogBMI_WHO', mydf = UAE_cogBMI_sumDat, sex = Gender, weight = Weight_kg, height = Height_cm, age = Age_mo)
UAE_cogBMI_WHOz = read.csv('Data/UAE_cogBMI_WHO_Temp_z.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))
names(UAE_cogBMI_WHOz)[111] = 'WHO_BMIz'

## Merge/final dsets
UAE_cogBMI_compiled = merge(UAE_cogBMI_sumDat_all[c(1:22, 24:30, 32:37, 39:40)], UAE_cogBMI_demoDat[c(1, 5:11, 13, 15:35)], id = 'ParID', all = TRUE)
UAE_cogBMI_compiled = merge(UAE_cogBMI_compiled, UAE_cogBMI_healthDat[c(1, 20:33)], id = 'ParID', all = TRUE) 
UAE_cogBMI_compiled = merge(UAE_cogBMI_compiled, UAE_cogBMI_SDQDat[c(1, 5:16)], id = 'ParID', all = TRUE) 
UAE_cogBMI_compiled = merge(UAE_cogBMI_compiled, UAE_cogBMI_CSHQDat[c(1, 5:16, 18)], id = 'ParID', all = TRUE)
UAE_cogBMI_compiled = merge(UAE_cogBMI_compiled, UAE_cogBMI_WHOz[c(1, 111)], id = 'ParID', all = TRUE)
UAE_cogBMI_compiled = UAE_cogBMI_compiled[!is.na(UAE_cogBMI_compiled$Visit_date), ]

##### Nback Data ####
UAE_cogBMI_NBACKDat = UAE_cogBMI_NBACKDat[UAE_cogBMI_NBACKDat$ParID != 1 & UAE_cogBMI_NBACKDat$ParID != 2, ]
UAE_cogBMI_NBACKDat$BothLoads = ifelse(!is.na(UAE_cogBMI_NBACKDat$B1_nTrials) & !is.na(UAE_cogBMI_NBACKDat$B2_nTrials), 'Y', "N")
UAE_cogBMI_NBACKDat$BothPractices = ifelse(UAE_cogBMI_NBACKDat$B1_Practice == 'Y' & UAE_cogBMI_NBACKDat$B2_Practice == 'Y', 'Y', "N")

NBack_goodDat = UAE_cogBMI_NBACKDat[UAE_cogBMI_NBACKDat$BothLoads == 'Y' & UAE_cogBMI_NBACKDat$BothPractices == 'Y', ]

NBack_goodDat_long = melt(NBack_goodDat[c(1, 16, 29)], id.vars = 'ParID')
NBack_goodDat_long$BalAcc = NBack_goodDat_long$value
NBack_goodDat_long$Load = ifelse(NBack_goodDat_long$variable == 'B1_BalAcc', 'B1', 'B2')
NBack_goodDat_long$Load = factor(NBack_goodDat_long$Load)

NBack_goodDat_nACClong = melt(NBack_goodDat[c(6, 19)])
NBack_goodDat_pACClong = melt(NBack_goodDat[c(7, 20)])
NBack_goodDat_nCorlong = melt(NBack_goodDat[c(8, 21)])
NBack_goodDat_pCorlong = melt(NBack_goodDat[c(9, 22)])
NBack_goodDat_meanRTlong = melt(NBack_goodDat[c(10, 23)])
NBack_goodDat_medCorlong = melt(NBack_goodDat[c(11, 24)])
NBack_goodDat_nFAlong = melt(NBack_goodDat[c(12, 25)])
NBack_goodDat_pFAlong = melt(NBack_goodDat[c(13, 26)])
NBack_goodDat_nMisslong = melt(NBack_goodDat[c(14, 27)])
NBack_goodDat_pMisslong = melt(NBack_goodDat[c(15, 28)])

NBack_goodDat_long = data.frame(NBack_goodDat_long[c(1, 4:5)], NBack_goodDat_nACClong[2], NBack_goodDat_pACClong[2],
                                NBack_goodDat_nCorlong[2], NBack_goodDat_pCorlong[2], NBack_goodDat_meanRTlong[2],
                                NBack_goodDat_medCorlong[2], NBack_goodDat_nFAlong[2], NBack_goodDat_pFAlong[2],
                                NBack_goodDat_nMisslong[2], NBack_goodDat_pMisslong[2])
names(NBack_goodDat_long) = c(names(NBack_goodDat_long)[1:3], 'nACC', 'pACC', 'nCor', 'pCor', 'meanRT', 'medRT',
                              'nFA', 'pFA', 'nMiss', 'pMiss')

#merge
UAE_cogBMI_NBACKDat.merge = merge(NBack_goodDat, UAE_cogBMI_compiled[c(1, 2:3, 5:21, 24, 26, 28, 31, 33, 37, 43, 64, 104)], id = 'ParID', all.x = TRUE, all.y = FALSE)
UAE_cogBMI_NBACKDat.merge_long = merge(NBack_goodDat_long, UAE_cogBMI_compiled[c(1, 2:3, 5:21, 24, 26, 28, 31, 33, 37, 43, 64, 104)], id = 'ParID', all.x = TRUE, all.y = FALSE)

#### SST ####
# UAE_cogBMI_SSTDat = merge(UAE_cogBMI_sumDat_all[c(1:22, 24:30, 32:37, 39:40)], UAE_cogBMI_SSTDat, id = 'ParID', all.x = TRUE, all.y = FALSE)
# UAE_cogBMI_SSTDat_long = merge(UAE_cogBMI_sumDat_all[c(1:22, 24:30, 32:37, 39:40)], UAE_cogBMI_SSTDat_long, id = 'ParID', all.x = TRUE, all.y = FALSE)



