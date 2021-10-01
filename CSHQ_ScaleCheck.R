library(rstudioapi)
library(psych)
library(Matrix)
library(corpcor)
library(psychTools)

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
UAE_cogBMI_CSHQ_responses = read.csv('Data/UAE_cogBMI_CSHQ_rawResponses.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "N/A"))


#scales
UAE_cogBMI_CSHQ_responses$BedtimeResistance = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question1', 'Question2', 'Question9', 'Question10', 
                                                  'Question11', 'Question12')])
UAE_cogBMI_CSHQ_responses$BedtimeResistance_no1 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question2', 'Question9', 'Question10', 
                                                                                    'Question11', 'Question12')])
UAE_cogBMI_CSHQ_responses$BedtimeResistance_no1.10 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question1', 'Question2', 'Question9', 
                                                                                    'Question11', 'Question12')])

UAE_cogBMI_CSHQ_responses$SleepOnsetDelay = UAE_cogBMI_CSHQ_responses[, c('Question3')]

UAE_cogBMI_CSHQ_responses$SleepDuration = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question13', 'Question4', 'Question5')])

UAE_cogBMI_CSHQ_responses$SleepAnxiety = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question11', 'Question14', 'Question12', 'Question15')])
UAE_cogBMI_CSHQ_responses$SleepAnxiety_no15 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question11', 'Question14', 'Question12')])

UAE_cogBMI_CSHQ_responses$NightWakings = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question16', 'Question17', 'Question18')])
UAE_cogBMI_CSHQ_responses$NightWakings_no16 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question17', 'Question18')])

UAE_cogBMI_CSHQ_responses$Parasomnias = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question22', 'Question19', 'Question20', 'Question21', 
                                            'Question23', 'Question25', 'Question24')])
UAE_cogBMI_CSHQ_responses$Parasomnias_no20 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question22', 'Question19', 'Question21', 
                                                                              'Question23', 'Question25', 'Question24')])
UAE_cogBMI_CSHQ_responses$Parasomnias_no20.22 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question19', 'Question21',
                                                                                   'Question23', 'Question25', 'Question24')])


UAE_cogBMI_CSHQ_responses$SleepDisorderedBreathing = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question26', 'Question27', 'Question28')])
UAE_cogBMI_CSHQ_responses$SleepDisorderedBreathing_no26 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question27', 'Question28')])

UAE_cogBMI_CSHQ_responses$DaytimeSleepiness = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question6', 'Question29', 'Question30', 'Question31', 
                                                  'Question32', 'Question33', 'Question7', 'Question8')])
UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question6', 'Question29', 'Question30', 'Question31', 
                                                                                    'Question32', 'Question33', 'Question7')])
UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8.7 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question6', 'Question29', 'Question30', 'Question31', 
                                                                                        'Question32', 'Question33')])
UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8.7.6 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question29', 'Question30', 'Question31', 
                                                                                          'Question32', 'Question33')])

UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8.7.6.30 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question29', 'Question31', 
                                                                                            'Question32', 'Question33')])

UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_8.7.6.30 = rowSums(UAE_cogBMI_CSHQ_responses[, c('Question6', 'Question30',
                                                                                     'Question7', 'Question8')])

##alpha
BedtimeResistance_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$BedtimeResistance), c('Question1', 'Question2', 'Question9', 'Question10', 
                                                                                    'Question11', 'Question12')])

BedtimeResistance_alpha_no1 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$BedtimeResistance_no1), c('Question2', 'Question9', 'Question10', 
                                                                                                                 'Question11', 'Question12')])

BedtimeResistance_alpha_no1.10 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$BedtimeResistance_no1.10), c('Question2', 'Question9', 
                                                                                                                     'Question11', 'Question12')])

SleepDuration_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$SleepDuration), c('Question13', 'Question4', 'Question5')])


SleepAnxiety_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$SleepAnxiety), c('Question11', 'Question14', 'Question12', 'Question15')])
SleepAnxiety_alpha_no15 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$SleepAnxiety_no15), c('Question11', 'Question14', 'Question12')])

NightWakings_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$NightWakings), c('Question16', 'Question17', 'Question18')])
NightWakings_no16_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$NightWakings_no16), c('Question17', 'Question18')])

Parasomnias_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$Parasomnias), c('Question22', 'Question19', 'Question20', 'Question21', 
                                                                              'Question23', 'Question25', 'Question24')])
Parasomnias_alpha_no20 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$Parasomnias_no20), c('Question22', 'Question19', 'Question21', 
                                                        'Question23', 'Question25', 'Question24')])
Parasomnias_alpha_no20.22 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$Parasomnias_no20.22), c('Question19', 'Question21', 
                                                                                                               'Question23', 'Question25', 'Question24')])

SleepDisorderedBreathing_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$SleepDisorderedBreathing), c('Question26', 'Question27', 'Question28')])
SleepDisorderedBreathing_alpha_no26 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$SleepDisorderedBreathing_no26), c('Question27', 'Question28')])

DaytimeSleepiness_alpha = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$DaytimeSleepiness), c('Question6', 'Question29', 'Question30', 'Question31', 
                                                                                    'Question32', 'Question33', 'Question7', 'Question8')])
DaytimeSleepiness_alpha_no8 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8), c('Question6', 'Question29', 'Question30', 'Question31', 
                                                              'Question32', 'Question33', 'Question7')])
DaytimeSleepiness_alpha_no8.7 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8.7), c('Question6', 'Question29', 'Question30', 'Question31', 
                                                                                                                         'Question32', 'Question33')])
DaytimeSleepiness_alpha_no8.7.6 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8.7.6), c('Question29', 'Question30', 'Question31', 
                                                                                                                             'Question32', 'Question33')])
DaytimeSleepiness_alpha_no8.7.6.30 = alpha(UAE_cogBMI_CSHQ_responses[!is.na(UAE_cogBMI_CSHQ_responses$DaytimeSleepiness_no8.7.6.30), c('Question29', 'Question31', 
                                                                                                                                 'Question32', 'Question33')])

##factor analysis
CSHQ_cor = cor(UAE_cogBMI_CSHQ_responses[c(5:37)], use = 'na.or.complete')
CSHQ_barlett = cortest.bartlett(CSHQ_cor, n=102)
CSHQ_det = det(CSHQ_cor)
CSHQ_nFA = nfactors(CSHQ_cor, n.obs=102)
CSHQ_nFAscree = fa.parallel(CSHQ_cor, n.obs=102)
CSHQ_vss = vss(CSHQ_cor, n.obs=102)

CSHQ_5FA = fa(CSHQ_cor,5,n.obs=102, rotate = "oblimin", fm = "pa")
CSHQ_3FA = fa(CSHQ_cor,3,n.obs=102, rotate = "oblimin", fm = "pa")

CSHQ_5FA = fa(CSHQ_cor,5,n.obs=102, rotate = "oblimin", fm = "pa")
CSHQ_3FA = fa(CSHQ_cor,3,n.obs=102, rotate = "oblimin", fm = "pa")
