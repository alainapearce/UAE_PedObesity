############ Basic Data Load/Setup########
library(reporttools)
library(xtable)
library(car)
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(reshape2)
library(lsr)
library(ordinal)
#library(LDdiag)
library(stats)
library(mediation)
library(plyr)
library(memisc)
library(rstudioapi)
library(emmeans)
library(effects)
library(psych)



#set working directory to location of script--not needed when called 
#through Rmarkdown doc. Uncomment below if running locally/manually
#this.dir = getActiveDocumentContext()$path
#setwd(dirname(this.dir))

source('functions.R')


#####################################
####                            
####  Data setup      ####
####                            
#####################################
##source data setup
source('1_DataOrg.R')

#####################################
####                            
####    Demo Data       ####
####                            
#####################################
##sex
sex_tab = xtabs(~Gender, data = UAE_cogBMI_compiled)

##Weight Class
#CDC
CDC_WeightClass_tab = xtabs(~CDC_WeightStatus, data = UAE_cogBMI_compiled)
CDC_WeightClass.sex_tab = xtabs(~CDC_WeightStatus + Gender, data = UAE_cogBMI_compiled)
CDC_WeightClass.sex_fisher = fisher.test(CDC_WeightClass.sex_tab)

#WHO
WHO_WeightClass_tab = xtabs(~WHO_WeightStatus, data = UAE_cogBMI_compiled)
WHO_WeightClass.sex_tab = xtabs(~WHO_WeightStatus + Gender, data = UAE_cogBMI_compiled)
WHO_WeightClass.sex_fisher = fisher.test(WHO_WeightClass.sex_tab)

#IOFT
IOTF_WeightClass_tab = xtabs(~IOTF_WeightStatus, data = UAE_cogBMI_compiled)
IOTF_WeightClass.sex_tab = xtabs(~IOTF_WeightStatus + Gender, data = UAE_cogBMI_compiled)
IOTF_WeightClass.sex_fisher = fisher.test(IOTF_WeightClass.sex_tab)

##BMI
BMI_sum = psych::describe(UAE_cogBMI_compiled$BMI, na.rm = TRUE)

BMI_sex_t = t.test(BMI~Gender, data = UAE_cogBMI_compiled)
BMI_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$BMI, UAE_cogBMI_compiled$Gender)
BMI_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$BMI, UAE_cogBMI_compiled$Gender)
BMI_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$BMI, UAE_cogBMI_compiled$Gender)

##BMI percentile
CDC_BMIp_sum = psych::describe(UAE_cogBMI_compiled$CDC_BMIptile, na.rm = TRUE)

CDC_BMIp_sex_t = t.test(CDC_BMIptile~Gender, data = UAE_cogBMI_compiled)
CDC_BMIp_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CDC_BMIptile, UAE_cogBMI_compiled$Gender)
CDC_BMIp_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CDC_BMIptile, UAE_cogBMI_compiled$Gender)
CDC_BMIp_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CDC_BMIptile, UAE_cogBMI_compiled$Gender)

CDC_BMIp_plot = ggplot(UAE_cogBMI_compiled, aes(x = CDC_BMIptile, color = Gender)) +
  geom_density() +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and WASI Performance IQ') 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##IOTF BMI25
UAE_cogBMI_compiled$IOTF_BMI25p = UAE_cogBMI_compiled$BMI/UAE_cogBMI_compiled$IOTF_BMI25

IOTF_BMI25p_sum = psych::describe(UAE_cogBMI_compiled$IOTF_BMI25p, na.rm = TRUE)

IOTF_BMI25p_sex_t = t.test(IOTF_BMI25p~Gender, data = UAE_cogBMI_compiled)
IOTF_BMI25p_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$IOTF_BMI25p, UAE_cogBMI_compiled$Gender)
IOTF_BMI25p_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$IOTF_BMI25p, UAE_cogBMI_compiled$Gender)
IOTF_BMI25p_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$IOTF_BMI25p, UAE_cogBMI_compiled$Gender)

IOTF_BMI25p_plot = ggplot(UAE_cogBMI_compiled, aes(x = IOTF_BMI25p, color = Gender)) +
  geom_density() +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and WASI Performance IQ') 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##WHO BMI Z
WHO_BMIz_sum = psych::describe(UAE_cogBMI_compiled$WHO_BMIz, na.rm = TRUE)

WHO_BMIz_sex_t = t.test(WHO_BMIz~Gender, data = UAE_cogBMI_compiled)
WHO_BMIz_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WHO_BMIz, UAE_cogBMI_compiled$Gender)
WHO_BMIz_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WHO_BMIz, UAE_cogBMI_compiled$Gender)
WHO_BMIz_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WHO_BMIz, UAE_cogBMI_compiled$Gender)

WHO_BMIz_plot = ggplot(UAE_cogBMI_compiled, aes(x = WHO_BMIz, color = Gender)) +
  geom_density() +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and WASI Performance IQ') 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##WHO BMIp85th
UAE_cogBMI_compiled$WHO_BMIp85p = UAE_cogBMI_compiled$BMI/UAE_cogBMI_compiled$WHO_BMIp85

WHO_BMIp85p_sum = psych::describe(UAE_cogBMI_compiled$WHO_BMIp85p, na.rm = TRUE)

WHO_BMIp85p_sex_t = t.test(WHO_BMIp85p~Gender, data = UAE_cogBMI_compiled)
WHO_BMIp85p_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WHO_BMIp85p, UAE_cogBMI_compiled$Gender)
WHO_BMIp85p_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WHO_BMIp85p, UAE_cogBMI_compiled$Gender)
WHO_BMIp85p_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WHO_BMIp85p, UAE_cogBMI_compiled$Gender)

WHO_BMIp85p_plot = ggplot(UAE_cogBMI_compiled, aes(x = WHO_BMIp85p, color = Gender)) +
  geom_density() +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and WASI Performance IQ') 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())


##Age - weight
age_sum = psych::describe(UAE_cogBMI_compiled$Age_yr, na.rm = TRUE)

age_sex_t = t.test(Age_yr~Gender, data = UAE_cogBMI_compiled)
age_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Age_yr, UAE_cogBMI_compiled$Gender)
age_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Age_yr, UAE_cogBMI_compiled$Gender)
age_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Age_yr, UAE_cogBMI_compiled$Gender)

##mEducation
mED_sum = psych::describe(UAE_cogBMI_compiled$Mother_ed, na.rm = TRUE)

mED_sex_t = t.test(Mother_ed~Gender, data = UAE_cogBMI_compiled)
mED_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Mother_ed, UAE_cogBMI_compiled$Gender)
mED_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Mother_ed, UAE_cogBMI_compiled$Gender)
mED_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Mother_ed, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Mother_ed), ])

##pEducation
pED_sum = psych::describe(UAE_cogBMI_compiled$Father_ed, na.rm = TRUE)

pED_sex_t = t.test(Father_ed~Gender, data = UAE_cogBMI_compiled)
pED_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Father_ed, UAE_cogBMI_compiled$Gender)
pED_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Father_ed, UAE_cogBMI_compiled$Gender)
pED_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Father_ed, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Father_ed), ])

##SES
SES_tab = xtabs(~Monthly.inc_AED, data = UAE_cogBMI_compiled)
SES_tab_condensed = as.matrix(c(33,55, 6, 9), ncol = 1, byrow = TRUE)

SES.sex_tab = xtabs(~Monthly.inc_AED + Gender, data = UAE_cogBMI_compiled)
UAE_cogBMI_compiled$SEScat = ifelse(UAE_cogBMI_compiled$Monthly.inc_AED == '<5,000AED' |
                                      UAE_cogBMI_compiled$Monthly.inc_AED == '5,000-15,000AED' |
                                      UAE_cogBMI_compiled$Monthly.inc_AED == '15,000-25,000AED', '<25,000AED',
                                    ifelse(UAE_cogBMI_compiled$Monthly.inc_AED == '25,000-35,000AED' | 
                                             UAE_cogBMI_compiled$Monthly.inc_AED == '35,000-45,000AED' |
                                             UAE_cogBMI_compiled$Monthly.inc_AED == '45,000-55,000AED', '25,000-55,000AED',
                                           ifelse(UAE_cogBMI_compiled$Monthly.inc_AED == '55,000-65,000AED' |
                                                    UAE_cogBMI_compiled$Monthly.inc_AED == '65,000-75,000AED', '55,000-75,000AED',
                                                  ifelse(UAE_cogBMI_compiled$Monthly.inc_AED == '75,000-85,000AED' |
                                                           UAE_cogBMI_compiled$Monthly.inc_AED == '85,000-95,000AED' |
                                                           UAE_cogBMI_compiled$Monthly.inc_AED == '100,000+AED' | 
                                                           UAE_cogBMI_compiled$Monthly.inc_AED == '200,000+AED', '>75,000AED', 'error'))))

SEScat.sex_tab = xtabs(~SEScat + Gender, data = UAE_cogBMI_compiled)
SES.sex_fisher = fisher.test(SEScat.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Monthly.inc_AED), ])

##DadNationality
DadNationality_tab = xtabs(~DadNationality, data = UAE_cogBMI_compiled)

DadNationality.sex_tab = xtabs(~DadNationality + Gender, data = UAE_cogBMI_compiled)
DadNationality.sex_fisher = fisher.test(DadNationality.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$DadNationality), ])

##MomNationality
MomNationality_tab = xtabs(~MomNationality, data = UAE_cogBMI_compiled)

MomNationality.sex_tab = xtabs(~MomNationality + Gender, data = UAE_cogBMI_compiled)
MomNationality.sex_fisher = fisher.test(MomNationality.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$MomNationality), ])

##correlations with IOTF_BMI25p
IOTF_BMI25p_demo_cor.varnames = names(UAE_cogBMI_compiled)[c(7, 42:43, 105)]
IOTF_BMI25p_demo_cor.vars = UAE_cogBMI_compiled[c('Age_yr', 'Father_ed', 'Mother_ed', 'IOTF_BMI25p')]
IOTF_BMI25p_demo_cormat = data.frame(cor.matrix(IOTF_BMI25p_demo_cor.vars, IOTF_BMI25p_demo_cor.varnames))

##group differences for IOTF_BMI25p
#sex
IOTF_BMI25p.sex_ttest = t.test(IOTF_BMI25p ~ Gender, data = UAE_cogBMI_compiled)
IOTF_BMI25p.sex_ttest_tab = data.frame(matrix(c(round(IOTF_BMI25p.sex_ttest$statistic, 2),
                                     round(IOTF_BMI25p.sex_ttest$parameter, 2),  
                                     round(IOTF_BMI25p.sex_ttest$p.value, 4),
                                     round(IOTF_BMI25p.sex_ttest$estimate[1], 2),
                                     round(IOTF_BMI25p.sex_ttest$estimate[2], 2)), byrow = TRUE, nrow = 1))
names(IOTF_BMI25p.sex_ttest_tab) = c('t', 'df', 'pvalue', 'meanGirls', 'meanBoys')

#SES
IOTF_BMI25p.SES_ANOVA = Anova(lm(IOTF_BMI25p~SEScat, data = UAE_cogBMI_compiled), test.statistic = 'F')
IOTF_BMI25p.SES_ANOVAtab = sig_stars.table(IOTF_BMI25p.SES_ANOVA, c('', ''))

#####################################
####                            
####    Medial Data       ####
####                            
#####################################

##VitDdeficiency
VitDdeficiency_tab = xtabs(~VitDdeficiency, data = UAE_cogBMI_compiled)

VitDdeficiency.sex_tab = xtabs(~VitDdeficiency + Gender, data = UAE_cogBMI_compiled)
VitDdeficiency.sex_fisher = fisher.test(VitDdeficiency.sex_tab)

##Anemia
UAE_cogBMI_compiled$Anemia_YN = ifelse(is.na(UAE_cogBMI_compiled$Anemia), 'N', 'Y')

Anemia_tab = xtabs(~Anemia, data = UAE_cogBMI_compiled)
AnemiaYN_tab = xtabs(~Anemia_YN, data = UAE_cogBMI_compiled)

Anemia.sex_tab = xtabs(~Anemia + Gender, data = UAE_cogBMI_compiled)

AnemiaYN.sex_tab = xtabs(~Anemia_YN + Gender, data = UAE_cogBMI_compiled)
AnemiaYN.sex_fisher = fisher.test(AnemiaYN.sex_tab)


##Hyperlipidemia
UAE_cogBMI_compiled$Hyperlipidemia_YN = ifelse(is.na(UAE_cogBMI_compiled$Hyperlipidemia), 'N', 'Y')
Hyperlipidemia_tab = xtabs(~Hyperlipidemia, data = UAE_cogBMI_compiled)

HyperlipidemiaYN_tab = xtabs(~Hyperlipidemia_YN, data = UAE_cogBMI_compiled)

Hyperlipidemia.sex_tab = xtabs(~Hyperlipidemia + Gender, data = UAE_cogBMI_compiled)

HyperlipidemiaYN.sex_tab = xtabs(~Hyperlipidemia_YN + Gender, data = UAE_cogBMI_compiled)
HyperlipidemiaYN.sex_fisher = fisher.test(HyperlipidemiaYN.sex_tab)

##Thyriod
UAE_cogBMI_compiled$Thyriod_YN = ifelse(is.na(UAE_cogBMI_compiled$Thyriod), 'N', 'Y')
Thyriod_tab = xtabs(~Thyriod, data = UAE_cogBMI_compiled)
ThyriodYN_tab = xtabs(~Thyriod_YN, data = UAE_cogBMI_compiled)

Thyriod.sex_tab = xtabs(~Thyriod + Gender, data = UAE_cogBMI_compiled)

ThyriodYN.sex_tab = xtabs(~Thyriod_YN + Gender, data = UAE_cogBMI_compiled)
ThyriodYN.sex_fisher = fisher.test(ThyriodYN.sex_tab)

##ImpariedGlucose
UAE_cogBMI_compiled$ImpariedGlucose_YN = ifelse(is.na(UAE_cogBMI_compiled$ImpariedGlucose), 'N', 'Y')
ImpariedGlucose_tab = xtabs(~ImpariedGlucose, data = UAE_cogBMI_compiled)
ImpariedGlucoseYN_tab = xtabs(~ImpariedGlucose, data = UAE_cogBMI_compiled)

ImpariedGlucose.sex_tab = xtabs(~ImpariedGlucose + Gender, data = UAE_cogBMI_compiled)

ImpariedGlucoseYN.sex_tab = xtabs(~ImpariedGlucose_YN + Gender, data = UAE_cogBMI_compiled)
ImpariedGlucoseYN.sex_fisher = fisher.test(ImpariedGlucoseYN.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$VitDdeficiency), ])
   
##AcanthosisNigricans
UAE_cogBMI_compiled$AcanthosisNigricans_YN = ifelse(is.na(UAE_cogBMI_compiled$AcanthosisNigricans), 'N', 'Y')
AcanthosisNigricans_tab = xtabs(~AcanthosisNigricans, data = UAE_cogBMI_compiled)
AcanthosisNigricansYN_tab = xtabs(~AcanthosisNigricans, data = UAE_cogBMI_compiled)

AcanthosisNigricans.sex_tab = xtabs(~AcanthosisNigricans + Gender, data = UAE_cogBMI_compiled)

AcanthosisNigricansYN.sex_tab = xtabs(~AcanthosisNigricans_YN + Gender, data = UAE_cogBMI_compiled)
AcanthosisNigricansYN.sex_fisher = fisher.test(AcanthosisNigricansYN.sex_tab)

##Hypertension
UAE_cogBMI_compiled$Hypertension_YN = ifelse(is.na(UAE_cogBMI_compiled$Hypertension), 'N', 'Y')
Hypertension_tab = xtabs(~Hypertension, data = UAE_cogBMI_compiled)
HypertensionYN_tab = xtabs(~Hypertension_YN, data = UAE_cogBMI_compiled)

Hypertension.sex_tab = xtabs(~Hypertension + Gender, data = UAE_cogBMI_compiled)

HypertensionYN.sex_tab = xtabs(~Hypertension_YN + Gender, data = UAE_cogBMI_compiled)
HypertensionYN.sex_fisher = fisher.test(HypertensionYN.sex_tab)

##Metabolic Syndrome
UAE_cogBMI_compiled$MetabolicSyndrome_YN = ifelse(is.na(UAE_cogBMI_compiled$MetabolicSyndrome), 'N', 'Y')
MetabolicSyndrome_tab = xtabs(~MetabolicSyndrome, data = UAE_cogBMI_compiled)

MetabolicSyndrome.sex_tab = xtabs(~MetabolicSyndrome_YN + Gender, data = UAE_cogBMI_compiled)
MetabolicSyndrome.sex_fisher = fisher.test(MetabolicSyndrome.sex_tab)

##Growth/Stature
UAE_cogBMI_compiled$Growth.Stature_YN = ifelse(is.na(UAE_cogBMI_compiled$Growth.Stature), 'N', 'Y')
GrowthStature_tab = xtabs(~Growth.Stature, data = UAE_cogBMI_compiled)
GrowthStatureYN_tab = xtabs(~Growth.Stature_YN, data = UAE_cogBMI_compiled)

GrowthStature.sex_tab = xtabs(~Growth.Stature + Gender, data = UAE_cogBMI_compiled)

GrowthStatureYN.sex_tab = xtabs(~Growth.Stature_YN + Gender, data = UAE_cogBMI_compiled)
GrowthStatureYN.sex_fisher = fisher.test(GrowthStatureYN.sex_tab)

##PCOS
UAE_cogBMI_compiled$PCOS_YN = ifelse(is.na(UAE_cogBMI_compiled$PCOS), 'N', 'Y')
PCOS_tab = xtabs(~PCOS, data = UAE_cogBMI_compiled)
PCOS_YN_tab = xtabs(~PCOS_YN, data = UAE_cogBMI_compiled)

PCOS.sex_tab = xtabs(~PCOS + Gender, data = UAE_cogBMI_compiled)

PCOS_YN.sex_tab = xtabs(~PCOS_YN + Gender, data = UAE_cogBMI_compiled)
PCOS_YN.sex_fisher = fisher.test(PCOS_YN.sex_tab)

##Other
UAE_cogBMI_compiled$Other_YN = ifelse(is.na(UAE_cogBMI_compiled$Other), 'N', 'Y')
Other_tab = xtabs(~Other, data = UAE_cogBMI_compiled)
OtherYN_tab = xtabs(~Other_YN, data = UAE_cogBMI_compiled)

Other.sex_tab = xtabs(~Other + Gender, data = UAE_cogBMI_compiled)

OtherYN.sex_tab = xtabs(~Other_YN + Gender, data = UAE_cogBMI_compiled)
OtherYN.sex_fisher = fisher.test(OtherYN.sex_tab)

#number of comorbid conditions
UAE_cogBMI_compiled$nComorbid = rowSums(UAE_cogBMI_compiled[c(65, 107:115)] == 'Y')

nComorbid_sum = psych::describe(UAE_cogBMI_compiled$nComorbid, na.rm = TRUE)

nComorbid_sex_t = t.test(nComorbid~Gender, data = UAE_cogBMI_compiled)
nComorbid_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nComorbid, UAE_cogBMI_compiled$Gender)
nComorbid_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nComorbid, UAE_cogBMI_compiled$Gender)
nComorbid_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nComorbid, UAE_cogBMI_compiled$Gender)

##waist
Waist_sum = psych::describe(UAE_cogBMI_compiled$Waist_cm, na.rm = TRUE)

Waist_sex_t = t.test(Waist_cm~Gender, data = UAE_cogBMI_compiled)
Waist_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Waist_cm, UAE_cogBMI_compiled$Gender)
Waist_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Waist_cm, UAE_cogBMI_compiled$Gender)
Waist_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Waist_cm, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Waist_cm), ])

##hips
Hip_sum = psych::describe(UAE_cogBMI_compiled$Hips_cm, na.rm = TRUE)

Hip_sex_t = t.test(Hips_cm~Gender, data = UAE_cogBMI_compiled)
Hip_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Hips_cm, UAE_cogBMI_compiled$Gender)
Hip_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Hips_cm, UAE_cogBMI_compiled$Gender)
Hip_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Hips_cm, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Hips_cm), ])

##hips
UAE_cogBMI_compiled$Wasit2Hip = UAE_cogBMI_compiled$Waist_cm/UAE_cogBMI_compiled$Hips_cm
Wasit2Hip_sum = psych::describe(UAE_cogBMI_compiled$Wasit2Hip, na.rm = TRUE)

Wasit2Hip_sex_t = t.test(Wasit2Hip~Gender, data = UAE_cogBMI_compiled)
Wasit2Hip_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Wasit2Hip, UAE_cogBMI_compiled$Gender)
Wasit2Hip_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Wasit2Hip, UAE_cogBMI_compiled$Gender)
Wasit2Hip_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Wasit2Hip, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Wasit2Hip), ])

##correlations with IOTF_BMI25p
IOTF_BMI25p_MedComorbid_cor.varnames = names(UAE_cogBMI_compiled)[c(118, 105)]
IOTF_BMI25p_MedComorbid_cor.vars = UAE_cogBMI_compiled[c('nComorbid', 'IOTF_BMI25p')]
IOTF_BMI25p_MedComorbid_cormat = cor.matrix(IOTF_BMI25p_MedComorbid_cor.vars, IOTF_BMI25p_MedComorbid_cor.varnames)


##group differences for IOTF_BMI25p
#VitD
IOTF_BMI25p.VitD_ttest = t.test(IOTF_BMI25p ~ VitDdeficiency, data = UAE_cogBMI_compiled)
IOTF_BMI25p.Anemia_ttest = t.test(IOTF_BMI25p ~ Anemia_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.Hyperlipidemia_ttest = t.test(IOTF_BMI25p ~ Hyperlipidemia_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.Thyroid_ttest = t.test(IOTF_BMI25p ~ Thyriod_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.Glucose_ttest = t.test(IOTF_BMI25p ~ ImpariedGlucose_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.AcanthosisNigricans_ttest = t.test(IOTF_BMI25p ~ AcanthosisNigricans_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.Hypertension_ttest = t.test(IOTF_BMI25p ~ Hypertension_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.MetabolicSyndrome_ttest = t.test(IOTF_BMI25p ~ MetabolicSyndrome_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.GrowthStature_ttest = t.test(IOTF_BMI25p ~ Growth.Stature_YN, data = UAE_cogBMI_compiled)


IOTF_BMI25p.MedComorbid_ttest_tab = data.frame(matrix(c(round(IOTF_BMI25p.VitD_ttest$statistic, 2), round(IOTF_BMI25p.VitD_ttest$parameter, 2),round(IOTF_BMI25p.VitD_ttest$p.value, 4),
                                     round(IOTF_BMI25p.VitD_ttest$estimate[1], 2),round(IOTF_BMI25p.VitD_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.Anemia_ttest$statistic, 2), round(IOTF_BMI25p.Anemia_ttest$parameter, 2), round(IOTF_BMI25p.Anemia_ttest$p.value, 4),
                                     round(IOTF_BMI25p.Anemia_ttest$estimate[1], 2), round(IOTF_BMI25p.Anemia_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.Hyperlipidemia_ttest$statistic, 2), round(IOTF_BMI25p.Hyperlipidemia_ttest$parameter, 2), round(IOTF_BMI25p.Hyperlipidemia_ttest$p.value, 4),
                                     round(IOTF_BMI25p.Hyperlipidemia_ttest$estimate[1], 2), round(IOTF_BMI25p.Hyperlipidemia_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.Thyroid_ttest$statistic, 2), round(IOTF_BMI25p.Thyroid_ttest$parameter, 2), round(IOTF_BMI25p.Thyroid_ttest$p.value, 4),
                                     round(IOTF_BMI25p.Thyroid_ttest$estimate[1], 2), round(IOTF_BMI25p.Thyroid_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.Glucose_ttest$statistic, 2), round(IOTF_BMI25p.Glucose_ttest$parameter, 2), round(IOTF_BMI25p.Glucose_ttest$p.value, 4),
                                     round(IOTF_BMI25p.Glucose_ttest$estimate[1], 2), round(IOTF_BMI25p.Glucose_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.AcanthosisNigricans_ttest$statistic, 2), round(IOTF_BMI25p.AcanthosisNigricans_ttest$parameter, 2), round(IOTF_BMI25p.AcanthosisNigricans_ttest$p.value, 4),
                                     round(IOTF_BMI25p.AcanthosisNigricans_ttest$estimate[1], 2), round(IOTF_BMI25p.AcanthosisNigricans_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.Hypertension_ttest$statistic, 2), round(IOTF_BMI25p.Hypertension_ttest$parameter, 2), round(IOTF_BMI25p.Hypertension_ttest$p.value, 4),
                                     round(IOTF_BMI25p.Hypertension_ttest$estimate[1], 2), round(IOTF_BMI25p.Hypertension_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.MetabolicSyndrome_ttest$statistic, 2), round(IOTF_BMI25p.MetabolicSyndrome_ttest$parameter, 2), round(IOTF_BMI25p.MetabolicSyndrome_ttest$p.value, 4),
                                     round(IOTF_BMI25p.MetabolicSyndrome_ttest$estimate[1], 2), round(IOTF_BMI25p.MetabolicSyndrome_ttest$estimate[2], 2),
                                     round(IOTF_BMI25p.GrowthStature_ttest$statistic, 2), round(IOTF_BMI25p.GrowthStature_ttest$parameter, 2), round(IOTF_BMI25p.GrowthStature_ttest$p.value, 4),
                                     round(IOTF_BMI25p.GrowthStature_ttest$estimate[1], 2), round(IOTF_BMI25p.GrowthStature_ttest$estimate[2], 2)), byrow = TRUE, nrow = 9))

names(IOTF_BMI25p.MedComorbid_ttest_tab) = c('t', 'df', 'pvalue', 'AbsentMean', 'PresentMean')
rownames(IOTF_BMI25p.MedComorbid_ttest_tab) = c('VitD Deficiency', 'Anemia', 'Hyperlipidemia', 'Thyroid Dysfunction', 
                                                'Impaired Glucose/Type 1 Diabetes', 'Acanthosis Nigricans', 'Hypertension',
                                                'Metabolic Syndrome', 'Growth/Stature')
IOTF_BMI25p.MedComorbid_ttest_tab$sig = c('', '**', '*', '', '', '*', '*', '', '')


##reduced table
IOTF_BMI25p.MedComorbid_ttest_tab_reduced = data.frame(matrix(c(round(IOTF_BMI25p.Anemia_ttest$statistic, 2), round(IOTF_BMI25p.Anemia_ttest$parameter, 2), round(IOTF_BMI25p.Anemia_ttest$p.value, 4),
                                                        round(IOTF_BMI25p.Anemia_ttest$estimate[1], 2), round(IOTF_BMI25p.Anemia_ttest$estimate[2], 2),
                                                        round(IOTF_BMI25p.Thyroid_ttest$statistic, 2), round(IOTF_BMI25p.Thyroid_ttest$parameter, 2), round(IOTF_BMI25p.Thyroid_ttest$p.value, 4),
                                                        round(IOTF_BMI25p.Thyroid_ttest$estimate[1], 2), round(IOTF_BMI25p.Thyroid_ttest$estimate[2], 2),
                                                        round(IOTF_BMI25p.Glucose_ttest$statistic, 2), round(IOTF_BMI25p.Glucose_ttest$parameter, 2), round(IOTF_BMI25p.Glucose_ttest$p.value, 4),
                                                        round(IOTF_BMI25p.Glucose_ttest$estimate[1], 2), round(IOTF_BMI25p.Glucose_ttest$estimate[2], 2)), byrow = TRUE, nrow = 3))

names(IOTF_BMI25p.MedComorbid_ttest_tab_reduced) = c('t', 'df', 'pvalue', 'AbsentMean', 'PresentMean')
rownames(IOTF_BMI25p.MedComorbid_ttest_tab_reduced) = c('Anemia', 'Thyroid Dysfunction', 
                                                'Impaired Glucose/Type 1 Diabetes')
IOTF_BMI25p.MedComorbid_ttest_tab$sig = c('**', '', '')

#####################################
####                            
####   Family History       ####
####                            
#####################################

##Diabetes
UAE_cogBMI_compiled$FamilyHistory_YN = ifelse(is.na(UAE_cogBMI_compiled$FamilyHistory), 'N', 'Y')
FamilyHistory_tab = xtabs(~FamilyHistory, data = UAE_cogBMI_compiled)
FamilyHistoryYN_tab = xtabs(~FamilyHistory, data = UAE_cogBMI_compiled)

FamilyHistory.sex_tab = xtabs(~FamilyHistory + Gender, data = UAE_cogBMI_compiled)

FamilyHistoryYN.sex_tab = xtabs(~FamilyHistory_YN + Gender, data = UAE_cogBMI_compiled)
FamilyHistoryYN.sex_fisher = fisher.test(FamilyHistoryYN.sex_tab)


##Obesity
UAE_cogBMI_compiled$Family.Members.OB.List = as.character(UAE_cogBMI_compiled$Family.Members.OB.List)
UAE_cogBMI_compiled$Mother_OBhistory = ifelse(grepl('1', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')
UAE_cogBMI_compiled$Father_OBhistory = ifelse(grepl('2', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')
UAE_cogBMI_compiled$Grandmother_OBhistory = ifelse(grepl('3', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')
UAE_cogBMI_compiled$Grandfather_OBhistory = ifelse(grepl('4', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')
UAE_cogBMI_compiled$Sister_OBhistory = ifelse(grepl('5', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')
UAE_cogBMI_compiled$Brother_OBhistory = ifelse(grepl('6', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')
UAE_cogBMI_compiled$Aunt_OBhistory = ifelse(grepl('7', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')
UAE_cogBMI_compiled$Uncle_OBhistory = ifelse(grepl('8', UAE_cogBMI_compiled$Family.Members.OB.List), 'Y', 'N')

##number Obese
UAE_cogBMI_compiled$nFamilyMembers_OBhistory = rowSums(UAE_cogBMI_compiled[c('Mother_OBhistory', 'Father_OBhistory',
                                                                             'Grandmother_OBhistory', 'Grandfather_OBhistory',
                                                                             'Sister_OBhistory', 'Brother_OBhistory',
                                                                             'Aunt_OBhistory', 'Uncle_OBhistory')] == 'Y')

nFamilyMembers_OB_sum = psych::describe(UAE_cogBMI_compiled$nFamilyMembers_OBhistory, na.rm = TRUE)

nFamilyMembers_OB_sex_t = t.test(nFamilyMembers_OBhistory~Gender, data = UAE_cogBMI_compiled)
nFamilyMembers_OB_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nFamilyMembers_OBhistory, UAE_cogBMI_compiled$Gender)
nFamilyMembers_OB_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nFamilyMembers_OBhistory, UAE_cogBMI_compiled$Gender)
nFamilyMembers_OB_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nFamilyMembers_OBhistory, UAE_cogBMI_compiled$Gender)

##crosstabs
OB_FamilyHistoryYN_tab = xtabs(~Family.Members.OB._yes..no., data = UAE_cogBMI_compiled)
OB_FamilyHistoryYN.sex_tab = xtabs(~Family.Members.OB._yes..no. + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistoryYN.sex_fisher = fisher.test(FamilyHistoryYN.sex_tab)

OB_FamilyHistoryMother.sex_tab = xtabs(~Mother_OBhistory + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistoryFather.sex_tab = xtabs(~Father_OBhistory + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistoryGrandmother.sex_tab = xtabs(~Grandmother_OBhistory + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistoryGrandfather.sex_tab = xtabs(~Grandfather_OBhistory + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistorySister.sex_tab = xtabs(~Sister_OBhistory + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistoryBrother.sex_tab = xtabs(~Brother_OBhistory + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistoryAunt.sex_tab = xtabs(~Aunt_OBhistory + Gender, data = UAE_cogBMI_compiled)
OB_FamilyHistoryUncle.sex_tab = xtabs(~Uncle_OBhistory + Gender, data = UAE_cogBMI_compiled)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Family.Members.OB._yes..no.), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Family.Members.OB.List) & UAE_cogBMI_compiled$Family.Members.OB._yes..no. == 'yes', ])

##Eating Disorder
UAE_cogBMI_compiled$Family.Members.ED.List = as.character(UAE_cogBMI_compiled$Family.Members.ED.List)
UAE_cogBMI_compiled$Mother_EDhistory = ifelse(grepl('1', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')
UAE_cogBMI_compiled$Father_EDhistory = ifelse(grepl('2', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')
UAE_cogBMI_compiled$Grandmother_EDhistory = ifelse(grepl('3', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')
UAE_cogBMI_compiled$Grandfather_EDhistory = ifelse(grepl('4', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')
UAE_cogBMI_compiled$Sister_EDhistory = ifelse(grepl('5', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')
UAE_cogBMI_compiled$Brother_EDhistory = ifelse(grepl('6', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')
UAE_cogBMI_compiled$Aunt_EDhistory = ifelse(grepl('7', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')
UAE_cogBMI_compiled$Uncle_EDhistory = ifelse(grepl('8', UAE_cogBMI_compiled$Family.Members.ED.List), 'Y', 'N')

##number Obese
UAE_cogBMI_compiled$nFamilyMembers_EDhistory = rowSums(UAE_cogBMI_compiled[c('Mother_EDhistory', 'Father_EDhistory',
                                                                             'Grandmother_EDhistory', 'Grandfather_EDhistory',
                                                                             'Sister_EDhistory', 'Brother_EDhistory',
                                                                             'Aunt_EDhistory', 'Uncle_EDhistory')] == 'Y')

nFamilyMembers_ED_sum = psych::describe(UAE_cogBMI_compiled$nFamilyMembers_EDhistory, na.rm = TRUE)

nFamilyMembers_ED_sex_t = t.test(nFamilyMembers_EDhistory~Gender, data = UAE_cogBMI_compiled)
nFamilyMembers_ED_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nFamilyMembers_EDhistory, UAE_cogBMI_compiled$Gender)
nFamilyMembers_ED_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nFamilyMembers_EDhistory, UAE_cogBMI_compiled$Gender)
nFamilyMembers_ED_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$nFamilyMembers_EDhistory, UAE_cogBMI_compiled$Gender)

##crosstabs
ED_FamilyHistoryYN_tab = xtabs(~Family.Members.ED..yes..no., data = UAE_cogBMI_compiled)
ED_FamilyHistoryYN.sex_tab = xtabs(~Family.Members.ED..yes..no. + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistoryYN.sex_fisher = fisher.test(FamilyHistoryYN.sex_tab)

ED_FamilyHistoryMother.sex_tab = xtabs(~Mother_EDhistory + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistoryFather.sex_tab = xtabs(~Father_EDhistory + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistoryGrandmother.sex_tab = xtabs(~Grandmother_EDhistory + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistoryGrandfather.sex_tab = xtabs(~Grandfather_EDhistory + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistorySister.sex_tab = xtabs(~Sister_EDhistory + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistoryBrother.sex_tab = xtabs(~Brother_EDhistory + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistoryAunt.sex_tab = xtabs(~Aunt_EDhistory + Gender, data = UAE_cogBMI_compiled)
ED_FamilyHistoryUncle.sex_tab = xtabs(~Uncle_EDhistory + Gender, data = UAE_cogBMI_compiled)                                                                            

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Family.Members.ED..yes..no.), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Family.Members.ED.List) & UAE_cogBMI_compiled$Family.Members.ED..yes..no. == 'yes', ])

##correlations with IOTF_BMI25p
IOTF_BMI25p_FamHistory_cor.varnames = names(UAE_cogBMI_compiled)[c(129, 138, 105)]
IOTF_BMI25p_FamHistory_cor.vars = UAE_cogBMI_compiled[c('nFamilyMembers_OBhistory', 'nFamilyMembers_EDhistory', 'IOTF_BMI25p')]
IOTF_BMI25p_FamHistory_cormat = cor.matrix(IOTF_BMI25p_FamHistory_cor.vars, IOTF_BMI25p_FamHistory_cor.varnames)

##ttests 
IOTF_BMI25p.DiabetesHistory_ttest = t.test(IOTF_BMI25p ~ FamilyHistory_YN, data = UAE_cogBMI_compiled)
IOTF_BMI25p.OBHistory_ttest = t.test(IOTF_BMI25p ~ Family.Members.OB._yes..no., data = UAE_cogBMI_compiled)
IOTF_BMI25p.EDHistory_ttest = t.test(IOTF_BMI25p ~ Family.Members.ED..yes..no., data = UAE_cogBMI_compiled)


IOTF_BMI25p.FamHistory_ttest_tab = data.frame(matrix(c(round(IOTF_BMI25p.DiabetesHistory_ttest$statistic, 2), round(IOTF_BMI25p.DiabetesHistory_ttest$parameter, 2),round(IOTF_BMI25p.DiabetesHistory_ttest$p.value, 4),
                                                        round(IOTF_BMI25p.DiabetesHistory_ttest$estimate[1], 2),round(IOTF_BMI25p.DiabetesHistory_ttest$estimate[2], 2),
                                                        round(IOTF_BMI25p.OBHistory_ttest$statistic, 2), round(IOTF_BMI25p.OBHistory_ttest$parameter, 2), round(IOTF_BMI25p.OBHistory_ttest$p.value, 4),
                                                        round(IOTF_BMI25p.OBHistory_ttest$estimate[1], 2), round(IOTF_BMI25p.OBHistory_ttest$estimate[2], 2),
                                                        round(IOTF_BMI25p.EDHistory_ttest$statistic, 2), round(IOTF_BMI25p.EDHistory_ttest$parameter, 2), round(IOTF_BMI25p.EDHistory_ttest$p.value, 4),
                                                        round(IOTF_BMI25p.EDHistory_ttest$estimate[1], 2), round(IOTF_BMI25p.EDHistory_ttest$estimate[2], 2)), byrow = TRUE, nrow = 3))

names(IOTF_BMI25p.FamHistory_ttest_tab) = c('t', 'df', 'pvalue', 'No', 'Yes')
rownames(IOTF_BMI25p.FamHistory_ttest_tab) = c('Family History of Diabetes', 'Family History of Obesity', 'Family History of Eating Disorder')
IOTF_BMI25p.FamHistory_ttest_tab$sig = c('', '***', '')

#####################################
####                            
####   Behavioral/Psychological       ####
####                            
#####################################

##Behavioral
UAE_cogBMI_compiled$Behavioral_YN = ifelse(is.na(UAE_cogBMI_compiled$Behavioral), 'N', 'Y')
Behavioral_tab = xtabs(~Behavioral, data = UAE_cogBMI_compiled)
BehavioralYN_tab = xtabs(~Behavioral_YN, data = UAE_cogBMI_compiled)

Behavioral.sex_tab = xtabs(~Behavioral + Gender, data = UAE_cogBMI_compiled)

BehavioralYN.sex_tab = xtabs(~Behavioral_YN + Gender, data = UAE_cogBMI_compiled)
BehavioralYN.sex_fisher = fisher.test(BehavioralYN.sex_tab)

#Emotional Problems
SDQ_EmotionalProbs_sum = psych::describe(UAE_cogBMI_compiled$Emotional.Problems_Raw, na.rm = TRUE)

SDQ_EmotionalProbs_sex_t = t.test(Emotional.Problems_Raw~Gender, data = UAE_cogBMI_compiled)
SDQ_EmotionalProbs_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Emotional.Problems_Raw, UAE_cogBMI_compiled$Gender)
SDQ_EmotionalProbs_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Emotional.Problems_Raw, UAE_cogBMI_compiled$Gender)
SDQ_EmotionalProbs_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Emotional.Problems_Raw, UAE_cogBMI_compiled$Gender)

SDQ_EmotionalProbs_tab = xtabs(~Emotional.problems.categorization, data = UAE_cogBMI_compiled)
SDQ_EmotionalProbs.sex_tab = xtabs(~Emotional.problems.categorization + Gender, data = UAE_cogBMI_compiled)
SDQ_EmotionalProbs.sex_fisher = fisher.test(SDQ_EmotionalProbs.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Emotional.Problems_Raw), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Emotional.problems.categorization), ])

#Emotional Problems
SDQ_ConductProbs_sum = psych::describe(UAE_cogBMI_compiled$Conduct.Problems_Raw, na.rm = TRUE)

SDQ_ConductProbs_sex_t = t.test(Conduct.Problems_Raw~Gender, data = UAE_cogBMI_compiled)
SDQ_ConductProbs_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Conduct.Problems_Raw, UAE_cogBMI_compiled$Gender)
SDQ_ConductProbs_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Conduct.Problems_Raw, UAE_cogBMI_compiled$Gender)
SDQ_ConductProbs_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Conduct.Problems_Raw, UAE_cogBMI_compiled$Gender)

SDQ_ConductProbs_tab = xtabs(~Conduct.problems.categorization, data = UAE_cogBMI_compiled)
SDQ_ConductProbs.sex_tab = xtabs(~Conduct.problems.categorization + Gender, data = UAE_cogBMI_compiled)
SDQ_ConductProbs.sex_fisher = fisher.test(SDQ_ConductProbs.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Conduct.Problems_Raw), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Conduct.problems.categorization), ])

#Hyperactivity
SDQ_Hyperactivity_sum = psych::describe(UAE_cogBMI_compiled$Hyperactivity_Raw, na.rm = TRUE)

SDQ_Hyperactivity_sex_t = t.test(Hyperactivity_Raw~Gender, data = UAE_cogBMI_compiled)
SDQ_Hyperactivity_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Hyperactivity_Raw, UAE_cogBMI_compiled$Gender)
SDQ_Hyperactivity_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Hyperactivity_Raw, UAE_cogBMI_compiled$Gender)
SDQ_Hyperactivity_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Hyperactivity_Raw, UAE_cogBMI_compiled$Gender)

SDQ_Hyperactivity_tab = xtabs(~Hyperactivity.categorization, data = UAE_cogBMI_compiled)
SDQ_Hyperactivity.sex_tab = xtabs(~Hyperactivity.categorization + Gender, data = UAE_cogBMI_compiled)
SDQ_Hyperactivity.sex_fisher = fisher.test(SDQ_Hyperactivity.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Hyperactivity_Raw), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Hyperactivity.categorization), ])

#Hyperactivity
SDQ_PeerProbs_sum = psych::describe(UAE_cogBMI_compiled$Peer.Problems_Raw, na.rm = TRUE)

SDQ_PeerProbs_sex_t = t.test(Peer.Problems_Raw~Gender, data = UAE_cogBMI_compiled)
SDQ_PeerProbs_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Peer.Problems_Raw, UAE_cogBMI_compiled$Gender)
SDQ_PeerProbs_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Peer.Problems_Raw, UAE_cogBMI_compiled$Gender)
SDQ_PeerProbs_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Peer.Problems_Raw, UAE_cogBMI_compiled$Gender)

SDQ_PeerProbs_tab = xtabs(~Peer.problems.categorization, data = UAE_cogBMI_compiled)
SDQ_PeerProbs.sex_tab = xtabs(~Peer.problems.categorization + Gender, data = UAE_cogBMI_compiled)
SDQ_PeerProbs.sex_fisher = fisher.test(SDQ_PeerProbs.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Peer.Problems_Raw), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Peer.problems.categorization), ])

#Prosocial
SDQ_Prosocial_sum = psych::describe(UAE_cogBMI_compiled$Prosocial_Raw, na.rm = TRUE)

SDQ_Prosocial_sex_t = t.test(Prosocial_Raw~Gender, data = UAE_cogBMI_compiled)
SDQ_Prosocial_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Prosocial_Raw, UAE_cogBMI_compiled$Gender)
SDQ_Prosocial_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Prosocial_Raw, UAE_cogBMI_compiled$Gender)
SDQ_Prosocial_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Prosocial_Raw, UAE_cogBMI_compiled$Gender)

SDQ_Prosocial_tab = xtabs(~Prosocial.categorization, data = UAE_cogBMI_compiled)
SDQ_Prosocial.sex_tab = xtabs(~Prosocial.categorization + Gender, data = UAE_cogBMI_compiled)
SDQ_Prosocial.sex_fisher = fisher.test(SDQ_Prosocial.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Peer.Problems_Raw), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Prosocial.categorization), ])

#Total problems
SDQ_TotalProbs_sum = psych::describe(UAE_cogBMI_compiled$Total.Difficulty.Score_Raw, na.rm = TRUE)

SDQ_TotalProbs_sex_t = t.test(Total.Difficulty.Score_Raw~Gender, data = UAE_cogBMI_compiled)
SDQ_TotalProbs_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Total.Difficulty.Score_Raw, UAE_cogBMI_compiled$Gender)
SDQ_TotalProbs_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Total.Difficulty.Score_Raw, UAE_cogBMI_compiled$Gender)
SDQ_TotalProbs_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Total.Difficulty.Score_Raw, UAE_cogBMI_compiled$Gender)

SDQ_TotalProbs_tab = xtabs(~Total.difficulty.Categorization, data = UAE_cogBMI_compiled)
SDQ_TotalProbs.sex_tab = xtabs(~Total.difficulty.Categorization + Gender, data = UAE_cogBMI_compiled)
SDQ_TotalProbs.sex_fisher = fisher.test(SDQ_TotalProbs.sex_tab)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Total.Difficulty.Score_Raw), ])
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Total.difficulty.Categorization), ])

##correlations with IOTF_BMI25p
IOTF_BMI25p_SDQ_cor.varnames = c('EmlProb', 'ConductProb', 'Hyper', 'PeerProb',  'TotalProb','Prosocial', 'IOTF_BMI25p')
IOTF_BMI25p_SDQ_cor.vars = UAE_cogBMI_compiled[c('Emotional.Problems_Raw', 'Conduct.Problems_Raw', 'Hyperactivity_Raw', 'Peer.Problems_Raw',
                                                 'Total.Difficulty.Score_Raw', 'Prosocial_Raw',  'IOTF_BMI25p')]
IOTF_BMI25p_SDQ_cormat = cor.matrix(IOTF_BMI25p_SDQ_cor.vars, IOTF_BMI25p_SDQ_cor.varnames)


#####################################
####                            
####   Sleep       ####
####                            
#####################################
#load sleep data
#source('CSHQ_ScaleCheck.R')

##SleepRelated
UAE_cogBMI_compiled$SleepRelated_YN = ifelse(is.na(UAE_cogBMI_compiled$SleepRelated), 'N', 'Y')
SleepRelated_tab = xtabs(~SleepRelated, data = UAE_cogBMI_compiled)
SleepRelatedYN_tab = xtabs(~SleepRelated_YN, data = UAE_cogBMI_compiled)

SleepRelated.sex_tab = xtabs(~SleepRelated + Gender, data = UAE_cogBMI_compiled)

SleepRelatedYN.sex_tab = xtabs(~SleepRelated_YN + Gender, data = UAE_cogBMI_compiled)
SleepRelatedYN.sex_fisher = fisher.test(SleepRelatedYN.sex_tab)

#Bedtime Resistance
CSHQ_BedtimeRes_sum = psych::describe(UAE_cogBMI_compiled$BedtimeRes, na.rm = TRUE)

CSHQ_BedtimeRes_sex_t = t.test(BedtimeRes~Gender, data = UAE_cogBMI_compiled)
CSHQ_BedtimeRes_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$BedtimeRes, UAE_cogBMI_compiled$Gender)
CSHQ_BedtimeRes_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$BedtimeRes, UAE_cogBMI_compiled$Gender)
CSHQ_BedtimeRes_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$BedtimeRes, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$BedtimeRes), ])

#Sleep Delay
CSHQ_SleepDelay_sum = psych::describe(UAE_cogBMI_compiled$SleepDelay, na.rm = TRUE)

CSHQ_SleepDelay_sex_t = t.test(SleepDelay~Gender, data = UAE_cogBMI_compiled)
CSHQ_SleepDelay_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepDelay, UAE_cogBMI_compiled$Gender)
CSHQ_SleepDelay_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepDelay, UAE_cogBMI_compiled$Gender)
CSHQ_SleepDelay_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepDelay, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$SleepDelay), ])

#Sleep Duration
CSHQ_SleepDuration_sum = psych::describe(UAE_cogBMI_compiled$SleepDuration, na.rm = TRUE)

CSHQ_SleepDuration_sex_t = t.test(SleepDuration~Gender, data = UAE_cogBMI_compiled)
CSHQ_SleepDuration_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepDuration, UAE_cogBMI_compiled$Gender)
CSHQ_SleepDuration_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepDuration, UAE_cogBMI_compiled$Gender)
CSHQ_SleepDuration_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepDuration, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$SleepDuration), ])

#Sleep Anxiety
CSHQ_SleepAnxiety_sum = psych::describe(UAE_cogBMI_compiled$SleepAnxiety, na.rm = TRUE)

CSHQ_SleepAnxiety_sex_t = t.test(SleepAnxiety~Gender, data = UAE_cogBMI_compiled)
CSHQ_SleepAnxiety_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepAnxiety, UAE_cogBMI_compiled$Gender)
CSHQ_SleepAnxiety_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepAnxiety, UAE_cogBMI_compiled$Gender)
CSHQ_SleepAnxiety_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$SleepAnxiety, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$SleepAnxiety), ])

#Night Waking
CSHQ_NightWake_sum = psych::describe(UAE_cogBMI_compiled$NightWake, na.rm = TRUE)

CSHQ_NightWake_sex_t = t.test(NightWake~Gender, data = UAE_cogBMI_compiled)
CSHQ_NightWake_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$NightWake, UAE_cogBMI_compiled$Gender)
CSHQ_NightWake_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$NightWake, UAE_cogBMI_compiled$Gender)
CSHQ_NightWake_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$NightWake, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$NightWake), ])

#Parasomnias
CSHQ_Parasomnias_sum = psych::describe(UAE_cogBMI_compiled$Parasomnias, na.rm = TRUE)

CSHQ_Parasomnias_sex_t = t.test(Parasomnias~Gender, data = UAE_cogBMI_compiled)
CSHQ_Parasomnias_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Parasomnias, UAE_cogBMI_compiled$Gender)
CSHQ_Parasomnias_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Parasomnias, UAE_cogBMI_compiled$Gender)
CSHQ_Parasomnias_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$Parasomnias, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$Parasomnias), ])

#Daytime Sleepiness
CSHQ_DaytimeSleepy_sum = psych::describe(UAE_cogBMI_compiled$DaytimeSleepy, na.rm = TRUE)

CSHQ_DaytimeSleepy_sex_t = t.test(DaytimeSleepy~Gender, data = UAE_cogBMI_compiled)
CSHQ_DaytimeSleepy_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DaytimeSleepy, UAE_cogBMI_compiled$Gender)
CSHQ_DaytimeSleepy_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DaytimeSleepy, UAE_cogBMI_compiled$Gender)
CSHQ_DaytimeSleepy_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DaytimeSleepy, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$DaytimeSleepy), ])

#Sleep Disordered Breathing
CSHQ_TotalScore_sum = psych::describe(UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ]$TotalScore, na.rm = TRUE)

CSHQ_TotalScore_sex_t = t.test(TotalScore~Gender, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ])
CSHQ_TotalScore_sex_means = means.function.na(UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ], UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ]$TotalScore, UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ]$Gender)
CSHQ_TotalScore_sex_sd = sd.function.na(UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ], UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ]$TotalScore, UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ]$Gender)
CSHQ_TotalScore_sex_range = range.function.na(UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ], UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ]$TotalScore, UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ]$Gender)

nrow(UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'N', ])    

##correlations with IOTF_BMI25p
IOTF_BMI25p_CSHQ_cor.varnames = names(UAE_cogBMI_compiled)[c(90:97, 104)]
IOTF_BMI25p_CSHQ_cor.vars = UAE_cogBMI_compiled[c('BedtimeRes', 'SleepDelay', 'SleepDuration', 'SleepAnxiety',
                                                 'NightWake', 'Parasomnias', 'SleepDisBreathing', 'DaytimeSleepy','IOTF_BMI25p')]
IOTF_BMI25p_CSHQ_cormat = cor.matrix(IOTF_BMI25p_CSHQ_cor.vars, IOTF_BMI25p_CSHQ_cor.varnames)


#####################################
####
####   Neuropsych Data       ####
####
#####################################
##WASI Block
WASI_BlockRaw_sum = psych::describe(UAE_cogBMI_compiled$WASI_BlockRaw, na.rm = TRUE)

WASI_BlockRaw_sex_t = t.test(WASI_BlockRaw~Gender, data = UAE_cogBMI_compiled)
WASI_BlockRaw_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_BlockRaw, UAE_cogBMI_compiled$Gender)
WASI_BlockRaw_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_BlockRaw, UAE_cogBMI_compiled$Gender)
WASI_BlockRaw_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_BlockRaw, UAE_cogBMI_compiled$Gender)


WASI_BlockT_sum = psych::describe(UAE_cogBMI_compiled$WASI_BlockT, na.rm = TRUE)

WASI_BlockT_sex_t = t.test(WASI_BlockT~Gender, data = UAE_cogBMI_compiled)
WASI_BlockT_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_BlockT, UAE_cogBMI_compiled$Gender)
WASI_BlockT_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_BlockT, UAE_cogBMI_compiled$Gender)
WASI_BlockT_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_BlockT, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$WASI_BlockT), ])

##WASI Matrix
WASI_MatrixRaw_sum = psych::describe(UAE_cogBMI_compiled$WASI_MatrixRaw, na.rm = TRUE)

WASI_MatrixRaw_sex_t = t.test(WASI_MatrixRaw~Gender, data = UAE_cogBMI_compiled)
WASI_MatrixRaw_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_MatrixRaw, UAE_cogBMI_compiled$Gender)
WASI_MatrixRaw_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_MatrixRaw, UAE_cogBMI_compiled$Gender)
WASI_MatrixRaw_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_MatrixRaw, UAE_cogBMI_compiled$Gender)


WASI_MatrixT_sum = psych::describe(UAE_cogBMI_compiled$WASI_MatrixT, na.rm = TRUE)

WASI_MatrixT_sex_t = t.test(WASI_MatrixT~Gender, data = UAE_cogBMI_compiled)
WASI_MatrixT_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_MatrixT, UAE_cogBMI_compiled$Gender)
WASI_MatrixT_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_MatrixT, UAE_cogBMI_compiled$Gender)
WASI_MatrixT_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_MatrixT, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$WASI_MatrixT), ])

##WASI PRI
WASI_PRI_sum = psych::describe(UAE_cogBMI_compiled$WASI_PRI_IQ, na.rm = TRUE)

WASI_PRI_sex_t = t.test(WASI_PRI_IQ~Gender, data = UAE_cogBMI_compiled)
WASI_PRI_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_PRI_IQ, UAE_cogBMI_compiled$Gender)
WASI_PRI_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_PRI_IQ, UAE_cogBMI_compiled$Gender)
WASI_PRI_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$WASI_PRI_IQ, UAE_cogBMI_compiled$Gender)

nrow(UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ < 70, ])

##Digit Forward
DS_ForwardRaw_sum = psych::describe(UAE_cogBMI_compiled$DigitSpan_ForwardRaw, na.rm = TRUE)

DS_ForwardRaw_sex_t = t.test(DigitSpan_ForwardRaw~Gender, data = UAE_cogBMI_compiled)
DS_ForwardRaw_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_ForwardRaw, UAE_cogBMI_compiled$Gender)
DS_ForwardRaw_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_ForwardRaw, UAE_cogBMI_compiled$Gender)
DS_ForwardRaw_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_ForwardRaw, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$DigitSpan_ForwardRaw), ])

DS_ForwardSS_sum = psych::describe(UAE_cogBMI_compiled$DigitSpan_ForwardSS, na.rm = TRUE)

DS_ForwardSS_sex_t = t.test(DigitSpan_ForwardSS~Gender, data = UAE_cogBMI_compiled)
DS_ForwardSS_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_ForwardSS, UAE_cogBMI_compiled$Gender)
DS_ForwardSS_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_ForwardSS, UAE_cogBMI_compiled$Gender)
DS_ForwardSS_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_ForwardSS, UAE_cogBMI_compiled$Gender)

##Digit Backward
DS_BackwardRaw_sum = psych::describe(UAE_cogBMI_compiled$DigitSpan_BackwardRaw, na.rm = TRUE)

DS_BackwardRaw_sex_t = t.test(DigitSpan_BackwardRaw~Gender, data = UAE_cogBMI_compiled)
DS_BackwardRaw_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_BackwardRaw, UAE_cogBMI_compiled$Gender)
DS_BackwardRaw_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_BackwardRaw, UAE_cogBMI_compiled$Gender)
DS_BackwardRaw_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_BackwardRaw, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$DigitSpan_BackwardRaw), ])

DS_BackwardSS_sum = psych::describe(UAE_cogBMI_compiled$DigitSpan_BackwardSS, na.rm = TRUE)

DS_BackwardSS_sex_t = t.test(DigitSpan_BackwardSS~Gender, data = UAE_cogBMI_compiled)
DS_BackwardSS_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_BackwardSS, UAE_cogBMI_compiled$Gender)
DS_BackwardSS_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_BackwardSS, UAE_cogBMI_compiled$Gender)
DS_BackwardSS_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$DigitSpan_BackwardSS, UAE_cogBMI_compiled$Gender)

##Coding
Coding_Raw_sum = psych::describe(UAE_cogBMI_compiled$CodingRaw, na.rm = TRUE)

Coding_Raw_sex_t = t.test(CodingRaw~Gender, data = UAE_cogBMI_compiled)
Coding_Raw_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CodingRaw, UAE_cogBMI_compiled$Gender)
Coding_Raw_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CodingRaw, UAE_cogBMI_compiled$Gender)
Coding_Raw_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CodingRaw, UAE_cogBMI_compiled$Gender)
nrow(UAE_cogBMI_compiled[is.na(UAE_cogBMI_compiled$CodingRaw), ])

Coding_SS_sum = psych::describe(UAE_cogBMI_compiled$CodingSS, na.rm = TRUE)

Coding_SS_sex_t = t.test(CodingSS~Gender, data = UAE_cogBMI_compiled)
Coding_SS_sex_means = means.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CodingSS, UAE_cogBMI_compiled$Gender)
Coding_SS_sex_sd = sd.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CodingSS, UAE_cogBMI_compiled$Gender)
Coding_SS_sex_range = range.function.na(UAE_cogBMI_compiled, UAE_cogBMI_compiled$CodingSS, UAE_cogBMI_compiled$Gender)

##correlations with IOTF_BMI25p
IOTF_BMI25p_cog_cor.varnames = c('Block', 'BlockT', 'Matrix', 'MatrixT',
                                 'IQ', 'dsF', 'dsFSS',
                                 'dsB', 'sdBSS', 'Code', 'CodeSS',
                                 'Age', 'BMI25p', 'W2H', 'nComorbid')
IOTF_BMI25p_cog_cor.vars = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, c('WASI_BlockRaw', 'WASI_BlockT', 'WASI_MatrixRaw', 'WASI_MatrixT',
                                                  'WASI_PRI_IQ', 'DigitSpan_ForwardRaw', 'DigitSpan_ForwardSS',
                                                 'DigitSpan_BackwardRaw', 'DigitSpan_BackwardSS', 'CodingRaw', 'CodingSS',
                                                 'Age_yr', 'IOTF_BMI25p', 'Wasit2Hip', 'nComorbid')]
IOTF_BMI25p_cog_cormat = cor.matrix(IOTF_BMI25p_cog_cor.vars, IOTF_BMI25p_cog_cor.varnames)

#####################################
####                            
####   Family History - Poisson Models      ####
####                            
#####################################
#center at 1 (100%) - right on overweight
UAE_cogBMI_compiled$IOTF_BMI25p_c100 = (UAE_cogBMI_compiled$IOTF_BMI25p*100) - 100

#make quadratic term
UAE_cogBMI_compiled$IOTF_BMI25p_c100_sq = UAE_cogBMI_compiled$IOTF_BMI25p*UAE_cogBMI_compiled$IOTF_BMI25p_c100


IOTF_BMI25p_nFamOBHistory_mod = glm(nFamilyMembers_OBhistory ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, family = poisson(link = "log"), data = UAE_cogBMI_compiled)
IOTF_BMI25p_nFamOBHistory_sum = summary(IOTF_BMI25p_nFamOBHistory_mod)
IOTF_BMI25p_nFamOBHistory_odds = exp(coef(IOTF_BMI25p_nFamOBHistory_mod))
IOTF_BMI25p_nFamOBHistory_oddsCI = exp(confint(IOTF_BMI25p_nFamOBHistory_mod))
IOTF_BMI25p_nFamOBHistory_tab = cbind.data.frame(IOTF_BMI25p_nFamOBHistory_sum$coefficients, c('', '', '', '', '.', '', '', '***'), IOTF_BMI25p_nFamOBHistory_odds, IOTF_BMI25p_nFamOBHistory_oddsCI)
names(IOTF_BMI25p_nFamOBHistory_tab) = c('b', 'se', 'z', 'p', ' ', 'e^b', 'e^2.5 CI', 'e^97.5 CI')


IOTF_BMI25p_nFamEDHistory_mod = glm(nFamilyMembers_EDhistory ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, family = poisson(link = "log"), data = UAE_cogBMI_compiled)
IOTF_BMI25p_nFamEDHistory_sum = summary(IOTF_BMI25p_nFamEDHistory_mod)
IOTF_BMI25p_nFamEDHistory_odds = exp(coef(IOTF_BMI25p_nFamEDHistory_mod))
#IOTF_BMI25p_nFamEDHistory_oddsCI = exp(confint(IOTF_BMI25p_nFamEDHistory_mod))
IOTF_BMI25p_nFamEDHistory_tab = cbind.data.frame(IOTF_BMI25p_nFamEDHistory_sum$coefficients, c('', '', '', '', '', '.', '', ''), IOTF_BMI25p_nFamEDHistory_odds)
names(IOTF_BMI25p_nFamEDHistory_tab) = c('b', 'se', 'z', 'p', ' ', 'e^b')

#####################################
####
####   WASI - Models  ####
####
#####################################

##Scatter Plots
###Block
IOTF_BMI25p_BlockT_plot = ggplot(UAE_cogBMI_compiled, aes(y = WASI_BlockT, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
#  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and WASI Block T-score') +
  scale_y_continuous(name='Block (T)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Matrix
IOTF_BMI25p_MatrixT_plot = ggplot(UAE_cogBMI_compiled, aes(y = WASI_MatrixT, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
#  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and WASI Matrix T-score') +
  scale_y_continuous(name='Matrix (T)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###PRI
IOTF_BMI25p_PRI_plot = ggplot(UAE_cogBMI_compiled, aes(y = WASI_PRI_IQ, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
#  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and WASI Performance IQ') +
  scale_y_continuous(name='IQ (PRI)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Quadratic Models

#BlockT
IOTF_BMI25p_BlockT_quad_mod = lm(WASI_BlockT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled)
IOTF_BMI25p_BlockT_quad_sum = summary(IOTF_BMI25p_BlockT_quad_mod)

IOTF_BMI25p_BlockT_quad_IQge70_mod = lm(WASI_BlockT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_BlockT_quad_IQge70_sum = summary(IOTF_BMI25p_BlockT_quad_IQge70_mod)

#MatrixT
IOTF_BMI25p_MatrixT_quad_mod = lm(WASI_MatrixT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled)
IOTF_BMI25p_MatrixT_quad_sum = summary(IOTF_BMI25p_MatrixT_quad_mod)

IOTF_BMI25p_MatrixT_quad_IQge70_mod = lm(WASI_MatrixT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_MatrixT_quad_IQge70_sum = summary(IOTF_BMI25p_MatrixT_quad_IQge70_mod)

IOTF_BMI25p_MatrixT_mod = lm(WASI_MatrixT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled)
IOTF_BMI25p_MatrixT_sum = summary(IOTF_BMI25p_MatrixT_mod)

IOTF_BMI25p_MatrixT_IQge70_mod = lm(WASI_MatrixT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_MatrixT_IQge70_sum = summary(IOTF_BMI25p_MatrixT_IQge70_mod)

#MatrixT
IOTF_BMI25p_PRI_quad_mod = lm(WASI_PRI_IQ ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled)
IOTF_BMI25p_PRI_quad_sum = summary(IOTF_BMI25p_PRI_quad_mod)

IOTF_BMI25p_PRI_quad_IQge70_mod = lm(WASI_PRI_IQ ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_PRI_quad_IQge70_sum = summary(IOTF_BMI25p_PRI_quad_IQge70_mod)

#####################################
####
####   WASI - Models with Glucose ####
####
#####################################

#BlockT
IOTF_GlucoseYN_BlockT_mod = lm(WASI_BlockT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled)
IOTF_GlucoseYN_BlockT_sum = summary(IOTF_GlucoseYN_BlockT_mod)

IOTF_GlucoseYN_BlockT_IQge70_mod = lm(WASI_BlockT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_GlucoseYN_BlockT_IQge70_sum = summary(IOTF_GlucoseYN_BlockT_IQge70_mod)

#MatrixT
IOTF_GlucoseYN_MatrixT_mod = lm(WASI_MatrixT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled)
IOTF_GlucoseYN_MatrixT_sum = summary(IOTF_GlucoseYN_MatrixT_mod)

IOTF_GlucoseYN_MatrixT_IQge70_mod = lm(WASI_MatrixT ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_GlucoseYN_MatrixT_IQge70_sum = summary(IOTF_GlucoseYN_MatrixT_IQge70_mod)


#PRI
IOTF_GlucoseYN_PRI_mod = lm(WASI_PRI_IQ ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled)
IOTF_GlucoseYN_PRI_sum = summary(IOTF_GlucoseYN_PRI_mod)

IOTF_GlucoseYN_PRI_IQge70_mod = lm(WASI_PRI_IQ ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_GlucoseYN_PRI_IQge70_sum = summary(IOTF_GlucoseYN_PRI_IQge70_mod)


#####################################
####
####   Digit Span/Coding - Models  ####
####
#####################################
##Scatter Plots
###Digit Span Forward
IOTF_BMI25p_DSforwardSS_plot = ggplot(UAE_cogBMI_compiled, aes(y = DigitSpan_ForwardSS, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Digit Spand - Forward (SS)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Digit Span Backward
IOTF_BMI25p_DSbackwardSS_plot = ggplot(UAE_cogBMI_compiled, aes(y = DigitSpan_BackwardSS, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Backward') +
  scale_y_continuous(name='Digit Spand - Backward (SS)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Coding
IOTF_BMI25p_CodingSS_plot = ggplot(UAE_cogBMI_compiled, aes(y = CodingSS, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Coding') +
  scale_y_continuous(name='Coding (SS)') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Quadratic Models
#DS Forward
IOTF_BMI25p_DSforwardSS_quad_mod = lm(DigitSpan_ForwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled)
IOTF_BMI25p_DSforwardSS_quad_sum = summary(IOTF_BMI25p_DSforwardSS_quad_mod)

IOTF_BMI25p_DSforwardSS_quad_IQge70_mod = lm(DigitSpan_ForwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_DSforwardSS_quad_IQge70_sum = summary(IOTF_BMI25p_DSforwardSS_quad_IQge70_mod)

IOTF_BMI25p_DSforwardSS_mod = lm(DigitSpan_ForwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled)
IOTF_BMI25p_DSforwardSS_sum = summary(IOTF_BMI25p_DSforwardSS_mod)

IOTF_BMI25p_DSforwardSS_IQge70_mod = lm(DigitSpan_ForwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_DSforwardSS_IQge70_sum = summary(IOTF_BMI25p_DSforwardSS_IQge70_mod)

#Digit Span Backwards
IOTF_BMI25p_DSbackwardSS_quad_mod = lm(DigitSpan_BackwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled)
IOTF_BMI25p_DSbackwardSS_quad_sum = summary(IOTF_BMI25p_DSbackwardSS_quad_mod)

IOTF_BMI25p_DSbackwardSS_quad_IQge70_mod = lm(DigitSpan_BackwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_DSbackwardSS_quad_IQge70_sum = summary(IOTF_BMI25p_DSbackwardSS_quad_IQge70_mod)

IOTF_BMI25p_DSbackwardSS_mod = lm(DigitSpan_BackwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled)
IOTF_BMI25p_DSbackwardSS_sum = summary(IOTF_BMI25p_DSbackwardSS_mod)

IOTF_BMI25p_DSbackwardSS_IQge70_mod = lm(DigitSpan_BackwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_DSbackwardSS_IQge70_sum = summary(IOTF_BMI25p_DSbackwardSS_IQge70_mod)

#MatrixT
IOTF_BMI25p_CodingSS_quad_mod = lm(CodingSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled)
IOTF_BMI25p_CodingSS_quad_sum = summary(IOTF_BMI25p_CodingSS_quad_mod)

IOTF_BMI25p_CodingSS_quad_IQge70_mod = lm(CodingSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + IOTF_BMI25p_c100_sq, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_CodingSS_quad_IQge70_sum = summary(IOTF_BMI25p_CodingSS_quad_IQge70_mod)

IOTF_BMI25p_CodingSS_mod = lm(CodingSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled)
IOTF_BMI25p_CodingSS_sum = summary(IOTF_BMI25p_MatrixT_mod)

IOTF_BMI25p_CodingSS_IQge70_mod = lm(CodingSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_CodingSS_IQge70_sum = summary(IOTF_BMI25p_CodingSS_IQge70_mod)


#####################################
####
####   Digit Span/Coding - Models Glucose  ####
####
#####################################
#DS Forward
IOTF_GlucoseYN_DSforwardSS_mod = lm(DigitSpan_ForwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled)
IOTF_GlucoseYN_DSforwardSS_sum = summary(IOTF_GlucoseYN_DSforwardSS_mod)

IOTF_GlucoseYN_DSforwardSS_IQge70_mod = lm(DigitSpan_ForwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_GlucoseYN_DSforwardSS_IQge70_sum = summary(IOTF_GlucoseYN_DSforwardSS_IQge70_mod)

#Digit Span Backwards
IOTF_GlucoseYN_DSbackwardSS_mod = lm(DigitSpan_BackwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled)
IOTF_GlucoseYN_DSbackwardSS_sum = summary(IOTF_GlucoseYN_DSbackwardSS_mod)

IOTF_GlucoseYN_DSbackwardSS_IQge70_mod = lm(DigitSpan_BackwardSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_GlucoseYN_DSbackwardSS_IQge70_sum = summary(IOTF_GlucoseYN_DSbackwardSS_IQge70_mod)

#MatrixT
IOTF_GlucoseYN_CodingSS_mod = lm(CodingSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled)
IOTF_GlucoseYN_CodingSS_sum = summary(IOTF_GlucoseYN_CodingSS_mod)

IOTF_GlucoseYN_CodingSS_IQge70_mod = lm(CodingSS ~ SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN, data = UAE_cogBMI_compiled[UAE_cogBMI_compiled$WASI_PRI_IQ >= 70, ])
IOTF_GlucoseYN_CodingSS_IQge70_sum = summary(IOTF_GlucoseYN_CodingSS_IQge70_mod)


#####################################
####
####   SDQ - Logit Models  ####
####
#####################################

#Emotional Problems - Elevated vs Not
IOTF_BMI25p_EmProbs.Sex_plot = ggplot(UAE_cogBMI_compiled, aes(y = Emotional.Problems_Raw, x = IOTF_BMI25p_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Emotional Problems') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_cogBMI_compiled$Emotional.problems.categorization2 = ifelse(UAE_cogBMI_compiled$Emotional.problems.categorization == 'VeryHigh', 'Elevated', 
                                                              ifelse(UAE_cogBMI_compiled$Emotional.problems.categorization == 'High', 'Elevated', 
                                                                     ifelse(UAE_cogBMI_compiled$Emotional.problems.categorization == 'SlightlyRaised', 'Elevated',
                                                                            ifelse(UAE_cogBMI_compiled$Emotional.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_cogBMI_compiled$Emotional.problems.categorization2 = factor(UAE_cogBMI_compiled$Emotional.problems.categorization2, 
                                                                levels = c('NotElevated', 'Elevated'))

IOTF_BMI25p_EmProbs_glm2_mod = glm(Emotional.problems.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_BMI25p_c100, family = binomial(link = 'logit'), data = UAE_cogBMI_compiled)
IOTF_BMI25p_EmProbs_glm2_sum = summary(IOTF_BMI25p_EmProbs_glm2_mod)
IOTF_BMI25p_EmProbs_glm2_odds = exp(coef(IOTF_BMI25p_EmProbs_glm2_mod))
IOTF_BMI25p_EmProbs_glm2_oddsCI = exp(confint(IOTF_BMI25p_EmProbs_glm2_mod))
IOTF_BMI25p_EmProbs_glm2_tab = cbind.data.frame(IOTF_BMI25p_EmProbs_glm2_sum$coefficients, IOTF_BMI25p_EmProbs_glm2_odds, IOTF_BMI25p_EmProbs_glm2_oddsCI)
names(IOTF_BMI25p_EmProbs_glm2_tab) = c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

#Conduct Problems - elevated vs not
IOTF_BMI25p_CondProbs.Sex_plot = ggplot(UAE_cogBMI_compiled, aes(y = Conduct.Problems_Raw, x = IOTF_BMI25p_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Conduct Problems') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_cogBMI_compiled$Conduct.problems.categorization2 = ifelse(UAE_cogBMI_compiled$Conduct.problems.categorization == 'VeryHigh', 'Elevated', 
                                                              ifelse(UAE_cogBMI_compiled$Conduct.problems.categorization == 'High', 'Elevated', 
                                                                     ifelse(UAE_cogBMI_compiled$Conduct.problems.categorization == 'SlightlyRaised', 'Elevated',
                                                                            ifelse(UAE_cogBMI_compiled$Conduct.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_cogBMI_compiled$Conduct.problems.categorization2 = factor(UAE_cogBMI_compiled$Conduct.problems.categorization2, 
                                                              levels = c('NotElevated', 'Elevated'))

IOTF_BMI25p_CondProbs_glm2_mod = glm(Conduct.problems.categorization2 ~ SEScat + Mother_ed+ nComorbid + Age_yr + Gender*IOTF_BMI25p_c100, family = binomial(link = 'logit'), data = UAE_cogBMI_compiled)
IOTF_BMI25p_CondProbs_glm2_sum = summary(IOTF_BMI25p_CondProbs_glm2_mod)
IOTF_BMI25p_CondProbs_glm2_odds = exp(coef(IOTF_BMI25p_CondProbs_glm2_mod))
IOTF_BMI25p_CondProbs_glm2_oddsCI = exp(confint(IOTF_BMI25p_CondProbs_glm2_mod))
IOTF_BMI25p_CondProbs_glm2_tab = cbind.data.frame(IOTF_BMI25p_CondProbs_glm2_sum$coefficients, c('', '', '', '', '', '.', '.', '', '', ''), IOTF_BMI25p_CondProbs_glm2_odds, IOTF_BMI25p_CondProbs_glm2_oddsCI)
names(IOTF_BMI25p_CondProbs_glm2_tab) = c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

#Hyperactivity - elevated vs not
IOTF_BMI25p_Hyper.Sex_plot = ggplot(UAE_cogBMI_compiled, aes(y = Hyperactivity_Raw, x = IOTF_BMI25p_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Hyperactivity') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_cogBMI_compiled$Hyperactivity.categorization2 = ifelse(UAE_cogBMI_compiled$Hyperactivity.categorization == 'VeryHigh', 'Elevated', 
                                                              ifelse(UAE_cogBMI_compiled$Hyperactivity.categorization == 'High', 'Elevated', 
                                                                     ifelse(UAE_cogBMI_compiled$Hyperactivity.categorization == 'SlightlyRaised', 'Elevated',
                                                                            ifelse(UAE_cogBMI_compiled$Hyperactivity.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_cogBMI_compiled$Hyperactivity.categorization2 = factor(UAE_cogBMI_compiled$Hyperactivity.categorization2, 
                                                           levels = c('NotElevated', 'Elevated'))

IOTF_BMI25p_Hyper_glm2_mod = glm(Hyperactivity.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_BMI25p_c100, family = binomial(link = 'logit'), data = UAE_cogBMI_compiled)
IOTF_BMI25p_Hyper_glm2_sum = summary(IOTF_BMI25p_Hyper_glm2_mod)
IOTF_BMI25p_Hyper_glm2_odds = exp(coef(IOTF_BMI25p_Hyper_glm2_mod))
IOTF_BMI25p_Hyper_glm2_oddsCI = exp(confint(IOTF_BMI25p_Hyper_glm2_mod))
IOTF_BMI25p_Hyper_glm2_tab = cbind.data.frame(IOTF_BMI25p_Hyper_glm2_sum$coefficients, IOTF_BMI25p_Hyper_glm2_odds, IOTF_BMI25p_Hyper_glm2_oddsCI)
names(IOTF_BMI25p_Hyper_glm2_tab) = c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

#Peer Problems - elevated vs not
UAE_cogBMI_compiled$Peer.problems.categorization2 = ifelse(UAE_cogBMI_compiled$Peer.problems.categorization == 'VeryHigh', 'Elevated', 
                                                           ifelse(UAE_cogBMI_compiled$Peer.problems.categorization == 'High', 'Elevated', 
                                                                  ifelse(UAE_cogBMI_compiled$Peer.problems.categorization == 'SlightlyRaised', 'Elevated',
                                                                         ifelse(UAE_cogBMI_compiled$Peer.problems.categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_cogBMI_compiled$Peer.problems.categorization2 = factor(UAE_cogBMI_compiled$Peer.problems.categorization2, 
                                                           levels = c('NotElevated', 'Elevated'))

IOTF_BMI25p_PeerProbs_glm2_mod = glm(Peer.problems.categorization2 ~ SEScat + Mother_ed+ nComorbid + Age_yr + Gender*IOTF_BMI25p_c100, family = binomial(link = 'logit'), data = UAE_cogBMI_compiled)
IOTF_BMI25p_PeerProbs_glm2_sum = summary(IOTF_BMI25p_PeerProbs_glm2_mod)
IOTF_BMI25p_PeerProbs_glm2_odds = exp(coef(IOTF_BMI25p_PeerProbs_glm2_mod))
IOTF_BMI25p_PeerProbs_glm2_oddsCI = exp(confint(IOTF_BMI25p_PeerProbs_glm2_mod))
IOTF_BMI25p_PeerProbs_glm2_tab = cbind.data.frame(IOTF_BMI25p_PeerProbs_glm2_sum$coefficients, c('','','','.','*','','','','*','*'), IOTF_BMI25p_PeerProbs_glm2_odds, IOTF_BMI25p_PeerProbs_glm2_oddsCI)
names(IOTF_BMI25p_PeerProbs_glm2_tab) = c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

IOTF_BMI25p_PeerProbs_glm2_emtrends = emtrends(IOTF_BMI25p_PeerProbs_glm2_mod, ~ IOTF_BMI25p_c100 | Gender , var = 'IOTF_BMI25p_c100', infer = c('TRUE', 'TRUE'))
IOTF_BMI25p_PeerProbs_glm2_emtrends_sum = summary(IOTF_BMI25p_PeerProbs_glm2_emtrends)

PeerProbs_dat = UAE_cogBMI_compiled[!is.na(UAE_cogBMI_compiled$Peer.problems.categorization2) & !is.na(UAE_cogBMI_compiled$SEScat) & !is.na(UAE_cogBMI_compiled$Mother_ed), 
                                    c('Gender', 'IOTF_BMI25p_c100')]
PeerProbs_dat$PredProb = predict(IOTF_BMI25p_PeerProbs_glm2_mod, type = 'response')
PeerProbs_dat$PredLogit = predict(IOTF_BMI25p_PeerProbs_glm2_mod, type = 'link')

IOTF_BMI25p_PeerProbs.Sex_ORplot = ggplot(PeerProbs_dat, aes(y = exp(PredLogit), x = IOTF_BMI25p_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  geom_point(aes(y = exp(PredLogit), shape = Gender), size = 3) +
  scale_y_continuous(name='Predicted Odds Ratio') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#Prosocial - elevated vs not
UAE_cogBMI_compiled$Prosocial.categorization2 = ifelse(UAE_cogBMI_compiled$Prosocial.categorization == 'VeryLow', 'Lowered', 
                                                           ifelse(UAE_cogBMI_compiled$Prosocial.categorization == 'Low', 'Lowered', 
                                                                  ifelse(UAE_cogBMI_compiled$Prosocial.categorization == 'SlightlyLowered', 'Lowered',
                                                                         ifelse(UAE_cogBMI_compiled$Prosocial.categorization == 'CloseToAverage', 'NotLowered', 'Error'))))

UAE_cogBMI_compiled$Prosocial.categorization2 = factor(UAE_cogBMI_compiled$Prosocial.categorization2, 
                                                           levels = c('NotLowered', 'Lowered'))

IOTF_BMI25p_Prosocial_glm2_mod = glm(Prosocial.categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_BMI25p_c100, family = binomial(link = 'logit'), data = UAE_cogBMI_compiled)
IOTF_BMI25p_Prosocial_glm2_sum = summary(IOTF_BMI25p_Prosocial_glm2_mod)
IOTF_BMI25p_Prosocial_glm2_odds = exp(coef(IOTF_BMI25p_Prosocial_glm2_mod))
IOTF_BMI25p_Prosocial_glm2_oddsCI = exp(confint(IOTF_BMI25p_Prosocial_glm2_mod))
IOTF_BMI25p_Prosocial_glm2_tab = cbind.data.frame(IOTF_BMI25p_Prosocial_glm2_sum$coefficients, c('','.','','','','','','','','.'), IOTF_BMI25p_Prosocial_glm2_odds, IOTF_BMI25p_Prosocial_glm2_oddsCI)
names(IOTF_BMI25p_Prosocial_glm2_tab) = c('b', 'se', 'z', 'p', '', 'e^b', 'e^2.5 CI', 'e^97.5 CI')

IOTF_BMI25p_Prosocial_glm2_emtrends = emtrends(IOTF_BMI25p_Prosocial_glm2_mod, ~ IOTF_BMI25p_c100 | Gender , var = 'IOTF_BMI25p_c100', infer = c('TRUE', 'TRUE'))
IOTF_BMI25p_Prosocial_glm2_emtrends_sum = summary(IOTF_BMI25p_Prosocial_glm2_emtrends)

Prosocial_dat = UAE_cogBMI_compiled[!is.na(UAE_cogBMI_compiled$Prosocial.categorization2) & !is.na(UAE_cogBMI_compiled$SEScat) & !is.na(UAE_cogBMI_compiled$Mother_ed), 
                                    c('Gender', 'IOTF_BMI25p_c100')]
Prosocial_dat$PredProb = predict(IOTF_BMI25p_Prosocial_glm2_mod, type = 'response')
Prosocial_dat$PredLogit = predict(IOTF_BMI25p_Prosocial_glm2_mod, type = 'link')

IOTF_BMI25p_Prosocial.Sex_ORplot = ggplot(Prosocial_dat, aes(y = exp(PredLogit), x = IOTF_BMI25p_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  geom_point(aes(y = exp(PredLogit), shape = Gender), size = 3) +
  scale_y_continuous(name='Predicted Odds Ratio') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#Total Problems - Elevated vs Not
IOTF_BMI25p_TotalProbs.Sex_plot = ggplot(UAE_cogBMI_compiled, aes(y = Emotional.Problems_Raw, x = IOTF_BMI25p_c100, group = Gender)) +
  geom_smooth(method = 'loess', formula = y~x, aes(group = Gender, linetype = Gender)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Digit Span Forward') +
  scale_y_continuous(name='Emotional Problems') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

UAE_cogBMI_compiled$Total.difficulty.Categorization2 = ifelse(UAE_cogBMI_compiled$Total.difficulty.Categorization == 'VeryHigh', 'Elevated', 
                                                                ifelse(UAE_cogBMI_compiled$Total.difficulty.Categorization == 'High', 'Elevated', 
                                                                       ifelse(UAE_cogBMI_compiled$Total.difficulty.Categorization == 'SlightlyRaised', 'Elevated',
                                                                              ifelse(UAE_cogBMI_compiled$Total.difficulty.Categorization == 'CloseToAverage', 'NotElevated', 'Error'))))

UAE_cogBMI_compiled$Total.difficulty.Categorization2 = factor(UAE_cogBMI_compiled$Total.difficulty.Categorization2, 
                                                                levels = c('NotElevated', 'Elevated'))

IOTF_BMI25p_TotalProbs_glm2_mod = glm(Total.difficulty.Categorization2 ~ SEScat + Mother_ed + nComorbid + Age_yr + Gender*IOTF_BMI25p_c100, family = binomial(link = 'logit'), data = UAE_cogBMI_compiled)
IOTF_BMI25p_TotalProbs_glm2_sum = summary(IOTF_BMI25p_TotalProbs_glm2_mod)
IOTF_BMI25p_TotalProbs_glm2_odds = exp(coef(IOTF_BMI25p_TotalProbs_glm2_mod))
IOTF_BMI25p_TotalProbs_glm2_oddsCI = exp(confint(IOTF_BMI25p_TotalProbs_glm2_mod))
IOTF_BMI25p_TotalProbs_glm2_tab = cbind.data.frame(IOTF_BMI25p_TotalProbs_glm2_sum$coefficients, IOTF_BMI25p_TotalProbs_glm2_odds, IOTF_BMI25p_TotalProbs_glm2_oddsCI)
names(IOTF_BMI25p_TotalProbs_glm2_tab) = c('b', 'se', 'z', 'p', 'e^b', 'e^2.5 CI', 'e^97.5 CI')


#####################################
####
####   CHSQ -  Models  ####
####
#####################################

##Scatter Plots
###Bedtime Resistance
IOTF_BMI25p_BedRes_plot = ggplot(UAE_cogBMI_compiled, aes(y = BedtimeRes, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Bedtime Resistance') +
  scale_y_continuous(name='Bedtime Resistance') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Delay
IOTF_BMI25p_SleepDelay_plot = ggplot(UAE_cogBMI_compiled, aes(y = SleepDelay, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Delay') +
  scale_y_continuous(name='Sleep Delay') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Duration
IOTF_BMI25p_SleepDur_plot = ggplot(UAE_cogBMI_compiled, aes(y = SleepDuration, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Duration') +
  scale_y_continuous(name='Sleep Duration Concerns') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Anxiety
IOTF_BMI25p_SleepAnxiety_plot = ggplot(UAE_cogBMI_compiled, aes(y = SleepAnxiety, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Anxiety') +
  scale_y_continuous(name='Sleep Anxiety') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Night Wake
IOTF_BMI25p_NightWake_plot = ggplot(UAE_cogBMI_compiled, aes(y = NightWake, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Night Waking') +
  scale_y_continuous(name='Sleep Anxiety') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Parasomnias
IOTF_BMI25p_Parasom_plot = ggplot(UAE_cogBMI_compiled, aes(y = Parasomnias, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Parasomnias') +
  scale_y_continuous(name='Parasomnias') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###Sleep Disordered Breathing
IOTF_BMI25p_SleepDisBreathing_plot = ggplot(UAE_cogBMI_compiled, aes(y = SleepDisBreathing, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Sleep Disordered Breathing') +
  scale_y_continuous(name='Sleep Disordered Breathing') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Daytime Sleepiness
IOTF_BMI25p_DaySleepy_plot = ggplot(UAE_cogBMI_compiled, aes(y = DaytimeSleepy, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Daytime Sleepiness') +
  scale_y_continuous(name='Daytime Sleepiness') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Total Score
IOTF_BMI25p_Totalgood_plot = ggplot(UAE_cogBMI_compiled[UAE_cogBMI_compiled$GoodTotalScore_missing16orless == 'Y', ], aes(y = TotalScore, x = IOTF_BMI25p_c100)) +
  geom_smooth(method = 'loess', formula = y~x) +
  geom_point(aes(color = Gender)) + 
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Daytime Sleepiness') +
  scale_y_continuous(name='Total Score') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#####################################
####                            
####         NBack           ####
####                            
#####################################

#sex
Nback_sex_tab = xtabs(~Gender, data = UAE_cogBMI_NBACKDat.merge)
107-nrow(UAE_cogBMI_NBACKDat.merge)

#dataframe for functions
LoadxSex = data.frame(UAE_cogBMI_NBACKDat.merge_long$Load, UAE_cogBMI_NBACKDat.merge_long$Gender)
LoadxSex_IQge70 = data.frame(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Load, 
                             UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Gender)

##Ballance Acc
Nback_BalAcc1_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B1_BalAcc)*100, na.rm = TRUE)
Nback_BalAcc2_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B2_BalAcc)*100, na.rm = TRUE)

Nback_BalAcc1_sex_t = t.test(B1_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge)
Nback_BalAcc2_sex_t = t.test(B2_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge)

Nback_BalAcc_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$BalAcc)*100, LoadxSex)
Nback_BalAcc_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$BalAcc)*100, LoadxSex)
Nback_BalAcc_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$BalAcc)*100, LoadxSex)

#IQ >= 70
Nback_BalAcc1_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B1_BalAcc)*100, na.rm = TRUE)
Nback_BalAcc2_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B2_BalAcc)*100, na.rm = TRUE)

Nback_BalAcc1_IQge70_sex_t = t.test(B1_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])
Nback_BalAcc2_IQge70_sex_t = t.test(B2_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])

Nback_BalAcc_IQge70_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc)*100, LoadxSex_IQge70)
Nback_BalAcc_IQge70_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc)*100, LoadxSex_IQge70)
Nback_BalAcc_IQge70_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc)*100, LoadxSex_IQge70)

##d'
library(psycho)
UAE_cogBMI_NBACKDat.merge$B1_nCorReject = 45 - UAE_cogBMI_NBACKDat.merge$B1_nFA
UAE_cogBMI_NBACKDat.merge$B2_nCorReject = 45 - UAE_cogBMI_NBACKDat.merge$B2_nFA
UAE_cogBMI_NBACKDat.merge_long$nCorReject = 45 - UAE_cogBMI_NBACKDat.merge_long$nFA

B1_dprime_mat = dprime(n_hit = UAE_cogBMI_NBACKDat.merge$B1_nCor, 
                   n_miss = UAE_cogBMI_NBACKDat.merge$B1_nMiss, 
                   n_fa = UAE_cogBMI_NBACKDat.merge$B1_nFA, 
                   n_cr = UAE_cogBMI_NBACKDat.merge$B1_nCorReject)
B2_dprime_mat = dprime(n_hit = UAE_cogBMI_NBACKDat.merge$B2_nCor, 
                   n_miss = UAE_cogBMI_NBACKDat.merge$B2_nMiss, 
                   n_fa = UAE_cogBMI_NBACKDat.merge$B2_nFA, 
                   n_cr = UAE_cogBMI_NBACKDat.merge$B2_nCorReject)

UAE_cogBMI_NBACKDat.merge$B1_dprime = B1_dprime_mat$dprime
UAE_cogBMI_NBACKDat.merge$B2_dprime = B2_dprime_mat$dprime

dprime_long = dprime(n_hit = UAE_cogBMI_NBACKDat.merge_long$nCor, 
                   n_miss = UAE_cogBMI_NBACKDat.merge_long$nMiss, 
                   n_fa = UAE_cogBMI_NBACKDat.merge_long$nFA, 
                   n_cr = UAE_cogBMI_NBACKDat.merge_long$nCorReject)

UAE_cogBMI_NBACKDat.merge_long$dprime = dprime_long$dprime

Nback_dPrime_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, UAE_cogBMI_NBACKDat.merge_long$Load)
Nback_dPrime_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, UAE_cogBMI_NBACKDat.merge_long$Load)

Nback_dPrime1_sex_t = t.test(B1_dprime~Gender, data = UAE_cogBMI_NBACKDat.merge)
Nback_dPrime2_sex_t = t.test(B2_dprime~Gender, data = UAE_cogBMI_NBACKDat.merge)

Nback_dPrime_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, LoadxSex)
Nback_dPrime_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, LoadxSex)
Nback_dPrime_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$dprime, LoadxSex)

#IQ >= 70
Nback_dPrime1_IQge70_sum = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$dprime, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$Load)
Nback_dPrime2_IQge70_sum = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$dprime, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$Load)

Nback_dPrime1_IQge70_sex_t = t.test(B1_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])
Nback_dPrime2_IQge70_sex_t = t.test(B2_BalAcc~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])

Nback_dPrime_IQge70_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$dprime, LoadxSex_IQge70)
Nback_dPrime_IQge70_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$dprime, LoadxSex_IQge70)
Nback_dPrime_IQge70_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$dprime, LoadxSex_IQge70)

##RT
Nback_meanRT1_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B1_meanRTcor)*1000, na.rm = TRUE)
Nback_meanRT2_sum = psych::describe((UAE_cogBMI_NBACKDat.merge$B2_meanRTcor)*1000, na.rm = TRUE)

Nback_meanRT1_sex_t = t.test(B1_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge)
Nback_meanRT2_sex_t = t.test(B2_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge)

Nback_meanRT_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$meanRT)*1000, LoadxSex)
Nback_meanRT_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$meanRT)*1000, LoadxSex)
Nback_meanRT_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long, (UAE_cogBMI_NBACKDat.merge_long$meanRT)*1000, LoadxSex)

#IQ >= 70
Nback_meanRT1_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B1_meanRTcor)*1000, na.rm = TRUE)
Nback_meanRT2_IQge70_sum = psych::describe((UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ]$B2_meanRTcor)*1000, na.rm = TRUE)

Nback_meanRT1_IQge70_sex_t = t.test(B1_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])
Nback_meanRT2_IQge70_sex_t = t.test(B2_meanRTcor~Gender, data = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, ])

Nback_meanRT_IQge70_Load.sex_means = means.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$meanRT)*1000, LoadxSex_IQge70)
Nback_meanRT_IQge70_Load.sex_sd = sd.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$meanRT)*1000, LoadxSex_IQge70)
Nback_meanRT_IQge70_Load.sex_range = range.function.na(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], (UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$meanRT)*1000, LoadxSex_IQge70)

##correlations with IOTF_BMI25p
UAE_cogBMI_NBACKDat.merge$Wasit2Hip = UAE_cogBMI_NBACKDat.merge$Waist_cm/UAE_cogBMI_NBACKDat.merge$Hips_cm
UAE_cogBMI_NBACKDat.merge$IOTF_BMI25p = UAE_cogBMI_NBACKDat.merge$BMI/UAE_cogBMI_NBACKDat.merge$IOTF_BMI25

UAE_cogBMI_NBACKDat.merge_long$Wasit2Hip = UAE_cogBMI_NBACKDat.merge_long$Waist_cm/UAE_cogBMI_NBACKDat.merge_long$Hips_cm
UAE_cogBMI_NBACKDat.merge_long$IOTF_BMI25p = UAE_cogBMI_NBACKDat.merge_long$BMI/UAE_cogBMI_NBACKDat.merge_long$IOTF_BMI25

IOTF_BMI25p_Nback_cor.varnames = c('BlockT', 'MatrixT',
                                 'IQ', 'B1acc', 'B2acc',
                                 'B1RT', 'B2RT', 'B1d', 'B2d',
                                 'Age', 'BMI25p', 'W2H')
IOTF_BMI25p_Nback_cor.vars = UAE_cogBMI_NBACKDat.merge[UAE_cogBMI_NBACKDat.merge$WASI_PRI_IQ >= 70, c('WASI_BlockT', 'WASI_MatrixT',
                                                                                        'WASI_PRI_IQ', 'B1_BalAcc', 'B2_BalAcc',
                                                                                        'B1_meanRTcor', 'B2_meanRTcor', 'B1_dprime', 'B1_dprime',
                                                                                        'Age_yr', 'IOTF_BMI25p', 'Wasit2Hip')]
IOTF_BMI25p_Nback_cormat = cor.matrix(IOTF_BMI25p_Nback_cor.vars, IOTF_BMI25p_Nback_cor.varnames)

#####################################
####
####   Nback Performance - Models  ####
####
#####################################
#merge
UAE_cogBMI_NBACKDat.merge_long = merge(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_compiled[c(1, 105:118, 141:142)], id ='ParID', all.x = TRUE, all.y=FALSE)
UAE_cogBMI_NBACKDat.merge = merge(UAE_cogBMI_NBACKDat.merge, UAE_cogBMI_compiled[c(1, 105:118, 141:142)], id ='ParID', all.x = TRUE, all.y=FALSE)

##Scatter Plots
###Ballanced Accuracy
IOTF_BMI25p_BalAcc_plot = ggplot(UAE_cogBMI_NBACKDat.merge_long, aes(y = BalAcc, x = IOTF_BMI25p_c100, Group = Load)) +
  geom_smooth(method = 'loess', formula = y~x, aes(linetype = Load)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and Ballanced Accuracy') +
  scale_y_continuous(name='Ballanced Accuracy') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###d'
IOTF_BMI25p_dprime_plot = ggplot(UAE_cogBMI_NBACKDat.merge_long, aes(y = dprime, x = IOTF_BMI25p_c100, Group = Load)) +
  geom_smooth(method = 'loess', formula = y~x, aes(linetype = Load)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and dprime') +
  scale_y_continuous(name='dprime') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

###RT
IOTF_BMI25p_meanRT_plot = ggplot(UAE_cogBMI_NBACKDat.merge_long, aes(y = meanRT*1000, x = IOTF_BMI25p_c100, Group = Load)) +
  geom_smooth(method = 'loess', formula = y~x, aes(linetype = Load)) +
  #  ggtitle('Corrleation between Percent of IOTF BMI 25 (overweight) cuttoff and mean RT') +
  scale_y_continuous(name='meanRT') +
  scale_x_continuous(name='Percent of IOTF Overweight Cuttoff') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank())

##Load x BMI25p

#BalAcc
IOTF_BMI25p_BalAcc.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BMI25p_BalAcc.Load_sum = summary(IOTF_BMI25p_BalAcc.Load_mod)

IOTF_BMI25p_BalAcc.Load_IQge70_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_BalAcc.Load_IQge70_sum = summary(IOTF_BMI25p_BalAcc.Load_mod)

#dprime
IOTF_BMI25p_dprime.Load_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BMI25p_dprime.Load_sum = summary(IOTF_BMI25p_dprime.Load_mod)

IOTF_BMI25p_dprime.Load_IQge70_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_dprime.Load_IQge70_sum = summary(IOTF_BMI25p_dprime.Load_IQge70_mod)

#meanRT
IOTF_BMI25p_meanRT.Load_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BMI25p_meanRT.Load_sum = summary(IOTF_BMI25p_meanRT.Load_mod)

IOTF_BMI25p_meanRT.Load_IQge70_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_meanRT.Load_IQge70_sum = summary(IOTF_BMI25p_meanRT.Load_IQge70_mod)

#nFA
IOTF_BMI25p_nFA.Load_mod = lmer(nFA ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BMI25p_nFA.Load_sum = summary(IOTF_BMI25p_nFA.Load_mod)

IOTF_BMI25p_nFA.Load_IQge70_mod = lmer(nFA ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_nFA.Load_IQge70_sum = summary(IOTF_BMI25p_nFA.Load_IQge70_mod)

#pMiss
IOTF_BMI25p_pMiss.Load_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BMI25p_pMiss.Load_sum = summary(IOTF_BMI25p_pMiss.Load_mod)

IOTF_BMI25p_pMiss.Load_IQge70_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_BMI25p_pMiss.Load_IQge70_sum = summary(IOTF_BMI25p_pMiss.Load_IQge70_mod)

##Load x Glucose Impairment
UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN = factor(UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)

#BalAcc
IOTF_BalAcc_Glucose.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BalAcc_Glucose.Load_sum = summary(IOTF_BalAcc_Glucose.Load_mod)
IOTF_BalAcc_Glucose.Load_emmeans = emmeans(IOTF_BalAcc_Glucose.Load_mod, pairwise ~ ImpariedGlucose_YN | Load, var ='ImpariedGlucose_YN')

IOTF_Glucose_BalAcc.Load_IQge70_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_Glucose_BalAcc.Load_IQge70_sum = summary(IOTF_Glucose_BalAcc.Load_IQge70_mod)

Glucose.Load = data.frame(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$ImpariedGlucose_YN, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Load)
Glucose_BalAcc.Load_means = means.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)
Glucose_BalAcc.Load_se = se.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)

IOTF_BalAcc_Glucose.Load_plot = bar_graph.se(Glucose_BalAcc.Load_means, Glucose_BalAcc.Load_se, 'Load', 'BalAcc', 1, 0.5, UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)

#dprime
IOTF_dprime_Glucose.Load_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_dprime_Glucose.Load_sum = summary(IOTF_dprime_Glucose.Load_mod)

IOTF_dprime_Glucose.Load_IQge70_mod = lmer(dprime ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_dprime_Glucose.Load_IQge70_sum = summary(IOTF_dprime_Glucose.Load_IQge70_mod)

#meanRT
IOTF_meanRT_Glucose.Load_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_meanRT_Glucose.Load_sum = summary(IOTF_meanRT_Glucose.Load_mod)

IOTF_meanRT_Glucose.Load_IQge70_mod = lmer(meanRT ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_meanRT_Glucose.Load_IQge70_sum = summary(IOTF_meanRT_Glucose.Load_IQge70_mod)

##Load x Glucose Impairment x BMI25p
UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN = factor(UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)

#BalAcc
IOTF_BalAcc_Glucose.Load.BMIp25_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
IOTF_BalAcc_Glucose.Load.BMIp25_sum = summary(IOTF_BalAcc_Glucose.Load.BMIp25_mod)
IOTF_BalAcc_Glucose.Load.BMIp25_emmeans = emmeans(IOTF_BalAcc_Glucose.Load.BMIp25_mod, pairwise ~ ImpariedGlucose_YN | Load, var ='ImpariedGlucose_YN')

IOTF_Glucose_BalAcc.Load_IQge70_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
IOTF_Glucose_BalAcc.Load_IQge70_sum = summary(IOTF_Glucose_BalAcc.Load_IQge70_mod)

Glucose.Load = data.frame(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$ImpariedGlucose_YN, UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$Load)
Glucose_BalAcc.Load_means = means.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)
Glucose_BalAcc.Load_se = se.function(UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ], UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ]$BalAcc, Glucose.Load)

IOTF_BalAcc_Glucose.Load_plot = bar_graph.se(Glucose_BalAcc.Load_means, Glucose_BalAcc.Load_se, 'Load', 'BalAcc', 1, 0.5, UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)


# 
# #pMiss
# IOTF_BMI25p_pMiss.Load_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_BMI25p_pMiss.Load_sum = summary(IOTF_BMI25p_pMiss.Load_mod)
# 
# IOTF_Glucose_pMiss.Load_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + ImpariedGlucose_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_Glucose_pMiss.Load_sum = summary(IOTF_Glucose_pMiss.Load_mod)
# IOTF_Glucose_BalAcc.Load_emmeans = emmeans(IOTF_Glucose_pMiss.Load_mod, pairwise ~ ImpariedGlucose_YN | Load, var ='ImpariedGlucose_YN')
# 
# Glucose_BalAcc.Load_means = means.function(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$pMiss, Glucose.Load)
# Glucose_BalAcc.Load_se = se.function(UAE_cogBMI_NBACKDat.merge_long, UAE_cogBMI_NBACKDat.merge_long$pMiss, Glucose.Load)
# 
# bar_graph.se(Glucose_BalAcc.Load_means, Glucose_BalAcc.Load_se, 'Load', 'BalAcc', 0.7, 0, UAE_cogBMI_NBACKDat.merge_long$ImpariedGlucose_YN)
# 
# 
# IOTF_BMI25p_pMiss.Load_IQge70_mod = lmer(pMiss ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long[UAE_cogBMI_NBACKDat.merge_long$WASI_PRI_IQ >= 70, ])
# IOTF_BMI25p_pMiss.Load_IQge70_sum = summary(IOTF_BMI25p_pMiss.Load_IQge70_mod)
# 
# #other
# IOTF_Anemia_BalAcc.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + Anemia_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_Anemia_BalAcc.Load_sum = summary(IOTF_Anemia_BalAcc.Load_mod)
# 
# IOTF_Thyroid_BalAcc.Load_mod = lmer(BalAcc ~ WASI_PRI_IQ + SEScat + Mother_ed + Age_yr + Gender + IOTF_BMI25p_c100 + Thyriod_YN*Load + (1|ParID), data = UAE_cogBMI_NBACKDat.merge_long)
# IOTF_Thyroid_BalAcc.Load_sum = summary(IOTF_Thyroid_BalAcc.Load_mod)


# 
# 
# #merge with demo data
# UAE_cogBMI_compiled$ParID = UAE_cogBMI_compiled$Participant.Number
# NBack_goodDat_long = merge(NBack_goodDat_long, UAE_cogBMI_compiled[c(3, 5, 10:12, 19, 99, 100)], by = "ParID", all.x = TRUE)
# 
# xtabs(~CDC_WeightStatus + Load, data = NBack_goodDat_long[NBack_goodDat_long$Load1 == "B1", ])
# 
# NBack_goodDat_noUW_long = NBack_goodDat_long[NBack_goodDat_long$CDC_WeightStatus != "Underweight", ]
# 
# #### Ballance Accuracy x OB status
# Nback_BalAcc_Load.WeightStatus_noUW_IQmod = lmer(BalAcc ~ WASI_PRI_IQ + CDC_WeightStatus*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.WeightStatus_noUW_IQanova = anova(Nback_BalAcc_Load.WeightStatus_noUW_IQmod)
# 
# Nback_BalAcc_Load.WeightStatus_noUW_mod = lmer(BalAcc ~ CDC_WeightStatus*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.WeightStatus_noUW_anova = anova(Nback_BalAcc_Load.WeightStatus_noUW_mod)
# 
# Nback_BalAcc_Load.WeightStatus_mod = lmer(BalAcc ~ CDC_WeightStatus*Load + (1|ParID), data = NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.WeightStatus_anova = anova(Nback_BalAcc_Load.WeightStatus_mod)
# 
# NBack_goodDat_noUW_long$CDC_WeightStatus = factor(NBack_goodDat_noUW_long$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# Nback_Load.WeightStatus_datframe = data.frame(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$CDC_WeightStatus, NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$Load)
# Nback_BalAcc_Load.WeightStatus_noUW_means = means.function(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ], NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$BalAcc, Nback_Load.WeightStatus_datframe)
# Nback_BalAcc_Load.WeightStatus_noUW_se = se.function(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ], NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$BalAcc, Nback_Load.WeightStatus_datframe)
# bar_graph.se(Nback_BalAcc_Load.WeightStatus_noUW_means, Nback_BalAcc_Load.WeightStatus_noUW_se, "Load", "Accuracy", 1, 0.5, NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ]$CDC_WeightStatus)
# 
# Nback_Load.WeightStatus_datframe = data.frame(NBack_goodDat_noUW_long$CDC_WeightStatus, NBack_goodDat_noUW_long$Load)
# Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_means = means.function(NBack_goodDat_noUW_long, NBack_goodDat_noUW_long$BalAcc, Nback_Load.WeightStatus_datframe)
# Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_se = se.function(NBack_goodDat_noUW_long, NBack_goodDat_noUW_long$BalAcc, Nback_Load.WeightStatus_datframe)
# bar_graph.se(Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_means, Nback_BalAcc_Load.WeightStatus_noUW_Allpcor_se, "Load", "Accuracy", 1, 0.5, NBack_goodDat_noUW_long$CDC_WeightStatus)
# 
# 
# Nback_BalAcc_Load.OBstatus_noUW_mod = lmer(BalAcc ~ CDC_ObesityStatus*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.OBstatus_noUW_anova = anova(Nback_BalAcc_Load.OBstatus_noUW_mod)
# 
# Nback_BalAcc_Load.BMIpercentile_mod = lmer(BalAcc ~ CDC_BMIptile*Load + (1|ParID), data = NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.BMIpercentile_anova = anova(Nback_BalAcc_Load.BMIpercentile_mod)
# Nback_BalAcc_Load.BMIpercentile_sum = summary(lmer(BalAcc ~ CDC_BMIptile +Load + (1|ParID), data = NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ])
# )
# ggplot(NBack_goodDat_long[NBack_goodDat_long$BalAcc > 0.50, ], aes(x=CDC_BMIptile, y=BalAcc)) + 
#   geom_point(aes(color = CDC_WeightStatus))+
#   geom_smooth(method=lm)
# 
# Nback_BalAcc_Load.BMIpercentile_noUW_mod = lmer(BalAcc ~ CDC_BMIptile*Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# Nback_BalAcc_Load.BMIpercentile_noUW_anova = anova(Nback_BalAcc_Load.BMIpercentile_mod)
# Nback_BalAcc_Load.BMIpercentile_noUW_sum = summary(lmer(BalAcc ~ CDC_BMIptile + Load + (1|ParID), data = NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ])
# )
# 
# ggplot(NBack_goodDat_noUW_long[NBack_goodDat_noUW_long$BalAcc > 0.50, ], aes(x=CDC_BMIptile, y=BalAcc)) + 
#   geom_point(aes(color = CDC_WeightStatus))+
#   geom_smooth(method=lm)
# 
# #####################################
# ####                            
# ####              SST           ####
# ####                            
# #####################################
# 
# ############## Across Blocks #######################
# UAE_cogBMI_SSTDat_GoodDat = UAE_cogBMI_SSTDat[UAE_cogBMI_SSTDat$Practice == 'Y', ]
# UAE_cogBMI_SSTDat_GoodDat$BothBlocks = ifelse(!is.na(UAE_cogBMI_SSTDat_GoodDat$HighB1_percStop), ifelse(
#   !is.na(UAE_cogBMI_SSTDat_GoodDat$HighB2_percStop), 'Y', 'N'), ifelse(
#     !is.na(UAE_cogBMI_SSTDat_GoodDat$LowB1_percStop), ifelse(
#       !is.na(UAE_cogBMI_SSTDat_GoodDat$LowB2_percStop), 'Y', 'N'), 'N'))
# UAE_cogBMI_SSTDat_GoodDat$BothLoads = ifelse(!is.na(UAE_cogBMI_SSTDat_GoodDat$High_percStop), ifelse(
#   !is.na(UAE_cogBMI_SSTDat_GoodDat$Low_percStop), 'Y', 'N'), 'N')
# 
# UAE_cogBMI_SSTDat_GoodDat = merge(UAE_cogBMI_SSTDat_GoodDat, UAE_cogBMI_compiled[c(1, 3, 5, 10:12, 19, 99)], by.x = "ParID", by.y = "Participant.Number", all.x = TRUE)
# UAE_cogBMI_SSTDat_GoodDat_noUW = UAE_cogBMI_SSTDat_GoodDat[UAE_cogBMI_SSTDat_GoodDat$CDC_WeightStatus != "Underweight", ]
# UAE_cogBMI_SSTDat_GoodDat_noUW$CDC_WeightStatus = factor(UAE_cogBMI_SSTDat_GoodDat_noUW$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# 
# 
# UAE_cogBMI_SSTDat_AvgBlocks_long = melt(UAE_cogBMI_SSTDat_GoodDat[c(1:4, 77:85, 11, 23)], id.vars = names(UAE_cogBMI_SSTDat_GoodDat)[c(1:4, 77:85)])
# UAE_cogBMI_SSTDat_AvgBlocks_long$ssd = UAE_cogBMI_SSTDat_AvgBlocks_long$value
# UAE_cogBMI_SSTDat_AvgBlocks_long$Load = ifelse(UAE_cogBMI_SSTDat_AvgBlocks_long$variable == 'High_ssd', 'High', ifelse(
#   UAE_cogBMI_SSTDat_AvgBlocks_long$variable == 'Low_ssd', 'Low', NA))
#   
# UAE_cogBMI_SSTDat_AvgBlocks_long$Load = factor(UAE_cogBMI_SSTDat_AvgBlocks_long$Load)
# 
# UAE_cogBMI_SSTDat_SSRTmean = melt(UAE_cogBMI_SSTDat_GoodDat[c(12, 24)])
# UAE_cogBMI_SSTDat_SSRTint = melt(UAE_cogBMI_SSTDat_GoodDat[c(13, 25)])
# 
# 
# UAE_cogBMI_SSTDat_AvgBlocks_long = data.frame(UAE_cogBMI_SSTDat_AvgBlocks_long[c(1:13, 16:17)], UAE_cogBMI_SSTDat_SSRTmean[2], UAE_cogBMI_SSTDat_SSRTmean[2])
# names(UAE_cogBMI_SSTDat_AvgBlocks_long) = c(names(UAE_cogBMI_SSTDat_AvgBlocks_long)[1:15], 'SSRTmean', 'SSRTint')
# 
# UAE_cogBMI_SSTDat_AvgBlocks_long = UAE_cogBMI_SSTDat_AvgBlocks_long[!is.na(UAE_cogBMI_SSTDat_AvgBlocks_long$ssd), ]
# UAE_cogBMI_SSTDat_AvgBlocks_noUW_long = UAE_cogBMI_SSTDat_AvgBlocks_long[UAE_cogBMI_SSTDat_AvgBlocks_long$CDC_WeightStatus != "Underweight", ]
# UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$CDC_WeightStatus = factor(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# 
# xtabs(~CDC_WeightStatus + BothBlocks + Load, data = UAE_cogBMI_SSTDat_AvgBlocks_long)
# 
# SST_SSRT_Load.OBOWstatus_mod = lmer(SSRTint ~ CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_SSRT_Load.OBOWstatus_anova = anova(SST_SSRT_Load.OBOWstatus_mod)
# 
# SST_ssd_Load.OBOWstatus_mod = lmer(ssd ~ CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_ssd_Load.OBOWstatus_anova = anova(SST_ssd_Load.OBOWstatus_mod)
# 
# SST_SSRT_Load.Weightstatus_mod = lmer(SSRTint ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_SSRT_Load.Weightstatus_anova = anova(SST_SSRT_Load.Weightstatus_mod)
# 
# SST_ssd_Load.Weightstatus_mod = lmer(ssd ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_ssd_Load.Weightstatus_anova = anova(SST_ssd_Load.Weightstatus_mod)
# 
# SSD_Load.WeightStatus_dataframe = data.frame(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$CDC_WeightStatus, UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$Load)
# SSD_Load.WeightStatus_means = means.function(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ], UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$ssd, SSD_Load.WeightStatus_dataframe)
# SSD_Load.WeightStatus_se = se.function(UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ], UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ]$ssd, SSD_Load.WeightStatus_dataframe)
# bar_graph.se(SSD_Load.WeightStatus_means, SSD_Load.WeightStatus_se, "Load", "SSD",900, 0, UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus)
# 
# SST_SSD_Load.BMIptile_mod = lmer(ssd ~ CDC_BMIptile*Load + (1|ParID), data = UAE_cogBMI_SSTDat_AvgBlocks_noUW_long[UAE_cogBMI_SSTDat_AvgBlocks_noUW_long$BothBlocks == 'Y', ])
# SST_SSD_Load.BMIptile_anova = anova(SST_SSD_Load.BMIptile_mod)
# 
# ############## Just Block 2 or Block 1 #######################
# UAE_cogBMI_SSTDat_long_prac = UAE_cogBMI_SSTDat_long[UAE_cogBMI_SSTDat_long$Practice == 'Y', ]
# UAE_cogBMI_SSTDat_long_prac = merge(UAE_cogBMI_SSTDat_long_prac, UAE_cogBMI_compiled[c(1, 3, 5, 10:12, 19, 99)], by.x = "ParID", by.y = "Participant.Number", all.x = TRUE)
# UAE_cogBMI_SSTDat_long_prac_noUW = UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$CDC_WeightStatus != "Underweight", ]
# UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus = factor(UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus, levels = c("Obese", "Overweight", "HW"))
# 
# xtabs(~Load + CDC_WeightStatus, data = UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ])
# 
# SST_SSRT_Load.OBOWstatus.Block_mod = lmer(ssrtInt_cor ~ WASI_PRI_IQ + CDC_ObesityStatus*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# SST_SSRT_Load.OBOWstatus.Block_anova = anova(SST_SSRT_Load.OBOWstatus.Block_mod)
# 
# 
# SSRT_Load.OBOWstatus.Block_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac$Load, UAE_cogBMI_SSTDat_long_prac$CDC_ObesityStatus)
# SSRT_Load.OBOWstatus.Block_means = means.function(UAE_cogBMI_SSTDat_long_prac, UAE_cogBMI_SSTDat_long_prac$ssrtInt_cor, SSRT_Load.OBOWstatus.Block_dataframe)
# SSRT_Load.OBOWstatus.Block_se = se.function(UAE_cogBMI_SSTDat_long_prac, UAE_cogBMI_SSTDat_long_prac$ssrtInt_cor, SSRT_Load.OBOWstatus.Block_dataframe)
# bar_graph.se(SSRT_Load.OBOWstatus.Block_means, SSRT_Load.OBOWstatus.Block_se, "SSRT", "Group",500, 0, UAE_cogBMI_SSTDat_long_prac$Load)
# 
# SST_SSD_Load.OBOWstatus.Block_mod = lmer(ssd ~ WASI_PRI_IQ + CDC_ObesityStatus*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# SST_SSD_Load.WeightStatus_mod = lmer(ssd ~ WASI_PRI_IQ + CDC_WeightStatus*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# SST_SSD_Load.BMIptile.Block_mod = lmer(ssd ~ CDC_BMIptile*Load*Block + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW)
# 
# ## Block 1
# SST_SSRT_Load.WeightStatus_mod = lmer(ssrtInt_cor ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSRT_Load.WeightStatus_anova = anova(SST_SSRT_Load.WeightStatus_mod)
# 
# SST_SSD_Load.OBOWstatusB1_mod = lmer(ssd ~ Load_Order + CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSD_Load.OBOWstatusB1_anova = anova(SST_SSD_Load.OBOWstatusB1_mod)
# 
# SSD_Load.OBOWstatusB1_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$CDC_ObesityStatus, UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$Load)
# SSD_Load.OBOWstatusB1_means = means.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$ssd, SSD_Load.OBOWstatusB1_dataframe)
# SSD_Load.OBOWstatusB1_se = se.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 1, ]$ssd, SSD_Load.OBOWstatusB1_dataframe)
# bar_graph.se(SSD_Load.OBOWstatusB1_means, SSD_Load.OBOWstatusB1_se, "Weight Status", "Load",900, 0, UAE_cogBMI_SSTDat_long_prac$CDC_ObesityStatus)
# 
# SST_SSD_Load.WeightStatusB1_mod = lmer(ssd ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSD_Load.WeightStatusB1_anova = anova(SST_SSD_Load.WeightStatusB1_mod)
# 
# SSD_Load.WeightStatusB1_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$CDC_WeightStatus, UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$Load)
# SSD_Load.WeightStatusB1_means = means.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$ssd, SSD_Load.WeightStatusB1_dataframe)
# SSD_Load.WeightStatusB1_se = se.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ]$ssd, SSD_Load.WeightStatusB1_dataframe)
# bar_graph.se(SSD_Load.WeightStatusB1_means, SSD_Load.WeightStatusB1_se, "Load", "SSD",900, 0, UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus)
# 
# SST_SSD_Load.BMIptileB1_mod = lmer(ssd ~ CDC_BMIptile*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 1, ])
# SST_SSD_Load.BMIptileB1_anova = anova(SST_SSD_Load.BMIptileB1_mod)
# 
# ## Block 2
# SST_SSRT_Load.WeightStatus_mod = lmer(ssrtInt_cor ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSRT_Load.WeightStatus_anova = anova(SST_SSRT_Load.WeightStatus_mod)
# 
# SST_SSD_Load.OBOWstatusB2_mod = lmer(ssd ~ Load_Order + CDC_ObesityStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSD_Load.OBOWstatusB2_anova = anova(SST_SSD_Load.OBOWstatusB2_mod)
# 
# SSD_Load.OBOWstatusB2_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$CDC_ObesityStatus, UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$Load)
# SSD_Load.OBOWstatusB2_means = means.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$ssd, SSD_Load.OBOWstatusB2_dataframe)
# SSD_Load.OBOWstatusB2_se = se.function(UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ], UAE_cogBMI_SSTDat_long_prac[UAE_cogBMI_SSTDat_long_prac$Block == 2, ]$ssd, SSD_Load.OBOWstatusB2_dataframe)
# bar_graph.se(SSD_Load.OBOWstatusB2_means, SSD_Load.OBOWstatusB2_se, "Weight Status", "Load",900, 0, UAE_cogBMI_SSTDat_long_prac$CDC_ObesityStatus)
# 
# SST_SSD_Load.WeightStatusB2_mod = lmer(ssd ~ CDC_WeightStatus*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSD_Load.WeightStatusB2_anova = anova(SST_SSD_Load.WeightStatusB2_mod)
# 
# SSD_Load.WeightStatusB2_dataframe = data.frame(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$CDC_WeightStatus, UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$Load)
# SSD_Load.WeightStatusB2_means = means.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$ssd, SSD_Load.WeightStatusB2_dataframe)
# SSD_Load.WeightStatusB2_se = se.function(UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ], UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ]$ssd, SSD_Load.WeightStatusB2_dataframe)
# bar_graph.se(SSD_Load.WeightStatusB2_means, SSD_Load.WeightStatusB2_se, "Load", "SSD",900, 0, UAE_cogBMI_SSTDat_long_prac_noUW$CDC_WeightStatus)
# 
# SST_SSD_Load.BMIptileB2_mod = lmer(ssd ~ CDC_BMIptile*Load + (1|ParID), data = UAE_cogBMI_SSTDat_long_prac_noUW[UAE_cogBMI_SSTDat_long_prac_noUW$Block == 2, ])
# SST_SSD_Load.BMIptileB2_anova = anova(SST_SSD_Load.BMIptileB2_mod)
