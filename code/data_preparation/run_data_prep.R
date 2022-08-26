#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Author: Olivia Bryant
# Date updated: 09/08/2022
# Notes: runs weekly outcome function for mental health issues for and produces
# analysis CSV files for national lockdowm and regional restrictions.
#-------------------------------------------------------------------------------
source('code/data_preparation/generate_weekly_outcome.R')

patientData <- read_dta('datafiles/denoms/cr_overall_strat_summary.dta')
weeklyDenoms <- read_dta('datafiles/denoms/cr_overall_weekly_denoms.dta')

# anxiety
anxietyData <- read_dta('datafiles/mental/out/cr_anxiety_outcomes.dta')
weekly_outcome(anxietyData, patientData, weeklyDenoms, '01jan2017', '05may2021','03jan2021', 'anxiety', 'national_lockdown_3' )
weekly_outcome(anxietyData, patientData, weeklyDenoms, '01jan2017', '05may2021','14oct2020', 'anxiety', 'regional_restrictions' )

# depression
depressionData <- read_dta('datafiles/mental/out/cr_depression_outcomes.dta')
weekly_outcome(depressionData, patientData, weeklyDenoms, '01jan2017', '05may2021', '03jan2021', 'depression', 'national_lockdown_3')
weekly_outcome(depressionData, patientData, weeklyDenoms, '01jan2017', '05may2021', '14oct2020', 'depression', 'regional_restrictions')

# feeding disorders
feedingDisorderData <- read_dta('datafiles/mental/out/cr_feedingdisorder_outcomes.dta')
weekly_outcome(feedingDisorderData, patientData, weeklyDenoms, '01jan2017', '05may2021', '03jan2021', 'feedingdisorder', 'national_lockdown_3')
weekly_outcome(feedingDisorderData, patientData, weeklyDenoms, '01jan2017', '05may2021', '14oct2020', 'feedingdisorder', 'regional_restrictions')
               
# ocd
ocdData <- read_dta('datafiles/mental/out/cr_ocd_outcomes.dta')
weekly_outcome(ocdData, patientData, weeklyDenoms, '01jan2017', '05may2021', '03jan2021', 'ocd', 'national_lockdown_3')
weekly_outcome(ocdData, patientData, weeklyDenoms, '01jan2017', '05may2021', '14oct2020', 'ocd', 'regional_restrictions')

# self harm
selfHarmData <- read_dta('datafiles/mental/out/cr_selfharm_outcomes.dta')
weekly_outcome(selfHarmData, patientData, weeklyDenoms, '01jan2017', '05may2021', '03jan2021', 'selfharm', 'national_lockdown_3')
weekly_outcome(selfHarmData, patientData, weeklyDenoms, '01jan2017', '05may2021', '14oct2020', 'selfharm', 'regional_restrictions')

# serious mental illness
smiData <- read_dta('datafiles/mental/out/cr_smi_outcomes.dta')
weekly_outcome(smiData, patientData, weeklyDenoms, '01jan2017', '05may2021', '03jan2021', 'smi', 'national_lockdown_3')
weekly_outcome(smiData, patientData, weeklyDenoms, '01jan2017', '05may2021', '14oct2021', 'smi', 'regional_restrictions')
