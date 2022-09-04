#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Author: Olivia Bryant
# Date updated: 09/08/2022
# Notes: function to aggregate CPRD data into weekly number of consultations
# and number eligible for the study. Stratified by gender, age, ethnicity, 
# region.
#-------------------------------------------------------------------------------
library(haven)
library(lubridate)
library(dplyr)

weekly_outcome <- function(observationData, patientData, weeklyDenoms, startDate, 
                           endDate, lockdown, study, directory){
  
  # check inputs are correct format, change to dates
  if(startDate != '' && endDate != '' && lockdown != ''){
    startDate = as.Date(startDate, format="%d%b%Y")
    endDate = as.Date(endDate, format="%d%b%Y")
    lockdown = as.Date(lockdown, format="%d%b%Y")
  } else {
    print('Enter a start date and end date as strings.')
  }
  
  # as study is mental health, using full Aurum population
  # convert to eventdate to date
  studyData <- observationData %>% 
    inner_join(patientData,by=c('patid')) %>%
    mutate(eventdate= as.Date(eventdate, format='%d%b%Y'))
  
  # filter between start and end date for study
  studyData <- studyData %>%
    filter(eventdate < endDate & eventdate >= startDate)
  
  # calculate study day, age, age group, study year
  # replace region if empty
  # filter by age
  
  studyData <- studyData %>%
    mutate(
      studyday = as.numeric(eventdate - startDate + 1),
      year = year(eventdate),
      age = year - yob,
      region = replace(region, region == "", 12)) %>%
    filter(age >= 11 & age <= 100) %>%
    mutate(agegroup = factor(10 * ceiling(age / 10)))
  
  # Generate number of weeks
  maxDays <- max(studyData$studyday)
  
  # check that study day > 0
  studyData <- studyData %>%
    filter(studyday > 0)
  
  # Use floor as only concerned with whole weeks
  numWeeks <- floor(maxDays/7)
  
  studyData <- studyData %>%
    mutate(week = ceiling(studyday/7)) %>%
    arrange(week, eventdate, patid)
  
  # Remove patients with multiple records in the same week
  studyData <- studyData[!duplicated(studyData[ , c('patid','week')]),]
  
  # Generate overall weekly outcomes...
  overallWeekly <- studyData %>%
    group_by(week) %>%
    summarise(numOutcome = n()) %>%
    mutate(
      weekDate = startDate + (7 * (week - 1)),
      stratifier = 'overall',
      category = 1) %>%
    select('weekDate','week','numOutcome', 'stratifier', 'category') %>%
    # delete partial weeks
    filter(weekDate + 7 < endDate)
  
  # Generate weekly outcomes by age
  ageWeekly <- studyData %>%
    group_by(week, agegroup) %>%
    summarise(numOutcome = n()) %>%
    mutate(
      weekDate = startDate + (7 * (week - 1)),
      stratifier = 'age',
      category = agegroup) %>%
    select('weekDate','week','numOutcome', 'stratifier', 'category') %>%
    # delete partial weeks
    filter(weekDate + 7 < endDate)
  
  # Generate weekly outcomes by gender
  genderWeekly <- studyData %>%
    group_by(week, gender) %>%
    summarise(numOutcome = n()) %>%
    mutate(
      weekDate = startDate + (7 * (week - 1)),
      stratifier = 'gender',
      category = gender) %>%
    select('weekDate','week','numOutcome', 'stratifier', 'category') %>%
    # delete partial weeks
    filter(weekDate + 7 < endDate)
  
  # Generate weekly outcomes by ethnicity
  ethnicityWeekly <- studyData %>%
    group_by(week, ethnicity) %>%
    summarise(numOutcome = n()) %>%
    mutate(
      weekDate = startDate + (7 * (week - 1)),
      stratifier = 'ethnicity',
      category = ethnicity) %>%
    select('weekDate','week','numOutcome', 'stratifier', 'category') %>%
    # delete partial weeks
    filter(weekDate + 7 < endDate)
  
  # Generate weekly outcomes by region
  regionWeekly <- studyData %>%
    group_by(week, region) %>%
    summarise(numOutcome = n()) %>%
    mutate(
      weekDate = startDate + (7 * (week - 1)),
      stratifier = 'region',
      category = region) %>%
    select('weekDate','week','numOutcome', 'stratifier', 'category') %>%
    # delete partial weeks
    filter(weekDate + 7 < endDate)
  
  # Creating weekly outcomes dataframe
  weeklyOutcomes <- rbind(overallWeekly, ageWeekly, genderWeekly,
                          ethnicityWeekly, regionWeekly)
  
  # Redact small cell counts
  weeklyOutcomes$numOutcome <- ifelse(weeklyOutcomes$numOutcome < 5, 5,
                                      weeklyOutcomes$numOutcome)
  
  # Add lockdown marker
  weeklyOutcomes$lockdown <- ifelse(weeklyOutcomes$weekDate < lockdown, 0, 1)
  
  weeklyOutcomesEligible <- merge(weeklyOutcomes, weeklyDenoms, 
                                  by = c('weekDate','stratifier','category'), 
                                  all.x = TRUE) %>%
    select(-time, -lockdown.y) %>%
    rename(lockdown = lockdown.x)
  
  # Save as csv file
  outputPath <- paste('analysis_data/',directory,'/an_',study, '.csv', sep='')
  write.csv(weeklyOutcomesEligible, file=outputPath, row.names=FALSE)
}

