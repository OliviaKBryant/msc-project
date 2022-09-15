# Assessing the Indirect Effects of COVID-19 Restrictions on Primary Care Contacts for Mental Health in England Using CPRD Aurum

This is a study assessing the impact of COVID-19 restrictinos in England on primary care contacts for anxiety, depression, OCD, self-harm, eating disorders, and severe mental illness using data from the Clinical Research Practice Datalink (CPRD) Aurum database. [Mansfield et al (2021)](https://www.thelancet.com/journals/landig/article/PIIS2589-7500(21)00017-0/fulltext) found evidence that there had been a significant reduction in primary care contacts for mental health illnesses following the start of the first COVID-19 lockdown. This study aims to extend the work of the original study by repeating the study with updated data to assess the impacts of the third national lockdown and the three-tier regional restrictions. The original code for the Mansfield et al study can be found [here](https://github.com/johntaz/COVID-Collateral).

### Table of contents
- [Project folder structure](#project-folder-structure)
  + [analysis](#analysis)
  + [analysis_data](#analysis_data)
  + [code](#code)
  + [plots](#plots)

##### analysis

- An analysis script for each part of the study (first lockdown, third lockdown, regional restrictions) is provided containing all of the functions run, and their inputs, to get the results of the study. 
- The analysis scripts source the functions from code/ITS.

##### analysis_data

- A folder is created for each part of the study and contains the preprocessed analysis CSV datasets for each of the outcomes.
- The analysis datasets contain the week of the study, the stratifier (overall, age, sex, ethnicity, region), category of stratifier, number of outcomes, number of people eligible, and a binary marker for lockdown.

##### code

###### ITS
ITS_binomial_models.R - contains functions to run the binomial GLM modelling the proportion of the population consulting for each outcome and the negative binomial ITS function that estimates the number of primary care contacts each week.
ITS_poisson_models.R - contains the function to run the Poisson ITS function that estimates the number of primary care contacts each week.
ITS_regional_model.R - contains an adapted version of the binomial GLM function that looks at groups of regions and the regional model that estimates the interaction between tier group and lockdown on primary care contacts.
ITS_help_functions.R - contains helper functions for the ITS including formatting tables and formatting outcome data.

###### data_preparation

- contains the function to generate the CSV files in analysis_data and runs the function for each of the outcomes and periods of restrictions.

###### plot_code

- contains functions for plots used in the ITS and the descriptive analysis.

##### plots

- contains folders for each of the studies: descriptive, first national lockdown, third national lockdown, regional restrictions
- contains all of the tables and figures created from running the files in analysis.

