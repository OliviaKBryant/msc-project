#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Author: Olivia Bryant
# Date updated: 16/08/2022
# Notes: descriptive analysis and EDA for third national UK lockdown data for
# mental health
#-------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(haven)
library(here)
library(lubridate)
library(scales)
library(patchwork)
source("code/plot_code/descriptive_plot_code.R")

lockdownFiles <- list.files(here::here("analysis_data/national_lockdown_3/"), 
                            pattern = "an_")
outcomes <- stringr::str_remove_all(lockdownFiles, c("an_|.csv"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
                                           "outcome_name" = (c("Anxiety",
                                                               "Depression",
                                                               "Eating Disorders", 
                                                               "OCD", 
                                                               "Self-harm", 
                                                               "Severe Mental Illness"))
)

for(ii in 1:length(outcomes)){
  load_file <- read.csv(here::here("analysis_data/national_lockdown_3/", paste0("an_", outcomes[ii], ".csv")))
  assign(outcomes[ii], load_file)
}

#-------------------------------------------------------------------------------
# Produce descriptive table for study population
#-------------------------------------------------------------------------------
weeklyDenoms <- read_dta('datafiles/denoms/cr_overall_weekly_denoms.dta')

weeklyDenoms <- weeklyDenoms %>%
  mutate(year = year(weekDate)) %>%
  group_by(year, stratifier, category) %>%
  summarize(count = sum(numEligible)) %>%
  group_by(year) %>%
  mutate(year_total = max(count),
         percentage = round((count/year_total)*100, 2))

#-------------------------------------------------------------------------------
# Overall outcomes plot
#-------------------------------------------------------------------------------

p1 <- overallOutcomePlot(anxiety, "Anxiety", 0.01, "2020-01-01")
p2 <- overallOutcomePlot(depression, "Depression", 0.01, "2020-01-01")
p3 <- overallOutcomePlot(ocd, "OCD", 0.001, "2020-01-01")
p4 <- overallOutcomePlot(feedingdisorder, "Feeding Disorder", 0.002, "2020-01-01")
p5 <- overallOutcomePlot(selfharm, "Self Harm", 0.001, "2020-01-01")
p6 <- overallOutcomePlot(smi, "Severe Mental Illness", 0.01, "2020-01-01")

overallOutcome <- (p1 + p2) / (p3 + p4) / (p5 + p6) 
ggsave(plot = overallOutcome,'plots/descriptive/overall_outcomes.pdf', width = 11.69, height = 8.27, units = "in")


#-------------------------------------------------------------------------------
# Outcome plots by stratifier - 2020 and 2021
#-------------------------------------------------------------------------------

anxiety_gender <- anxiety %>%
  filter(stratifier == 'gender') %>%
  filter(weekDate >= "2020-01-01") %>%
  mutate(proportion = (numOutcome/numEligible)*100) %>%
  mutate(gender = as.factor(category))

ggplot(anxiety_gender, aes(x =as.Date(weekDate), y = proportion, color = gender)) +
  geom_line(aes(group=gender)) +
  geom_point(size = 0.1) +
  xlab("Date") +
  ylab("Proportion Overall") +
  ggtitle("Anxiety") +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size=.1, color="grey"),
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.text.y = element_text()) +
  scale_x_date(date_labels = "%b %Y", breaks = breaks_pretty(10),labels = scales::label_date_short())+
  expand_limits(y=0)


