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
library(forcats)
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
  arrange(weekDate) %>%
  group_by(year, stratifier, category) %>%
  filter(row_number() == 1) %>%
  group_by(year) %>%
  mutate(strat_total = max(numEligible)) %>%
  mutate(percentage = roun((numEligible / strat_total) * 100), 2)

write.csv(weeklyDenoms, "plots/descriptive/overall_population_summary.csv")
#-------------------------------------------------------------------------------
# Overall outcomes plot - showing Christmas
#-------------------------------------------------------------------------------

p1 <- overall_outcome_plot(anxiety, "Anxiety", "2017-01-01")
p2 <- overall_outcome_plot(depression, "Depression", "2017-01-01")
p3 <- overall_outcome_plot(ocd, "OCD", "2017-01-01")
p4 <- overall_outcome_plot(feedingdisorder, "Feeding Disorder", "2017-01-01")
p5 <- overall_outcome_plot(selfharm, "Self Harm", "2017-01-01")
p6 <- overall_outcome_plot(smi, "Severe Mental Illness","2017-01-01")

overall_outcome <- (p1 + p2) / (p3 + p4) / (p5 + p6)
overall_outcome
ggsave(plot = overall_outcome,'plots/descriptive/overall_outcomes.pdf', width = 11, height = 8.27, units = "in")


#-------------------------------------------------------------------------------
# Outcome plots by stratifier - 2020 and 2021
#-------------------------------------------------------------------------------
anxiety$outcome <- 'Anxiety'
depression$outcome <- 'Depression'
feedingdisorder$outcome <- 'Eating Disorder'
ocd$outcome <- 'OCD'
selfharm$outcome <- 'Self-Harm'
smi$outcome <- 'Severe Mental Illness'

all_outcomes <- rbind(anxiety, depression, feedingdisorder, ocd, selfharm, smi)%>%
  mutate(category_cat = category) %>%
  mutate(proportion = (numOutcome / numEligible) * 100) %>%
  mutate_at("category_cat" , ~ifelse(stratifier == "gender", 
                                     recode(.,`1` = "Female", `2` = "Male"),
                                     ifelse(stratifier == "age", 
                                            recode(.,`10` = "10-30", `20` = "10-30", `30` = "30-50", `40` = "30-50", `50` = "50-70", `60` = "50-70", `70` = "70+", `80` = "70+", `90` = "70+", `100` = "70+"),
                                            ifelse(stratifier == "region", 
                                                   recode(.,`1` = "North East",`2` = "North West",`3` = "Yorkshire & the Humber",`4` = "East Midlands",`5` = "West Midlands",`6` = "Eastern",`7` = "South West",`8` = "South Central",`9` = "London",`10` = "South East",`11` = "Northern Ireland"),
                                                   .)
                                     )
  )
  )

all_outcomes_gender <- all_outcomes %>%
  filter(stratifier == 'gender') %>%
  filter(weekDate >= "2020-01-01")

gender_plot_df <- all_outcomes_gender %>%
  mutate("category_cat" = recode(all_outcomes_gender$category,
                                 `1` = "Female",  
                                 `2` = "Male"))

all_outcomes_ethnicity <- all_outcomes %>%
  filter(stratifier == 'ethnicity') %>%
  filter(weekDate >= "2020-01-01") 

ethnicity_plot_df <- all_outcomes_ethnicity %>%
  mutate("category_cat" = recode(all_outcomes_ethnicity$category,
                                 `0` = "White",  
                                 `1` = "South Asian",
                                 `2` = "Black",
                                 `3` = "Other",
                                 `4` = "Mixed",
                                 `5` = "Not stated"))
# plot for regional differences
all_outcomes_regions <- all_outcomes %>%
  filter(stratifier == "region") %>%
  filter(category_cat != 12) %>%
  filter(category_cat != 11) %>%
  drop_na() %>%
  filter(weekDate >= "2020-01-01") 

all_outcomes_regions <- all_outcomes_regions%>%
  mutate("category_cat" = recode(all_outcomes_regions$category,
                                `1` = "North",  
                                `2` = "North", 
                                `3` = "North",
                                `4` = "Midlands",   
                                `5` = "Midlands",  
                                `6` = "Midlands", 
                                `7` = "South",  
                                `8` = "South",  
                                `9` = "London", 
                                `10` = "South"))
  
regional_plot_df <- all_outcomes_regions %>%
  group_by(category_cat, weekDate, outcome) %>%
  summarise(proportion = mean(proportion))

regional_plot <- stratified_plot(regional_plot_df)
gender_plot <- stratified_plot(gender_plot_df)
ethnicity_plot <-stratified_plot(ethnicity_plot_df)

ggsave(plot = regional_plot,'plots/descriptive/regional_stratifier_plot.pdf', width = 10, height = 8.27, units = "in")
ggsave(plot = gender_plot,'plots/descriptive/gender_stratifier_plot.pdf', width = 10, height = 8.27, units = "in")
ggsave(plot = ethnicity_plot,'plots/descriptive/ethnicity_stratifier_plot.pdf', width = 10, height = 8.27, units = "in")

#-------------------------------------------------------------------------------
# Pre-pandemic data compared to 2020 and 2021
#-------------------------------------------------------------------------------

plot_2020 <- NULL
for(ii in outcomes){
  print(ii)
  plot_2020 <- plot_2020 %>%
    bind_rows(
      format_historical_plot_data(ii, 2020, 2021)
    )
}

bkg_colour <- "white"
colors <- c("2020" = "cadetblue4", "2017-2019 average" = "black")
figure_2020_hist <- ggplot(plot_2020, aes(x = plotWeek, y = value, group = year)) +
  geom_line(data = filter(plot_2020, year == 2017), alpha = 0.2) +  
  geom_line(data = filter(plot_2020, year == 2018), alpha = 0.2) +   
  geom_line(data = filter(plot_2020, year == 2019), alpha = 0.2) +
  geom_line(aes(y = value_hist, col = "2017-2019 average"), lwd = 1.2) +
  geom_line(aes(y = value_20, col = "2020"), lty = 5, lwd = 0.8) +
  geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = "cadetblue", lty = 0) +
  scale_x_date(date_labels = "%b", breaks = "2 months") +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
  labs(x = "Date", y = "% Study Population With Contacts for Condition", caption = "OCD: Obsessive Compulsive Disorder") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
        legend.position = "top",
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill = bkg_colour, colour = NA),
        strip.background = element_rect(fill = bkg_colour, colour =  NA),
        strip.text = element_text(size = 12, hjust = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
        panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
  scale_color_manual(name = "",
                     breaks = c("2017-2019 average", "2020"),
                     labels = c("2017-2019 average", "2020"),
                     values = colors)

ggsave(plot = figure_2020_hist,'plots/descriptive/overall_2020_historical_comparison.pdf', width = 11.69, height = 8.27, units = "in")

plot_2021 <- NULL
for(ii in outcomes){
  print(ii)
  plot_2021 <- plot_2021 %>%
    bind_rows(
      format_historical_plot_data(ii, 2021, 2020)
    )
}

plot_2021 <- plot_2021[complete.cases(plot_2021), ]

bkg_colour <- "white"
colors <- c("2020" = "cadetblue4", "2017-2019 average" = "black", "2021" = "coral4")

figure_2020_hist <- ggplot(plot_2020, aes(x = plotWeek, y = value, group = year)) +
  geom_line(data = filter(plot_2020, year == 2017), alpha = 0.2) +  
  geom_line(data = filter(plot_2020, year == 2018), alpha = 0.2) +   
  geom_line(data = filter(plot_2020, year == 2019), alpha = 0.2) +
  geom_line(aes(y = value_hist, col = "2017-2019 average"), lwd = 1.2) +
  geom_line(aes(y = value_20, col = "2020"), lty = 5, lwd = 0.8) +
  geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = "cadetblue", lty = 0, alpha = 0.3) +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = "darkgrey") +
  geom_vline(xintercept = as.Date("1991-11-05"), linetype = "dashed", col = "darkgrey") +
  labs(x = "Date", y = "% Study Population With Contacts for Condition", caption = "OCD: Obsessive Compulsive Disorder") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
        legend.position = "top",
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill = bkg_colour, colour = NA),
        strip.background = element_rect(fill = bkg_colour, colour =  NA),
        strip.text = element_text(size = 12, hjust = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
        panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
  scale_color_manual(name = "",
                     breaks = c("2017-2019 average", "2020"),
                     labels = c("2017-2019 average", "2020"),
                     values = colors)

figure_2021_hist <- ggplot(plot_2021, aes(x = plotWeek, y = value, group = year)) +
  geom_line(data = filter(plot_2021, year == 2017), alpha = 0.2) +  
  geom_line(data = filter(plot_2021, year == 2018), alpha = 0.2) +   
  geom_line(data = filter(plot_2021, year == 2019), alpha = 0.2) +
  geom_line(aes(y = value_hist, col = "2017-2019 average"), lwd = 1.2) +
  geom_line(aes(y = value_20, col = "2021"), lty = 5, lwd = 0.8) +
  geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = "coral", lty = 0, alpha = 0.3) +
  scale_x_date(date_labels = "%b", breaks = "2 months") +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  xlim(c(as.Date("1990-12-31"), as.Date("1991-05-01")))+
  geom_vline(xintercept = as.Date("1991-01-06"), linetype = "dashed", col = "darkgrey") +
  labs(x = "Date", y = "% Study Population With Contacts for Condition", caption = "OCD: Obsessive Compulsive Disorder") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
        legend.position = "top",
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill = bkg_colour, colour = NA),
        strip.background = element_rect(fill = bkg_colour, colour =  NA),
        strip.text = element_text(size = 12, hjust = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
        panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
  scale_color_manual(name = "",
                     breaks = c("2017-2019 average", "2021"),
                     labels = c("2017-2019 average", "2021"),
                     values = colors)

ggsave(plot = figure_2021_hist,'plots/descriptive/overall_2021_historical_comparison.pdf', width = 11.69, height = 8.27, units = "in")

