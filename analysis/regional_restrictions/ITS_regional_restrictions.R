#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS National Lockdown 3
# Authors: Olivia Bryant
# Date Updated: 23/08/2022
# Notes: ITS analysis of regional restrictions on mental health outcomes
# Ref:
#-------------------------------------------------------------------------------
source("code/ITS/ITS_regional_models.R")
source("code/ITS/ITS_binomial_models.R")
source("code/ITS/ITS_poisson_models.R")
source("code/ITS/ITS_help_functions.R")

bkg_colour <- "gray99"


# read data from csv file
all_files <- list.files(here::here("analysis_data/regional_restrictions/"), pattern = "an_")
outcomes <- stringr::str_remove_all(all_files, c("an_|.csv"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
                                           "outcome_name" = (c("Anxiety",
                                                               "Depression",
                                                               "Eating Disorders", 
                                                               "OCD", 
                                                               "Self-harm", 
                                                               "Severe Mental Illness"))
)
plot_order <- c(1, 2, 3, 4, 5, 6) 
# load data --------------------------------------------------------------------
for(ii in 1:length(outcomes)){
  load_file <- read.csv(here::here("analysis_data/regional_restrictions/", paste0("an_", outcomes[ii], ".csv")))
  assign(outcomes[ii], load_file)
}

# split data into two tier groups ----------------------------------------------
# high tier: London, North East, North West, South West, West Midlands
# low tier: East Midlands, East of England, South Central, South East Coast, Yorkshire

low_group <- c(1, 2, 5, 7, 9)
high_group <- c(3, 4, 6, 8, 10)

anxiety_high <- split_regional_data('anxiety', high_group)
anxiety_low <- split_regional_data('anxiety', low_group)

depression_high <- split_regional_data('depression',high_group)
depression_low <- split_regional_data('depression', low_group)

feedingdisorder_high <- split_regional_data('feedingdisorder', high_group)
feedingdisorder_low <- split_regional_data('feedingdisorder', low_group)

ocd_high <- split_regional_data('ocd', high_group)
ocd_low <- split_regional_data('ocd', low_group)

selfharm_high <- split_regional_data('selfharm', high_group)
selfharm_low <- split_regional_data('selfharm', low_group)

smi_high <- split_regional_data('smi', high_group)
smi_low <- split_regional_data('smi', low_group)

#-------------------------------------------------------------------------------
# High Restriction Group
#-------------------------------------------------------------------------------

# Binomial ITS 3 week adjustment period: High restrictions group
anxiety <- anxiety_high
depression <- depression_high
ocd <- ocd_high
feedingdisorder <- feedingdisorder_high
selfharm <- selfharm_high
smi <- smi_high

pdf(file = here::here("plots/regional_restrictions/high", paste0("Tiers_3wks_HighRestrictions", ".pdf")), 
    width = 13, height = 14)
binomial_its_function_group(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-22"),
                      start_lockdown = as.Date("2020-10-14"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-03-31"),
                      chop_selfharm = F, 
                      display_from = as.Date("2020-04-01"),
                      table_path = "plots/regional_restrictions/high",
                      group = "high_tier",
                      remove_xmas = TRUE,
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

pdf(file = here::here("plots/regional_restrictions/high", paste0("Tiers_3wks_8thMarch", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-22"),
                            start_lockdown = as.Date("2020-10-14"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-03-31"),
                            display_from = as.Date("2020-04-01"),
                            table_path = "plots/regional_restrictions/high/poisson_counts_3wks_high_tier.csv")
dev.off()

#-------------------------------------------------------------------------------
# Low Restriction Group
#-------------------------------------------------------------------------------

# Binomial ITS 3 week adjustment period: low restrictions group
anxiety <- anxiety_low
depression <- depression_low
ocd <- ocd_high
feedingdisorder <- feedingdisorder_low
selfharm <- selfharm_low
smi <- smi_low

pdf(file = here::here("plots/regional_restrictions/low", paste0("Tiers_3wks_LowRestrictions", ".pdf")), 
    width = 13, height = 14)
binomial_its_function_group(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-22"),
                      start_lockdown = as.Date("2020-10-14"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-04-30"),
                      display_from = as.Date("2020-04-01"),
                      table_path = "plots/regional_restrictions/low",
                      remove_xmas = TRUE,
                      group = "low_tier",
                      incl_no_ldn_ribbon = FALSE
                      
)
dev.off()

pdf(file = here::here("plots/regional_restrictions/low", paste0("Tiers_3wks_8thMarch", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-22"),
                            start_lockdown = as.Date("2020-10-14"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-04-30"),
                            display_from = as.Date("2020-04-01"),
                            table_path = "plots/regional_restrictions/low/poisson_counts_3wks_low_tier.csv",
                            remove_xmas = TRUE,
                            incl_no_ldn_ribbon = FALSE
)
dev.off()

#-------------------------------------------------------------------------------
# Modelling with Both Groups
#-------------------------------------------------------------------------------
anxiety_high$group <- 'high tier'
anxiety_low$group <- 'low tier'
depression_high$group <- 'high tier'
depression_low$group <- 'low tier'
ocd_high$group <- 'high tier'
ocd_low$group <- 'low tier'
feedingdisorder_high$group <- 'high tier'
feedingdisorder_low$group <- 'low tier'
smi_high$group <- 'high tier'
smi_low$group <- 'low tier'
selfharm_high$group <- 'high tier'
selfharm_low$group <- 'low tier'

anxiety <- rbind(anxiety_low, anxiety_high)
depression <- rbind(depression_low, depression_high)
feedingdisorder <- rbind(feedingdisorder_low, feedingdisorder_high)
ocd <- rbind(ocd_low, ocd_high)
selfharm <- rbind(selfharm_low, selfharm_high)
smi <- rbind(smi_low, smi_high)

binomial_its_function_regional(outcomes_vec = outcomes,
                            cut_data = as.Date("2020-03-22"),
                            start_lockdown = as.Date("2020-10-14"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-03-31"),
                            chop_selfharm = FALSE, 
                            display_from = as.Date("2020-04-01"),
                            table_path = "plots/regional_restrictions/",
                            remove_xmas = TRUE
                            
)
