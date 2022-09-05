#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS National Lockdown 1
# Authors: Olivia Bryant
# Date Updated: 29/08/2022
# Notes: ITS analysis of lockdown 1 on mental health outcomes
# Ref:
#-------------------------------------------------------------------------------
source("code/ITS/ITS_binomial_models.R")
source("code/ITS/ITS_poisson_models.R")

bkg_colour <- "gray99"


# read data from csv file
all_files <- list.files(here::here("analysis_data/national_lockdown_1/"), pattern = "an_")
outcomes <- stringr::str_remove_all(all_files, c("an_|.csv"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
                                           "outcome_name" = (c("Anxiety",
                                                               "Depression","Eating Disorders", 
                                                               "OCD", "Self-harm", "Severe Mental Illness"))
)
plot_order <- c(1, 2, 3, 4, 5, 6) 

# load data --------------------------------------------------------------------
for(ii in 1:length(outcomes)){
  load_file <- read.csv(here::here("analysis_data/national_lockdown_1/", paste0("an_", outcomes[ii], ".csv")))
  assign(outcomes[ii], load_file)
}

#-------------------------------------------------------------------------------
# BINOMIAL ITS
#-------------------------------------------------------------------------------

# Binomial ITS 3 week adjustment: 8th March ------------------------------------
pdf(file = here::here("plots/national_lockdown_1", paste0("FirstLockdown_3wks_8thMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2017-01-01"),
                      start_lockdown = as.Date("2020-03-08"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      display_from = as.Date("2020-01-01"),
                      table_path = "plots/national_lockdown_1"
)
dev.off()

# Binomial ITS 3 week adjustment: 8th March - NO XMAS --------------------------
pdf(file = here::here("plots/national_lockdown_1", paste0("FirstLockdown_3wks_8thMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2017-01-01"),
                      start_lockdown = as.Date("2020-03-08"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      display_from = as.Date("2020-01-01"),
                      table_path = "plots/national_lockdown_1",
                      remove_xmas = TRUE
)
dev.off()


# Binomial ITS 3 week adjustment: 22nd March ------------------------------------
pdf(file = here::here("plots/national_lockdown_1", paste0("FirstLockdown_3wks_22ndMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2017-01-01"),
                      start_lockdown = as.Date("2020-03-22"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      display_from = as.Date("2020-01-01"),
                      table_path = "plots/national_lockdown_1"
)
dev.off()

# Binomial ITS 3 week adjustment: 22nd March - NO XMAS --------------------------
pdf(file = here::here("plots/national_lockdown_1", paste0("FirstLockdown_3wks_22ndMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2017-01-01"),
                      start_lockdown = as.Date("2020-03-22"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      display_from = as.Date("2020-01-01"),
                      table_path = "plots/national_lockdown_1",
                      remove_xmas = TRUE
)
dev.off()

#-------------------------------------------------------------------------------
# POISSON ITS
#-------------------------------------------------------------------------------

# Poisson Counts ITS 3 week adjustment: 8th March ------------------------------
pdf(file = here::here("plots/national_lockdown_1/", paste0("FirstLockdown_3wks_8thMarch_Poisson", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2017-01-01"),
                            start_lockdown = as.Date("2020-03-08"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-05-31"),
                            display_from = as.Date("2020-01-01"),
                            table_path = "plots/national_lockdown_1/poisson_counts_3wks_8March.csv")
dev.off()

# Poisson Counts ITS 3 week adjustment: 22nd March ------------------------------
pdf(file = here::here("plots/national_lockdown_1/", paste0("FirstLockdown_3wks_22ndMarch_Poisson", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2017-01-01"),
                            start_lockdown = as.Date("2020-03-22"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-05-31"),
                            display_from = as.Date("2020-01-01"),
                            table_path = "plots/national_lockdown_1/poisson_counts_3wks_22March.csv")
dev.off()

