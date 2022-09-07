#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS National Lockdown 3
# Authors: Alasdair Henderson, Olivia Bryant
# Date Updated: 23/08/2022
# Notes: ITS analysis of lockdown 3 on mental health outcomes
# Ref:
#-------------------------------------------------------------------------------
source("code/ITS/ITS_binomial_models.R")
source("code/ITS/ITS_poisson_models.R")

bkg_colour <- "gray99"


# read data from csv file
all_files <- list.files(here::here("analysis_data/national_lockdown_3/"), pattern = "an_")
outcomes <- stringr::str_remove_all(all_files, c("an_|.csv"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
																					 "outcome_name" = (c("Anxiety",
																					 										"Depression","Eating Disorders", 
																					 									 "OCD", "Self-harm", "Severe Mental Illness"))
)
plot_order <- c(1, 2, 3, 4, 5, 6) 
# load data --------------------------------------------------------------------
for(ii in 1:length(outcomes)){
	load_file <- read.csv(here::here("analysis_data/national_lockdown_3/", paste0("an_", outcomes[ii], ".csv")))
	assign(outcomes[ii], load_file)
}

#-------------------------------------------------------------------------------
# BINOMIAL ITS
#-------------------------------------------------------------------------------

# Binomial ITS 0 week adjustment: 22nd March -----------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_0wks_22ndMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
             cut_data = as.Date("2020-03-22"),
             start_lockdown = as.Date("2021-01-05"),
             lockdown_adjustment_period_wks = 0,
             end_post_lockdown_period = as.Date("2021-05-31"),
             chop_selfharm = T, 
             display_from = as.Date("2020-09-01"),
             table_path = "plots/national_lockdown_3/binomial",
             incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 3 week adjustment: 22nd March -----------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_3wks_22ndMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
						 cut_data = as.Date("2020-03-22"),
						 start_lockdown = as.Date("2021-01-05"),
						 lockdown_adjustment_period_wks = 3,
						 end_post_lockdown_period = as.Date("2021-05-31"),
						 chop_selfharm = T, 
						 display_from = as.Date("2020-09-01"),
						 table_path = "plots/national_lockdown_3/binomial",
						 incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 3 week adjustment: 22nd March - XMAS REMOVED --------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_3wks_22ndMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-22"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      remove_xmas = TRUE,
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 5 week adjustment: 22nd March -----------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_5wks_22ndMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
             cut_data = as.Date("2020-03-22"),
             start_lockdown = as.Date("2021-01-05"),
             lockdown_adjustment_period_wks = 5,
             end_post_lockdown_period = as.Date("2021-05-31"),
             chop_selfharm = T, 
             display_from = as.Date("2020-09-01"),
             table_path = "plots/national_lockdown_3/binomial",
             incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 5 week adjustment: 22nd March - XMAS REMOVED --------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_5wks_22ndMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-22"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 5,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      remove_xmas = TRUE,
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 7 week adjustment: 22nd March -----------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_7wks_22ndMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
             cut_data = as.Date("2020-03-22"),
             start_lockdown = as.Date("2021-01-05"),
             lockdown_adjustment_period_wks = 7,
             end_post_lockdown_period = as.Date("2021-05-31"),
             chop_selfharm = T, 
             display_from = as.Date("2020-09-01"),
             table_path = "plots/national_lockdown_3/binomial",
             incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 7 week adjustment: 22nd March - XMAS REMOVED --------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_7wks_22ndMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-22"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 7,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      remove_xmas = TRUE,
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 0 week adjustment: 8th March ------------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_0wks_8thMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-08"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 0,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 3 week adjustment: 8th March ------------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_3wks_8thMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-08"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 3 week adjustment: 8th March - XMAS REMOVED ---------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_3wks_8thMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-08"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 3,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      remove_xmas = TRUE,
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 5 week adjustment: 8th March ------------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_5wks_8thMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-08"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 5,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 5 week adjustment: 8th March - NO XMAS --------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_5wks_8thMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-08"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 5,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      remove_xmas = TRUE,
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 7 week adjustment: 8th March ------------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_7wks_8thMarch", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-08"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 7,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

# Binomial ITS 7 week adjustment: 8th March - NO XMAS---------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_7wks_8thMarch_noXmas", ".pdf")), 
    width = 13, height = 14)
binomial_its_function(outcomes_vec = outcomes,
                      cut_data = as.Date("2020-03-08"),
                      start_lockdown = as.Date("2021-01-05"),
                      lockdown_adjustment_period_wks = 7,
                      end_post_lockdown_period = as.Date("2021-05-31"),
                      chop_selfharm = T, 
                      display_from = as.Date("2020-09-01"),
                      table_path = "plots/national_lockdown_3/binomial",
                      remove_xmas = TRUE,
                      incl_no_ldn_ribbon = FALSE
)
dev.off()

#-------------------------------------------------------------------------------
# POISSON ITS
#-------------------------------------------------------------------------------

# Poisson Counts ITS 3 week adjustment: 8th March - NO XMAS---------------------
pdf(file = here::here("plots/national_lockdown_3/poisson", paste0("ThirdLockdown_3wks_8thMarch_noXmas", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-08"),
                            start_lockdown = as.Date("2021-01-05"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-08-01"),
                            display_from = as.Date("2020-09-01"),
                            remove_xmas = TRUE,
                            incl_no_ldn_ribbon = FALSE,
                            table_path = "plots/national_lockdown_3/poisson/poisson_counts_3wks_8March_noXmas.csv")
dev.off()

# Poisson Counts ITS 5 week adjustment: 8th March ------------------------------
pdf(file = here::here("plots/national_lockdown_3/poisson", paste0("ThirdLockdown_5wks_8thMarch", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-08"),
                            start_lockdown =   as.Date("2021-01-05"),
                            lockdown_adjustment_period_wks = 5,
                            end_post_lockdown_period = as.Date("2021-08-01"),
                            display_from = as.Date("2020-09-01"),
                            remove_xmas = TRUE,
                            incl_no_ldn_ribbon = FALSE,
                            table_path = "plots/national_lockdown_3/poisson/poisson_counts_5wks_8March.csv")
dev.off()

# Poisson Counts ITS 7 week adjustment: 8th March ------------------------------
pdf(file = here::here("plots/national_lockdown_3/poisson", paste0("ThirdLockdown_7wks_8thMarch", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-08"),
                            start_lockdown =   as.Date("2021-01-05"),
                            lockdown_adjustment_period_wks = 7,
                            end_post_lockdown_period = as.Date("2021-08-01"),
                            display_from = as.Date("2020-09-01"),
                            remove_xmas = TRUE,
                            incl_no_ldn_ribbon = FALSE,
                            table_path = "plots/national_lockdown_3/poisson/poisson_counts_7wks_8March.csv")
dev.off()

# Poisson Counts ITS 3 week adjustment: 22th March ------------------------------
pdf(file = here::here("plots/national_lockdown_3/poisson", paste0("ThirdLockdown_3wks_22ndMarch", ".pdf")), 
    width = 13, height = 10)
its_counts_poisson_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-22"),
                            start_lockdown = as.Date("2021-01-05"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-08-01"),
                            display_from = as.Date("2020-09-01"),
                            remove_xmas = TRUE,
                            incl_no_ldn_ribbon = FALSE,
                            table_path = "plots/national_lockdown_3/poisson/poisson_counts_3wks_22March.csv")
dev.off()


#-------------------------------------------------------------------------------
# NEGATIVE BINOMIAL ITS
#-------------------------------------------------------------------------------

# Negative Binomial Counts ITS 3 week adjustment: 8th March - NO XMAS ----------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_3wks_8thMarch_negBinom_noXmas", ".pdf")), 
    width = 13, height = 10)
neg_binomial_counts_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-08"),
                            start_lockdown = as.Date("2021-01-05"),
                            lockdown_adjustment_period_wks = 3,
                            end_post_lockdown_period = as.Date("2021-08-01"),
                            display_from = as.Date("2020-09-01"),
                            remove_xmas = TRUE,
                            table_path = "plots/national_lockdown_3/binomial/negBinom_counts_3wks_8March_noXmas.csv")
dev.off()

# Negative Binomial Counts ITS 5 week adjustment: 8th March ------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_5wks_8thMarch_negBinom", ".pdf")), 
    width = 13, height = 10)
neg_binomial_counts_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-08"),
                            start_lockdown =   as.Date("2021-01-05"),
                            lockdown_adjustment_period_wks = 5,
                            end_post_lockdown_period = as.Date("2021-08-01"),
                            display_from = as.Date("2020-09-01"),
                            table_path = "plots/national_lockdown_3/binomial/negBinom_counts_5wks_8March.csv")
dev.off()

# Negative Binomial Counts ITS 7 week adjustment: 8th March ------------------------------
pdf(file = here::here("plots/national_lockdown_3/binomial", paste0("ThirdLockdown_7wks_8thMarch_negBinom", ".pdf")), 
    width = 13, height = 10)
neg_binomial_counts_function(outcome = outcomes,
                            cut_data = as.Date("2020-03-08"),
                            start_lockdown =   as.Date("2021-01-05"),
                            lockdown_adjustment_period_wks = 7,
                            end_post_lockdown_period = as.Date("2021-08-01"),
                            display_from = as.Date("2020-09-01"),
                            table_path = "plots/national_lockdown_3/binomial/negBinom_counts_7wks_8March.csv")
dev.off()


