#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS Helper Functions
# Authors: Alasdair Henderson, Olivia Bryant
# Date Updated: 11/09/2022
# Notes: Helper functions for binomial, negative binomial, and Poisson ITS
#-------------------------------------------------------------------------------

# FORMAT OUTCOME DATA
# Helper function that formats the overall observation data from the analysis
# CSV datasets for the ITS models
# Outputs formatted dataframe
format_outcome_data <- function(outcome, 
                              start_lockdown, 
                              lockdown_adjustment_period_wks, 
                              end_post_lockdown_period, 
                              cut_data) {
  
  # Christmas dates are 22nd to 29th inclusive. Christmas weeks are weeks
  # commencing on one of those dates.
  xmas_dates <- c(
    seq.Date(as.Date("2017-12-22"), as.Date("2017-12-29"), by = "1 day"),
    seq.Date(as.Date("2018-12-22"), as.Date("2018-12-29"), by = "1 day"),
    seq.Date(as.Date("2019-12-22"), as.Date("2019-12-29"), by = "1 day"),
    seq.Date(as.Date("2020-12-22"), as.Date("2020-12-29"), by = "1 day"))
  
  # load data
  df_outcome <- get(outcome)
  
  df_outcome <- df_outcome %>%
    filter(stratifier == "overall") %>%
    dplyr::select(-stratifier, -category) %>%
    mutate(time = week - 1) %>%
    mutate(weekPlot = (time * 7) + as.Date("2017-01-01")) %>%
    mutate(months = as.numeric(format.Date(weekPlot, "%m"))) %>%
    mutate(year = as.numeric(format.Date(weekPlot, "%Y")) - 2017) %>%
    mutate(pre_lockdown = ifelse(weekPlot < start_lockdown, 1 , 0),
           post_lockdown = ifelse(weekPlot >= 
                                    (start_lockdown + 
                                       (7 * lockdown_adjustment_period_wks)) & 
                                    weekPlot < end_post_lockdown_period, 1, 0)) %>%
    mutate_at("lockdown", ~ifelse(pre_lockdown == 1, 0,
                                  ifelse(post_lockdown == 1, 1,
                                         NA))) %>%				
    mutate(xmas = ifelse(weekPlot %in% xmas_dates, 1, 0)) %>%
    filter(weekPlot >= cut_data)
  return(df_outcome)
}

# FORMAT COUNTS TABLE
# Helper function that formats the counts table for the Poisson and negative
# binomial functions. 
# Outputs dataframe with the estimated counts in the weeks 1 month and 2 months
# after the end of the lockdown adjustment period, with 95% CI, and the 
# estimated cumulative sum since the end of lockdown.
format_counts_table <- function(outcome,
                                df_outcome, 
                                df_se, 
                                start_lockdown, 
                                lockdown_adjustment_period_wks) { 
  
  # identify the week 1 month and 2 months post-adjustment period
  mo1_post_ldn <- start_lockdown + (lockdown_adjustment_period_wks * 7) + 30
  mo3_post_ldn <- start_lockdown + (lockdown_adjustment_period_wks * 7) + 92
  
  
  tab_dates <- bind_cols("weekPlot" = df_outcome$weekPlot, df_se) %>%
    mutate(target_1mo = mo1_post_ldn,
           target_3mo = mo3_post_ldn,
           days2 = abs(target_1mo - weekPlot),
           days3 = abs(target_3mo - weekPlot),
           # estimated number of weekly outcomes without lockdown
           col1 = paste0(prettyNum(probline_noLdn * 1e6, 
                                   big.mark =",", 
                                   digits = 1, 
                                   scientific = FALSE), 
                         " (", prettyNum(lci_noLdn * 1e6,
                                         big.mark=",", 
                                         digits = 1, 
                                         scientific = FALSE), 
                         " - ", prettyNum(uci_noLdn * 1e6,
                                          big.mark = ",", 
                                          digits = 1, 
                                          scientific = FALSE),")"),
           # estimated number of weekly outcomes with lockdown
           col3 = paste0(prettyNum(predicted_vals * 1e6,
                                   big.mark = ",", 
                                   digits = 1, 
                                   scientific = FALSE), 
                         " (", prettyNum(lci * 1e6,
                                         big.mark = ",",
                                         digits = 1, 
                                         scientific = FALSE), 
                         " - ", prettyNum(uci * 1e6,
                                          big.mark = ",",
                                          digits = 1, 
                                          scientific = FALSE),")")) %>%
    # filter to post-lockdown data only
    filter(weekPlot >= start_lockdown+(lockdown_adjustment_period_wks * 7)) %>%
    ## calculate cumulative sum of predicted values  with and without lockdown 
    mutate(cumsum_ldn = cumsum(predicted_vals * 1e6),
           lci_cumsum_ldn = cumsum(lci * 1e6),
           uci_cumsum_ldn = cumsum(uci * 1e6),
           cumsum_noLdn = cumsum(probline_noLdn * 1e6),
           lci_cumsum_noLdn = cumsum(low_noLdn* 1e6),
           uci_cumsum_noLdn = cumsum(upp_noLdn * 1e6),
           prettyNum(uci * 1e6, big.mark=",", digits = 1, scientific = FALSE),
           # weekly difference in lockdown vs without lockdown
           col5 = prettyNum(signif((probline_noLdn * 1e6) - 
                                     (predicted_vals * 1e6), 3), 
                            big.mark = ",", digits = 1, scientific = FALSE),
           # cumulative sum of lockdown vs without lockdown
           col6 = prettyNum(signif((cumsum_noLdn) - (cumsum_ldn), 3), 
                            big.mark = ",", digits = 1, scientific = FALSE)) %>%
    # suppress small counts
    mutate(diff_predicted = (probline_noLdn * 1e6) - (predicted_vals * 1e6),
           cumsum_diff_predicted = (cumsum_noLdn) - (cumsum_ldn)) %>%
    mutate_at(.vars = c("col5"), ~ifelse(diff_predicted < 10 & diff_predicted > 0,
                                         "<10", 
                                         ifelse(diff_predicted < 100 
                                                & diff_predicted > 0,
                                                "<100",
                                                ifelse(diff_predicted > -10 
                                                       & diff_predicted < 0,
                                                       ">-10",
                                                       ifelse(diff_predicted > -100 
                                                              & diff_predicted < 0,
                                                              ">-100",
                                                              .))))) %>%
    mutate_at(.vars = c("col6"), ~ifelse(cumsum_diff_predicted < 10 
                                         & cumsum_diff_predicted > 0,
                                         "<10", 
                                         ifelse(cumsum_diff_predicted < 100 
                                                & cumsum_diff_predicted > 0,
                                                "<100",
                                                ifelse(cumsum_diff_predicted > -10 
                                                       & cumsum_diff_predicted < 0,
                                                       ">-10",
                                                       ifelse(cumsum_diff_predicted > -100 
                                                              & cumsum_diff_predicted < 0,
                                                              ">-100",
                                                              .))))) %>%
    # only keep the data for 1 month and 2 months post lockdown
    filter(days2 == min(days2) | 
             days3 == min(days3)) 
  
  rate_diff <- tab_dates %>% 
    mutate(rate_diff = (exp(pred) / denom) - (exp(pred_noLdn) / denom),
           chisq_stat = (exp(pred) - (((exp(pred) + exp(pred_noLdn)) * denom) / 
                                        (denom + denom)))^2 / 
             (((exp(pred) + exp(pred_noLdn)) * denom * denom) / (denom^2)),
           lci_rd = rate_diff - 1.96 * (sqrt((rate_diff^2) / chisq_stat)),
           uci_rd = rate_diff + 1.96 * (sqrt((rate_diff^2) / chisq_stat))) %>%
    dplyr::select(rate_diff, lci_rd, uci_rd)
  
  
  tab_fmt <- tab_dates %>% 
    bind_cols(rate_diff) %>%
    mutate(outcome = outcome_of_interest_namematch$outcome_name[outcome_of_interest_namematch$outcome == outcome]) %>%
    dplyr::select(outcome, weekPlot, starts_with("col")) %>%
    mutate_at("weekPlot", ~as.character(format.Date(., "%d-%b"))) %>%
    mutate_at("outcome", ~ifelse(row_number(.) == 2, "", .))
  
  return(tab_fmt)
}

# SPLIT REGIONAL DATA
# Helper function that splits the observations into groups as defined by the 
# tier restriction groupings, combines results, and formats in the same way as
# format_outcome_data
# Outputs formatted dataframe
split_regional_data <- function(outcome, group_region_codes) {
 
  df_outcome <- get(outcome)
  
  df_outcome <- df_outcome %>%
    filter(stratifier == "region") %>%
    filter(category %in% group_region_codes) %>%
    filter(year(weekDate) != 2021) %>%
    dplyr::select(-stratifier, -category) %>%
    group_by(weekDate, week, lockdown) %>%
    summarise(numOutcome = sum(numOutcome),
              numEligible = sum(numEligible), .groups='keep')
  
  df_outcome$stratifier <- rep('overall', nrow(df_outcome))
  df_outcome$category<- rep(1, nrow(df_outcome))
  
  return(df_outcome)
}
  
  
  
  
  
  
  
  
  
  
