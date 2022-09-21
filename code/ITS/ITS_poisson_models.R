#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS Poisson Models
# Authors: Alasdair Henderson, Olivia Bryant
# Date Updated: 23/08/2022
# Notes: ITS analysis of lockdown on multiple outcomes using Poisson models
#-------------------------------------------------------------------------------
source("code/ITS/ITS_help_functions.R")
source("code/plot_code/ITS_plot_code.R")

# load the packages
library(foreign)
library(tsModel)
library(lmtest)
library(Epi)
library(multcomp)
library(splines)
library(vcd)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(scales)

#-------------------------------------------------------------------------------
# ITS COUNTS POISSON FUNCTION
#-------------------------------------------------------------------------------
# ITS function that uses Poisson regression to model counts of primary care 
# contacts.
# Inputs: cut_data is the date of the start of the pre-interruption period.
# table_path is the relative path where the user wants to save the counts
# table
# incl_no_ldn_ribbon is a Boolean of whether to include the 95% CI ribbon for
# the counterfactual predictions
# Outputs: plot of counterfactual vs observed counts. Table of estimated 
# numbers of primary care contacts with/without lockdown and cumultive 
# difference.
#-------------------------------------------------------------------------------

its_counts_poisson_function <- function(outcomes,
                                        cut_data = as.Date("2018-01-01"),
                                        start_lockdown =   as.Date("2020-03-08"),
                                        lockdown_adjustment_period_wks = 3,
                                        end_post_lockdown_period = as.Date("2020-08-01"),
                                        display_from = as.Date("2020-01-01"),
                                        chop_selfharm = TRUE,
                                        table_path,
                                        remove_xmas = FALSE,
                                        incl_no_ldn_ribbon = TRUE) {
  
    
      plot_outcome <- function(outcome) {
        
        # cut self-harm data pre-lockdown to be from 2019
        if (outcome == "selfharm" & chop_selfharm) {
          cutData <- as.Date("2019-01-01")
        }
        
        # format outcome data
        df_outcome <- format_outcome_data(outcome, 
                                          start_lockdown, 
                                          lockdown_adjustment_period_wks, 
                                          end_post_lockdown_period,
                                          cut_data)
          
        if (remove_xmas) {
          df_outcome <- df_outcome %>%
            filter(xmas == 0)
        }
        
        # start of post-lockdown period
        ldn_centre <- df_outcome$time[min(which(df_outcome$lockdown == 1))]
        
        # Poisson model to obtain residuals. Quasipoisson to allow for 
        # dispersion parameter to be input later
        po_model1 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time 
                         + I(time-ldn_centre):lockdown + as.factor(months) , 
                         family=quasipoisson, 
                         data = filter(df_outcome,
                                       !is.na(lockdown)))
        
        # store lagged residuals
        lagres1 <- lag(residuals(po_model1))
        
        # full model with lagged residuals to adjust for seasonality
        po_model2 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + 
                           time + I(time-ldn_centre):lockdown + 
                           as.factor(months) + lagres1, family=quasipoisson, 
                         data = filter(df_outcome, !is.na(lockdown)))
        
        # adjust predicted values with dispersion parameter
        pearson_gof <- sum(residuals(po_model2, type = "pearson")^2)
        df <- po_model2$df.residual
        deviance_adjustment <- pearson_gof / df
        
        po_lagres_timing <- bind_cols("time" = df_outcome$time[!is.na(df_outcome$lockdown)],
                                      "lagres1" = lagres1)
        
        # sets up data frame to predict values from 
        outcome_pred <- df_outcome %>%
          left_join(po_lagres_timing, by = "time") %>%
          mutate_at("lagres1", ~(. = 0))
        
        # predict values if there is a lockdown
        pred1 <- predict(po_model2, newdata = outcome_pred, se.fit = TRUE, 
                         interval="confidence", dispersion = deviance_adjustment)
        predicted_vals <- pred1$fit
        stbp <- pred1$se.fit
        
        # predict values if no lockdown 
        outcome_pred_nointervention <- outcome_pred %>%
          mutate_at("lockdown", ~(. = 0))
        
        predicted_vals_nointervention <- 
          predict(po_model2, 
                  newdata = outcome_pred_nointervention, 
                  se.fit = TRUE, 
                  dispersion = deviance_adjustment) 
        # save predicted values and SE
        stbp_noLdn <- predicted_vals_nointervention$se.fit	
        predicted_vals_noLdn <- predicted_vals_nointervention$fit	
        
        # calculate and transform confidence intervals
        df_se <- bind_cols(stbp = stbp, 
                           pred = predicted_vals, 
                           stbp_noLdn = stbp_noLdn, 
                           pred_noLdn = predicted_vals_noLdn, 
                           denom = df_outcome$numEligible) %>%
          mutate(
            # CIs
            upp = pred + (1.96 * stbp),
            low = pred - (1.96 * stbp),
            upp_noLdn = pred_noLdn + (1.96 * stbp_noLdn),
            low_noLdn = pred_noLdn - (1.96 * stbp_noLdn),
            # probline
            predicted_vals = exp(pred) / denom,
            probline_noLdn = exp(pred_noLdn) / denom,
            # CIs converted to estimate number
            uci = exp(upp) / denom,
            lci = exp(low) / denom,
            uci_noLdn = exp(upp_noLdn) / denom,
            lci_noLdn = exp(low_noLdn) / denom
          )
        
        sigdig <- 2
        model_out <- signif(ci.exp(po_model2)[2,], sigdig)
        
        outcome_plot <- bind_cols(outcome_pred, df_se) %>%
          mutate(var = outcome)
        
        table_formatted <- 
          format_counts_table(outcome, df_outcome, df_se, start_lockdown, 
                              lockdown_adjustment_period_wks)
        return(list(table_formatted = table_formatted, model_output = outcome_plot))
      }
      
      main_plot_data <- NULL
      full_table <- NULL
      for(ii in 1:length(outcomes)){
        main_plot_data <- main_plot_data %>%
          bind_rows(
            plot_outcome(outcomes[ii])$model_output
          )
        full_table <- bind_rows(full_table,
                          plot_outcome(outcome = outcomes[ii])$table_formatted)
        full_table[nrow(full_table) + 1,] <- ""
      }
      
      # write the counts table to the path provided
      write.csv(full_table, file = here::here(table_path), row.names = F)
      
      main_plot_data <- main_plot_data %>%
        mutate_at(.vars = c("pred", "upp", "low", "pred_noLdn", "upp_noLdn", "low_noLdn"), 
                ~ exp(.)) %>%
        left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
      
      main_plot_data$outcome_name <- 
        factor(main_plot_data$outcome_name, 
               levels = outcome_of_interest_namematch$outcome_name[plot_order])
      
      # create Poisson counterfactual vs lockdown plot
      poisson_count_plot(main_plot_data, start_lockdown, display_from, incl_no_ldn_ribbon)
}


