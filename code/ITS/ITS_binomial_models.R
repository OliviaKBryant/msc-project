#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS Binomial Models
# Authors: Alasdair Henderson, Olivia Bryant
# Date Updated: 23/08/2022
# Notes: ITS analysis of lockdown on multiple outcomes using binomial and 
# negative binomial models
# Ref:
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
library(lubridate)
library(stringr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(MASS)
library(scales)

#-------------------------------------------------------------------------------
# BINOMIAL ITS FUNCTION
#-------------------------------------------------------------------------------
# ITS function that uses binomial GLM to model proportion with primary care 
# contacts for outcomes
# Inputs: cut_data is the date of the start of the pre-interruption period.
# table_path is the relative path where the user wants to save the counts
# table
# incl_no_ldn_ribbon is a Boolean of whether to include the 95% CI ribbon for
# the counterfactual predictions
# Outputs: plot of counterfactual vs observed counts. CSV of estimated ORs for
# level change at table path provided. CSV of estimated recovery ORs.
#-------------------------------------------------------------------------------
binomial_its_function <- function(outcomes_vec = outcomes,
												 cut_data = as.Date("2018-01-01"),
												 start_lockdown =   as.Date("2020-03-08"),
												 lockdown_adjustment_period_wks = 3,
												 end_post_lockdown_period = as.Date("2020-08-01"),
												 chop_selfharm = TRUE,
												 display_from = as.Date("2020-01-01"),
												 table_path,
												 remove_xmas = FALSE,
												 incl_no_ldn_ribbon = TRUE){
			
		plot_outcome <- function(outcome) {
		  
		  
		  if (outcome == "selfharm" & chop_selfharm) {
		    cutData <- as.Date("2019-01-01")
		    }
		  
		  # format the outcome data
			df_outcome <- format_outcome_data(outcome, start_lockdown, 
			                                lockdown_adjustment_period_wks, 
			                                end_post_lockdown_period,
			                                cut_data)
			
			# filter out Christmas weeks if argument is true
			if (remove_xmas) {
			  df_outcome <- df_outcome %>%
			    filter(xmas == 0)
			}
			
			# Binomial GLM (change in level and slope)
			# include interaction with time (centred at end of lockdown adjustment period)
			ldn_centre <- df_outcome$time[min(which(df_outcome$lockdown == 1))]
			
			# Fit model to calculate lagged residuals
			binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown 
			                    + I(time-ldn_centre) + I(time-ldn_centre):lockdown 
			                    + as.factor(months) , family=binomial, 
			                    data = filter(df_outcome, !is.na(lockdown)))
			ci.exp(binom_model1)
			# store residuals
			binom_lagres <- lag(residuals(binom_model1)) %>% as.numeric()
			res1 <- residuals(binom_model1,type="deviance")
			
			# reformat data to make it cleaner
			model_data <- df_outcome %>% 
				mutate(timeC = time - ldn_centre) %>%
				mutate_at("months", ~as.factor(.)) 
			
			# fit full model with lagged residuals 
			binom_model2 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown 
			                    + timeC + timeC:lockdown + as.factor(months)  
			                    + binom_lagres, family = binomial, 
			                    data = filter(model_data, !is.na(lockdown)))
			ci.exp(binom_model2)
			
			# Pearson Goodness-of-fit statistic
			pearson_gof <- sum(residuals(binom_model2, type = "pearson")^2)
			df <- binom_model2$df.residual
			
			# calculate deviance adjustment parameter
			deviance_adjustment <- pearson_gof / df
			
			# merging the lagged residuals variable back with the original data
			missing_data_start <- min(which(is.na(model_data$lockdown)))
			missing_data_end <- max(which(is.na(model_data$lockdown)))
			missing_data_restart <- max(which(is.na(model_data$lockdown)))
			binom_lagres_timing <- bind_cols("time" = model_data$time[!is.na(model_data$lockdown)],
																			 "binom_lagres" = binom_lagres)
			
			# set up data frame to calculate linear predictions
			outcome_pred <- model_data %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) 
			
			# set up data frame to calculate linear predictions with month and Christmas
			# averaged at September if it had not been removed
			outcome_pred_zeroed <- model_data %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) %>%
				mutate_at("xmas", ~(. = 0)) %>%
				mutate_at("year", ~(. = 0)) %>% 
				mutate_at("months", ~(. = 9)) 
			
			# predict values adjusted for overdispersion
			pred1 <- predict(binom_model2, newdata = outcome_pred, se.fit = TRUE, 
			                 interval="confidence", dispersion = deviance_adjustment)
			predicted_vals <- pred1$fit
			
			# save standard error for confidence intervals
			stbp <- pred1$se.fit
			
			# predict values adjusted for overdispersion
			pred0 <- predict(binom_model2, newdata = outcome_pred_zeroed, 
			                 se.fit = TRUE, interval="confidence", 
			                 dispersion = deviance_adjustment)
			predicted_vals_0 <- pred0$fit
			stbp0 <- pred0$se.fit
			
			# set up data frame to calculate linear predictions with no 
			# lockdown and predict values
			outcome_pred_nointervention <- outcome_pred %>%
				mutate_at("lockdown", ~(. = 0))
			pred_noLockdown <- predict(binom_model2, 
			                           newdata = outcome_pred_nointervention, 
			                           se.fit = TRUE, interval="confidence", 
			                           dispersion = deviance_adjustment) 
			# save predictions for counterfactual
			pred_noLdn <- pred_noLockdown$fit
			
			# save standard error for counterfactual for confidence intervals
			stbp_noLdn <- pred_noLockdown$se.fit
				
			# combine all those predictions (with/without lockdwon) and convert from 
			# log odds to percentage reporting
			df_se <- bind_cols(stbp = stbp, stbp0 = stbp0, stbp_noLdn = stbp_noLdn, 
												 pred = predicted_vals, pred0 = predicted_vals_0, 
												 pred_noLdn = pred_noLdn) %>%
				mutate(
					#CIs
					upp = pred + (1.96 * stbp),
					low = pred - (1.96 * stbp),
					upp0 = pred0 + (1.96 * stbp0),
					low0 = pred0 - (1.96 * stbp0),
					upp_noLdn = pred_noLdn + (1.96 * stbp_noLdn),
					low0_noLdn = pred_noLdn - (1.96 * stbp_noLdn),
					
					predicted_vals = exp(pred) / (1 + exp(pred)),
					probline_0 = exp(pred0) / (1 + exp(pred0)),
					probline_noLdn = exp(pred_noLdn) / (1 + exp(pred_noLdn)),
					# transform CIs
					uci = exp(upp) / (1 + exp(upp)),
					lci = exp(low) / (1 + exp(low)),
					# transform CIs
					uci0 = exp(upp0) / (1 + exp(upp0)),
					lci0 = exp(low0) / (1 + exp(low0)),
					# transform CIs
					uci_noLdn = exp(upp_noLdn) / (1 + exp(upp_noLdn)),
					lci_noLdn = exp(low0_noLdn) / (1 + exp(low0_noLdn)) 
					)
			
			# combine data set and predictions
			outcome_plot <- bind_cols(outcome_pred, df_se) %>%
				mutate(var = outcome)
			
			# Get ORs for effect of lockdown
			parameter_estimates <- as.data.frame(ci.exp(binom_model2))
			vals_to_print <- parameter_estimates %>%
				mutate(var = rownames(parameter_estimates)) %>%
			  filter(var == "lockdown") %>%
				mutate(var = outcome)
			
			# Get ORs for effect of time on outcome after lockdown (time + interaction of time:lockdown)
			interaction_lincom <- glht(binom_model2, linfct = c("timeC + lockdown:timeC = 0"))
			summary(interaction_lincom)
			
			out <- confint(interaction_lincom)
			time_grad_postLdn <- out$confint[1,] %>% exp() %>% t() %>% as.data.frame() 
			interaction_to_print <- time_grad_postLdn %>%
				mutate(var = outcome)
			
		return(list(df_1 = outcome_plot, vals_to_print = vals_to_print, 
		            interaction_to_print = interaction_to_print))
		}
		
		main_plot_data <- NULL
		forest_plot_data <- NULL
		interaction_tbl_data <- NULL
		for(ii in 1:length(outcomes_vec)){
			main_plot_data <- main_plot_data %>%
				bind_rows(
					plot_outcome(outcomes_vec[ii])$df_1
				)
			forest_plot_data <- forest_plot_data %>%
				bind_rows(
					plot_outcome(outcomes_vec[ii])$vals_to_print
				)
			interaction_tbl_data <- interaction_tbl_data %>%
				bind_rows(
					plot_outcome(outcomes_vec[ii])$interaction_to_print
				)
		}
		
		# convert proportions into percentage 
		main_plot_data <- main_plot_data %>%
			mutate(pc_consult = (numOutcome / numEligible) * 100) %>%
			mutate_at(.vars = c("predicted_vals", "lci", "uci", "probline_noLdn", 
			                    "uci_noLdn", "lci_noLdn", "probline_0", "lci0", "uci0"), 
								~. * 100) %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		
		# replace outcome name with the pretty name for printing on results
		main_plot_data$outcome_name <- factor(main_plot_data$outcome_name, 
		                                      levels = outcome_of_interest_namematch$outcome_name[plot_order])
		
		plot1 <- binomial_proportion_plot(main_plot_data, start_lockdown, 
		                                  display_from, incl_no_ldn_ribbon)
		
		# Forest plot of interaction terms (recovery)
		interaction_tbl_data <- interaction_tbl_data %>%
			rename("Est" = "Estimate", lci = lwr, uci = upr) %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		
		interaction_tbl_data$outcome_name <- factor(interaction_tbl_data$outcome_name, 
		                                            levels = outcome_of_interest_namematch$outcome_name[plot_order])
		pastename_year_cut_data <- lubridate::year(cut_data)
		
		if(remove_xmas){
		  pastename_year_cut_data <- paste(pastename_year_cut_data, "_noXmas")
		}
		
		write.csv(interaction_tbl_data, file = here::here(table_path, 
		                                                  paste0("its_main_INTORs_",
		                                                         start_lockdown,"_",
		                                                         lockdown_adjustment_period_wks, 
		                                                         "_", pastename_year_cut_data, ".csv")))
		
		# forest plot of estimates
		forest_plot_recovery <- forest_plot(interaction_tbl_data, 
		                                    interaction_tbl_data$outcome_name, "B: Recovery") +
			scale_x_discrete(limits = rev(levels(as.factor(interaction_tbl_data$outcome_name))))
		
		# Forest plot of ORs (reduction)
		forest_plot_df <- forest_plot_data %>%
			rename("Est" = "exp(Est.)", "lci" = "2.5%", "uci" = "97.5%") %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		
		# changes the names of outcomes to full names
		forest_plot_df$outcome_name <- factor(forest_plot_df$outcome_name, 
		                                      levels = outcome_of_interest_namematch$outcome_name[plot_order])
					# export table of results for the appendix 
					write.csv(forest_plot_df, file = here::here(table_path, 
					                                            paste0("its_main_ORs_",
					                                                   start_lockdown,"_",
					                                                   lockdown_adjustment_period_wks, 
					                                                   "_", pastename_year_cut_data, ".csv")))
		
		forest_plot_df <- forest_plot_df %>%
			mutate(dummy_facet = "A")
		
		forest_plot_reduction <- forest_plot(forest_plot_df, 
		                                     forest_plot_df$dummy_facet, "C: Reduction") +
		  facet_wrap(~outcome_name, ncol = 1, dir = "h", strip.position = "right") +
		  theme(strip.text.y = element_text(hjust = 0.5, vjust = 0, angle = 0, 
		                                    size = 10))
		
		# uses patchwork package to combine plots
		layout = "
			AAAA
			AAAA
			BBCC
		"
		plot1 + forest_plot_reduction + forest_plot_recovery + 
			plot_layout(design = layout) 
}

#-------------------------------------------------------------------------------
# NEGATIVE BINOMIAL TO MODEL COUNTS
#-------------------------------------------------------------------------------
# ITS function that uses negative binomial regression to model counts of primary 
# care contacts.
# Inputs: cut_data is the date of the start of the pre-interruption period.
# table_path is the relative path where the user wants to save the counts
# table
# incl_no_ldn_ribbon is a Boolean of whether to include the 95% CI ribbon for
# the counterfactual predictions
# Outputs: plot of counterfactual vs observed counts. Table of estimated 
# numbers of primary care contacts with/without lockdown and cumultive 
# difference.
#-------------------------------------------------------------------------------
neg_binomial_counts_function <- function(outcomes,
                                        cut_data = as.Date("2018-01-01"),
                                        start_lockdown =   as.Date("2020-03-08"),
                                        lockdown_adjustment_period_wks = 3,
                                        end_post_lockdown_period = 
                                          as.Date("2020-08-01"),
                                        display_from = as.Date("2020-01-01"),
                                        remove_xmas = TRUE,
                                        table_path) {
  
  
  plot_outcome <- function(outcome) {
    
    # format outcome data for given outcome
    df_outcome <- format_outcome_data(outcome, start_lockdown, 
                                      lockdown_adjustment_period_wks, 
                                      end_post_lockdown_period,
                                      cut_data)
    
    if (remove_xmas) {
      df_outcome <- df_outcome %>%
        filter(xmas == 0)
    }
    
    
    # start of post-lockdown period
    ldn_centre <- df_outcome$time[min(which(df_outcome$lockdown == 1))]
    
    # model negative binomial to get lagged residuals
    neg_binom_model1 <- glm.nb(numOutcome ~ offset(log(numEligible)) + lockdown
                               + time + I(time-ldn_centre):lockdown 
                               + as.factor(months), 
                               data = filter(df_outcome, !is.na(lockdown)))
    # get lagged residuals
    lagres1 <- lag(residuals(neg_binom_model1))
    
    # full model with lagged residuals
    neg_binom_model2 <- glm.nb(numOutcome ~ offset(log(numEligible)) + lockdown 
                               + time + I(time-ldn_centre):lockdown 
                               + as.factor(months) + lagres1,  
                               data = filter(df_outcome, !is.na(lockdown)))
    
    # adjust predicted values according to dispersion correction parameter
    pearson_gof <- sum(residuals(neg_binom_model2, type = "pearson")^2)
    df <- neg_binom_model2$df.residual
    deviance_adjustment <- pearson_gof/df
    
    neg_binom_lagres_timing <- bind_cols("time" = df_outcome$time[!is.na(df_outcome$lockdown)],
                                  "lagres1" = lagres1)
    
    # data frame to predict values from 
    outcome_pred <- df_outcome %>%
      left_join(neg_binom_lagres_timing, by = "time") %>%
      mutate_at("lagres1", ~(. = 0))
    
    # predict values with lockdown
    pred1 <- predict(neg_binom_model2, newdata = outcome_pred, se.fit = TRUE, 
                     interval="confidence", dispersion = deviance_adjustment)
    # save predicted values
    predicted_vals <- pred1$fit
    # save standard error
    stbp <- pred1$se.fit
    
    # predict values if no lockdown 
    outcome_pred_nointervention <- outcome_pred %>%
      mutate_at("lockdown", ~(. = 0))
    predicted_vals_nointervention <- predict(neg_binom_model2, 
                                             newdata = outcome_pred_nointervention, 
                                             se.fit = TRUE, 
                                             dispersion = deviance_adjustment) 
    # save predicted values
    stbp_noLdn <- predicted_vals_nointervention$se.fit	
    # save standard errors
    predicted_vals_noLdn <- predicted_vals_nointervention$fit	
    
    # format CIs
    df_se <- bind_cols(stbp = stbp, 
                       pred = predicted_vals, 
                       stbp_noLdn = stbp_noLdn, 
                       pred_noLdn = predicted_vals_noLdn, 
                       denom = df_outcome$numEligible) %>%
      mutate(
        #CIs
        upp = pred + (1.96 * stbp),
        low = pred - (1.96 * stbp),
        upp_noLdn = pred_noLdn + (1.96 * stbp_noLdn),
        low_noLdn = pred_noLdn - (1.96 * stbp_noLdn),
        # probabilit yline
        predicted_vals = exp(pred) / denom,
        probline_noLdn = exp(pred_noLdn) / denom,
        # transform CIs
        uci = exp(upp) / denom,
        lci = exp(low) / denom,
        uci_noLdn = exp(upp_noLdn) / denom,
        lci_noLdn = exp(low_noLdn) / denom
      )
    
    sigdig <- 2
    # round outputs to 2dp
    model_out <- signif(ci.exp(neg_binom_model2)[2,], sigdig)
    
    # format dataframe for the outcome plot
    outcome_plot <- bind_cols(outcome_pred, df_se) %>%
      mutate(var = outcome)
    
    table_formatted <- format_counts_table(outcome, df_outcome, df_se, 
                                           start_lockdown, 
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
  
  # write estimates of counts to CSV
  write.csv(full_table, file = here::here(table_path), row.names = F)
  
  main_plot_data <- main_plot_data %>%
    mutate_at(.vars = c("pred", "upp", "low", "pred_noLdn", "upp_noLdn", "low_noLdn"), 
              ~ exp(.)) %>%
    left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
  
  # give outcomes nice titles
  main_plot_data$outcome_name <- factor(main_plot_data$outcome_name, 
                                        levels = outcome_of_interest_namematch$outcome_name[plot_order])
  
  # create plot like the Poisson plot
  poisson_count_plot(main_plot_data, start_lockdown, display_from)
}
