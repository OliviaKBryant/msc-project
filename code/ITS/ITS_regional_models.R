#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS Regional Models
# Authors: Alasdair Henderson, Olivia Bryant
# Date Updated: 23/08/2022
# Notes: ITS analysis of regional restrictions by each group separately
# and modelling the interaction between group and lockdown.
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

binomial_its_function_group <- function(outcomes_vec = outcomes,
												 cut_data = as.Date("2018-01-01"),
												 start_lockdown =   as.Date("2020-03-08"),
												 lockdown_adjustment_period_wks = 3,
												 end_post_lockdown_period = as.Date("2020-08-01"),
												 chop_selfharm = TRUE,
												 display_from = as.Date("2020-01-01"),
												 table_path,
												 group,
												 remove_xmas = FALSE,
												 incl_no_ldn_ribbon = TRUE){
			
		plot_outcome <- function(outcome){
			df_outcome <- format_outcome_data(outcome, start_lockdown, 
			                                lockdown_adjustment_period_wks, 
			                                end_post_lockdown_period,
			                                cut_data)
			
			if(remove_xmas){
			  df_outcome <- df_outcome %>%
			    filter(xmas == 0)
			}
			
			## model binomial 
			# Change in level + slope:
			### include interaction with time (centred at end of Lockdown adjustment period)
			ldn_centre <- df_outcome$time[min(which(df_outcome$lockdown == 1))]
			
			## fit model, calculate lagged residuals to fit in final model
			binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + I(time-ldn_centre) + I(time-ldn_centre):lockdown + as.factor(months) , family=binomial, data = filter(df_outcome, !is.na(lockdown)))
			ci.exp(binom_model1)
			binom_lagres <- lag(residuals(binom_model1)) %>% as.numeric()
			res1 <- residuals(binom_model1,type="deviance")
			
			## manipulate data so output looks cleaner
			model_data <- df_outcome %>% 
				mutate(timeC = time - ldn_centre) %>%
				mutate_at("months", ~as.factor(.)) 
			
			## fit model with lagged residuals 
			binom_model2 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + timeC + timeC:lockdown + as.factor(months)  + binom_lagres, family=binomial, data = filter(model_data, !is.na(lockdown)))
			ci.exp(binom_model2)
			summary.glm(binom_model2)
			
			## calculate dispersion adjustment parameter -- https://online.stat.psu.edu/stat504/node/162/
			#Pearson Goodness-of-fit statistic
			pearson_gof <- sum(residuals(binom_model2, type = "pearson")^2)
			df <- binom_model2$df.residual
			deviance_adjustment <- pearson_gof / df
			
			## some manual manipulation to merge the lagged residuals variable back with the original data
			missing_data_start <- min(which(is.na(model_data$lockdown)))
			missing_data_end <- max(which(is.na(model_data$lockdown)))
			missing_data_restart <- max(which(is.na(model_data$lockdown)))
			binom_lagres_timing <- bind_cols("time" = model_data$time[!is.na(model_data$lockdown)],
																			 "binom_lagres" = binom_lagres)
			
			## set up data frame to calculate linear predictions
			outcome_pred <- model_data %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) 
			
			## set up data frame to calculate linear predictions with month and xmas averaged at Sep
			outcome_pred_zeroed <- model_data %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) %>%
				mutate_at("xmas", ~(. = 0)) %>%
				mutate_at("year", ~(. = 0)) %>% 
				mutate_at("months", ~(. = 9)) 
			
			## predict values adjusted for overdispersion
			pred1 <- predict(binom_model2, newdata = outcome_pred, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
			predicted_vals <- pred1$fit
			stbp <- pred1$se.fit
			
			## predict values adjusted for overdispersion
			pred0 <- predict(binom_model2, newdata = outcome_pred_zeroed, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
			predicted_vals_0 <- pred0$fit
			stbp0 <- pred0$se.fit
			
			## set up data frame to calculate linear predictions with no Lockdown and predict values
			outcome_pred_nointervention <- outcome_pred %>%
				mutate_at("lockdown", ~(.=0))
			pred_noLockdown <- predict(binom_model2, newdata = outcome_pred_nointervention, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment) 
			pred_noLdn <- pred_noLockdown$fit
			stbp_noLdn <- pred_noLockdown$se.fit
				
			## combine all those predictions and convert from log odds to percentage reporting
			df_se <- bind_cols(stbp = stbp, stbp0 = stbp0, stbp_noLdn = stbp_noLdn, 
												 pred = predicted_vals, pred0 = predicted_vals_0, pred_noLdn = pred_noLdn) %>%
				mutate(
					#CIs
					upp = pred + (1.96 * stbp),
					low = pred - (1.96 * stbp),
					upp0 = pred0 + (1.96 * stbp0),
					low0 = pred0 - (1.96 * stbp0),
					upp_noLdn = pred_noLdn + (1.96 * stbp_noLdn),
					low0_noLdn = pred_noLdn - (1.96 * stbp_noLdn),
					# probline
					predicted_vals = exp(pred) / (1 + exp(pred)),
					probline_0 = exp(pred0) / (1 + exp(pred0)),
					probline_noLdn = exp(pred_noLdn) / (1 + exp(pred_noLdn)),
					#
					uci = exp(upp) / (1 + exp(upp)),
					lci = exp(low) / (1 + exp(low)),
					#
					uci0 = exp(upp0) / (1 + exp(upp0)),
					lci0 = exp(low0) / (1 + exp(low0)),
					#
					uci_noLdn = exp(upp_noLdn) / (1 + exp(upp_noLdn)),
					lci_noLdn = exp(low0_noLdn) / (1 + exp(low0_noLdn)) 
					)
			
			## combine data set and predictions
			outcome_plot <- bind_cols(outcome_pred, df_se) %>%
				mutate(var = outcome)
			
			## Get ORs for effect of lockdown
			parameter_estimates <- as.data.frame(ci.exp(binom_model2))
			vals_to_print <- parameter_estimates %>%
				mutate(var = rownames(parameter_estimates)) %>%
				filter(var == "lockdown") %>%
				mutate(var = outcome)
			
			## output
		return(list(df_1 = outcome_plot, vals_to_print = vals_to_print))
		}
		
		# the plot ----------------------------------------------------------------
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
		}
		
		## convert proportions into percentage 
		main_plot_data <- main_plot_data %>%
			mutate(pc_consult = (numOutcome / numEligible) * 100) %>%
			mutate_at(.vars = c("predicted_vals", "lci", "uci", "probline_noLdn", "uci_noLdn", "lci_noLdn", "probline_0", "lci0", "uci0"), 
								~. * 100) %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		
		## replace outcome name with the pretty name for printing on results
		main_plot_data$outcome_name <- factor(main_plot_data$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
		
		plot1 <- binomial_proportion_plot(main_plot_data, start_lockdown, display_from, incl_no_ldn_ribbon)
		
		pastename_year_cut_data <- lubridate::year(cut_data)
		
		if (remove_xmas) {
		  pastename_year_cut_data <- paste0(pastename_year_cut_data, "_", "noXmas")
		}

		# Forest plot of ORs ------------------------------------------------------
		forest_plot_df <- forest_plot_data %>%
			rename("Est" = "exp(Est.)", "lci" = "2.5%", "uci" = "97.5%") %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		
		# changes the names of outcomes to full names
		forest_plot_df$outcome_name <- factor(forest_plot_df$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
					# export table of results for the appendix 
					write.csv(forest_plot_df, file = here::here(table_path, paste0("its_main_ORs_",start_lockdown,"_",lockdown_adjustment_period_wks,  "_", pastename_year_cut_data,"_", group, ".csv")))
		
		forest_plot_df <- forest_plot_df %>%
			mutate(dummy_facet = "A")
		
		forest_plot_reduction <- forest_plot(forest_plot_df, forest_plot_df$dummy_facet, "C: Reduction") +
		  facet_wrap(~outcome_name, ncol = 1, dir = "h", strip.position = "right") +
		  theme(strip.text.y = element_text(hjust = 0.5, vjust = 0, angle = 0, size = 10))
		
		## uses patchwork package to combine plots
		layout = "
			AA
			AA
			BB
		"
		plot1 + forest_plot_reduction +
			plot_layout(design = layout) 
}

#-------------------------------------------------------------------------------
# Regional model to get interaction between tier group and lockdown
#-------------------------------------------------------------------------------
binomial_its_function_regional <- function(outcomes_vec = outcomes,
                                  cut_data = as.Date("2018-01-01"),
                                  start_lockdown =   as.Date("2020-03-08"),
                                  lockdown_adjustment_period_wks = 3,
                                  end_post_lockdown_period = as.Date("2020-08-01"),
                                  chop_selfharm = TRUE,
                                  display_from = as.Date("2020-01-01"),
                                  table_path,
                                  remove_xmas = FALSE){
  
  plot_outcome <- function(outcome){
    df_outcome <- format_outcome_data(outcome, start_lockdown, 
                                      lockdown_adjustment_period_wks, 
                                      end_post_lockdown_period,
                                      cut_data)
    
    if(remove_xmas){
      df_outcome <- df_outcome %>%
        filter(xmas == 0)
    }
    
    ## model binomial 
    # Change in level + slope:
    ### include interaction with time (centred at end of Lockdown adjustment period)
    ldn_centre <- df_outcome$time[min(which(df_outcome$lockdown == 1))]
    
    ## fit model, calculate lagged residuals to fit in final model
    binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + group + group:lockdown + as.factor(months) , family=binomial, data = filter(df_outcome, !is.na(lockdown)))
    ci.exp(binom_model1)
    binom_lagres <- lag(residuals(binom_model1)) %>% as.numeric()
    res1 <- residuals(binom_model1,type="deviance")
    
    ## manipulate data so output looks cleaner
    model_data <- df_outcome %>% 
      mutate(timeC = time - ldn_centre) %>%
      mutate_at("months", ~as.factor(.)) 
    
    ## fit model with lagged residuals 
    binom_model2 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + group + group:lockdown + as.factor(months) + binom_lagres, family=binomial, data = filter(model_data, !is.na(lockdown)))
    ci.exp(binom_model2)
    summary.glm(binom_model2)
    
    ## Get ORs for effect of lockdown
    parameter_estimates <- as.data.frame(ci.exp(binom_model2))
    p_val_estimates <- as.data.frame(coef(summary(binom_model2))[,4])
    vals_to_print <- parameter_estimates %>%
      mutate(var = rownames(parameter_estimates)) %>%
      mutate(var = outcome)
    
    vals_to_print <- merge(vals_to_print, p_val_estimates, by=0, all=TRUE, row.names=FALSE)
    colnames(vals_to_print) <- c("term", "exp(Est.)", "2.5%", "97.5%", "outcome", "p-value")
    
    vals_to_print <- vals_to_print %>%
      filter(term %in% c("lockdown", "grouplow tier", "lockdown:grouplow tier"))
    
    ## output
    return(vals_to_print)
  }
  
  estimates_data <- NULL
  for(ii in 1:length(outcomes_vec)){
    estimates_data <- estimates_data %>%
      bind_rows(
        plot_outcome(outcomes_vec[ii])
      )
  }

  estimates_data <- estimates_data %>%
    rename("Est" = "exp(Est.)", "lci" = "2.5%", "uci" = "97.5%") %>%
    left_join(outcome_of_interest_namematch, by = c("outcome" = "outcome"))
  
  # changes the names of outcomes to full names
  estimates_data $outcome_name <- factor( estimates_data $outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
  # estimates_data  <-estimates_data %>% select(-var)
  pastename_year_cut_data <- lubridate::year(cut_data)
  
  # export table of results for the appendix 
  write.csv(estimates_data, file = here::here(table_path, paste0("its_main_ORs_Regional_",lockdown_adjustment_period_wks, ".csv")))
  
}