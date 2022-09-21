#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Author: Olivia Bryant
# Date updated: 16/08/2022
# Notes: plot helper functions for descriptive analysis
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Overall outcome plot
#-------------------------------------------------------------------------------
# output is a plot for one outcome input with dashed lines showing when
# Christmas is. This plot was used to assess seasonality.
#-------------------------------------------------------------------------------

overall_outcome_plot <- function(data, title, startDate) {
  data <- data %>%
    filter(stratifier == 'overall') %>%
    filter(weekDate >= startDate) %>%
    mutate(proportion = (numOutcome/numEligible)*100)
  figure <- ggplot(data, aes(x = as.Date(weekDate), y = proportion)) +
  geom_line() +
  geom_point(size = 0.1) +
  xlab("Date") +
  ylab("Proportion Overall") +
  ggtitle(title) +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size=.1, color="grey"),
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.text.y = element_text()) +
    # limits the number of date labels to avoid it being cluttered.
  scale_x_date(date_labels = "%b %Y", breaks = breaks_pretty(10),
               labels = scales::label_date_short())+
    expand_limits(y = 0) +
    geom_vline(xintercept = as.Date("2020-12-25"), linetype="dotted") + 
    geom_vline(xintercept = as.Date("2019-12-25"), linetype="dotted") +
    geom_vline(xintercept = as.Date("2018-12-25"), linetype="dotted") +
    geom_vline(xintercept = as.Date("2017-12-25"), linetype="dotted") 
  return(figure)
}

#-------------------------------------------------------------------------------
# Outcome Plots Stratified by Category
#-------------------------------------------------------------------------------
# output is a plot for each outcome grouped by the categories of each strata
#-------------------------------------------------------------------------------

stratified_plot <- function(data) {
  ggplot(data, aes(x = as.Date(weekDate), 
                   y = proportion, color = category_cat)) +
    geom_line(aes(group = category_cat)) +
    geom_point(size = 0.1) +
    xlab("Date") +
    ylab("% of Population Consulting for Condition Per Week") +
    theme_classic() +
    facet_wrap(~outcome, scales = "free", ncol = 2)+
    theme(plot.background = element_rect(fill = "white", colour = "white"),
          panel.background = element_rect(fill = "white", colour = "white"),
          legend.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size = .1, color = "grey"),
          axis.text.x = element_text(angle = 70, hjust = 1),
          axis.text.y = element_text(),
          legend.title=element_blank()) +
    scale_x_date(date_labels = "%b %Y", breaks = breaks_pretty(10),
                 labels = scales::label_date_short())+
    expand_limits(y = 0)
}

#-------------------------------------------------------------------------------
# Formatting Data for Historical Comparison Plot
#-------------------------------------------------------------------------------
# outputs dataframe of historical average and the data for the year being 
# plotted. year_include is the year being compared with the historical 
# average, year_exclude is the year not being compared (e.g. if wanting to
# compare the historical average with 2021, 2020 needs to be excluded)
#-------------------------------------------------------------------------------

format_historical_plot_data <- function(outcome, year_include, year_exclude) {
  df_outcome <- get(outcome) %>%
    mutate(value = (numOutcome / numEligible) * 100) %>%
    # only want to plot overall observations
    filter(stratifier == "overall") %>%
    mutate(year = year(weekDate)) %>%
    mutate(week = week(weekDate))  %>%
    filter(year != year_exclude)
  
  plot_year_include <- df_outcome %>%
    filter(year == year_include) %>% 
    dplyr::select(week, "value_20" = value) 
  
  plot_historical <- df_outcome %>%
    filter(year != year_include) %>%
    group_by(week) %>%
    summarise(value = mean(value)) %>% 
    rename("value_hist" = value)
  
  historical_df <- df_outcome %>% 
    filter(year != year_include) %>%
    left_join(plot_historical, by = "week") %>%
    left_join(plot_year_include, by = "week") %>%
    # plot relative to 2000
    mutate(plotWeek = as.Date("2000-01-01") + (week * 7)) %>%
    mutate(value_20_low = ifelse(value_20 <= value_hist, value_20, value_hist),
           value_20_hi = ifelse(value_20 > value_hist, value_20, value_hist))
  
  return(historical_df)
}

