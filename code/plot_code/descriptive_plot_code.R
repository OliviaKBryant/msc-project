#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Author: Olivia Bryant
# Date updated: 16/08/2022
# Notes: plot helper functions for descriptive analysis
#-------------------------------------------------------------------------------

overallOutcomePlot <- function(data, title, label, startDate) {
  data <- data %>%
    filter(stratifier == 'overall') %>%
    filter(weekDate >= startDate) %>%
    mutate(proportion = (numOutcome/numEligible)*100)
  figure <- ggplot(data, aes(x =as.Date(weekDate), y = proportion)) +
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
  scale_x_date(date_labels = "%b %Y", breaks = breaks_pretty(10),labels = scales::label_date_short())+
    expand_limits(y=0) +
  annotate("rect", # first lockdown
           fill = "red", 
           alpha = 0.2,
           xmin = as.Date("2020-03-23"), 
           xmax = as.Date("2020-05-17"),
           ymin=-Inf, ymax=Inf) + 
  annotate("rect", # second lockdown
           fill = "red", 
           alpha = 0.2, 
           xmin = as.Date("2020-11-05"), 
           xmax = as.Date("2020-12-02"),
           ymin=-Inf, 
           ymax=Inf) +
  annotate("rect", # third lockdown
           fill = "red", 
           alpha = 0.2, 
           xmin = as.Date("2021-01-05"), 
           xmax = as.Date("2021-04-12"),
           ymin=-Inf, 
           ymax=Inf) +
  annotate("rect", # tier restrictions 1
           fill = "blue",
           alpha = 0.2,
           xmin = as.Date("2020-10-14"),
           xmax = as.Date("2020-11-05"),
           ymin=-Inf, 
           ymax=Inf) +
  annotate("rect", # tier restrictions 2
           fill = "blue",
           alpha = 0.2,
           xmin = as.Date("2020-12-02"),
           xmax = as.Date("2020-12-19"),
           ymin=-Inf, 
           ymax=Inf)
  return(figure)
}
