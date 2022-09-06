#-------------------------------------------------------------------------------
# Project: MSc LSHTM Summer Project: COVID Collateral
# Program Name: ITS Plot Code
# Authors: Alasdair Henderson, Olivia Bryant
# Date Updated: 23/08/2022
# Notes: Plot code functions for ITS
# Ref:
#-------------------------------------------------------------------------------

binomial_proportion_plot <- function(data, start_lockdown, display_from,
                                     incl_no_ldn_ribbon = TRUE){
  
  abline_max <- data$weekPlot[max(which(is.na(data$lockdown))) + 1]
  abline_min <- data$weekPlot[min(which(is.na(data$lockdown))) - 1]
  
  if (is.na(abline_min) & is.na(abline_max)) {
    abline_min <- start_lockdown
    abline_max <- start_lockdown
  }
  
  plot <- ggplot(filter(data, weekPlot >= display_from), aes(x = weekPlot, y = pc_consult, group = outcome_name)) +
    # the data
    geom_line(col = "gray60") +
    ### the probability if there was no lockdown
    geom_line(data = filter(data, weekPlot >= abline_min), aes(y = probline_noLdn), col = 2, lty = 2) +
    ### probability with model (inc. std. error)
    geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
    geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4, 0.4), lty = 0) +
    ### format the plot
    facet_wrap(~outcome_name, scales = "free", ncol = 3) +
    geom_vline(xintercept = c(abline_min, 
                              abline_max), col = 1, lwd = 1) + 
    labs(y = "% of people consulting for condition", caption = "OCD: Obsessive Compulsive Disorder") +
    theme_classic() +
    theme(axis.title = element_text(size = 26), 
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1, size = 16),
          legend.position = "top",
          plot.background = element_rect(fill = bkg_colour, colour =  NA),
          panel.background = element_rect(fill = bkg_colour, colour =  NA),
          legend.background = element_rect(fill = bkg_colour, colour = NA),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 16, hjust = 0),
          strip.background = element_rect(fill = bkg_colour, colour =  NA),
          panel.grid.major = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
          panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)))  + 
      scale_x_date(breaks = "1 month", date_labels = "%b")
  
    if(incl_no_ldn_ribbon){
      plot <- plot + geom_ribbon(data = filter(data, weekPlot >= abline_min), aes(ymin = lci_noLdn, ymax=uci_noLdn), fill = alpha(2, 0.4), lty = 0) 
    }
    return(plot)
}

poisson_count_plot <- function(data, start_lockdown, display_from,
                               incl_no_ldn_ribbon = TRUE){
  
  abline_max <- data$weekPlot[max(which(is.na(data$lockdown))) + 1]
  abline_min <- data$weekPlot[min(which(is.na(data$lockdown))) - 1]
  
  if (is.na(abline_min) & is.na(abline_max)) {
    abline_min <- start_lockdown
    abline_max <- start_lockdown
  }
  
  plot <- ggplot(filter(data, weekPlot >= display_from), aes(x = weekPlot, y = numOutcome, group = outcome_name)) +
    # the data
    geom_line(col = "gray60") +
    ### the probability if there was no lockdown
    geom_line(data = filter(data, weekPlot >= abline_min), aes(y = pred_noLdn), col = 2, lty = 2) +
    ### probability with model (inc. std. error)
    geom_line(aes(y = pred), col = 4, lty = 2) +
    geom_ribbon(aes(ymin = low, ymax = upp), fill = alpha(4,0.4), lty = 0) +
    ### format the plot
    facet_wrap(~outcome_name, scales = "free", ncol = 3) +
    geom_vline(xintercept = c(abline_min, 
                              abline_max), col = 1, lwd = 1) + 
    labs(y = "Number of consultations per week per 1 million patients", title = "A", caption = "OCD: Obsessive Compulsive Disorder") +
    theme_classic() +
    theme(axis.title = element_text(size = 16), 
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
          legend.position = "top",
          plot.background = element_rect(fill = bkg_colour, colour =  NA),
          panel.background = element_rect(fill = bkg_colour, colour =  NA),
          legend.background = element_rect(fill = bkg_colour, colour = NA),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          strip.text = element_text(size = 12, hjust = 0),
          strip.background = element_rect(fill = bkg_colour, colour =  NA),
          panel.grid.major = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
          panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)))  + 
    scale_x_date(breaks = "1 month", date_labels = "%b")
  
  if (incl_no_ldn_ribbon){
    plot <- plot +  geom_ribbon(data = filter(data, weekPlot >= abline_min), aes(ymin = low_noLdn, ymax = upp_noLdn), fill = alpha(2,0.4), lty = 0)
  }
  return(plot)
}

forest_plot <- function(data, xVar, title){
  ggplot(data = data, aes(x = xVar, y = Est, ymin = lci, ymax = uci)) +
    geom_point(size = 2.5, pch = 16, colour = "darkred") +
    geom_linerange(lwd = 1.5, colour = "darkred") +
    geom_hline(yintercept = 1, lty = 2) + 
    coord_flip() +
    labs(x = "", y = "95% CI", title = title) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.line.y.left = element_blank(),
          axis.line.y.right = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 0),
          legend.position = "top",
          plot.background = element_rect(fill = bkg_colour, colour =  NA),
          panel.background = element_rect(fill = bkg_colour, colour =  NA),
          legend.background = element_rect(fill = bkg_colour, colour = NA),
          strip.background = element_rect(fill = bkg_colour, colour =  NA),
          panel.grid.major = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
          panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)))
} 
  
  
  