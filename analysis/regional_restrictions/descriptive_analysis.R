library(tidyverse)
library(ggplot2)

tiers <- read.csv("analysis_data/regional_restrictions/3_tier_restrictions.csv")
population_est <- read.csv("analysis_data/regional_restrictions/ONS_populaton_estimates_midyear2019.csv")

# join to get population estimates by ltla
tier_restrict = tiers %>% 
  right_join(population_est,by=c("ltla_code"="area_code"), keep=FALSE) %>%
  mutate(population = estimate_over_16_2019) %>%
  select(-region_code, -region_name, -area_name, -estimate_over_16_2019) %>%
  filter(tier != "lockdown") %>%
  mutate(tier = as.numeric(tier)) %>%
  mutate(date = as.Date(date, format="%d/%m/%Y")) %>%
  mutate(population = as.numeric(gsub(",","",population))) 

tier_scores <- tier_restrict %>%
  group_by(health_authority, date) %>%
  summarise(tier_score = weighted.mean(tier,population), .groups='keep')

# add lockdown dates to dataset to create a break
tier_scores_plot <- tier_scores
lockdown_dates <- seq.Date(as.Date("2020-11-5"), as.Date("2020-12-01"), by = "1 day")
for(region in unique(tier_scores_plot$health_authority)){
  lockdown_df <- data.frame (health_authority = region,
      date  = lockdown_dates,
      tier_score = rep(NA, length(lockdown_dates))
  )
  tier_scores <- rbind(tier_scores_plot, lockdown_df)
}

tier_scores <- tier_scores[complete.cases(tier_scores), ]

# plot tier scores over time to assess patterns
ggplot(tier_scores, aes(x=date, y=tier_score, group=health_authority, color=health_authority)) +
  geom_line()+
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.x = element_line(size=.1, color="grey") ,
        panel.grid.major.y = element_line(size=.1, color="grey"),
        axis.text.x = element_text(hjust = 1),
        axis.text.y = element_text())+
  annotate("rect", # second lockdown
           fill = "grey", 
           alpha = 0.2, 
           xmin = as.Date("2020-11-04"), 
           xmax = as.Date("2020-12-02"),
           ymin=-Inf, 
           ymax=Inf) +
  labs(x = "Date", y = "Tier Score", color = "Health Authority")

# table of number of days with tier score 2 or above
tier_above_2 <- tier_scores %>%
  mutate(two_plus = ifelse(tier_score >= 2, 1, 0)) %>%
  group_by(health_authority) %>%
  summarise(sum_two_plus = sum(two_plus))
tier_above_2

# percentage of population in at least tier 2
total_pop_region <- tier_restrict %>%
  group_by(health_authority) %>%
  summarise(total_pop = sum(unique(population)), .groups = 'keep')

total_pop_region <- total_pop_region %>% 
  left_join(tier_restrict, by="health_authority") %>%
  mutate(pc_pop = (population/total_pop) * 100) %>%
  mutate(two_plus = ifelse(tier >= 2, 1, 0))

total_pop_region <- total_pop_region %>%
  group_by(health_authority, date, two_plus) %>%
  summarise(pc_two_plus = sum(unique(pc_pop)), .groups="keep") %>%
  filter(two_plus == 1)

ggplot(total_pop_region, aes(x=date, y=pc_two_plus, group=health_authority, color=health_authority)) +
  geom_line()+
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.x = element_line(size=.1, color="grey") ,
        panel.grid.major.y = element_line(size=.1, color="grey"),
        axis.text.x = element_text(hjust = 1),
        axis.text.y = element_text())+
  annotate("rect", # second lockdown
           fill = "grey", 
           alpha = 0.2, 
           xmin = as.Date("2020-11-04"), 
           xmax = as.Date("2020-12-02"),
           ymin=-Inf, 
           ymax=Inf) +
  labs(x = "Date", y = "Tier Score", color = "Health Authority")
