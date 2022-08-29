library(tidyverse)

tiers <- read.csv("analysis_data/regional_restrictions/ltla_tiers.csv")
tiers$health_authority <- ifelse(tiers$health_authority == "", tiers$region, tiers$health_authority)
tiers$health_authority <- ifelse(tiers$health_authority == "South East", "South East Coast", tiers$health_authority)

tiers <- tiers %>%
  extract(tier, c("stim", "tier"), "(.*)_([^_]+)") %>%
  select(-stim)

write.csv(tiers, "analysis_data/regional_restrictions/ltla_tiers.csv", row.names=FALSE)