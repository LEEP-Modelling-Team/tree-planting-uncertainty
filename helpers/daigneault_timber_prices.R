# Clean Daigneault et al. (2022) data to extract only roundwood prices

library(tidyverse)
library(readxl)

# Rename "baseline" scenarios to RCP
global_forest_scenarios <- read_excel('data/1-s2.0-S0959378022001200-mmc2.xlsx', sheet = 'Estimates') %>%
  select(-`...17`) %>%
  filter(Estimate == "Wt Avg Roundwood Price ($/m3)") %>%
  mutate(RCP = ifelse(RCP == 'Baseline' & SSP %in% c("SSP2","SSP3","SSP4","SSP5"), "RCP 8.5", RCP),
         RCP = ifelse(RCP == 'Baseline' & SSP %in% c("SSP1"), "RCP 6.0", RCP)) %>%
  pivot_longer(as.character(seq(2015, 2105, 10)), names_to = 'year', values_to = 'value') %>%
  mutate(year = as.numeric(year)) %>%
  mutate(RCP = factor(RCP, levels = c("RCP 1.9", "RCP 2.6", "RCP 3.4", "RCP 4.5", "RCP 6.0", "RCP 8.5"), 
                      labels = c("rcp19", "rcp26", "rcp34", "rcp45", "rcp60", "rcp85")))%>%
  mutate(SSP = as.numeric(substring(SSP, 4,5)))

# Calculate percentage change from 2015
df <- global_forest_scenarios %>%
  mutate(delta = ifelse(Model == 'GTM', value/79, ifelse(Model == 'GFPM', value/102, value / 55))) %>%
  group_by(Model, RCP, SSP) %>%
  # Generate interpolated data within each group
  do({
    data <- .
    years <- seq(min(data$year), max(data$year), by = 1)
    values <- approx(data$year, data$delta, xout = years)$y
    data.frame(year = years, value = values)
  }) %>%
  ungroup()

readr::write_csv(df, file = "data/daigneault_timber_price_delta.csv")

unique_scen <- df %>%
  mutate(scen_string = paste(RCP, SSP, Model, sep = "_"))
unique(unique_scen$scen_string) %>% length()
