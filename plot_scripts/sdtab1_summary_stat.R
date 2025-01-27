## Create supplementary summary statistics table of planting strategies

library(purrr)
library(readr)
library(dplyr)
library(tidyr)

source('plot_scripts/define_cer.R')
source('plot_scripts/helpers.R')

returns_table_cer <- lapply(CER, function(cer) read_csv(paste0("output/tables/oc_returns_table_rs_", cer, ".csv"),show_col_types = FALSE))

returns_table <- read_csv(paste0('output/tables/oc_returns_table_mix.csv'), show_col_types = FALSE)

decision_table_cer <- lapply(CER, function(cer) read_csv(paste0("output/tables/oc_decision_table_rs_", cer, ".csv"),show_col_types = FALSE))

decision_table <- read_csv(paste0('output/tables/oc_decision_table_mix.csv'), show_col_types = FALSE)

in_scenario_ghg_table <- read_csv(paste0('output/tables/in_scenario_ghg_table.csv'))

returns_cer <- map(returns_table_cer, \(x) x$EV) %>% bind_cols()
names(returns_cer) <- p_cer_names
returns_ev_ra <- returns_table[,c('EV', 'CVaR')]
names(returns_ev_ra) <- c('P-EV', 'P-RA')
returns_all <- cbind(returns_cer, returns_ev_ra) %>%
  fcn_normalise_benefits()

returns_summary <- returns_all %>%
  summarise_all(list(Mean = mean, 
                     NH = ~ .x[CER[1]],
                     ME = ~ .x[CER[2]],
                     HE = ~ .x[CER[3]],
                     Minimum = min, Maximum = max,
                     `CVaR-0.9` = ~ mean(.x[.x < quantile(.x, .1)]), `Pr(-10B)` = ~ mean(if_else(.x < -10, 1, 0)))) %>%
  pivot_longer(everything(), names_sep = "_", names_to = c('strategy', 'stat')) %>%
  pivot_wider(names_from = 'strategy', values_from = 'value')

decisions_cer <- bind_cols(map(decision_table_cer, ~ .x$EV))
names(decisions_cer) <- p_cer_names

decision_table_all <- decision_table %>%
  cbind(decisions_cer)

decision_summary <- decision_table_all %>%
  rename(`P-EV` = EV, `P-RA` = CVaR) %>%
  pivot_longer(all_of(c(p_cer_names, 'P-EV', 'P-RA')), values_to = 'decision', names_to = 'strategy') %>%
  mutate(hectares_planted = hectares * decision) %>%
  group_by(strategy, species) %>%
  mutate(species = factor(species, c('SS', 'POK'), c('Conifers', 'Broadleaf'))) %>%
  summarise(hectares = sum(hectares_planted) / 1e6, ghg_ann = sum(ghg_ann*decision) / 1e6) 

hectares_summary <- decision_summary %>%
  pivot_wider(id_cols = c('species'), names_from = 'strategy', values_from = 'hectares')

hectares_summary_all <- hectares_summary %>%
  select(-species) %>%
  summarise_all(sum) %>%
  mutate(species = 'All')
  
hectares_summary <- hectares_summary %>%
  bind_rows(hectares_summary_all)

ghg_summary <- decision_summary %>%
  pivot_wider(id_cols = c('species'), names_from = 'strategy', values_from = 'ghg_ann')

ghg_summary_all <- ghg_summary %>%
  select(-species) %>%
  summarise_all(sum) %>%
  mutate(species = 'All')

ghg_summary <- ghg_summary %>%
  bind_rows(ghg_summary_all)

# Add CER-specific GHG outcomes
in_scenario_ghg <- in_scenario_ghg_table[CER, CER]
ev_ra_ghg <- returns_table[CER ,c("EV_ghg", "CVaR_ghg")]

ghg_summary_scen <- cbind(in_scenario_ghg, ev_ra_ghg) / 1e6
ghg_summary_scen <- cbind(cer_names, ghg_summary_scen)
colnames(ghg_summary_scen) <- names(ghg_summary)

ghg_summary <- rbind(ghg_summary, ghg_summary_scen)

cols <- c('P-NH', 'P-ME', 'P-HE', 'P-EV', 'P-RA')

summary_all <- bind_rows('NPV (£)' = returns_summary, 'Planted Area (million ha)' = hectares_summary, 'MtCO2e (annualised)' = ghg_summary, .id = 'category') %>%
  select(category, stat, species, cols) %>%
  mutate_at(vars(cols), round, digits = 4)

write_csv(summary_all, 'output/figures/sdtable3_summary_table.csv')
