# Code to replicate Figure 1 chart
# Author: Frankie Cho
rm(list = ls())

library(ggbump)
library(ggridges)
library(patchwork)
library(tidyverse)
library(ggpubr)
library(ggsci)

# Set working directory
source("plot_scripts/helpers.R")

## 1. Plot time-series line plots for three focus CERs ------
source("plot_scripts/define_cer.R")

# Extract data for CERs
param_table <- read_csv("data/param_table.csv") %>%
  mutate(run_index = substr(run_index, 1, 5) %>% as.numeric())

carbon_price_assumption <- read_csv("data/price_assumptions/carbon_price_assumption.csv", col_names = F)
price_wheat_assumption <- read_csv("data/price_assumptions/price_wheat_assumption.csv", col_names = F)
price_dairy_assumption <- read_csv("data/price_assumptions/price_dairy_assumption.csv", col_names = F)
price_timber_assumption <- read_csv("data/price_assumptions/price_timber_assumption.csv", col_names = F)
colnames(carbon_price_assumption) <- colnames(price_wheat_assumption) <- colnames(price_dairy_assumption) <- colnames(price_timber_assumption) <- 2020:2049

# List economic variables
economic_var <- list(carbon_price = carbon_price_assumption,
                     price_wheat = price_wheat_assumption,
                     price_dairy = price_dairy_assumption,
                     price_timber = price_timber_assumption*22) %>%
  lapply(function(x) cbind(param_table,x)) %>%
  bind_rows(.id = "var")

# Climate variables
climate_data <- read_csv("data/climate_delta_ts.csv") %>%
  separate(var, c('Climate','cells','ukcp18','clim_scen_string','var','climate_model_member')) %>%
  select(-c('Climate','cells','ukcp18'))
climate_var <- param_table %>%
  right_join(climate_data, by = c("clim_scen_string", "climate_model_member"), relationship = "many-to-many") %>%
  select(colnames(economic_var))

# Join climate and economic variables
var_names =  c('temp', 'rain', 'carbon_price', 'price_wheat', 'price_dairy', 'price_timber')
var_labels = c("Temperature (°C)", "Rainfall (mm)", 'Carbon Price (£/tCO2e)', 
               'Wheat (£/t)', 'Dairy (£/ppl)', 'Timber (£/m3)')
clim_econ_var <- rbind(economic_var, climate_var) %>%
  pivot_longer(cols = as.character(2020:2049), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(var = factor(var, var_names, var_labels))

# Extract climate-economy variables for focus CERs
clim_econ_var_cer <- clim_econ_var %>%
  filter(run_index %in% CER) %>%
  mutate(cer = cer_names[match(run_index, CER)])

# Calculate min and max of the distribution for each variable
# min, max and 95% intervals
clim_econ_range <- clim_econ_var %>%
  group_by(var, year) %>%
  summarise(min = min(value), max = max(value), lb = quantile(value, 0.05), ub = quantile(value, 0.95))

clim_econ_var <- clim_econ_range %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = lb, ymax = ub), fill = "gray80") +
  geom_line(data = clim_econ_var_cer, aes(x = year, y = value, color = cer), size = 0.8) +
  scale_color_manual("CER", values = cer_colors) +
  scale_x_continuous("Year", expand = c(0,0)) +
  scale_y_continuous(expand = c(0.1,0.1)) +
  facet_wrap(~var, scales = "free_y", nrow = 2) +
  theme_pubr() +
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.position = 'right')
clim_econ_var

## 2. Plot bump charts ---------
returns_table <- read_csv(paste0('output/tables/oc_returns_table_mix.csv'))
in_scenario_returns_table <- read_csv(paste0('output/tables/in_scenario_returns_table.csv'))

returns_table_cer <- lapply(CER, function(cer) read_csv(paste0("output/tables/oc_returns_table_rs_", cer, ".csv")))
names(returns_table_cer) <- CER
costs_cer <- returns_table_cer %>%
  lapply(function(tb) tb %>% filter(as.numeric(substr(run_index, 1, 5)) %in% CER)) %>%
  lapply(function(tb) tb$EV) %>%
  bind_rows()
bump_data <- costs_cer %>%
  fcn_normalise_benefits() %>%
  as.data.frame()
hist_data <- in_scenario_returns_table[CER,] %>%
  fcn_normalise_benefits() %>%
  t() %>%
  as.data.frame()
colnames(hist_data) <- c(3,2,1)
hist_data_long <- hist_data %>%
  pivot_longer(c(3,2,1), names_to = 'position')
hist_data_short <- hist_data_long %>%
  group_by(position) %>%
  summarise(min = min(value), max = max(value))

colnames(bump_data) <- c('NH', 'ME', 'HE')
bump_data$scenario <-  c('NH', 'ME', 'HE')
bump_data$position <- c(3,2,1)

bump_plot <- bump_data %>%
  as.data.frame() %>%
  pivot_longer(c('NH', 'ME', 'HE')) %>%
  mutate(name = factor(name, labels = c('NH', 'ME', 'HE'), levels = c('NH', 'ME', 'HE'))) %>%
  group_by(scenario) %>%
  mutate(rank = rank(-value, ties.method = 'random')) %>%
  mutate(optimal = name == scenario) %>%
  mutate(position = ifelse(rank == 1, position - 0, ifelse(rank == 3, position + 0, position))) %>%
  mutate(name = factor(name, labels = c('P-NH', 'P-ME', 'P-HE'), levels = c('NH', 'ME', 'HE'))) %>%
  #mutate(position = ifelse(name == 'NH', position - 0.05, ifelse(name == 'HE', position + 0.05, position))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = 'gray80') +
  #geom_density_ridges(data = hist_data_long, aes(x = value, y = position), 
  #                    stat = 'binline', draw_baseline = FALSE, scale = 0.95, bins = 100,
  #                    color = NA, fill = 'gray70') +
  geom_rect(data = hist_data_short, aes(ymin = min, ymax = max, xmin = as.numeric(position) - 0.1,
                                        xmax = as.numeric(position) + 0.1), color = 'gray70', fill = 'gray90') +
  geom_text(aes(label = ifelse(optimal, scenario, ''), x = position + 0, y = -25)) +
  geom_bump(aes(y = value, x = position, color = fct_rev(name)), smooth = 7, size = 1)+
  geom_point(aes(y = value, x = position, color = fct_rev(name), 
                 size = ifelse(optimal, 1,0)))+
  # ggrepel::geom_label_repel(aes(y = value, x = position,
  #                              label = ifelse(rank %in% c(1,2,3),
  #                                             paste0(round(value,1)), NA),
  #              color = name, size = ifelse(optimal,6,1),
  #              fontface = ifelse(optimal, 'bold', 'plain')), direction = 'x', nudge_x = 0.3) +
  scale_color_manual("Planting \nstrategy", values = p_cer_colors) +
  ggpubr::theme_pubr() +
  scale_size_continuous(range = c(2,5), guide = 'none')+
  scale_y_continuous('NPV (billion £)', expand = c(0.1,0.1))+
  scale_x_continuous(limits = c(0.5, 3.5), expand = c(0,0)) +
  coord_flip() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
bump_plot
## 3. Plot planting maps -----
source("./plot_scripts/gridnet_init.R")

decision_table_cer <- lapply(CER, function(cer) read_csv(paste0("output/tables/oc_decision_table_rs_", cer, ".csv"))) %>%
  lapply(function(x) x %>% mutate(hectares_planted = hectares * EV))
p_cer_names <- c("P-NH", "P-ME", "P-HE")
names(decision_table_cer) <- p_cer_names
plot_list_decision <- decision_table_cer %>%
  fcn_plot_planting_mix(facet_dir = 'row')


## Combine plots
layout <- "
AAAA
AAAA
BBBB
BBBB
BBBB
CCCC
CCCC
CCCC
"
bump_plot_var_map <- clim_econ_var + plot_list_decision + bump_plot + plot_layout(design = layout) & plot_annotation(tag_levels = 'a') & theme(legend.position = 'right')
ggsave('output/figures/fig1_bump_chart.png', bump_plot_var_map, width = 2000, height = 3000, 
       units = 'px', scale = 1.2)

## Extract carbon sequestration numbers ------
in_scenario_ghg_table <- read_csv(paste0('output/tables/in_scenario_ghg_table.csv'))
ghg_cer <- returns_table_cer %>%
  lapply(function(tb) tb %>% filter(as.numeric(substr(run_index, 1, 5)) %in% CER)) %>%
  lapply(function(tb) tb$EV_ghg) %>%
  bind_rows()
ghg_cer <- as.data.frame(ghg_cer)
rownames(ghg_cer) <- c('NH', 'ME', 'HE')
colnames(ghg_cer) <- c('P-NH', 'P-ME', 'P-HE')
ghg_cer
