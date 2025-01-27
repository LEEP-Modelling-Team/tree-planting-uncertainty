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

fields <- c('price_wheat', 'price_osr', 'price_wbar', 'price_pot', 'price_sb', 'price_dairy',
            'price_beef', 'price_sheep', 'price_fert', 'price_timber')
field_names <- c('Wheat (£/t)', 'Oilseed Rape (£/t)', 'Barley (£/t)', 'Potatoes (£/t)', 'Sugarbeet (£/t)',
                 'Milk (£/ppl)', 'Beef (p/kg deadweight)', 'Sheep (p/kg deadweight)',
                 'Fertiliser (£/t)', 'Timber (£/m3)')

price_assumptions <- lapply(fields, function(f) {
  df <- read_csv(glue::glue("data/price_assumptions/{f}_assumption.csv"), col_names = F)
  colnames(df) <- 2020:2049
  if (f=='price_timber') {
    # Standardise timber prices
    df <- df * 22
  }
  return(df)
})
names(price_assumptions) <- fields

carbon_price_assumption <- read_csv("data/price_assumptions/carbon_price_assumption.csv", col_names = F)
colnames(carbon_price_assumption) <- 2020:(2020+ncol(carbon_price_assumption))

# List economic variables
economic_var <- price_assumptions %>%
  lapply(function(x) cbind(param_table,x)) %>%
  bind_rows(.id = "var")
carbon_price_var <- cbind(param_table, carbon_price_assumption)

# Climate variables
climate_data <- read_csv("data/climate_delta_ts.csv") %>%
  separate(var, c('Climate','cells','ukcp18','clim_scen_string','var','climate_model_member')) %>%
  select(-c('Climate','cells','ukcp18'))
climate_var <- param_table %>%
  right_join(climate_data, by = c("clim_scen_string", "climate_model_member"), relationship = "many-to-many") 
climate_var_long <- pivot_longer(climate_var, as.character(2020:2080), names_to = 'year', values_to = 'value') %>%
  mutate(year = as.numeric(year))
carbon_price_long <- pivot_longer(carbon_price_var, colnames(carbon_price_assumption), names_to = 'year', values_to = 'value') %>%
  mutate(year = as.numeric(year), var = "carbon_price")

# Join climate and economic variables
var_names =  c('temp', 'rain', 'carbon_price', fields)
var_labels = c("Temperature (°C)", "Rainfall (mm)", 'Carbon Price (£/tCO2e)', field_names)
climate_var_subset <- climate_var %>%
  select(-as.character(2050:2080))
econ_df <- economic_var %>%
  pivot_longer(cols = as.character(2020:2049), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(var = factor(var, var_names, var_labels))

## Alternative version -- climate variables to 2080
fcn_plot_var <- function(df) {
  clim_econ_var_cer <- df %>%
    filter(run_index %in% CER) %>%
    mutate(cer = cer_names[match(run_index, CER)])
  clim_econ_range <- df %>%
    group_by(var, year) %>%
    summarise(min = min(value), max = max(value), lb = quantile(value, 0.05), ub = quantile(value, 0.95))
  if (max(df$year) == 2080) {
    lab_gap = 20
  } else {
    lab_gap = 10
  }
  clim_econ_var <- clim_econ_range %>%
    ggplot() +
    geom_ribbon(aes(x = year, ymin = lb, ymax = ub), fill = "gray80") +
    geom_line(data = clim_econ_var_cer, aes(x = year, y = value, color = cer)) +
    scale_color_manual("CER", values = cer_colors) +
    scale_x_continuous("Year", expand = c(0,0), breaks = seq(2020, max(df$year)-1, by = lab_gap)) +
    scale_y_continuous(expand = c(0.1,0.1)) +
    coord_cartesian(xlim = c(2020, max(df$year))) +
    theme_pubr() +
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(fill = NA, colour = 'black', linewidth = .5),
          legend.position = 'right')
  clim_econ_var
}

temp_plot <- fcn_plot_var(filter(climate_var_long, var == 'temp')) +
  scale_y_continuous('') +
  ggtitle('Temperature (°C)')
rain_plot <- fcn_plot_var(filter(climate_var_long, var == 'rain')) +
  scale_y_continuous('') +
  ggtitle('Rainfall (mm)')
carbon_price_plot <- fcn_plot_var(carbon_price_long) +
  ggtitle('Carbon Price (£/tCO2e)') +
  scale_y_continuous('') +
  coord_cartesian(xlim = c(2020,2055), ylim = c(0, 600))

ag_plots <- lapply(field_names, \(x) {
  fcn_plot_var(filter(econ_df, var == x)) +
    scale_y_continuous("") +
    ggtitle(x)
})

all_plots <- list(temp_plot, rain_plot, carbon_price_plot)
all_plots <- append(all_plots, ag_plots)

cer_shaded_plots <- wrap_plots(all_plots) + plot_layout(guides = 'collect', ncol = 5) +
  plot_annotation(title = "Trends of key climate and economic variables across CERs")
ggsave('output/figures/sdfig3_cer_trends.png', cer_shaded_plots, scale = 1.5, width = 30, height = 20, units = 'cm')
ggsave('output/figures/sdfig3_cer_trends.pdf', cer_shaded_plots, scale = 1.5, width = 30, height = 20, units = 'cm')

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
  scale_y_continuous('NPV (billion £)', expand = c(0,0.1), limits = c(-32, 42))+
  scale_x_continuous('', limits = c(0.5, 3.5), expand = c(0,0)) +
  annotate('text', x = 2, y = -30, label = 'Climate-economy\nRealisation (CER)', angle = 90) +
  coord_flip() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_text(size = 10),
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
  fcn_plot_planting_mix(facet_dir = 'col')
plot_list_maps <- decision_table_cer %>%
  lapply(fcn_plot_planting) %>%
  rev()
plot_list_maps_labelled <- seq_along(plot_list_maps) %>%
  lapply(function(i){
    plt <- plot_list_maps[[i]] + 
      ggtitle(names(plot_list_maps)[i]) +
      theme(plot.title = element_text(color = p_cer_colors[i], face = 'bold', hjust = 0.5),
          plot.background = element_rect(colour = p_cer_colors[i], linewidth=1, fill=NA),
          legend.direction = 'vertical') + theme(legend.position = 'none')
    bar_plot <- fcn_bar_plot(decision_table_cer[[length(decision_table_cer)+1-i]]) + 
      theme(legend.position = 'none', plot.margin = unit(c(.1,0,0,0), "cm")) + 
      ylab("Area ") + theme(axis.title.y =  element_text())
    whole_plt <- plt / free(bar_plot) + plot_layout(heights = c(0.9, 0.1))
    return(wrap_elements(whole_plt))
  })
names(plot_list_maps_labelled) <- names(plot_list_maps)

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
#bump_plot_var_map <- clim_econ_var + plot_list_decision + bump_plot + plot_layout(design = layout) & plot_annotation(tag_levels = 'a') & theme(legend.position = 'right')
#ggsave('output/figures/fig1_bump_chart.png', bump_plot_var_map, width = 2000, height = 3000, 
#       units = 'px', scale = 1.2)

clim_econ_var_plots <- temp_plot +rain_plot + carbon_price_plot + 
  plot_layout(guides = 'collect', nrow = 3)

legends <- wrap_elements(cowplot::plot_grid(cowplot::get_plot_component(plot_list_maps[[1]]+theme(legend.direction = 'horizontal'), 'guide-box', return_all=TRUE)[[1]],
                                            cowplot::get_plot_component(bump_plot, 'guide-box', return_all=TRUE)[[4]]))

bump_plot_horizontal <- (plot_list_maps_labelled$`P-NH` | plot_list_maps_labelled$`P-ME` | plot_list_maps_labelled$`P-HE`) / 
  bump_plot / legends +
  plot_layout(guides = 'collect', heights = c(.55, .4, .05)) & plot_annotation(tag_levels = list(c('a','b','c','d'))) &
  theme(legend.position = 'none', legend.direction = 'horizontal', 
        plot.tag = element_text(face = "bold"), 
        legend.justification = "left")
ggsave('output/figures/fig1_bump_chart_horizontal.png', bump_plot_horizontal, width = 2000, height = 2200, 
       units = 'px', scale = 1)
ggsave('output/figures/fig1_bump_chart_horizontal.pdf', bump_plot_horizontal, width = 2000, height = 2200, 
       units = 'px', scale = 1)

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
