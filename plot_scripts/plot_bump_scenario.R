library(ggbump)
library(ggridges)
library(patchwork)

source('../scripts/helpers.R')
source('../scripts/fcn_get_rs.R')
returns_table <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_returns_table_mix_no_foregone_ag_ghg_value.csv'))
in_scenario_returns_table <- read_csv(paste0('../../output/',date_string,'_decision_table/in_scenario_returns_table.csv'))

rs <- fcn_get_rs(within_sample = T, return_index = T)
rs_run_index_num <- rs$run_index %>%
  substr(0,5) %>%
  as.numeric()
rs_returns_list <- lapply(rs_run_index_num, function(x) read_csv(paste0('../../output/',date_string,'_decision_table/oc_returns_table_rs_',x,'.csv'))) %>%
  lapply(function(x) x$EV)
names(rs_returns_list) <- c('P-NH', 'P-ME', 'P-HE')
rs_returns_df <- rs_returns_list %>%
  bind_rows(.id = 'rs')

CER <- c(457, 2544, 3083)
cer_names <- c('P-NH', 'P-ME', 'P-HE')
returns_table_cer <- lapply(CER, function(cer) read_csv(paste0("../../output/20220901_decision_table/oc_returns_table_rs_", cer, ".csv")))
names(returns_table_cer) <- CER
costs_cer <- returns_table_cer %>%
  lapply(function(tb) tb %>% filter(as.numeric(substr(run_index, 1, 5)) %in% CER)) %>%
  lapply(function(tb) tb$EV) %>%
  bind_rows()
bump_data <- costs_cer %>%
  fcn_normalise_benefits() %>%
  as.data.frame()
hist_data <- in_scenario_returns_table[rs_run_index_num,] %>%
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

# Normalise it to P-NH if necessary
bump_data_norm <- bump_data
bump_data_norm[,c("NH", "ME","HE")] = bump_data[,c("NH", "ME","HE")]# - bump_data[,"NH"]

hist_data_short_norm <- hist_data_short
hist_data_short_norm$p_nh <- rev( bump_data[,"NH"])
hist_data_short_norm[,c('min', 'max')] <- hist_data_short[,c('min', 'max')] # - hist_data_short_norm$p_nh
  
bump_plot <- bump_data_norm %>%
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
  geom_rect(data = hist_data_short_norm, aes(ymin = min, ymax = max, xmin = as.numeric(position) - 0.1,
                                       xmax = as.numeric(position) + 0.1), color = 'gray70', fill = 'gray90') +
  geom_text(aes(label = ifelse(optimal, scenario, ''), x = position + 0, y = value), position = position_nudge(y = 3)) +
  geom_bump(aes(y = value, x = position, color = fct_rev(name)), smooth = 7, linewidth = 1)+
  geom_point(aes(y = value, x = position, color = fct_rev(name), 
                 size = ifelse(optimal, 1,0)))+
  # ggrepel::geom_label_repel(aes(y = value, x = position,
  #                              label = ifelse(rank %in% c(1,2,3),
  #                                             paste0(round(value,1)), NA),
  #              color = name, size = ifelse(optimal,6,1),
  #              fontface = ifelse(optimal, 'bold', 'plain')), direction = 'x', nudge_x = 0.3) +
  ggsci::scale_color_nejm() +
  ggpubr::theme_pubr() +
  scale_size_continuous(range = c(2,5),guide = 'none')+
  scale_y_continuous('NPV (billion £)', expand = c(0.1,0.1))+
  scale_x_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(3.5,0.5)) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
dev.off()
bump_plot

source('../scripts/plot_climate_delta.R')

# Plot change in carbon sequestration across all three CERs
library(dplyr)
library(readr)
ghg_table <- read_csv(paste0('../../output/',date_string,'_decision_table/in_scenario_ghg_table.csv'))

ghg_table_cer <- ghg_table[CER,] / 1.2e7
ghg_cer <- ghg_table[CER,CER] / 1.2e7
bump_data <-  ghg_cer %>%
  as.data.frame()
hist_data <- ghg_table_cer %>%
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

# Normalise it to P-NH
bump_data_norm <- bump_data
bump_data_norm[,c("NH", "ME","HE")] = bump_data[,c("NH", "ME","HE")]# - bump_data[,"NH"]

hist_data_short_norm <- hist_data_short
hist_data_short_norm$p_nh <- rev( bump_data[,"NH"])
hist_data_short_norm[,c('min', 'max')] <- hist_data_short[,c('min', 'max')] # - hist_data_short_norm$p_nh

bump_data_norm$text_pos <- rev(hist_data_short_norm$max)
bump_plot_ghg <- bump_data_norm %>%
  as.data.frame() %>%
  pivot_longer(c('NH', 'ME', 'HE')) %>%
  mutate(name = factor(name, labels = c('NH', 'ME', 'HE'), levels = c('NH','ME','HE'))) %>%
  group_by(scenario) %>%
  mutate(rank = rank(-value, ties.method = 'random')) %>%
  mutate(optimal = name == scenario) %>%
  mutate(position = ifelse(rank == 1, position - 0, ifelse(rank == 3, position + 0, position))) %>%
  #mutate(position = ifelse(name == 'NH', position - 0.05, ifelse(name == 'HE', position + 0.05, position))) %>%
  mutate(name = factor(name, labels = c('P-NH', 'P-ME', 'P-HE'), levels = c('NH','ME','HE'))) %>%
  ggplot() +
  geom_hline(yintercept = 1, color = 'gray80') +
  #geom_density_ridges(data = hist_data_long, aes(x = value, y = position), 
  #                    stat = 'binline', draw_baseline = FALSE, scale = 0.95, bins = 100,
  #                    color = NA, fill = 'gray70') +
  geom_rect(data = hist_data_short_norm, aes(ymin = min, ymax = max, xmin = as.numeric(position) - 0.1,
                                             xmax = as.numeric(position) + 0.1), color = 'gray70', fill = 'gray90') +
  geom_text(aes(label = ifelse(optimal, scenario, ''), x = position + 0, y = text_pos), position = position_nudge(y = 0.01)) +
  geom_bump(aes(y = value, x = position, color = fct_rev(name)), smooth = 7, linewidth = 1)+
  geom_point(aes(y = value, x = position, color = fct_rev(name), 
                 size = ifelse(optimal, 1,0)))+
  # ggrepel::geom_label_repel(aes(y = value, x = position,
  #                              label = ifelse(rank %in% c(1,2,3),
  #                                             paste0(round(value,1)), NA),
  #              color = name, size = ifelse(optimal,6,1),
  #              fontface = ifelse(optimal, 'bold', 'plain')), direction = 'x', nudge_x = 0.3) +
  ggsci::scale_color_nejm() +
  ggpubr::theme_pubr() +
  scale_size_continuous(range = c(2,5),guide = 'none')+
  scale_y_continuous('Carbon removal\ntarget reached (%)', expand = c(0.02,0.02), labels = scales::percent)+
  scale_x_continuous(expand = c(0,0)) +
  coord_cartesian( c(3.5, 0.5)) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
dev.off()
bump_plot_ghg

source("../scripts/plot_rep_scen_grid.R")
decision_table_cer <- lapply(CER, function(cer) read_csv(paste0("../../output/20220901_decision_table/oc_decision_table_rs_", cer, ".csv"))) %>%
  lapply(function(x) x %>% mutate(hectares_planted = hectares * EV))
names(returns_table_cer) <- names(decision_table_cer) <- CER
plot_list_decision <- decision_table_cer %>%
  fcn_plot_planting_mix(facet_dir = 'row')
hectares_cer <- decision_table_cer %>%
  lapply(function(tb) tb$EV %*% tb$hectares) %>%
  bind_rows() %>%
  t()
ghg_cer <- decision_table_cer %>%
  lapply(function(tb) tb$EV %*% tb$ghg_ann) %>%
  bind_rows() %>%
  t()
hectares_cer
ghg_cer / hectares_cer

ss_prop <- decision_table_cer %>%
  lapply(function(tb) (tb$EV * tb$hectares) %*% (tb$species == "SS") / tb$EV %*% tb$hectares) %>%
  bind_rows()
ss_prop

layout <- "
AAAACC
BBBBCC
BBBBCC
"
bump_plot_map <- var_change_pct_plot + bump_plot + plot_list_decision + plot_layout(design = layout, guides = 'collect') & plot_annotation(tag_levels = 'a') & theme(legend.position = 'bottom')

ggsave('../output/20220901/bump_plot_map.png', bump_plot_map, width = 2000, height = 2200, 
       units = 'px', scale = 1.2)
 
