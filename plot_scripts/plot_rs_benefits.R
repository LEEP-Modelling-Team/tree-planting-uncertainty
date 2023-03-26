source('../scripts/plot_rep_scen_grid.R')

returns_table <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_returns_table_mix_no_foregone_ag_ghg_value.csv'))
decision_table <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_decision_table_mix_no_foregone_ag_ghg_value.csv'))
returns_table_rcp85 <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_returns_table_mix_no_foregone_ag_ghg_value_rcp85.csv'))
decision_table_rcp85 <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_decision_table_mix_no_foregone_ag_ghg_value_rcp85.csv'))
rs_decision_table <- read_csv(paste0('../../output/',date_string,'_decision_table/rs_decision_table_mix_no_foregone_ag_ghg_value.csv'))
rs_returns_table <- read_csv(paste0('../../output/',date_string,'_decision_table/rs_returns_table_mix_no_foregone_ag_ghg_value.csv'))
in_scenario_returns_table <- read_csv(paste0('../../output/', date_string, '_decision_table/in_scenario_returns_table.csv'))
colnames(in_scenario_returns_table) <- returns_table$run_index

rs_hd <- 'rs_rcp85_custom_06'

source("../scripts/fcn_get_rs.R")
df_list <- fcn_get_rs(within_sample = T)
df_decision_list <- lapply(fcn_get_rs(within_sample = T), fcn_optimal_scenario_planting)
df_decision_list[[3]]$EV_rcp85 <- ifelse(df_decision_list[[3]]$hectares_planted > 0, 1, 0)
df_decision_list_comb <- decision_table %>%
  left_join(df_decision_list[[3]], by = c('new2kid', 'species')) %>%
  mutate(EV_rcp85 = ifelse(is.na(EV_rcp85), 0, EV_rcp85))
decision_table$EV_rcp85 <- df_decision_list_comb$EV_rcp85
fcn_rs_benefits <- function(rs) {
  df <- rs %>%
    pivot_longer(c('cost_carbon_forestry_SS', 'cost_carbon_forestry_POK'), names_to = 'species', 
                 values_to = 'cost_carbon_forestry', names_prefix = "cost_carbon_forestry_") %>%
    right_join(decision_table, by = c("new2kid", "species")) 
  ev_benefits <- fcn_normalise_benefits(df$cost_carbon_forestry %*% df$EV)%>% as.vector()
  cvar_benefits <- fcn_normalise_benefits(df$cost_carbon_forestry %*% df$CVaR) %>% as.vector()
  ev_rcp85_benefits <- fcn_normalise_benefits(df$cost_carbon_forestry %*% df$EV_rcp85) %>% as.vector()
  return(c(ev_benefits, cvar_benefits, ev_rcp85_benefits))
}
rs_benefits <- lapply(df_list, fcn_rs_benefits) %>%
  bind_rows(.id = 'column_label') %>%
  t()
colnames(rs_benefits) <- c('EV', 'CVaR', 'P_HD')
rownames(rs_benefits) <- c('NH', 'ME', 'HE')
rs_benefits_df <- rs_benefits %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("rs") %>%
  pivot_longer(c('EV', 'CVaR', 'P_HD'), names_to = 'model', values_to = 'xintercept') %>%
  mutate(model = factor(model, levels = c('P_HD', 'EV', 'CVaR'), labels = c('P-HD', 'EV', 'RA')))%>%
  filter(model != 'EV')

returns_table$EV_rcp85 <- in_scenario_returns_table[[fcn_get_rs(within_sample = T, return_index = T)$run_index[3]]]
plot_df <- returns_table %>%
  select(EV_rcp85, EV, CVaR, clim_scen_string, run_index) %>%
  pivot_longer(c(EV_rcp85, EV, CVaR), names_to = 'model', values_to = 'benefits') %>%
  mutate(benefits = fcn_normalise_benefits(benefits)) %>%
  mutate(model = factor(model, levels = c('EV_rcp85', 'EV', 'CVaR'), labels = c('P-HD', 'EV', 'RA')))%>%
  filter(model != 'EV') %>%
  group_by(model) %>%
  mutate(at_risk = ifelse(benefits < quantile(benefits, 0.1), 'At risk', 'Not at risk')) %>%
  mutate(five_billion = ifelse(benefits < -5, 'Cost > £5B', 'Cost ≤ £5B'))

rs_benefit_plot <- ggplot() +
  geom_histogram(data = plot_df, position = 'stack', aes(x = benefits, fill = fct_rev(clim_scen_string)), bins = 100) +
  geom_vline(data = rs_benefits_df, aes(xintercept = xintercept), color = 'gray20') +
  ggrepel::geom_label_repel(data = rs_benefits_df, aes(x = xintercept, y = 190, label = rs), 
                            color = 'gray20', direction = 'y', size = 2) +
  facet_grid(rows = vars(model)) +
  ggsci::scale_fill_npg() +
  scale_x_continuous('NPV (£)', labels = scales::unit_format(suffix = 'B', scale = 1), expand = c(0,0)) +
  scale_y_continuous('Frequency', limits = c(0, 250), expand = c(0,0)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')
rs_benefit_plot
ggsave(rs_benefit_plot, filename = paste0('../output/', date_string, '/rs_benefit_plot.png'), units = 'mm', width = 120, height = 70, dpi = 300, scale = 1.7)

rs_benefit_plot_no_rcp <- ggplot() +
  geom_histogram(data = plot_df, aes(x = benefits, fill = five_billion), position = 'stack', bins = 100) +
  geom_vline(data = rs_benefits_df, aes(xintercept = xintercept), color = 'gray20') +
  geom_vline(xintercept = -5, linetype = 2, color = 'gray50') +
  ggrepel::geom_label_repel(data = rs_benefits_df, aes(x = xintercept, y = 430, label = rs), 
                            color = 'gray20', direction = 'y', size = 3) +
  facet_grid(rows = vars(model)) +
  scale_fill_manual(values = c('#E64B35FF', 'gray50')) +
  scale_x_continuous('NPV (£)', labels = scales::unit_format(suffix = 'B', scale = 1), expand = c(0.05,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')+
  coord_cartesian(ylim = c(0,250))
rs_benefit_plot_no_rcp

ggsave(rs_benefit_plot, filename = paste0('../output/', date_string, '/rs_benefit_plot.png'), units = 'mm', width = 120, height = 70, dpi = 300, scale = 1.7)
ggsave(rs_benefit_plot_no_rcp, filename = paste0('../output/', date_string, '/rs_benefit_plot_no_rcp.png'), units = 'mm', width = 120, height = 70, dpi = 300, scale = 1.7)

## Dot plot representation
# Worst 10% outcomes of P-HE
p_he <- plot_df %>%
  filter(model == 'P-HD')
p_he_worst <- p_he %>%
  filter(benefits <= quantile(p_he$benefits, 0.1))

jitter_df <- plot_df %>%
  mutate(worst_p_he = ifelse(run_index %in% p_he_worst$run_index, T, F))
mean_group <- jitter_df %>%
  filter(worst_p_he) %>%
  group_by(model) %>%
  summarise(mean = mean(benefits), min = min(benefits), max = max(benefits)) %>%
  mutate(group = 'group')

jitter_sigmoid <- jitter_df %>%
  group_by(clim_scen_string) %>% 
  slice_sample(n=200) %>%
  ggplot(aes(y = fct_rev(model), x = benefits)) +
  geom_jitter(aes(color = worst_p_he), height = 0.1)+
  geom_segment(data = mean_group, 
               aes(y = fct_rev(model), yend = fct_rev(model), x = min, xend = max),
               color = '#E64B35FF',
               linewidth = 1) +
  geom_point(data = mean_group, aes(y = fct_rev(model), x = mean), color = '#E64B35FF', size = 4) +
  geom_bump(data = mean_group, aes(y = fct_rev(model), x = mean, group = group), color = '#E64B35FF', direction = 'y') +
  geom_label(data = mean_group, aes(label = round(mean, 1), x = mean),color = '#E64B35FF', nudge_y = 0.1) +
  scale_color_manual(values = c('gray80','#F39B7FFF'), guide = 'none') +
  theme_void()

jitter_sigmoid
plot_df %>%
  filter(model == 'P-HD') %>%
  ggplot(aes(x = benefits, fill = five_billion)) +
  geom_histogram(bins = 100) +
  scale_fill_manual(values = c('#E64B35FF', 'gray50'), guide = 'none') +
  theme_void()


fcn_loss_threshold <- function(threshold) {
  cvar <- sum(fcn_normalise_benefits(returns_table$CVaR, billions = F) < threshold) 
  ev <- sum(fcn_normalise_benefits(returns_table$EV, billions = F) < threshold)
  ev_rcp85 <- sum(fcn_normalise_benefits(returns_table$EV_rcp85, billions = F) < threshold)
  c(ev, ev_rcp85, cvar) / 4000
}

x_axis <- seq(-10e9, 0, length.out = 101)
losses <- lapply(x_axis, fcn_loss_threshold)
names(losses) <- x_axis
losses_df <- losses %>% bind_rows() %>% t() %>% as.data.frame()
losses_df$threshold <- -x_axis
colnames(losses_df) <- c('EV','P-HE', 'RA', 'threshold')
exceedance <- losses_df %>%
  pivot_longer(c('EV','P-HE', 'RA')) %>%
  mutate(name = factor(name, levels = c('P-HE','EV', 'RA'))) %>%
  filter(name != 'EV') %>%
  ggplot(aes(x = threshold, y = value, color = name)) +
  geom_line() +
  ggsci::scale_color_nejm() +
  scale_y_continuous('Probability') +
  scale_x_continuous('Net loss (£)', labels = scales::unit_format(suffix = 'B', scale = 1e-9)) +
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = 'bottom')
exceedance
ggsave(exceedance, filename = paste0('../output/', date_string, '/ep_curve.png'), units = 'mm', width = 120, height = 70, dpi = 300, scale = 1)
ggsave(rs_benefit_plot_no_rcp | exceedance, filename = paste0('../output/', date_string, '/rs_benefit_comb.png'), units = 'mm', width = 180, height = 100, dpi = 300, scale = 1.3)

## Scenario table
in_scenario_returns_table <- read_csv(paste0('../../output/', date_string, '_decision_table/in_scenario_returns_table.csv'))
colnames(in_scenario_returns_table) <- returns_table$run_index
rs_run_index <- fcn_get_rs(within_sample = T, return_index = T)$run_index
nh_returns <- in_scenario_returns_table[[rs_run_index[1]]]
d_returns <- in_scenario_returns_table[[rs_run_index[2]]]
hd_returns <- in_scenario_returns_table[[rs_run_index[3]]]

nh_returns[as.numeric(substr(rs_run_index,0,5))] %>%
  fcn_normalise_benefits()

rs_returns_dist <- data.frame(NH = nh_returns, D = d_returns, HD = hd_returns) %>%
  pivot_longer(c('NH', 'D', 'HD'), names_to = 'scenario') %>%
  mutate(value = fcn_normalise_benefits(value)) %>%
  filter(scenario %in% c('NH', 'HD')) %>%
  ggplot(aes(x = value)) +
  facet_grid(rows = vars(fct_rev(scenario))) +
  geom_histogram() +
  scale_x_continuous('NPV (billion £)') +
  scale_y_continuous('Frequency', expand = c(0,0), limits = c(0,670)) +
  theme_bw()
rs_returns_dist
ggsave('../output/20220901/nh_hd_npv.png',rs_returns_dist,
       height = 1250, width = 1000, units = 'px', dpi=300)








