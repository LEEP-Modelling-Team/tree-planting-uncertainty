library(tidyverse)
library(gganimate)

source('../scripts/helpers.R')
source('../scripts/fcn_get_rs.R')

rs_list <- lapply(c('rcp26', 'rcp45', 'rcp60', 'rcp85'), function(rcp) {
  l <- lapply(c('01','04','06','15'), function(m) {
    d <- read_csv(paste0('../../mc_data/20220901/representative_scenarios/climate_average_rs_',rcp,'_custom_',m,'.csv'))
    colnames(d) <- c('new2kid', 'temp2020', 'temp2050', 'rain2020', 'rain2050')
    d
    })
  names(l) <- c('01','04','06','15')
  l %>% bind_rows(.id = 'model')
})

names(rs_list) <- c('rcp26', 'rcp45', 'rcp60', 'rcp85')

delta_table <- rs_list %>%
  bind_rows(.id = 'rcp') %>%
  group_by(rcp, model) %>%
  summarise(temp_2020 = mean(temp2020), 
            temp_2050 = mean(temp2050), 
            rain_2020 = mean(rain2020), 
            rain_2050 = mean(rain2050))

rs <- fcn_get_rs(within_sample = T, return_index = T)%>%
  mutate(clim_scen_comb = paste(clim_scen_string, climate_model_member, sep = '_'))

baseline_temp = 12.02274
baseline_rain = 466.4416

delta_table <- delta_table %>%
  mutate(lab = NA) %>%
  mutate(lab = ifelse(rcp == rs$clim_scen_string[1] & model == rs$climate_model_member[1], 'NH', lab)) %>%
  mutate(lab = ifelse(rcp == rs$clim_scen_string[2] & model == rs$climate_model_member[2], 'D', lab)) %>%
  mutate(lab = ifelse(rcp == rs$clim_scen_string[3] & model == rs$climate_model_member[3], 'HD', lab))

mean_climate_table <- delta_table %>%
  mutate(rcp = factor(rcp, levels = c('rcp26','rcp45','rcp60','rcp85'), 
                      labels = c('RCP2.6', 'RCP4.5', 'RCP6.0','RCP8.5'))) %>%
  pivot_longer(c(temp_2020, temp_2050, rain_2020, rain_2050)) %>%
  separate(name, c('var', 'year')) %>%
  pivot_wider(names_from = 'var', values_from = 'value') %>%
  mutate(rcp_model = paste(rcp, model))
  #mutate(lab = ifelse(year == 2040, NA, lab)) %>%
mean_climate <- ggplot(mean_climate_table, aes(y = temp, x = rain, color=fct_rev(rcp), group=year)) +
  #geom_errorbar(aes(ymin = temp_mean-temp_sd, ymax=temp_mean+temp_sd)) +
  #geom_errorbarh(aes(xmin = rain_mean-rain_sd, xmax=rain_mean+rain_sd)) +
  geom_point(data = filter(mean_climate_table, year == 2050), aes(color = fct_rev(rcp), shape = model), size = 2, alpha= 0.3) +
  geom_hline(yintercept = baseline_temp, color = 'gray50') +
  geom_vline(xintercept = baseline_rain, color = 'gray50') +
  geom_line(aes(color = fct_rev(rcp), group = rcp_model), alpha= 0.3) +
  geom_point(data = filter(mean_climate_table, !is.na(lab) & year == 2050), 
             aes(color = fct_rev(rcp), shape = model), size = 4) +
  geom_line(data = filter(mean_climate_table, !is.na(lab)), 
            aes(group = rcp_model)) +
  geom_label(aes(label = ifelse(year == 2020, lab, NA)),box.padding = 0.5, max.overlaps = Inf) +
  scale_y_continuous('Average growing season temperature (°C)') +
  scale_x_continuous('Average growing season rainfall (mm)') +
  scale_shape_discrete('Model member') +
  ggsci::scale_color_npg() +
  theme_bw() +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text = element_blank()) +
  guides(alpha = 'none') +
  labs(caption = element_text('Lines show 1980-2000 baseline'),
       color = element_text('RCP'))
mean_climate
ggsave(mean_climate, filename = paste0( '../output/',date_string,'/mean_climate.png'),
       dpi = 300, height = 100, width = 150, units = 'mm')

ts_table <- read_csv('../../output/20220726_decision_table/climate_delta_ts.csv') %>%
  pivot_longer(-var, names_to = 'year', values_to = 'value')%>%
  separate(var, c('Climate','cells','ukcp18','rcp','var','model')) %>%
  select(-c('Climate','cells','ukcp18')) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(lab = NA) %>%
  mutate(lab = ifelse(rcp == rs$clim_scen_string[1] & model == rs$climate_model_member[1], 'NH', lab)) %>%
  mutate(lab = ifelse(rcp == rs$clim_scen_string[2] & model == rs$climate_model_member[2], 'D', lab)) %>%
  mutate(lab = ifelse(rcp == rs$clim_scen_string[3] & model == rs$climate_model_member[3], 'HD', lab)) %>%
  mutate(clim_scen_comb = paste(rcp, model, sep = '_'))

baseline <- data.frame(
  var = c('Temperature (°C)', 'Rainfall (mm)'),
  value = c(baseline_temp, baseline_rain)
) 
ts_table <- ts_table %>%
  mutate(rcp = factor(rcp, levels = c('rcp26','rcp45','rcp60','rcp85'), 
                      labels = c('RCP2.6', 'RCP4.5', 'RCP6.0','RCP8.5'))) %>%
  mutate(var = factor(var, levels = c('rain','temp'), labels = c('Rainfall (mm)', 'Temperature (°C)')))
  
ts_plot <- ts_table %>%
  ggplot(aes(x = year, y = value, color = fct_rev(rcp))) +
  geom_hline(data = baseline, aes(yintercept=value), color = 'gray50') +
  geom_label(data = filter(ts_table, year == 2080 & var == 'Temperature (°C)'), aes(label = lab), x = 2050, y = 17, show_guide  = FALSE) +
  geom_smooth(se = F, size = 0.5) +
  geom_smooth(data = filter(ts_table, !is.na(lab)), size = 1, se = F) +
  scale_x_continuous('Year') +
  scale_y_continuous('') +
  facet_grid(rows = vars(fct_rev(var)), cols = vars(model), scales = 'free_y',
             switch="y") +
  ggsci::scale_color_npg() +
  labs(color = 'RCP') +
  coord_cartesian(xlim = c(2020, 2085)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.placement = "outside",
        strip.background.y = element_blank())
ts_plot

ggsave(ts_plot, filename = paste0('../output/', date_string, '/ts_plot.png'),
       dpi = 300, height = 100, width = 150, units = 'mm', scale = 1.2)



## Plot the trends in carbon price, wheat and dairy
rs_position <- rs$run_index %>%
  substr(0,5) %>%
  as.numeric()
carbon_price_assumption <- read_csv("../../output/20220901_decision_table/carbon_price_assumption.csv", col_names = F)
price_wheat_assumption <- read_csv("../../output/20220901_decision_table/price_wheat_assumption.csv", col_names = F)
price_dairy_assumption <- read_csv("../../output/20220901_decision_table/price_dairy_assumption.csv", col_names = F)
price_timber_assumption <- read_csv("../../output/20220901_decision_table/price_timber_assumption.csv", col_names = F)
price_list <- list(carbon_price = carbon_price_assumption,
                   price_wheat = price_wheat_assumption,
                   price_dairy = price_dairy_assumption,
                   price_timber = price_timber_assumption*22)

price_change <- lapply(price_list,function(field) {
  price_rs <- field[rs_position,] %>% t() %>% as.data.frame()
  rownames(price_rs) <- NULL
  colnames(price_rs) <- c('NH', 'ME', 'HE')
  price_rs$year <- 2020:2049
  price_rs
  }) %>%
  bind_rows(.id = 'var') %>%
  pivot_longer( c('NH', 'ME', 'HE'), names_to = 'scenario')
ts_table_rs <- ts_table %>%
  filter(clim_scen_comb %in% rs$clim_scen_comb) %>%
  mutate(scenario =  c('NH', 'ME', 'HE')[match(clim_scen_comb, rs$clim_scen_comb)]) %>%
  select(colnames(price_change)) %>%
  filter(year <= 2049)
var_change <- rbind(price_change, ts_table_rs) %>%
  mutate(var = factor(var, levels = unique( rbind(price_change, ts_table_rs)$var ), labels = c('Carbon Price (£/tCO2e)', 'Wheat (£/t)', 'Dairy (ppl)',
                                                                       'Timber (%)', "Temperature (°C)", "Rainfall (mm)"   )))

load_plot <- function(keep_var) {
  var_change %>%
    filter(scenario %in% keep_var) %>%
    ggplot(aes(x = year, y = value, group = scenario)) +
    geom_line(color = 'gray60') +
    geom_smooth(se = F, method = 'loess', aes(color = fct_rev(scenario))) +
    facet_wrap(~var, scales = 'free_y', nrow = 3, strip.position = 'left')+
    scale_y_continuous('') +
    ggsci::scale_color_aaas(drop = F) +
    theme_bw()+
    theme(strip.placement = "outside",
          strip.background.y = element_blank(),
          legend.title = element_blank(),
          legend.position = 'bottom')
}

ggsave('../output/20220901/ts_plot_var_nh.png', load_plot(c('NH')), height = 1800, width = 1500, units = 'px')
ggsave('../output/20220901/ts_plot_var_nh_d.png', load_plot(c('NH', 'ME')), height = 1800, width = 1500, units = 'px')
ggsave('../output/20220901/ts_plot_var_nh_hd.png', load_plot(c('NH', 'HE')), height = 1800, width = 1500, units = 'px')
ggsave('../output/20220901/ts_plot_var_all.png', load_plot(c('NH', 'ME', 'HE')), height = 1800, width = 1500, units = 'px')

# Plot by scenario by normalising to a baseline
price_mean <- price_list %>%
  lapply(function(x) apply(x,2,mean)[1])
price_sd <- price_list %>%
  lapply(function(x) apply(x,2,sd)[30])
ts_sd <- ts_table %>%
  group_by(var) %>%
  summarize(sd = sd(value))

baseline_values <- data.frame(
  var = c("carbon_price", "price_wheat",  "price_dairy", "price_timber", "Temperature (°C)", "Rainfall (mm)"),
  baseline = c(price_mean$carbon_price,138.88,27.12,22,baseline_temp, baseline_rain),
  sd = c(price_sd$carbon_price, price_sd$price_wheat, price_sd$price_dairy, price_sd$price_timber,
         ts_sd$sd[2],ts_sd$sd[1]),
  prefix = c('£','£','£','£','',''),
  suffix = c('/tCO2e','/t','ppl','/m3','°C','mm'),
  decimals = c(0,1,1,1,1,0)
)

var_change_pct <- rbind(price_change, ts_table_rs) %>%
  left_join(baseline_values, by = 'var') %>%
  mutate(pct_change = value/baseline) %>%
  mutate(norm_change = (value - baseline)/sd) %>%
  mutate(label = paste0(prefix, round(value,decimals), suffix)) %>%
  mutate(var = factor(var, levels = unique( rbind(price_change, ts_table_rs)$var), labels = c('Carbon Price', 'Wheat', 'Dairy',
                                                                                              'Timber', "Temperature", "Rainfall"   ))) 
var_change_lab <- var_change_pct %>%
  filter(year >= 2020 & year <= 2049) %>%
  group_by(var, scenario) %>%
  summarise(value = mean(value-baseline), pos = max(abs(norm_change))*sign(mean(norm_change)), 
            prefix = dplyr::first(prefix), 
            suffix = dplyr::first(suffix), decimals = dplyr::first(decimals),
            year = 2050) %>%
  mutate(label = paste0(ifelse(value >= 0, '+', '-'), prefix, round(abs(value), decimals), suffix))
options(ggrepel.max.overlaps = Inf)
var_change_pct_plot <- var_change_pct %>%
  ggplot(aes(x = year, y = norm_change, color = var)) +

  geom_hline(yintercept = 0) +
  #geom_line() +
  facet_wrap(~fct_rev(scenario), nrow = 1) + 

  geom_smooth(se = F, linewidth = .5)+
  # ggrepel::geom_label_repel(data = var_change_lab,
  #                           hjust = 0, nudge_y = 0.1, size = 2, 
  #                           aes(y = pos, label = label, color = var),
  #                           show.legend = FALSE,
  #                           direction = 'y', xlim = c(2030, 2050), 
  #                           ylim = c(-Inf, Inf),
  #                           force = 1,
  #                           x = 2045,
  #                           segment.size=0) +
  scale_y_continuous('SD') +
  scale_x_continuous('Year', expand = c(0,0)) +
  ggsci::scale_color_npg() +
  ggpubr::theme_pubr() +
  coord_cartesian(ylim = c(-3,5), xlim = c(2020, 2049), clip = 'on') +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 6), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"))
var_change_pct_plot
ggsave('../output/20220901/var_change_pct_plot.png',var_change_pct_plot,
       width = 2000, height = 1000, units = 'px', dpi = 300)
dev.off()
