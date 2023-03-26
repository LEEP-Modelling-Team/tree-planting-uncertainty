library(patchwork)
library(showtext)
library(ggsci)
#showtext::showtext_auto()
#showtext_opts(dpi = 300)
#font_add(family = "Helvetica", regular = "~/.fonts/Helvetica.ttf")

source('../scripts/helpers.R')
source('../scripts/plot_mix_planting.R')
source("../scripts/nightingale_plot.R")

# Representative scenarios
facet_width <- (210-2.54)/5
facet_height <- (273-2.54)

source("../scripts/fcn_get_rs.R")
source("../scripts/plot_rep_scen_grid.R")
df_list <- fcn_get_rs(within_sample = T)
CER <- c(457, 2544, 3083)
rs_list <- lapply(CER, function(x) read_csv(paste0('../../output/',date_string,'_decision_table/oc_decision_table_rs_',x,'.csv'))) %>%
  lapply(function(x) x %>% mutate(hectares_planted = hectares * EV))
names(rs_list) <- c("P-NH", "P-ME", "P-HE")
plot_list_decision <- rs_list %>%
  fcn_plot_planting_mix(facet_dir = 'row')
ggsave(plot_list_decision,filename = paste0('../output/', date_string, '/rs_decision.png'), width = facet_width, height = facet_height*0.75,
       units = 'mm', scale = 1, dpi=300)

plot_vs <- fcn_spatial_plot_var(df_list, fcn_value_surface, qt = 1, var_label = "Quantile (NPV £/ha)", var_name = 'benefits_qt', 
                                midpoint = .5, plot_limits = c(0.8,1))
ggsave(plot_vs,filename = paste0('../output/', date_string, '/rs_value_surface.png'), width = facet_width*2, height = facet_height*0.75,
       units = 'mm', scale = 1, dpi=300)

plot_farm_profit <- fcn_plot_opp_cost(df_list)
ggsave(plot_farm_profit,filename = paste0('../output/', date_string, '/rs_farm_profit.png'), width = facet_width*2, height = facet_height*0.75,
       units = 'mm', scale = 1, dpi=300)



plot_list_bar <- fcn_plot_bar_location(df_list)
ggsave(plot_list_bar,filename = paste0('../output/', date_string, '/rs_bar.png'), width = facet_width*2, height = facet_height*0.75,
       units = 'mm', scale = 1.2, dpi=300)

plot_list_doughnut <- fcn_plot_bar_doughnut(rs_list)
ggsave(plot_list_doughnut,filename = paste0('../output/', date_string, '/rs_doughnut.png'),
       width = facet_width*1.5, height = facet_height*0.5,
       units = 'mm', scale = 1.2, dpi=300)

decision_table <- fcn_get_decision_table(suffix = '', strategies = c('EV','CVaR'))[c('EV','CVaR')]
names(decision_table) <- c('Expected Value', 'Risk Averse')
#decision_table_rcp85 <- fcn_get_decision_table('no_foregone_ag_ghg_value_rcp85')['EV']
#decision_table$`High Impact` <- decision_table_rcp85$EV
plot_list_decision_table <- decision_table %>% fcn_plot_planting_mix(facet_dir = 'col')
ggsave(plot_list_decision_table,filename = paste0('../output/', date_string, '/decision_table.png'), width = facet_width*5, height = facet_height*0.5,
       units = 'mm', scale = 1, dpi=300)

df_climate_list <- fcn_get_rs(prefix = 'climate_average_', suffix = '', within_sample = T) %>%
  lapply(function(x) {
    colnames(x) <- c('new2kid', 'temp2020', 'temp2050', 'rain2020', 'rain2050')
    x <- x %>%
      mutate(average_temp = (temp2020+temp2050)/2, average_rain = (rain2020+rain2050)/2)
    x
  })
plot_temp <- fcn_plot_average_climate(df_climate_list, 'temp')
plot_rain <- fcn_plot_average_climate(df_climate_list, 'rain')
ggsave(plot_temp,filename = paste0('../output/', date_string, '/rs_climate_temp.png'), width = facet_width, height = facet_height*0.75,
       units = 'mm', scale = 1, dpi=300)
ggsave(plot_rain,filename = paste0('../output/', date_string, '/rs_climate_rain.png'), width = facet_width, height = facet_height*0.75,
       units = 'mm', scale = 1, dpi=300)

source("../scripts/plot_mix_share.R")

rs_mix_share_simple <- fcn_plot_mix_share(facetting = T,smooth = F, rcp_point_coloring = F, labels = F)
ggsave(rs_mix_share_simple, filename = paste0('../output/', date_string, '/rs_mix_share_simple.png'), width = facet_width*5, height = facet_height*0.3, units = 'mm', scale = 1)

rs_mix_share <- fcn_plot_mix_share()
ggsave(rs_mix_share, filename = paste0('../output/', date_string, '/rs_mix_share.png'), width = facet_width*5, height = facet_height*0.3, units = 'mm', scale = 1)

#Returns tail
source("../scripts/plot_returns_tail.R")
upside_vec <- c(F, T, F, T)
downside_vec <- c(F, F, T, T)
name_vec <- c("no_color", "upside", "downside", "all")

for (i in 1:length(upside_vec)) {
  returns_tail <- fcn_plot_returns_tail(color_upside = upside_vec[i], 
                                        color_downside = downside_vec[i],
                                        selected_cols = c(1,2,3,4,5))
  ggsave(returns_tail, filename = paste0('../output/', date_string, '/returns_tail_ra_', name_vec[i], '.png'), width = facet_width*5, height = facet_height*0.5, units = 'mm', scale = 1)
}

#Efficiency frontiers ------
source('../scripts/plot_efficiency_frontier.R')
ef_plot <- fcn_plot_efficiency_frontier()
ef_plot
ggsave(ef_plot, filename = paste0('../output/', date_string, '/efficiency_frontier.png'), width = facet_width*3, height = facet_height*0.4, units = 'mm', scale = 1.2)
