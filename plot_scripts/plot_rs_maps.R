CER <- c(457, 2544, 3083)
rs_list <- lapply(CER, function(x) read_csv(paste0('../../output/',date_string,'_decision_table/oc_decision_table_rs_',x,'.csv'))) %>%
  lapply(function(x) x %>% mutate(hectares_planted = hectares * EV))
names(rs_list) <- c('P-NH', 'P-ME', 'P-HE')
rs_maps <- rs_list %>%
  fcn_plot_planting_mix(facet_dir = 'col')
facet_width <- (210-2.54)/5
facet_height <- (273-2.54)
ggsave(rs_maps,filename = paste0('../output/', date_string, '/rs_maps_ghg_target.png'), width = 20, height = 10,
       units = 'cm', scale = 1, dpi=300)
dev.off()

decision_table <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_decision_table_mix.csv'))
EV_solution <- decision_table %>% mutate(hectares_planted = hectares * EV)
CVaR_solution <- decision_table %>% mutate(hectares_planted = hectares * CVaR)
selected_solutions <- list(`P-NH` = rs_list$`P-NH`, `P-RA` = CVaR_solution)
nh_ra_maps <- selected_solutions %>%
  fcn_plot_planting_mix(facet_dir = 'row')
he_ra_maps <- list(`P-EV` = EV_solution, `P-RA` = CVaR_solution) %>%
  fcn_plot_planting_mix(facet_dir = 'row') +
  guides(color = guide_legend(title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                              label.theme = element_text(size = 6), title.theme = element_text(size = 8)))

## Plot scatter plots and difference maps
zone_diff_costs <- read_csv('../../output/20220901_decision_table/zone_differences_costs.csv')
zone_diff_location <- read_csv('../../output/20220901_decision_table/zone_differences_location.csv')

selected_solutions <- list(`P-HE` = rs_list$`P-HE`, `P-RA` = CVaR_solution)
zone_diff_loc <- selected_solutions
zone_diff_loc$`Both` <- zone_diff_loc$`P-HE` %>%
  left_join(zone_diff_location, by = c("new2kid", "species")) %>%
  mutate(hectares_planted = hectares_planted * Both)
zone_diff_loc$`P-HE` <- zone_diff_loc$`P-HE` %>%
  left_join(zone_diff_location, by = c("new2kid", "species")) %>%
  mutate(hectares_planted = hectares_planted * A_only)
zone_diff_loc$`P-RA` <- zone_diff_loc$`P-RA` %>%
  left_join(zone_diff_location, by = c("new2kid", "species")) %>%
  mutate(hectares_planted = hectares_planted * B_only)
zone_diff_loc <- zone_diff_loc[c('P-HE', 'Both', 'P-RA')]
zone_diff_loc_maps <- zone_diff_loc %>%
  fcn_plot_planting_mix()
ggsave(zone_diff_loc_maps,filename = paste0('../output/', date_string, '/zone_diff_maps.png'), width = 20, height = 10,
       units = 'cm', scale = 1, dpi=300)

pal <- ggsci::pal_nejm()(8)
x_axis_col <- pal[4]
y_axis_col <- pal[3]
p_nh_scatter <- zone_diff_costs %>%
  fcn_normalise_benefits(billions = F) %>%
  ggplot(aes(x = Both, y = A_only)) +
  geom_hline(yintercept = 0, color = 'gray30', linetype = 3) +
  geom_vline(xintercept = 0, color = 'gray30', linetype = 3) +
  geom_point(color = y_axis_col, alpha = 0.5)+
  ggpubr::theme_pubr()+
  geom_smooth(method = 'lm', se = F, color = 'gray50') +
  ggpmisc::stat_correlation(small.r = T) +
  theme(axis.line.x.bottom=element_line(color=x_axis_col),
        axis.text.x = element_text(colour = x_axis_col),
        axis.ticks.x = element_line(colour = x_axis_col),
        axis.title.x = element_text(colour = x_axis_col),
        axis.line.y.left = element_line(color=y_axis_col),
        axis.text.y = element_text(colour = y_axis_col),
        axis.ticks.y = element_line(colour = y_axis_col),
        axis.title.y = element_text(colour = y_axis_col),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank())+
  labs(x = "NPV (£/ha): P-HE & P-RA", y = "NPV (£/ha): P-HE only") 

y_axis_col <- pal[6]
p_ra_scatter <- zone_diff_costs %>%
  fcn_normalise_benefits(billions = F) %>%
  ggplot(aes(x = Both, y = B_only)) +
  geom_hline(yintercept = 0, color = 'gray30', linetype = 3) +
  geom_vline(xintercept = 0, color = 'gray30', linetype = 3) +
  geom_point(color = y_axis_col, alpha = 0.5)+
  ggpubr::theme_pubr()+
  geom_smooth(method = 'lm', se = F, color = 'gray50') +
  ggpmisc::stat_correlation(small.r = T) +
  theme(axis.line.x.bottom=element_line(color=x_axis_col),
        axis.text.x = element_text(colour = x_axis_col),
        axis.ticks.x = element_line(colour = x_axis_col),
        axis.title.x = element_text(colour = x_axis_col),
        axis.line.y.left = element_line(color=y_axis_col),
        axis.text.y = element_text(colour = y_axis_col),
        axis.ticks.y = element_line(colour = y_axis_col),
        axis.title.y = element_text(colour = y_axis_col),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank())+
  labs(x = "NPV (£/ha): P-HE & P-RA", y = "NPV (£/ha): P-RA only") 

layout_design <- "
AAC
AAC
BBC
BBC
"

zone_diff_loc_maps_cost <- p_nh_scatter + p_ra_scatter + zone_diff_loc_maps + plot_layout(design = layout_design) +
  plot_annotation(tag_levels = 'a')
ggsave(zone_diff_loc_maps_cost,filename = paste0('../output/', date_string, '/zone_diff_maps_cost.png'), width = 20, height = 20,
       units = 'cm', scale = 1, dpi=300)


## Covariance matrix
ev_cov_mat <- read_csv(paste0('../../output/',date_string,'_decision_table/ev_cov_mat.csv'), col_names = F)
cvar_cov_mat <- read_csv(paste0('../../output/',date_string,'_decision_table/cvar_cov_mat.csv'), col_names = F)

colnames(ev_cov_mat) <- c('Conifers', 'Broadleaf')
ev_cov_mat$species <- c('Conifers', 'Broadleaf')
colnames(cvar_cov_mat) <- c('Conifers', 'Broadleaf')
cvar_cov_mat$species <- c('Conifers', 'Broadleaf')
ev_cvar_cov_mat <- list(`P-EV` = ev_cov_mat, `P-RA` = cvar_cov_mat) %>%
  bind_rows(.id = 'strategy')

cov_mat_plot <- ev_cvar_cov_mat %>%
  pivot_longer(c('Conifers', 'Broadleaf')) %>%
  ggplot(aes(x = species, y = name)) +
  geom_tile(aes(fill = value), color = 'black') +
  geom_text(aes(label = round(value/1e18,1),color = value > 1.41e19)) +
  facet_wrap(~strategy, nrow = 2) +
  scale_fill_viridis_b(option = 'magma', bquote('x'~10^19), labels = scales::unit_format(suffix = '', scale = 1e-19), trans = 'log10', direction = -1) +
  scale_color_manual(guide = 'none', values = c("black", "white")) +
  ggpubr::theme_pubr() +
  guides(fill = 'none', colour = 'none') +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 8))
cov_mat_plot


