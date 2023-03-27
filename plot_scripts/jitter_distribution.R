
library(tidyverse)
source("../scripts/helpers.R")

CER <- c(457, 2544, 3083)
cer_names <- c('P-NH', 'P-ME', 'P-HE')
names(rs_list) <- cer_names
in_scenario_returns_table <- read_csv(paste0('../../output/', date_string, '_decision_table/in_scenario_returns_table.csv'))
decision_table <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_decision_table_mix.csv'))
returns_table <- read_csv(paste0('../../output/',date_string,'_decision_table/oc_returns_table_mix.csv'))

colnames(in_scenario_returns_table) <- 1:4000

selected_cer <- CER[3]
cer_benefits <- in_scenario_returns_table[,CER]%>%
  fcn_normalise_benefits()
colnames(cer_benefits) <- cer_names
cer_benefits_long <- cer_benefits %>% 
  pivot_longer(cer_names) 

distribution_lims <- c(min(c(jitter_df$benefits, cer_benefits_long$value))-0.1, max(jitter_df$benefits, cer_benefits_long$value)+0.1)
cer_benefits_dist_plot <- cer_benefits_long %>%
  ggplot(aes(x = value, color = name, fill = name)) +
  geom_density(alpha = 0.3) +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  coord_cartesian(xlim = distribution_lims, expand = F) +
  ggpubr::theme_pubr() +
  theme_void() +
  guides(color = guide_legend('',title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                              label.theme = element_text(size = 6), title.theme = element_text(size = 8)),
         fill = guide_legend('',title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                                     label.theme = element_text(size = 6), title.theme = element_text(size = 8)))+
  theme(axis.line.x = element_line(), legend.title = element_blank())
cer_benefits_dist_plot

cer_at_risk <- cer_returns < quantile(cer_returns, 0.1)
ra_returns <- returns_table$CVaR %>%
  fcn_normalise_benefits()
cer_returns <- returns_table$EV %>%
  fcn_normalise_benefits()
cer_at_risk <- cer_returns < quantile(cer_returns, 0.1)
run_index <- returns_table$run_index %>% substr(0,5) %>% as.numeric()
which(run_index %in% CER, arr.ind = T)
rs_returns_point <- data.frame(CER = c('NH', 'ME', 'HE'), P.HE = cer_returns[CER], P.RA = ra_returns[CER])
jitter_df <- data.frame(P_HE = cer_returns, RA = ra_returns, at_risk = cer_at_risk, 
                        clim_scen_string = returns_table$clim_scen_string) %>%
  pivot_longer(c('P_HE', 'RA'), names_to = 'model', values_to = 'benefits') %>%
  mutate(model = factor(model, c('P_HE', 'RA'), c('P-HE', 'P-RA')))
mean_group <- jitter_df %>%
  filter(at_risk) %>%
  group_by(model) %>%
  summarise(mean = mean(benefits), min = min(benefits), max = max(benefits), group = 'group')
set.seed(100)
rs_returns_point_long <- rs_returns_point %>%
  pivot_longer(c('P.HE','P.RA'), names_to = 'model', values_to = 'benefits') %>%
  mutate(model = factor(model, c('P.HE','P.RA'), c('P-HE','P-RA')))

jitter_sigmoid <- jitter_df %>%
  group_by(clim_scen_string) %>% 
  slice_sample(n=200) %>%
  ggplot(aes(y = fct_rev(model), x = benefits)) +
  ggnewscale::new_scale_color()+
  geom_jitter(aes(color = at_risk), height = 0.15)+
  scale_color_manual(values = c('gray60','#F39B7FFF'), guide = 'none') +
  ggnewscale::new_scale_color()+
  #geom_label(data = rs_returns_point_long, aes(y = model, label = CER, color = CER), nudge_y = -0.1) +
  geom_bump(data = rs_returns_point_long, aes(color = CER, group = CER), direction = 'y') +
  geom_point(data = rs_returns_point_long, aes(color = CER), size = 2) +
  ggsci::scale_color_nejm() +
  guides(color = guide_legend(title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                              label.theme = element_text(size = 6), title.theme = element_text(size = 8)))+
  ggnewscale::new_scale_color()+
  #geom_jitter(aes(color = at_risk), height = 0.15)+
  geom_segment(data = mean_group, 
               aes(y = fct_rev(model), yend = fct_rev(model), x = min, xend = max),
               color = '#E64B35FF',
               linewidth = 1,
               position = position_nudge(y = 0.2)) +
  geom_point(data = mean_group, aes(y = fct_rev(model), x = mean), color = '#E64B35FF', size = 4,
             position = position_nudge(y = 0.2), shape = 15) +
  geom_bump(data = mean_group, aes(y = as.numeric(fct_rev(model))+0.2, x = mean, group = group), 
            color = '#E64B35FF', direction = 'y', size = 1) +
  geom_label(data = mean_group, aes(label = paste0(round(mean, 1), 'B'), x = mean),color = '#E64B35FF', nudge_y = 0.4) +
  scale_color_manual(values = c('gray60','#F39B7FFF'), guide = 'none')+

  coord_cartesian(xlim = distribution_lims, expand = 0, ylim = c(0.8, 2.6))+
  guides(color = guide_legend(title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                              label.theme = element_text(size = 6), title.theme = element_text(size = 8))) +
  theme_void()
jitter_sigmoid


dist_p_he <- jitter_df %>%
  filter(model == 'P-HE') %>%
  ggplot(aes(x = benefits, fill = at_risk)) +
  geom_histogram(binwidth = .5) +
  #geom_segment(data = rs_returns_point, aes(x = P.NH, y = 50, xend = P.NH, yend = 0, fill = NA)) +
  #geom_point(data = rs_returns_point, aes(x = P.NH, y = 0, fill = NA)) +
  #geom_label(data = rs_returns_point, aes(x = P.NH, y = 50, label = CER), fill = 'white') +
  theme_void() +
  scale_fill_manual(values = c('gray60','#F39B7FFF'), guide = 'none') +
  coord_cartesian(xlim = distribution_lims, 
                  expand = 0)+
  theme(axis.line.x = element_line())
dist_p_he
dist_ra <- jitter_df %>%
  filter(model == 'P-RA') %>%
  ggplot(aes(x = benefits, fill = at_risk)) +
  geom_histogram(binwidth = .5) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = c('gray60','#F39B7FFF'), guide = 'none') +
  coord_cartesian(xlim = distribution_lims, expand = 0) +
  scale_x_continuous('NPV (£)', labels = scales::unit_format(suffix = 'B', scale = 1))+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), axis.line.y = element_blank())
dist_ra
#source('plot_rs_maps.R')

layout_design <- "
AAA##
BBBEF
CCCEF
CCCEF
DDDEF
"
jitter_distribution_plot <- cer_benefits_dist_plot + dist_p_he + jitter_sigmoid + dist_ra + he_ra_maps + cov_mat_plot +
  plot_layout(design = layout_design, heights = c(1,1,1,1,1), guides = 'collect') +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12), legend.position = 'bottom')
jitter_distribution_plot
ggsave('../output/20220901/jitter_distribution.png', jitter_distribution_plot, width = 2500, height = 2000, units = 'px')
dev.off()

## Efficiency frontier
mean(returns_table$EV) %>% fcn_normalise_benefits()
in_scenario_ev <- in_scenario_returns_table %>%
  fcn_normalise_benefits() %>%
  apply(2, mean)
in_scenario_risk <- in_scenario_returns_table %>%
  fcn_normalise_benefits() %>%
  #apply(2, function(x) mean(x[x < quantile(x, 0.1)]))
  apply(2, sd)
plot(in_scenario_risk, in_scenario_ev)

