vpxi <- read_csv('../../output/20220901_decision_table/vpxi0.csv')

evpxi <- fcn_normalise_benefits(vpxi) %>%
  colMeans()

evpxi_plot <- fcn_normalise_benefits(vpxi) %>%
  pivot_longer(colnames(vpxi)) %>%
  mutate(name = factor(name, levels = c('emissions', 'emissions_model', 'carbon_value', 'all'),
                       labels = c('E', 'E+M', 'E+M+C', 'E+M+C+A&T (VPI)'))) %>%
  ggplot(aes(x = value, y = fct_rev(name))) +
  geom_vline(xintercept = 0, color = 'gray50') +
  #geom_boxplot(outlier.shape = NA, fill = NA, width = 0.2)+
  geom_boxplot(outlier.shape = NA, fill = 'gray50', width = 0.4, color = 'white', coef = 0)+
  scale_x_continuous('VOI (billion £)')+
  scale_y_discrete('')+
  coord_cartesian(xlim = c(-2, 8)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = c(.1, 1.075),
        legend.background = element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
evpxi_plot

ggsave('../output/20220901/evpxi_plot.png', evpxi_plot)

evpxi_cleveland_plot <- fcn_normalise_benefits(vpxi) %>%
  pivot_longer(colnames(vpxi)) %>%
  mutate(name = factor(name, levels = c('emissions', 'emissions_model', 'carbon_value', 'all'),
                       labels = c('E', 'M', 'C', 'A&T'))) %>%
  group_by(name) %>%
  summarise(mean = mean(value), lb = quantile(value, .1), ub = quantile(value, .9)) %>%
  ggplot(aes(x = mean, y = fct_rev(name))) +
  geom_vline(xintercept = 0, color = 'gray80') +
  geom_errorbarh(aes(xmin = lb, y = fct_rev(name), xmax = ub), 
               color = 'gray50', height = 0.1) +
  geom_point(aes(color = fct_rev(name)), size = 3) +
  ggsci::scale_color_nejm() +
  scale_x_continuous('VOI (billion £)')+
  scale_y_discrete('')+
  coord_cartesian(xlim = c(-5, 20)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = 'none',
        legend.background = element_blank(),
        legend.direction="horizontal",
                plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
evpxi_cleveland_plot
ggsave('../output/20220901/evpxi_plot.png', evpxi_cleveland_plot, width = 1000, height = 1000, dpi = 300, units = 'px')

## Sigmoid plot
vpxi <- read_csv('../../output/20220901_decision_table/vpxi0.csv')
vpxi_ra <- read_csv('../../output/20220901_decision_table/vpxi1.csv')
#vpxi <- vpxi_ra
vpxi_norm <- fcn_normalise_benefits(vpxi[c('emissions', 'emissions_model', 'carbon_value', 'all')]) %>%
  cbind(vpxi[c("clim_scen","model","carbon_value_group","index")])
obj_func <- function(v, lambda=0) (1-lambda) * mean(v) + lambda*mean(v[v < quantile(v, 0.1)])

### EV of RCP
emissions_evpxi <- vpxi_norm %>%
  group_by(clim_scen) %>%
  summarise(emissions = obj_func(emissions))
### EV of climate model
model_evpxi <- vpxi_norm %>%
  group_by(clim_scen, model) %>%
  summarise(emissions_model = obj_func(emissions_model))
### EV of carbon value
carbon_value_evpxi <- vpxi_norm %>%
  group_by(clim_scen, model, carbon_value_group) %>%
  summarise(carbon_value = obj_func(carbon_value))

vpxi_norm_group <- vpxi_norm %>%
  select(c("clim_scen","model","carbon_value_group","index")) %>%
  left_join(emissions_evpxi, by = c('clim_scen')) %>%
  left_join(model_evpxi, by = c('clim_scen', 'model')) %>%
  left_join(carbon_value_evpxi, by = c('clim_scen', 'model', 'carbon_value_group')) %>%
  left_join(select(vpxi_norm, 'index', 'all'), by = 'index')

vpxi_bump <- vpxi_norm_group %>%
  mutate(full_group = paste0(clim_scen, model, carbon_value_group, sep = '-')) %>%
  group_by(full_group) %>%
  slice_sample(n=1) %>%
  pivot_longer(c('emissions', 'emissions_model', 'carbon_value', 'all')) %>%
  mutate(name = factor(name, levels = c('emissions', 'emissions_model', 'carbon_value', 'all'),
                       labels = c('E', 'M', 'C', 'A&T'))) %>%
  ggplot(aes(x = name, y = value, color = factor(clim_scen))) +
  ggbump::geom_bump(aes(group = index)) +
  geom_point() +
  scale_y_log10() +
  theme_minimal()
 
evpxi_cleveland_plot <- vpxi_norm_group %>%
  pivot_longer(c('emissions', 'emissions_model', 'carbon_value', 'all')) %>%
  mutate(name = factor(name, levels = c('emissions', 'emissions_model', 'carbon_value', 'all'),
                       labels = c('E', 'M', 'C', 'A&T'))) %>%
  group_by(name) %>%
  summarise(mean = mean(value), lb = quantile(value, .05), ub = quantile(value, .95)) %>%
  ggplot(aes(x = mean, y = fct_rev(name))) +
  geom_vline(xintercept = 0, color = 'gray80') +
  geom_errorbarh(aes(xmin = lb, y = fct_rev(name), xmax = ub), 
                 color = 'gray50', height = 0.1) +
  geom_text(aes(color = fct_rev(name), label = paste0(fct_rev(name))), nudge_y = 0.3) +
  geom_point(aes(color = fct_rev(name)), size = 3) +
  ggsci::scale_color_nejm() +
  scale_x_continuous('VOI (billion £)')+
  scale_y_discrete('')+
  coord_cartesian(xlim = c(-1, 8)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = 'none',
        legend.background = element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
evpxi_cleveland_plot
ggsave('../output/20220901/evpxi_plot.png', evpxi_cleveland_plot, width = 1000, height = 1000, dpi = 300, units = 'px')

## VOI under risk aversion
vpxi_ra <- read_csv('../../output/20220901_decision_table/vpxi_ce_0.5.csv')
vpxi_ra %>%
  select(c('emissions', 'emissions_model', 'carbon_value', 'all')) %>%
  fcn_normalise_benefits() %>%
  colMeans() 

