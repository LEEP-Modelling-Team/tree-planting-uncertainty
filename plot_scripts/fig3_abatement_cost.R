library(ggplot2)
library(tidyverse)
library(patchwork)

rm(list=ls())

source('./plot_scripts/helpers.R')

cst <- seq(0,100,2.5)
decision_table_cst <- lapply(cst, function(x) {
  a1 <- read_csv(paste0('output/tables/oc_decision_table_mix_lambda_50_abatement_', as.character(x), '_ctarget_12.csv'))
  a2 <- read_csv(paste0('output/tables/oc_decision_table_mix_lambda_100_abatement_', as.character(x), '_ctarget_12.csv'))
  a2$CVaR_50 <- a1$CVaR
  a2
})

names(decision_table_cst) <- cst
hectares <- decision_table_cst %>%
  lapply(function(x) {
    x <- x %>% mutate(EV = hectares * EV, CVaR = hectares * CVaR, CVaR_50 = hectares * CVaR_50)
    c(sum(x$EV), sum(x$CVaR_50), sum(x$CVaR))
  }) %>%
  bind_rows() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column('abatement_cost')
colnames(hectares) <- c('abatement_cost', 'λ=0 (P-EV)', 'λ=0.5', 'λ=1 (P-RA)')

abatement_hectares_plot <- hectares %>%
  pivot_longer(c('λ=0 (P-EV)', 'λ=0.5', 'λ=1 (P-RA)')) %>%
  ggplot(aes(y = value, x = as.numeric(abatement_cost), color = name)) +
  geom_line(size =1) +
  scale_x_continuous('Net GGR Cost (£/tCO2e/yr)') +
  scale_y_continuous('Hectares', labels = scales::unit_format(suffix='M', scale = 1e-6)) +
  ggsci::scale_color_jama()+
  ggsci::scale_fill_jama()+
  ggpubr::theme_pubr() +
  coord_cartesian(xlim = c(0,75), ylim = c(0, 2e6), expand = F) +
  labs(color = '')+
  theme(legend.position = 'none')
abatement_hectares_plot


returns_table_cst <- lapply(cst, function(x) {
  a1 <- read_csv(paste0('output/tables/oc_returns_table_mix_lambda_50_abatement_', as.character(x), '_ctarget_12.csv'))
  a2 <- read_csv(paste0('output/tables/oc_returns_table_mix_lambda_100_abatement_', as.character(x), '_ctarget_12.csv'))
  a2$CVaR_50 <- a1$CVaR
  a2$CVaR_ghg_50 <- a1$CVaR_ghg
  a2
})
names(returns_table_cst) <- cst
lb <- function(x) {
  a <- quantile(x, 0.05)
  names(a) <- NA
  a
}
ub <- function(x) {
  a <- quantile(x, 0.95)
  names(a) <- NA
  a
}
ghg <- 1:length(cst) %>%
  lapply(function(i) {
    x <- returns_table_cst[[i]]
    c <- cst[i]
    x$EV <- x$EV + max(c(0, c*(12e6 - mean(x$EV_ghg))))
    x$CVaR_50 <- x$CVaR_50 + max(c(0, c*(12e6 - mean(x$CVaR_ghg_50))))
    x$CVaR <- x$CVaR + max(c(0, c*(12e6 - mean(x$CVaR_ghg))))
    
    cost_vec <- c(mean(x$EV), mean(x$CVaR_50), mean(x$CVaR), 
    lb(x$EV), lb(x$CVaR_50), lb(x$CVaR),
    ub(x$EV), ub(x$CVaR_50), ub(x$CVaR)) %>%
      fcn_normalise_benefits()
    
    c(cost_vec,
      mean(x$EV_ghg), mean(x$CVaR_ghg_50), mean(x$CVaR_ghg),
      lb(x$EV_ghg), lb(x$CVaR_ghg_50), lb(x$CVaR_ghg),
      ub(x$EV_ghg), ub(x$CVaR_ghg_50), ub(x$CVaR_ghg))
  })
names(ghg) <- cst
ghg <- ghg %>%
  bind_rows(.id = 'abatement_cost') %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column('abatement_cost')
View(ghg)
colnames(ghg) <- c('abatement_cost', 'cost_ev_mean', 'cost_cvar50_mean', 'cost_cvar100_mean', 
                   'cost_ev_lb', 'cost_cvar50_lb', 'cost_cvar100_lb', 
                   'cost_ev_ub', 'cost_cvar50_ub', 'cost_cvar100_ub', 
                   'ghg_ev_mean', 'ghg_cvar50_mean', 'ghg_cvar100_mean',
                   'ghg_ev_lb', 'ghg_cvar50_lb', 'ghg_cvar100_lb',
                   'ghg_ev_ub', 'ghg_cvar50_ub', 'ghg_cvar100_ub')

abatement_ghg_plot <- ghg %>%
  pivot_longer(-abatement_cost) %>%
  separate(name, c('var', 'strategy', 'dist')) %>%
  filter(var == 'ghg') %>%
  #filter(!(var == 'cost' & dist %in% c('lb', 'ub'))) %>%
  pivot_wider(names_from = dist, values_from = value) %>%
  mutate(abatement_cost = as.numeric(abatement_cost)) %>%
  mutate(strategy = factor(strategy, c('ev', 'cvar50', 'cvar100'), c('λ=0 (P-EV)', 'λ=0.5', 'λ=1 (P-RA)'))) %>%
  ggplot(aes(x = abatement_cost, y = mean, fill = strategy, color = strategy)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), color = NA, alpha = 0.3) +
  geom_line(aes(y = mean), size = 1) +
  ggsci::scale_color_jama()+
  ggsci::scale_fill_jama()+
  scale_y_continuous('MtCO2e/yr', labels = scales::unit_format(suffix = '', scale = 1e-6))+
  scale_x_continuous('Abatement Cost (£/t/yr)') +
  ggpubr::theme_pubr()+
  labs(color = '', fill = '')+
  theme(legend.position = 'none')+
  theme(legend.position = 'bottom')+
  coord_cartesian(xlim = c(0,75), ylim = c(0, 15e6), expand = F) +
  guides(fill = 'none')
abatement_ghg_plot

abatement_cost_plot <- ghg %>%
  pivot_longer(-abatement_cost) %>%
  separate(name, c('var', 'strategy', 'dist')) %>%
  filter(var == 'cost') %>%
  #filter(!(var == 'cost' & dist %in% c('lb', 'ub'))) %>%
  pivot_wider(names_from = dist, values_from = value) %>%
  mutate(abatement_cost = as.numeric(abatement_cost)) %>%
  mutate(strategy = factor(strategy, c('ev', 'cvar50', 'cvar100'), c('λ=0 (P-EV)', 'λ=0.5', 'λ=1 (P-RA)'))) %>%
  ggplot(aes(x = abatement_cost, y = mean, fill = strategy, color = strategy)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), color = NA, alpha = 0.3) +
  geom_line(aes(y = mean), size = 1) +
  ggsci::scale_color_jama()+
  ggsci::scale_fill_jama()+
  scale_y_continuous('NPV (£)', labels = scales::unit_format(suffix = 'B', scale = 1))+
  scale_x_continuous('Net GGR Cost (£/t/yr)') +
  coord_cartesian(ylim = c(-15, 25), xlim = c(0,75), expand = F) +
  ggpubr::theme_pubr()+
  labs(color = '', fill = '') +
  theme(legend.position = 'bottom')+
  guides(fill = 'none')
abatement_cost_plot

abatement_plot <- abatement_hectares_plot + abatement_ghg_plot + abatement_cost_plot +
  plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom') &
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
abatement_plot
ggsave('output/figures/abatement_cost_plot.png', abatement_plot, width = 3000, height = 1200, units = 'px')
