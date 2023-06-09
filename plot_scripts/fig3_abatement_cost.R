library(ggplot2)
library(tidyverse)
library(patchwork)

rm(list=ls())

source('./plot_scripts/helpers.R')

cst <- seq(0,250,10)

# CDR technology: Fuss et al (2018) estimates -------
## Lower and upper bounds in 2011 USD
beccs <- c(100,200)
daccs <- c(100,300)
ar <- c(5,50)
ew <- c(50,200)
bc <- c(30,120)
scs <- c(0,100)

cdr <- rbind(beccs, daccs, ar, ew, bc, scs) %>%
  as.data.frame()
colnames(cdr) <- c('lb','ub')

# Convert 2011 USD to 2020 GBP
exch_rate <- 0.6236 # Average exchange rate in 2011
gdp_deflator <- 1.209512 # 2011 to 2020

cdr_gbp <- cdr
cdr_gbp$lb <- cdr$lb * exch_rate * gdp_deflator
cdr_gbp$ub <- cdr$ub * exch_rate * gdp_deflator

cdr_gbp$name <- rownames(cdr_gbp)

ra_labels <- c('Maximise EV', 'Moderate RA', 'Minimise Risk')

cdr_tech_intervals <- cdr_gbp["daccs",c('lb','ub')]
cdr_intervals <- function() {
  list(
    geom_vline(aes(xintercept = as.numeric(cdr_tech_intervals[1])), color = 'gray80'),
    geom_vline(aes(xintercept = as.numeric(cdr_tech_intervals[2])), color = 'gray80')
  )
}

cdr_plot <- cdr_gbp %>%
  mutate(name = factor(name, c('beccs', 'daccs', 'ar', 'ew', 'bc', 'scs'),
                       c('BECCS', 'DACCS', 'AR', 'EW', 'BC', 'SCS'))) %>%
  ggplot(aes(y = as.numeric(name))) +
  cdr_intervals() +
  geom_rect(aes(xmin = lb, xmax = ub, ymin = as.numeric(name)-0.45, ymax = as.numeric(name)+0.45), linewidth=0.5, fill = 'gray50', color = 'gray80') +
  geom_text(aes(x = (lb+ub)/2, label = name), color = 'white')+
  coord_cartesian(xlim = range(cst), ylim = rev(c(.5,6.5)), expand = F) +
  theme_void()
cdr_plot

# CDR plots: hectares, tree planting-based carbon sequestration and total CDR cost -----

decision_table_cst <- lapply(cst, function(x) {
  a1 <- read_csv(paste0('output/tables/oc_decision_table_opp_cost_lambda_50_abatement_', as.character(x), '_ctarget_12.csv'))
  a2 <- read_csv(paste0('output/tables/oc_decision_table_opp_cost_lambda_100_abatement_', as.character(x), '_ctarget_12.csv'))
  a2$CVaR_50 <- a1$CVaR
  a2
})

returns_table_cst <- lapply(cst, function(x) {
  a1 <- read_csv(paste0('output/tables/oc_returns_table_opp_cost_lambda_50_abatement_', as.character(x), '_ctarget_12.csv'))
  a2 <- read_csv(paste0('output/tables/oc_returns_table_opp_cost_lambda_100_abatement_', as.character(x), '_ctarget_12.csv'))
  a2$CVaR_50 <- a1$CVaR
  a2$CVaR_ghg_50 <- a1$CVaR_ghg
  a2
})

names(decision_table_cst) <- cst
names(returns_table_cst) <- cst

max_lb_ub <- TRUE
lb <- function(x) {
  if (max_lb_ub) {
    a <- max(x)
  } else {
    a <- quantile(x, 0.05)
  }
  
  names(a) <- NA
  a
}
ub <- function(x) {
  if (max_lb_ub) {
    a <- min(x)
  } else {
    a <- quantile(x, 0.95)
  }
  names(a) <- NA
  a
}

hectares <- decision_table_cst %>%
  lapply(function(x) {
    x <- x %>% mutate(EV = hectares * EV, CVaR = hectares * CVaR, CVaR_50 = hectares * CVaR_50)
    c(sum(x$EV), sum(x$CVaR_50), sum(x$CVaR))
  }) %>%
  bind_rows() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column('abatement_cost')
colnames(hectares) <- c('abatement_cost', ra_labels)

remove_x_axis <- function() {
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
}

abatement_hectares_plot <- hectares %>%
  pivot_longer(ra_labels) %>%
  filter(name != ra_labels[2]) %>%
  ggplot(aes(y = value, x = as.numeric(abatement_cost), color = name)) +
  cdr_intervals() +
  geom_line(size =1) +
  scale_x_continuous('Riskless CDR\nCost (£/tCO2e/yr)') +
  scale_y_continuous('Tree planting (ha)', labels = scales::unit_format(suffix='M', scale = 1e-6)) +
  ggsci::scale_color_nejm()+
  ggsci::scale_fill_nejm()+
  ggpubr::theme_pubr() +
  coord_cartesian( ylim = c(0, 2e6), expand = F) +
  labs(color = '')+
  theme(legend.position = 'none')
abatement_hectares_plot

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
  filter(strategy != 'cvar50') %>%
  #filter(!(var == 'cost' & dist %in% c('lb', 'ub'))) %>%
  pivot_wider(names_from = dist, values_from = value) %>%
  mutate(abatement_cost = as.numeric(abatement_cost)) %>%
  mutate(strategy = factor(strategy, c('ev', 'cvar50', 'cvar100'), ra_labels)) %>%
  mutate(mean = mean / (12*1e6), lb = lb / (12*1e6), ub = ub / (12*1e6)) %>%
  ggplot(aes(x = abatement_cost, y = mean, fill = strategy, color = strategy)) +
  geom_hline(yintercept = 1, color = 'gray30') +
  cdr_intervals() +
  geom_ribbon(aes(ymin = lb, ymax = ub), color = NA, alpha = 0.3) +
  geom_line(aes(y = mean), size = 1) +
  ggsci::scale_color_nejm()+
  ggsci::scale_fill_nejm()+
  #scale_y_continuous('MtCO2e/yr', labels = scales::unit_format(suffix = '', scale = 1e-6))+
  scale_y_continuous('Contribution to CDR target', labels = scales::percent_format())+
  scale_x_continuous('Riskless CDR \nCost (£/tCO2e/yr)') +
  
  ggpubr::theme_pubr()+
  labs(color = '', fill = '')+
  theme(legend.position = 'none')+
  theme(legend.position = 'bottom')+
  coord_cartesian(ylim = c(0, 1.25), expand = F) +
  guides(fill = 'none')
abatement_ghg_plot

abatement_cost_plot <- ghg %>%
  pivot_longer(-abatement_cost) %>%
  separate(name, c('var', 'strategy', 'dist')) %>%
  filter(var == 'cost') %>%
  filter(strategy != 'cvar50') %>%
  #filter(!(var == 'cost' & dist %in% c('lb', 'ub'))) %>%
  pivot_wider(names_from = dist, values_from = value) %>%
  mutate(abatement_cost = as.numeric(abatement_cost)) %>%
  mutate(strategy = factor(strategy, c('ev', 'cvar50', 'cvar100'), ra_labels)) %>%
  ggplot(aes(x = abatement_cost, y = -mean, fill = strategy, color = strategy)) +
  cdr_intervals() +
  geom_ribbon(aes(ymin = -lb, ymax = -ub), color = NA, alpha = 0.3) +
  geom_line(aes(y = -mean), size = 1) +
  ggsci::scale_color_nejm()+
  ggsci::scale_fill_nejm()+
  scale_y_continuous('Total Cost (£)', labels = scales::unit_format(suffix = 'B', scale = 1))+
  scale_x_continuous('Riskless CDR\ncost (£/tCO2e/yr)') +
  coord_cartesian(expand = F, ylim = c(0,32)) +
  ggpubr::theme_pubr()+
  labs(color = '', fill = '') +
  theme(legend.position = 'bottom')+
  guides(fill = 'none')
abatement_cost_plot

abatement_plot_vertical <- cdr_plot + (abatement_hectares_plot+remove_x_axis()) + (abatement_ghg_plot+remove_x_axis()) + abatement_cost_plot +
  plot_layout(guides = 'collect', heights = c(1,2,2,2))&
  theme(legend.position = 'bottom') &
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
abatement_plot_vertical

layout <- "
AABBCC
DDEEFF"
abatement_plot_horizontal <- cdr_plot + cdr_plot + cdr_plot + abatement_hectares_plot + abatement_ghg_plot + abatement_cost_plot +
  plot_layout(guides = 'collect', design = layout, heights = c(1,3))&
  theme(legend.position = 'bottom') &
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
abatement_plot_horizontal
ggsave('output/figures/fig3_abatement_cost_plot.png', abatement_plot_vertical, width = 1200, height = 3000, units = 'px')
ggsave('output/figures/fig3_abatement_cost_plot_horizontal.png', abatement_plot_horizontal, width = 2500, height = 1300, units = 'px', scale = 1.2)

