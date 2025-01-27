library(ggplot2)
library(tidyverse)
library(ggbump)
library(patchwork)
library(ggpubr)

rm(list=ls())
source("plot_scripts/helpers.R")
source("plot_scripts/define_cer.R")

returns_table_cer <- lapply(CER, function(cer) read_csv(paste0("output/tables/oc_returns_table_rs_", cer, ".csv")))

returns_table <- read_csv(paste0('output/tables/oc_returns_table_mix.csv'))

common_guides <- function(title = '') {
  guides(color = guide_legend(title = title, title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                            label.theme = element_text(size = 6), title.theme = element_text(size = 8)),
         fill = guide_legend(title = title, title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                              label.theme = element_text(size = 6), title.theme = element_text(size = 8)))
}

## 1. Distribution of outcomes for each CER ----------
cer_returns_list <- returns_table_cer %>%
  lapply(function(x) x$EV) 

names(cer_returns_list) <- p_cer_names
cer_benefits <- cer_returns_list %>%
  bind_rows(.id = 'cer') %>%
  fcn_normalise_benefits()
cer_benefits_long <- cer_benefits %>% 
  pivot_longer(p_cer_names)

distribution_lims <- c(min(cer_benefits_long$value)-10, max(cer_benefits_long$value)+1)

cer_benefits_dist_plot <- cer_benefits_long %>%
  ggplot(aes(x = value, color = name, fill = name)) +
  geom_density(alpha = 0.3) +
  scale_color_manual(values = p_cer_colors) +
  scale_fill_manual(values = p_cer_colors) +
  ggpubr::theme_pubr() +
  coord_cartesian(xlim = distribution_lims, expand = F) +
  theme_void() +
  common_guides("NPV distribution") +
  theme(axis.line.x = element_line())
cer_benefits_dist_plot

cer_summary <- cer_benefits_long %>%
  group_by(name) %>%
  summarise(min = min(value), lb = quantile(value, 0.25), mean = mean(value), ub = quantile(value, 0.75), max = max(value))
cer_benefits_dist_boxwhisk_plot <- cer_summary %>%
  mutate(name_num = as.numeric(as.factor(name))) %>%
  ggplot(aes(y = name_num, color = name, fill = name)) +
  geom_errorbarh(aes(xmin = min, xmax = max, y = name_num), linewidth = 1, height = .4) +
  geom_rect(aes(xmin = lb, xmax = ub, ymin = name_num-0.4, ymax = name_num+0.4)) +
  geom_segment(aes(x = mean, xend = mean, y = name_num-0.45, yend = name_num+0.45), linewidth = 0.75, color = 'white') +
  geom_text(aes(label = name, x = min(distribution_lims), y = name_num), hjust = 0) +
  scale_fill_manual(values = p_cer_colors) +
  scale_color_manual(values = p_cer_colors) +
  ggpubr::theme_pubr() +
  coord_cartesian(xlim = distribution_lims, expand = F) +
  guides(color = 'none', fill = 'none') +
  theme_void() +
  theme(axis.line.x = element_blank())
cer_benefits_dist_boxwhisk_plot

## 2. Jitter distribution -------
ev_returns <- returns_table$EV %>%
  fcn_normalise_benefits()
ra_returns <- returns_table$CVaR %>%
  fcn_normalise_benefits()

at_risk_threshold <- -10

run_index <- returns_table$run_index %>% substr(0,5) %>% as.numeric()
which(run_index %in% CER, arr.ind = T)
rs_returns_point <- data.frame(CER = c('NH', 'ME', 'HE'), P.EV = ev_returns[CER], P.RA = ra_returns[CER])
jitter_df <- data.frame(EV = ev_returns, RA = ra_returns, 
                        clim_scen_string = returns_table$clim_scen_string) %>%
  pivot_longer(c('EV', 'RA'), names_to = 'model', values_to = 'benefits') %>%
  mutate(model = factor(model, c('EV', 'RA'), c('P-EV', 'P-RA'))) %>%
  mutate(at_risk = benefits < -10)
mean_group <- jitter_df %>%
  filter(at_risk) %>%
  group_by(model) %>%
  summarise(mean = mean(benefits), min = min(benefits), max = max(benefits), group = 'group')
set.seed(100)
rs_returns_point_long <- rs_returns_point %>%
  pivot_longer(c('P.EV','P.RA'), names_to = 'model', values_to = 'benefits') %>%
  mutate(model = factor(model, c('P.EV','P.RA'), c('P-EV','P-RA')))

prob_risk <- jitter_df %>%
  group_by(model) %>%
  summarise(prob = sum(at_risk) / n())

jitter_sigmoid <- jitter_df %>%
  group_by(clim_scen_string) %>% 
  slice_sample(n=400) %>%
  ggplot(aes(y = fct_rev(model), x = benefits)) +
  ggnewscale::new_scale_color()+
  geom_vline(aes(xintercept = at_risk_threshold), color = 'gray70', linewidth = .5) +
  geom_jitter(aes(color = at_risk), height = 0.25, size = .5)+
  scale_color_manual(values = c('gray50','#DC3220'), guide = 'none') +
  ggnewscale::new_scale_color()+
  #geom_label(data = rs_returns_point_long, aes(y = model, label = CER, color = CER), nudge_y = -0.1) +
  geom_point(data = rs_returns_point_long, color = 'white', size = 4) +
  geom_bump(data = rs_returns_point_long, aes(group = CER), color = 'white', direction = 'y', size = 2) +
  geom_bump(data = rs_returns_point_long, aes(color = CER, group = CER), direction = 'y', size = 1) +
  geom_point(data = rs_returns_point_long, aes(color = CER), size = 3) +
  geom_label(data = rs_returns_point_long %>% group_by(CER) %>% summarise(benefits = mean(benefits)), 
            aes(label = CER, color = CER, x = benefits, y = 1.5), hjust = 1, nudge_x = -2) +
  scale_color_manual("CER", values = cer_colors) +
  guides(color = 'none', fill = 'none')+
  ggnewscale::new_scale_color()+
  #geom_jitter(aes(color = at_risk), height = 0.15)+
  #geom_segment(data = mean_group, 
  #             aes(y = fct_rev(model), yend = fct_rev(model), x = min, xend = max),
  #             color = '#E64B35FF',
  #             size = 1,
  #             position = position_nudge(y = 0.2)) +
  #geom_point(data = mean_group, aes(y = fct_rev(model), x = mean), color = '#E64B35FF', size = 4,
  #           position = position_nudge(y = 0.2), shape = 15) +
  #geom_bump(data = mean_group, aes(y = as.numeric(fct_rev(model))+0.2, x = mean, group = group), 
  #          color = '#E64B35FF', direction = 'y', size = 1) +
  #geom_label(data = mean_group, aes(label = paste0(round(mean, 1), 'B'), x = mean),color = '#E64B35FF', nudge_y = 0.4) +
  scale_color_manual(values = c('gray60','#f22a18'), guide = 'none')+
  coord_cartesian(xlim = distribution_lims, expand = 0, ylim = c(0.73, 2.27))+
  geom_text(data=prob_risk, aes(y = fct_rev(model), 
                                label = paste0(model, ": ", round(prob*100,1), "%")), x = min(distribution_lims), color = '#f22a18', hjust = 0) +
  guides(color = guide_legend(title.position = "top", keyheight = 0.5, title.hjust = 0.5, label.position = "bottom", 
                              label.theme = element_text(size = 6), title.theme = element_text(size = 8))) +
  theme_void()
jitter_sigmoid

## Histogram of returns ------

dist_p_ev <- jitter_df %>%
  filter(model == 'P-EV') %>%
  ggplot(aes(x = benefits, fill = at_risk, color = at_risk)) +
  geom_histogram(binwidth = 2) +
  geom_vline(xintercept = mean(jitter_df[jitter_df$model == 'P-EV',]$benefits), linewidth = 0.75, color = 'white') +
  #geom_segment(data = rs_returns_point, aes(x = P.NH, y = 50, xend = P.NH, yend = 0, fill = NA)) +
  #geom_point(data = rs_returns_point, aes(x = P.NH, y = 0, fill = NA)) +
  #geom_label(data = rs_returns_point, aes(x = P.NH, y = 50, label = CER), fill = 'white') +
  theme_void() +
  scale_fill_manual(values = c('gray60','#F39B7FFF'), guide = 'none') +
  scale_color_manual(values = c('gray60','#F39B7FFF'), guide = 'none') +
  coord_cartesian(xlim = distribution_lims, expand = F)+
  theme(axis.line.x = element_line())

ev_benefits <- filter(jitter_df, model == 'P-EV')$benefits
fcn_plot_shaded_density <- function(dist, risk_thres = at_risk_threshold, vlines = c(), cols = c(), title = "") {
  dens <- density(dist)
  data <- tibble(x = dens$x, y = dens$y) %>% 
    mutate(at_risk = x < risk_thres) 
  vline_df <- data.frame(vlines = c(vlines, mean(dist)), cols = c(cols, 'white'))
  vline_df$dens = lapply(vline_df$vlines, function(x) dens$y[which.min(abs(dens$x-x))])
  ggplot(data, aes(x, y)) + 
    geom_errorbarh(aes(xmin = min(dist), xmax = max(dist), y = max(dens$y)/3), height = 8e-3, 
                   color = 'gray50', linewidth = 1) +
    geom_area(fill = 'gray50') +
    geom_area(data = filter(data, at_risk), fill = '#f22a18') + 
    geom_segment(data = vline_df, aes(x = vlines, xend = vlines, y = 0, yend = as.numeric(dens), color = I(cols)), linewidth = 0.5) +
    geom_line(color = 'white', linewidth = 0.5) +
    annotate('text', label = title, x = min(distribution_lims), y = max(dens$y), hjust = 0, vjust = 1) +
    theme_void() +
    coord_cartesian(xlim = distribution_lims, ylim = c(0, max(dens$y+1e-3)), expand = F)+
    theme(axis.line.x = element_line())
}

dist_ra <- jitter_df %>%
  filter(model == 'P-RA') %>%
  ggplot(aes(x = benefits, fill = at_risk)) +
  geom_histogram(binwidth = 2) +
  geom_vline(xintercept = mean(jitter_df[jitter_df$model == 'P-RA',]$benefits), linewidth = 0.75, color = 'white') +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = c('gray60','#F39B7FFF'), guide = 'none') +
  scale_color_manual(values = c('gray60','#F39B7FFF'), guide = 'none') +
  coord_cartesian(xlim = distribution_lims, expand = 0) +
  scale_x_continuous('NPV (£)', labels = scales::unit_format(suffix = 'B', scale = 1))+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), axis.line.y = element_blank())
dist_ra

dist_p_ev <- fcn_plot_shaded_density(filter(jitter_df, model == 'P-EV')$benefits, vlines = c(), cols = p_cer_colors, title = "P-EV")
dist_p_ra <- fcn_plot_shaded_density(filter(jitter_df, model == 'P-RA')$benefits, vlines = c(), cols = p_cer_colors, title = "P-RA") + 
  scale_x_continuous('NPV (£)', labels = scales::unit_format(suffix = 'B', scale = 1)) + 
  ggpubr::theme_pubr() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), axis.line.y = element_blank())
## 3. Plot planting maps -----------
source("./plot_scripts/gridnet_init.R")
decision_table <- read_csv(paste0('output/tables/oc_decision_table_mix.csv'))
EV_solution <- decision_table %>% mutate(hectares_planted = hectares * EV)
CVaR_solution <- decision_table %>% mutate(hectares_planted = hectares * CVaR)
selected_solutions <- list(`P-EV` = EV_solution, `P-RA` = CVaR_solution)
he_ra_maps <- selected_solutions %>%
  fcn_plot_planting_mix(facet_dir = 'row') +
  common_guides("Species") +
  theme(strip.text.y.left = element_text(angle = 0))

selected_maps <- selected_solutions %>%
  lapply(fcn_plot_planting)
selected_maps_labelled <- seq_along(selected_maps) %>%
  lapply(function(i){
    plt <- selected_maps[[i]] +
      annotate("text",  x=Inf, y = Inf, label = names(selected_maps)[i], vjust=0.99, hjust=0.99) +
      theme(legend.position = 'none', legend.direction = 'vertical', plot.margin = unit(c(0,0,0,0), 'cm'))
    wrap_elements(full = plt, clip = F)
  })
names(selected_maps_labelled) <- names(selected_maps)

## 4. Covariance matrix -----
ev_cov_mat <- read_csv(paste0('data/ev_cov_mat.csv'), col_names = F)
cvar_cov_mat <- read_csv(paste0('data/cvar_cov_mat.csv'), col_names = F)

colnames(ev_cov_mat) <- c('Conifers', 'Broadleaf')
ev_cov_mat$species <- c('Conifers', 'Broadleaf')
colnames(cvar_cov_mat) <- c('Conifers', 'Broadleaf')
cvar_cov_mat$species <- c('Conifers', 'Broadleaf')
ev_cvar_cov_mat <- list(`P-EV` = ev_cov_mat, `P-RA` = cvar_cov_mat) %>%
  bind_rows(.id = 'strategy')

cov_mat_plot <- ev_cvar_cov_mat %>%
  pivot_longer(c('Conifers', 'Broadleaf')) %>%
  mutate(species = factor(species, c('Conifers', 'Broadleaf'), c('C','B')),
         name = factor(name, c('Conifers', 'Broadleaf'), c('C','B'))) %>%
  ggplot(aes(x = species, y = name)) +
  geom_tile(aes(fill = value), color = 'black') +
  geom_text(aes(label = round(value/1e18,1),color = value > 1.41e19)) +
  facet_wrap(~strategy, nrow = 2) +
  scale_fill_viridis_b(option = 'magma', trans = 'log10', direction = -1) +
  scale_color_manual(guide = 'none', values = c("black", "white")) +
  scale_y_discrete(limits = c('C', 'B')) +
  scale_x_discrete(limits = c('B', 'C')) +
  ggpubr::theme_pubr() +
  guides(fill = 'none', colour = 'none') +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        axis.title = element_blank())
cov_mat_plot

ev_cov_sums <- read_csv(paste0('output/tables/ev_cost_sums.csv'), col_names = F)
cvar_cov_sums <- read_csv(paste0('output/tables/cvar_cost_sums.csv'), col_names = F)

colnames(ev_cov_sums) <- c("C", "B")
colnames(cvar_cov_sums) <- c("C", "B")

fcn_npv_scatter <- function(df) {
  cov_mat <- cov(df / 1e9) %>% as.data.frame()
  colnames(cov_mat) <- c("Conifers", "Broadleaf")
  cov_mat$species <- c("Conifers", "Broadleaf")
  
  cov_mat_plot <- cov_mat %>%
    pivot_longer(c('Conifers', 'Broadleaf')) %>%
    mutate(species = factor(species, c('Conifers', 'Broadleaf'), c('C','B')),
           name = factor(name, c('Conifers', 'Broadleaf'), c('C','B'))) %>%
    ggplot(aes(x = species, y = name)) +
    geom_tile(aes(fill = value, alpha = value), color = 'black', fill = 'gray50') +
    geom_text(aes(label = round(value,1), color = value > 25)) +
    scale_color_manual(guide = 'none', values = c("black", "white")) +
    scale_alpha_continuous(limits = c(0,50)) +
    scale_y_discrete(limits = c('C', 'B')) +
    scale_x_discrete(limits = c('B', 'C')) +
    ggpubr::theme_pubr() +
    guides(fill = 'none', colour = 'none', alpha = 'none') +
    coord_fixed() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          axis.title = element_blank())
  
  best_fit_lm <- lm(B ~ C, data = df)
  
  scatter <- df %>%
    ggplot(aes(x = C, y = B)) +
    geom_vline(xintercept = 0, color = 'gray80') +
    geom_hline(yintercept = 0, color = 'gray80') +
    geom_point(alpha = 0.3, color = 'gray50', shape = 20) +
    geom_abline(slope = best_fit_lm$coefficients[['C']], intercept = best_fit_lm$coefficients[['(Intercept)']], color = "#D55E00") +
    theme_pubr() +
    coord_fixed(ylim = c(-2e10, 3e10), xlim = c(-2e10, 3e10)) +
    scale_x_continuous("NPV: all conifers (£)", labels = scales::unit_format(suffix = 'B', scale = 1e-9), n.breaks = 3) +
    scale_y_continuous("NPV: all broadleaves (£)", labels = scales::unit_format(suffix = 'B', scale = 1e-9), n.breaks = 3)
  
  scatter | cov_mat_plot
}

npv_scatter_cvar <- fcn_npv_scatter(cvar_cov_sums)
npv_scatter_ev <- fcn_npv_scatter(ev_cov_sums)

layout_design <- "
A##I
BEGG
CEGG
CFHH
DFHH"

jitter_distribution_plot <- cer_benefits_dist_boxwhisk_plot + dist_p_ev + 
  jitter_sigmoid + dist_p_ra + selected_maps_labelled$`P-EV` + selected_maps_labelled$`P-RA` + 
  npv_scatter_ev + npv_scatter_cvar + wrap_elements(cowplot::get_legend(selected_maps$`P-EV`)) +
  plot_layout(design = layout_design, heights = c(1,1,1,1,1), widths = c(2.5,1.5,1.3,0.6), guides = 'collect') +
  plot_annotation(tag_levels = list(c('a','b','c','d','e','f','g','h','i','j'))) & 
  theme(legend.position = 'bottom', plot.tag = element_text(face = 'bold'))
ggsave('output/figures/fig2_jitter_distribution.png', jitter_distribution_plot, width = 3500, height = 2000, units = 'px')
ggsave('output/figures/fig2_jitter_distribution.pdf', jitter_distribution_plot, width = 3500, height = 2000, units = 'px')


dev.off()
