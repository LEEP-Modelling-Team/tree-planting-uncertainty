library(sf)
library(tidyverse)
library(patchwork)

simplified_gb <- FALSE
## Constants ------------

if (!exists('gb_border')) {
  gb_border <- st_read("data/shapefiles/gb_border/gb_border.shp")
}

if (simplified_gb) {
  gb_border <- st_read("data/shapefiles/gb_border_generalised_5000/")
}

fcn_gb_bounding_box <- function(sf) {
  sf %>%
    st_transform(4326) %>%
    st_crop( xmin = -7.57216793459, ymin = 49.959999905, xmax = 1.68153079591, ymax = 58.6350001085) %>%
    st_transform(27700)
}

if (!exists('gridnet')) {
  gridnet <- st_read('./data/shapefiles/SEER_GRID', layer = 'SEER_net2km') %>% 
    st_simplify(preserveTopology = T, dTolerance = 2000) %>%
    fcn_gb_bounding_box()
}

## Functions ----------
fcn_plot_planting <- function(df_optim) {
  df <- right_join(gridnet, df_optim, by = 'new2kid') %>%
    filter(hectares_planted > 0) %>%
    mutate(species = factor(species, c('POK', 'SS'), c('Broadleaf', 'Conifer')))
  plot <- ggplot() +
    geom_sf(data = gb_border, color = "gray20", fill = "#f7fcb94D", lwd = 0.2, inherit.aes = FALSE) +
    geom_sf(data = df, aes(fill = species, alpha = hectares_planted+1), lwd = 0, color = NA) +
    scale_fill_manual(values = c("#D55E00", "#009E73")) +
    scale_alpha_continuous(guide = "none", range = c(0,1), trans = "log")+
    labs(fill = 'Species')+
    theme_void()
  plot
}

fcn_plot_planting_mix <- function(df_list, facet_dir = 'row') {
  df_optim <- df_list %>%
    bind_rows(.id = "column_label") %>%
    mutate(column_label = factor(column_label, levels = names(df_list)))
  
  df <- right_join(gridnet, df_optim, by = 'new2kid') %>%
    filter(hectares_planted > 0) %>%
    mutate(species = factor(species, c('POK', 'SS'), c('Broadleaf', 'Conifer')))
  
  plot <- ggplot() +
    geom_sf(data = gb_border, color = "gray20", fill = "#f7fcb94D", lwd = 0.2, inherit.aes = FALSE) +
    geom_sf(data = df, aes(fill = species, alpha = hectares_planted+1), lwd = 0, color = NA) +
    scale_fill_manual(values = c("#D55E00", "#009E73")) +
    scale_alpha_continuous(guide = "none", range = c(0,1), trans = "log")+
    labs(fill = 'Species')+
    theme_void()
  
  if (identical(facet_dir, 'row')) {
    plot <- plot + facet_grid(rows = vars(column_label), switch = 'y')
    return(plot)
  } else {
    plot <- plot + facet_grid(cols = vars(column_label)) + theme(strip.text.y.left = element_text(angle = 0))
    return(plot)
  }
}

fcn_bar_plot <- function(df, xmax = 2.5e6, margins = 0.1) {
  proportion <- sum(df[df$species == 'POK',]$hectares_planted) / sum(df$hectares_planted)
  residual <- xmax - sum(df$hectares_planted)
  bar_plot_df <- df %>%
    group_by(species) %>%
    summarise(hectares_planted = sum(hectares_planted)) %>%
    mutate(species = factor(species, c('POK', 'SS', 'None'), c('Broadleaf', 'Conifer', 'None'))) %>%
    mutate(y = as.factor(1)) 
  bar_plot_df <- rbind(bar_plot_df, data.frame(hectares_planted = residual, species = "None", y = 1))
  
  bar_plot <- bar_plot_df %>%
    mutate(species = rev(species)) %>%
    ggplot(aes(x = hectares_planted, y = y, fill = species)) +
    geom_col(color = 'gray') +
    scale_fill_manual(values = c("white", "#009E73", "#D55E00"), guide = 'none') +
    labs(fill = 'Species')+
    theme_void() +
    scale_x_continuous(limits = c(0, xmax+0.1), breaks = c(0, 2.5e6), labels = c('0', '2.5 Mha')) +
    coord_cartesian(expand = F) +
    #annotate('text', label = paste0(' ', round(proportion * 100, digits = 0), '%'), x = 0.01, y = 1, hjust = 0, color = 'white', size = 4)+ 
    #annotate('text', label = paste0(round(sum(df$hectares_planted)/1e6, digits = 1), 'Mha'), x = xmax, y = 1, hjust = 1, color = 'darkgray', size = 4)+ 
    theme(legend.position = "none", 
          plot.margin = unit(c(margins, margins, margins, margins), "cm"),
          axis.text.x = element_text(size = 8, hjust = c(0, 1)))
  bar_plot
}

fcn_bar_lb_ub_plot <- function(est, lb = -40, ub = 40, margins = 0.1) {
  est <- est %>%
    mutate(y = 0)
  ggplot(est, aes(x = median, y = y)) +
    annotate('rect', xmin = lb, xmax = 0, ymin = -10, ymax = 10, fill = '#e75757', alpha = 0.8) +
    annotate('rect', xmin = 0, xmax = ub, ymin = -10, ymax = 10, fill = '#79ea86', alpha = 0.8) +
    geom_vline(xintercept = 0, color = "gray", linewidth = 1) +
    geom_point() +
    geom_segment(aes(x = lb, xend = ub, y = y, yend = y)) +
    coord_cartesian(xlim = c(lb, ub), ylim = c(-1, 1), expand = F) +
    theme_void()+
    annotate('text', x = lb, y = 0, label = paste0(lb, "B"), hjust = 0, color = 'white')+
    annotate('text', x = ub, y = 0, label = paste0(ub, "B"), hjust = 1, color = 'white')+
    theme(panel.border = element_rect(fill = NA, color = 'gray'), plot.margin = unit(c(margins, margins, margins, margins), "cm"))
}

fcn_plot_planting_bar <- function(decision_table, decision_var, returns_table = NULL, plot_name='') {
  decision_table <- decision_table %>%
    mutate(hectares_planted := hectares * !!as.symbol(decision_var))
  plt <- decision_table %>% fcn_plot_planting()
  plt <- plt 
  bar_plt <- fcn_bar_plot(decision_table) + ylab("Area ") + theme(axis.title.y =  element_text())
  if (!is.null(returns_table)) {
    est <- returns_table[[decision_var]] %>%
      fcn_normalise_benefits() %>%
      quantile(c(0.05, 0.5, 0.95))
    est_df <- data.frame(median = est[2], lb = est[1], ub = est[3])
    cost_bar <- fcn_bar_lb_ub_plot(est_df)+ ylab("NPV ") + theme(axis.title.y =  element_text())
    plt + ggtitle(plot_name) +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5),
            legend.direction = 'vertical') + 
      bar_plt + 
      cost_bar + 
      plot_layout(ncol = 1, heights = c(0.9, 0.05, 0.05))
  } else {
  plt + ggtitle(plot_name) +
     theme(plot.title = element_text(face = 'bold', hjust = 0.5),
           legend.direction = 'vertical') + 
      bar_plt + 
      plot_layout(ncol = 1, heights = c(0.9, 0.1)) 
  }
}
