library(sf)
library(tidyverse)

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

fcn_plot_planting_mix <- function(df_list, facet_dir = 'row') {
  df_optim <- df_list %>%
    bind_rows(.id = "column_label") %>%
    mutate(column_label = factor(column_label, levels = names(df_list)))
  
  df <- right_join(gridnet, df_optim, by = 'new2kid') %>%
    filter(hectares_planted > 0) %>%
    mutate(species = factor(species, c('POK', 'SS'), c('Broadleaf', 'Conifer')))
  
  plot <- ggplot() +
    geom_sf(data = gb_border, color = "gray20", fill = "#f7fcb94D", lwd = 0.2, inherit.aes = FALSE) +
    geom_sf(data = df, aes(fill = species, alpha = log(hectares_planted+1)), lwd = 0, color = NA) +
    scale_fill_manual(values = c("#7876B1FF", "#20854EFF")) +
    scale_alpha_continuous(guide = "none", range = c(0,1))+
    labs(fill = 'Species')+
    theme_void()
  
  if (facet_dir == 'row') {
    plot <- plot + facet_grid(rows = vars(column_label), switch = 'y')
  } else {
    plot <- plot + facet_grid(cols = vars(column_label)) + theme(strip.text.y.left = element_text(angle = 0))
  }
  plot
}

