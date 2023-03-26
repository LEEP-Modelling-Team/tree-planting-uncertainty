library(tidyverse)
library(future.apply)
library(patchwork)
library(sf)

# Parameters ------
source('../scripts/helpers.R')
rcp <- "rcp60"
dir <- paste0("../../output/",date_string,"_decision_table/")
output_dir <- paste0("../output/",date_string,"/")
file_loc <- paste0("../../mc_data/",date_string,"/representative_scenarios/")
hex_code <- read_csv('../../mc_run/prices/hex_code_transparency.csv')
a1_landuse_chg <- read_csv(paste0(file_loc, '../tables/a1_landuse_chg.csv')) %>%
  mutate(hectares = wood_ha_chg) %>%
  select(new2kid, hectares)

# Start of code ------


fcn_percentile_rank_sites_plot <- function(df_list, n_hectares = 750000, 
                                      pct_limits = c(90,100)) {
  fcn_percentile_rank_sites <- function(df) {
    df_best <- df %>%
      left_join(a1_landuse_chg, by = 'new2kid') %>%
      filter(hectares > 1) %>%
      filter(new2kid %in% subset_cells) %>%
      mutate(benefits_perha_SS = -cost_carbon_forestry_SS / hectares) %>%
      mutate(benefits_perha_POK = -cost_carbon_forestry_POK / hectares) %>%
      mutate(species_pref = ifelse(benefits_perha_SS > benefits_perha_POK, 'SS', 'POK')) %>%
      pivot_longer(c('benefits_perha_SS', 'benefits_perha_POK'), names_to = 'species', values_to = 'benefits_perha') %>%
      mutate(species = ifelse(species == 'benefits_perha_SS', 'SS', 'POK')) %>%
      mutate(benefits_perha = ifelse(species == species_pref, benefits_perha, NA)) %>%
      group_by(species) %>%
      arrange(-benefits_perha) %>%
      mutate(cum_hectares = cumsum(hectares)) %>%
      mutate(benefits_best = ifelse(cum_hectares < n_hectares, benefits_perha, NA)) %>%
      ungroup() %>%
      mutate(benefits_perha_rank = rank(benefits_perha, na.last=FALSE)) %>%
      mutate(percentile = 100 * benefits_perha_rank / max(benefits_perha_rank, na.rm = TRUE)) %>%
      mutate(percentile = ifelse(benefits_best, percentile, NA)) %>%
      arrange(new2kid)
  }
  
  df_comb <- df_list %>%
    lapply(fcn_percentile_rank_sites) %>%
    bind_rows(.id = "column_label")
  
  df_comb <- gridnet %>%
    right_join(df_comb)
  
  df_plot <- df_comb %>%
    filter(!is.na(percentile))
  
  df_none <- df_comb %>%
    filter(is.na(percentile))
  
    best_loc <- ggplot() +
      geom_sf(data = df_none, fill = '#f7fcb94D', lwd = 0, color = NA) +
      geom_sf(data = df_plot, aes(fill = percentile), lwd = 0, color = NA) +
      #scale_color_viridis_b(option = "plasma", direction = -1, na.value = '#f7fcb94D', limits = pct_limits, oob = scales::squish) +
      scale_fill_viridis_b("Percentile", option = "plasma", direction = -1, na.value = '#f7fcb94D', limits = pct_limits, oob = scales::squish) +
      facet_grid(rows = vars(column_label), cols = vars(species)) +
      map_theme()
    best_loc
}

fcn_value_surface <- function(df) {
  df_out <- df %>%
    left_join(a1_landuse_chg, by = 'new2kid') %>%
    filter(hectares > 1) %>%
    filter(new2kid %in% subset_cells) %>%
    pivot_longer(c('cost_carbon_forestry_SS', 'cost_carbon_forestry_POK'), names_to = 'species', 
                 values_to = 'cost_carbon_forestry', names_prefix = "cost_carbon_forestry_") %>%
    mutate(benefits = fcn_normalise_benefits(cost_carbon_forestry,
                                             perha = F, billions = F, years = 30,
                                             annualise = F))
  benefits_ecdf <- ecdf(df_out$benefits / df_out$hectares)
  df_out$benefits_qt <- benefits_ecdf(df_out$benefits / df_out$hectares)
  df_out
}

fcn_spatial_plot_var <- function(df_list, lapply_fun, var_name = 'benefits', var_label = "", qt = 0, midpoint = 0,
                                 plot_limits = c(.85, 1)) {
  df_comb <- df_list %>%
    lapply(lapply_fun) %>%
    bind_rows(.id = "column_label")
  
  var_vec <- df_comb[,var_name]
  var_qt <- quantile(var_vec, qt, na.rm = T)
  var_filter <- !is.na(!!df_comb[,var_name]) #& between(var_vec[[1]], var_qt, max(var_vec, na.rm = T))
  
  df_plot <- df_comb %>%
    filter(var_filter)
  
  df_none <- df_comb %>%
    filter(!var_filter)
  
  df_plot <- gridnet %>%
    right_join(df_plot, by='new2kid')
  df_none <- gridnet %>%
    right_join(df_none, by='new2kid')
  
  if (var_name == 'benefits') {
    scaler <- scale_fill_gradient2(var_label, na.value = '#f7fcb94D', midpoint = 0,
                                   labels = scales::label_number(suffix = " M", scale = 1e-6),
                                   limits = c(-quantile(var_vec, qt, na.rm=T), quantile(var_vec, qt, na.rm=T)),
                                   oob = scales::squish,
                                   low = ggsci::pal_npg()(10)[5],
                                   high = ggsci::pal_npg()(10)[4],
                                   mid = "#f7fcb94D") 
  } else {
    scaler <- scale_fill_viridis_c(var_label, 
                                   direction = -1, 
                                   limits = plot_limits, oob = scales::censor, 
                                   na.value = '#f7fcb94D')
  }
  
  plot <- ggplot() +
    geom_sf(data = df_none, fill = '#f7fcb94D', lwd = 0, color = NA) +
    geom_sf(data = df_plot, aes_string(fill = var_name), lwd = 0, color = NA) +
    #scale_color_viridis_b(option = "plasma", direction = -1, na.value = '#f7fcb94D', limits = pct_limits, oob = scales::squish) +
    scaler +
    facet_grid(rows = vars(column_label), cols = vars(species)) +
    map_theme()
  plot
}

fcn_plot_opp_cost <- function(df_list, var_name = 'farm_profit_ann') {
  df_comb <- df_list %>%
    bind_rows(.id = "column_label")
  df_plot <- gridnet %>%
    right_join(df_comb, by='new2kid')
  df_none <- gridnet %>%
    filter(!(new2kid %in% df_comb$new2kid))
  scaler <- scale_fill_viridis_c(option = 'mako', direction = -1, limits = c(500, max(df_comb[var_name])))
  plot <- ggplot() +
    geom_sf(data = df_none, fill = '#f7fcb94D', lwd = 0, color = NA) +
    geom_sf(data = df_plot, aes_string(fill = var_name), lwd = 0, color = NA) +
    #scale_color_viridis_b(option = "plasma", direction = -1, na.value = '#f7fcb94D', limits = pct_limits, oob = scales::squish) +
    scaler +
    facet_grid(rows = vars(column_label)) +
    map_theme()
  plot
}

fcn_optimal_scenario_planting <- function(df, n_hectares = 750000) {
  # Optimisation of planting mix given a a1 df with cost_carbon_forestry_SS and cost_carbon_forestry_POK
  df_optim <- df %>%
    left_join(a1_landuse_chg, by = 'new2kid') %>%
    #filter(hectares > 1) %>%
    filter(new2kid %in% subset_cells) %>%
    mutate(SS = -cost_carbon_forestry_SS / hectares) %>%
    mutate(POK = -cost_carbon_forestry_POK / hectares) %>%
    mutate(species_choice = ifelse(POK > SS, 'POK', 'SS')) %>%
    pivot_longer(c('SS', 'POK'), names_to = 'species', values_to = 'benefits_perha') %>%
    filter(species_choice == species) 
  
  if (n_hectares > 0) {
    df_optim <- df_optim %>% 
      arrange(-benefits_perha) %>%
      mutate(cum_hectares = cumsum(hectares)) %>%
      mutate(hectares_planted = ifelse(cum_hectares < n_hectares, hectares, 0)) %>%
      arrange(new2kid)
  } else {
    df_optim <- df_optim %>%
      mutate(hectares_planted = ifelse(benefits_perha > 0, hectares, 0))
  }
  return(df_optim)
}

fcn_plot_planting_mix <- function(df_list, facet_dir = 'row') {
  df_optim <- df_list %>%
    bind_rows(.id = "column_label") %>%
    mutate(column_label = factor(column_label, levels = names(df_list)))
  
  # Plots optimal planting mix given a data frame with columns 'species' ('SS/POK') and 'hectares_planted' (number of hectares planted)
  fcn_get_hex <- function(percent, min_percent = 0) {
    hex_code_string <- hex_code$hex
    percent_rescaled <- min_percent + percent * (100-min_percent) / 100
    return(as.character(hex_code_string[101-ceiling(percent_rescaled)]))
  }

  df_none <- right_join(gridnet, df_optim, by = 'new2kid', multiple = "all") %>%
    filter(hectares_planted == 0) %>%
    mutate(column_label = factor(column_label, levels = names(df_list), labels = names(df_list)))
  
  df_plot <- right_join(gridnet, df_optim, by = 'new2kid', multiple = "all") %>%
    filter(hectares_planted > 0) %>%
    mutate(species = factor(species, levels = c('SS', 'POK'), labels = c('Conifer', 'Broadleaf'))) %>%
    mutate(column_label = factor(column_label, levels = names(df_list), labels = names(df_list)))
  
  if (facet_dir == 'row') {
    facetter <- facet_grid(rows = vars(column_label))
  } else {
    facetter <- facet_grid(cols = vars(column_label))
  }
  
  plot <- ggplot() +
    geom_sf(data = df_none, lwd = 0, color = NA, fill="#f7fcb94D", alpha = 1) +
    geom_sf(data = df_plot, lwd = 0, color = NA, fill='white') +
    geom_sf(data = df_plot, aes(fill = species, alpha = hectares_planted), lwd = 0, color = NA) +
    scale_fill_manual(values = c(ggsci::pal_npg()(4)[3], ggsci::pal_npg()(4)[1]))+
    scale_alpha_continuous(guide = "none", breaks = c(50,100), range = c(0.5,1))+
    facetter +
    labs(fill = 'Species')+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                                   label.position = "bottom", label.theme = element_text(size = 6), 
                                  title.theme = element_text(size = 8),
                                  barheight = 0.5)) +
    map_theme(continuous_guide = F)
  plot
}

fcn_plot_average_climate <- function(df_list, clim_var = 'temp') {

  df_comb <- df_list %>%
    bind_rows(.id = "column_label")
  
  meantemp <- read_csv('/mnt/d/NEV/Climate Data/baseline/monthly_meantemp_2km.csv')
  rainfall <- read_csv('/mnt/d/NEV/Climate Data/baseline/monthly_rainfall_2km.csv')
  
  historical_climate <- meantemp %>%
    left_join(dplyr::select(rainfall, new2kid, monthly_rainfall), by = 'new2kid')
  
  df_comb <- df_comb %>%
    left_join(historical_climate, by = 'new2kid') %>%
    mutate(temp_change = average_temp - monthly_meantemp,
           rain_change = average_rain - monthly_rainfall,
           temp_change_pct = (average_temp - monthly_meantemp)/monthly_meantemp,
           rain_change_pct = (average_rain - monthly_rainfall)/monthly_rainfall)
  
  new2kid_vec <- unique(decision_table$new2kid)
  
  if (clim_var == 'temp') {
  output_plot <- gridnet %>%
    right_join(df_comb, by = 'new2kid') %>%
    ggplot() +
    geom_sf(aes(fill = temp_change), lwd = 0, color = NA) +
    #scale_fill_viridis_b(direction = -1, "Temperature (°C)", option = "rocket",
    #                     oob = scales::squish, limits = c(12.5,max(df_comb$average_temp))) +
    scale_fill_distiller(direction = -1, palette = 'RdYlBu',"Temperature (°C)",
                         limits = c(-4,4), oob = scales::squish) +
    facet_grid(rows = vars(column_label)) +
    theme(legend.text = element_text(rel(.8))) +
    map_theme()
  } else {
  output_plot <- gridnet %>%
    right_join(df_comb, by = 'new2kid') %>%
    ggplot() + 
    geom_sf(aes(fill = rain_change_pct), lwd = 0, color = NA) +
    facet_grid(rows = vars(column_label)) +
    #scale_fill_viridis_b(direction = -1, "Precipitation (mm)", option = "mako", 
    #                     oob = scales::squish, limits = c(min(df_comb$average_rain),650)) +
    scale_fill_distiller(palette = 'BrBG', "Precipitation (% change)", direction = 1,
                         limits = c(-0.25,0.25), oob = scales::squish, labels = scales::label_percent()) +
    theme(legend.text = element_text(rel(.8))) +
    map_theme()
  }
  output_plot
}

fcn_plot_bar_location <- function(df_list) {
  region_keys <- read_csv('../data/region_keys.csv')
  rs <- lapply(df_list, fcn_optimal_scenario_planting)
  rs %>%
    bind_rows(.id = 'column_label') %>%
    select(new2kid, species, hectares, hectares_planted, column_label) %>%
    left_join(region_keys, by = 'new2kid') %>%
    mutate(region = ifelse(id %in% c('UKC','UKD','UKE','UKM'), 'North', 
                           ifelse(id %in% c('UKF','UKH','UKG','UKD', 'UKL'), 'Midlands',
                                  ifelse(id %in% c('UKJ','UKK','UKI'),'South','')))) %>%
    group_by(column_label, species, region) %>%
    summarise(proportion = sum(hectares_planted) / sum(hectares)) %>%
    mutate(proportion = ifelse(species == 'POK', proportion, -proportion)) %>%
    mutate(region = factor(region, levels = c('South', 'Midlands', 'North'), labels = c('S', 'M', 'N'))) %>%
    ggplot(aes(y = region, x = proportion, fill = species)) +
    geom_col(width = 0.6) +
    scale_x_continuous(limits = c(-0.33, 0.33)) +
    geom_vline(xintercept = 0) +
    geom_text(aes(label = paste0(abs(round(100*proportion,1)),'%'), 
                  x = 0.32*(proportion/abs(proportion)))) +
    geom_label(aes(label = region), x = 0, fill = 'white') +
    facet_grid(rows = vars(column_label)) +
    ggsci::scale_fill_npg() +
    theme_void() +
    theme(legend.position = 'none',
          legend.title = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text = element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
}

fcn_plot_bar_doughnut <- function(rs) {
  hsize <- 3
  rs_data <- rs %>%
    lapply(function(x) {
      total_hectares <- sum(x$hectares_planted)
      x %>% 
        group_by( species) %>%
        summarise(proportion = sum(hectares_planted)/total_hectares)
    }) %>%
    bind_rows(.id = 'column_label')
    
  rs_data %>%
    ggplot(aes(fill = species, y = proportion, x = hsize)) +
    geom_col() +
    coord_polar(theta = 'y') +
    facet_grid(rows = vars(column_label)) +
    xlim(c(0.2, hsize + 0.5)) +
    theme_void() +
    scale_fill_manual(values = c(ggsci::pal_npg()(4)[1], ggsci::pal_npg()(4)[3]))+
    geom_text(data = filter(rs_data, species == 'POK'),
              aes(label = scales::percent(proportion,accuracy = 1)), 
              x = 0, y = 0,
              size = 8) +
    scale_x_continuous(limits = c(0,5))+
    theme(legend.position = 'none',
          legend.title = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y =element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text = element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
}
