## Supplementary data figure 2: excluding land area in CRAFTY-GB maps

library(tools)

source('plot_scripts/gridnet_init.R')
source('plot_scripts/helpers.R')

crafty_gb <- list.files("data/new2kid_crafty_gb")
crafty_gb_scen <- crafty_gb %>% 
  file_path_sans_ext()
names(crafty_gb_scen) <- c("RCP2.6-SSP1", "RCP4.5-SSP2", "RCP4.5-SSP4", "RCP6.0-SSP3", "RCP8.5-SSP2", "RCP8.5-SSP5")
crafty_gb_new2kid <- crafty_gb %>%
  purrr::map(\(x) read_csv(paste0('data/new2kid_crafty_gb/', x)))
crafty_gb_planting <- crafty_gb_scen %>%
  purrr::map(\(x) sprintf("output/tables/oc_decision_table_crafty_%s.csv", x)) %>%
  purrr::map(read_csv)
crafty_gb_returns <- crafty_gb_scen %>%
  purrr::map(\(x) sprintf("output/tables/oc_returns_table_crafty_%s.csv", x)) %>%
  purrr::map(read_csv)

returns_table <- read_csv(paste0('output/tables/oc_returns_table_mix.csv'))

fcn_plot_planting_urban_exclude <- function(df_optim, new2kid_exclude, returns_table = NULL, clim_scen_string = NULL, plot_name = "", decision_var = "EV") {
  df_optim <- df_optim %>%
    mutate(hectares_planted := hectares * !!as.symbol(decision_var)) %>%
    filter(hectares_planted > 0)
  df <- right_join(gridnet, df_optim, by = 'new2kid') %>%
    mutate(species = factor(species, c('POK', 'SS', 'Urban'), c('Broadleaf', 'Conifer', 'Urban')))
  df_exclude <- right_join(gridnet, new2kid_exclude, by = 'new2kid') %>%
    filter(LUI_2050)
  df <- df %>%
    bind_rows(df_exclude) %>%
    mutate(species = if_else(!is.na(LUI_2050) & LUI_2050, 'Urban', species),
           hectares_planted = if_else(!is.na(LUI_2050) & LUI_2050, 400, hectares_planted))
  
  plt <- ggplot() +
    geom_sf(data = gb_border, color = "gray20", fill = "#f7fcb94D", lwd = 0.2, inherit.aes = FALSE) +
    geom_sf(data = df, aes(fill = species, alpha = hectares_planted+1), lwd = 0, color = NA) +
    scale_fill_manual(values = c("#D55E00", "#009E73", "#0072B2")) +
    scale_alpha_continuous(guide = "none", range = c(0,1), trans = "log")+
    labs(fill = 'Planting/ Land-use')+
    theme_void()
  bar <- fcn_bar_plot(df_optim)+ ylab("Area ") + theme(axis.title.y =  element_text())
  plt_out <- (plt + bar + plot_layout(ncol = 1, heights = c(0.9, 0.1))) +
    plot_annotation(title = plot_name) +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5),
          legend.direction = 'vertical')
  if (!is.null(returns_table)) {
    est <- quantile(unlist(returns_table[returns_table$clim_scen_string == clim_scen_string,decision_var]), c(0.05, 0.5, 0.95)) %>%
      fcn_normalise_benefits()
    est_df <- data.frame(median = est[2], lb = est[1], ub = est[3])
    cost_bar <- fcn_bar_lb_ub_plot(est_df)
    plt_out <- plt +
                  ggtitle(plot_name) +
                  theme(plot.title = element_text(face = 'bold', hjust = 0.5),
                        legend.direction = 'vertical') + 
                  bar  + plot_layout(ncol = 1, heights = c(0.9, 0.05))
  }
  plt_out
}

crafty_gb_maps <- purrr::pmap(list(
  df_optim = crafty_gb_planting, 
  new2kid_exclude = crafty_gb_new2kid, 
  returns_table = crafty_gb_returns,
  plot_name = names(crafty_gb_scen),
  clim_scen_string = c("rcp26", "rcp45", "rcp45", "rcp60", "rcp85", "rcp85"),
  decision_var = "EV"
), fcn_plot_planting_urban_exclude)



crafty_gb_plts <- wrap_plots(crafty_gb_maps, nrow = 1) + 
  plot_layout(guides = 'collect', nrow = 2) +
  plot_annotation(
    title = "P-EV strategy excluding projected urban land-use under CRAFTY-GB projections"
  )
ggsave("output/figures/sdfig2_crafty_gb_plts.png", crafty_gb_plts, height = 25, width = 25, units = 'cm')
ggsave("output/figures/sfig4_crafty_gb_plts.pdf", crafty_gb_plts, height = 25, width = 25, units = 'cm')


## Compute the overlap in planting maps without exclusion with future urban land development -----
source("plot_scripts/define_cer.R")
decision_table <- read_csv("output/tables/oc_decision_table_mix.csv")
decision_table_cer <- lapply(CER, function(cer) read_csv(paste0("output/tables/oc_decision_table_rs_", cer, ".csv"))) %>%
  lapply(function(x) x %>% mutate(hectares_planted = hectares * EV))
names(decision_table_cer) <- paste0("P-", cer_names)
decision_table_cer <- decision_table_cer %>%
  bind_rows(.id = "strategy") %>%
  pivot_wider(names_from = 'strategy', values_from = 'EV', id_cols = c('new2kid', 'species'))
decision_table_comb <- decision_table %>%
  left_join(decision_table_cer, by = c('new2kid', 'species'))

fcn_urban_overlap <- function(x) {
  decision_table_comb %>%
    left_join(x, by = 'new2kid') %>%
    mutate(LUI_2050 = if_else(is.na(LUI_2050), F, LUI_2050)) %>%
    group_by(LUI_2050) %>%
    summarise(
      NH = sum(`P-NH` * hectares),
      ME = sum(`P-ME` * hectares),
      HE = sum(`P-HE` * hectares),
      EV = sum(EV * hectares), 
      CVaR = sum(CVaR * hectares)) %>%
    mutate(pct_NH = NH / sum(NH), 
           pct_ME = ME / sum(ME),
           pct_HE = HE / sum(HE),
           pct_EV = EV/ sum(EV), 
           pct_CVaR = CVaR / sum(CVaR)) %>%
    mutate(`2050 Urban Land-use` = if_else(LUI_2050, 'Yes', 'No')) %>%
    mutate(
      `P-NH` = glue::glue("{round(NH)} ha ({round(pct_NH*100)}%)"),
      `P-ME` = glue::glue("{round(ME)} ha ({round(pct_ME*100)}%)"),
      `P-HE` = glue::glue("{round(HE)} ha ({round(pct_HE*100)}%)"),
      `P-EV` = glue::glue("{round(EV)} ha ({round(pct_EV*100)}%)"),
      `P-RA` = glue::glue("{round(CVaR)} ha ({round(pct_CVaR*100)}%)")) %>%
    select(`2050 Urban Land-use`, `P-NH`, `P-ME`, `P-HE`, `P-EV`, `P-RA`)
}

overlap_tables <- purrr::map(crafty_gb_new2kid, fcn_urban_overlap)
names(overlap_tables) <- names(crafty_gb_scen)
overlap_tables <- bind_rows(overlap_tables, .id = 'Scenario')

write_csv(overlap_tables, "output/figures/sdtable1_crafty_overlap.csv")

# Change in NPV relative to default EV planting strategies

crafty_gb_returns_diff <- crafty_gb_returns %>%
  map(function(x) {
    x$EV_unrestricted <- returns_table$EV
    x %>% 
      mutate(EV_diff = fcn_normalise_benefits(EV - EV_unrestricted, billions = T)) %>%
      mutate(EV_pct_diff = EV_diff / EV_unrestricted)
  }) %>%
  map(function(x) {
    data.frame(mean = mean(x$EV_diff), lb = unname(quantile(x$EV_diff, 0.05)), ub = unname(quantile(x$EV_diff, 0.95)))
  }) %>%
  bind_rows(.id = 'Scenario') %>%
  mutate(`NPV (Mean and 90% intervals, in billion £)` = glue::glue("{round(mean, 2)} ({round(lb, 2)} - {round(ub, 2)})")) %>%
  rename(`Land-use Projection` = Scenario) %>%
  select(`Land-use Projection`, `NPV (Mean and 90% intervals, in billion £)`)

write_csv(crafty_gb_returns_diff, "output/figures/sdtable2_crafty_gb_returns_diff.csv")

