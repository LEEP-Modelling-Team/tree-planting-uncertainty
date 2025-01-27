## Supplementary data Figure 1: Planting maps for maximising EV and CVaR across the four RCPs

source("./plot_scripts/gridnet_init.R")
source('./plot_scripts/helpers.R')

rcp_scen <- c(rcp26 = "RCP2.6", "rcp45"= "RCP4.5", "rcp60" = "RCP6.0", "rcp85" = "RCP8.5")
decision_vars <- c("EV", "CVaR")

rcp <- rcp_scen[1]
decision_var <- decision_vars[1]

fcn_plot_planting_bar_rcp <- function(rcp, decision_var = "EV") {
  decision_table <- read_csv(sprintf("output/tables/oc_decision_table_rcp_run_%s.csv", rcp))
  returns_table <- read_csv(sprintf("output/tables/oc_returns_table_rcp_run_%s.csv", rcp))
  fcn_plot_planting_bar(decision_table, decision_var, returns_table, rcp_scen[[rcp]])
}

ev_solutions <- purrr::map(names(rcp_scen), fcn_plot_planting_bar_rcp, decision_var = "EV")
names(ev_solutions) <- rcp_scen

ev_plts <- wrap_plots(ev_solutions, nrow = 1) + 
  plot_layout(guides = 'collect', nrow = 1) +
  plot_annotation(
    title = "Expected Value (EV) planting strategies that assume one RCP"
  )
ggsave("output/figures/sdfig1a_ev_plts.png",ev_plts, height = 15, width = 35, units = 'cm')


cvar_solutions <- purrr::map(names(rcp_scen), fcn_plot_planting_bar_rcp, decision_var = "CVaR")
names(cvar_solutions) <- rcp_scen

cvar_plts <- wrap_plots(cvar_solutions, nrow = 1) + 
  plot_layout(guides = 'collect', nrow = 1) +
  plot_annotation(
    title = "Risk-averse (RA) planting strategies that assume one RCP"
  )
ggsave("output/figures/sdfig1b_cvar_plts.png",cvar_plts, height = 15, width = 35, units = 'cm')

ggsave("output/figures/sfig3_ev_cvar_rcp_plt.pdf", ev_plts / cvar_plts, height = 30, width = 35, units = 'cm')
