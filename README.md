# Tree planting decision-making under uncertainty in Great Britain
Replication code for paper on tree planting uncertainty, supporting the findings published in "Resilient tree-planting under compounding climate and economic uncertainty", by Frankie Cho, Paolo Aglonucci, Ian Bateman, Christopher Lee, Andrew Lovett, Mattia Mancini, Chrysanthi Rapti and Brett Day.

Full data directory uploaded to [Zenodo link](https://dx.doi.org/10.5281/zenodo.14744237).

## Replication steps

1. Ensure that the data directory contains data downloaded from Zenodo.
2. Execute `OptimiseDecisions.m` in Matlab to generate decision and returns tables of the planting strategies in `output/tables`. Ensure that all scripts in the directory `optimisation` and `helpers`, including subfolders, were included in the path.
3. Open the R project `tree-planting-uncertainty.Rproj` in R (or RStudio) to plot figures. Figures are in `output/figures`
    a. Execute `fig1_bump_chart.R` to get Figure 1 in the main manuscript `fig1_bump_chart_horizontal.png`, the figure of the CERs and planting strategies obtained using scenario analysis and optimisation techniques. This script also generates `sdfig3_cer_trends.png`, which is Fig S2 of the manuscript's supporting information.
    b. Execute `fig2_jitter_distribution.R`, which generates the composite diagram in Figure 2 of the expected value and risk-averse planting strategies, and the correlation between the species' planting returns.
    c. Execute `fig3_abatement_cost.R`, which generates Figure 3 of the main manuscript of costs of the alternative greenhouse gas removal technology versus the forest-based carbon removal.
    d. Execute `sdfig_rcp_planting.R` to get planting maps for maximising EV and CVaR across the four RCPs. This corresponds to Fig S5 in the supplementary.
    e. Execute `sdfig2_crafty_gb.R` to get `sdfig2_crafty_gb_plts.png`. This corresponds to Figure S6 of the supplementary. It also generates Table S2 (from `sdtable1_crafty_overlap.csv`) and Table S3 (from `sdtabler2_crafty_gb_returns_diff.csv`).
    f. Execute `sdfig4_yield_class.R` to get yield class carbon sequestration time-series and the spatial variation of predicted yield classes across the UK under climate change. This corresponds to Figures S3 and S4 in the supplementary.
    g. Execute `sdtab1_summary_stat.R` to generate `sdtable3_summary_table.csv`, which is the data underlying Table S1 in the supplementary.

## Folder structure

### data
The Github repository only stores the shapefiles required for analysis. A full record of the data required for the analysis is available on Zenodo. This includes:

* `1-s2.0-S0959378022001200-mmc2.xlsx`: supplementary data 2 of Diagneault et al. (2022), Global Environmental Change: [link](https://www.sciencedirect.com/science/article/pii/S0959378022001200#s0115). Contains timber price projections up to 2105.
* `a1_landuse_chg.mat`: Landuse change from A1 (afforestation on arable and semi-natural grassland only); A2 afforestation (planting on arable, semi-natural grassland and permanent grassland) not examined in this study
* `climate_delta_ts.csv`: Climate variable time-series averaged across the UK, for RCP2.6, 4.5 6.0 and 8.5, four representative concentration pathways (RCPs), and temperature and precipitation. The "var" column documents the variable, and the columns represent the years.
* `climate_ts.csv`: Climate variable time-series averaged across the UK, for RCP2.6, 4.5 6.0 and 8.5, four representative concentration pathways (RCPs), and temperature and precipitation. Reshaped to a long data format, with var = "varaible", model = "climate model member", year = "year", rcp = "representative concentration pathway", and clim_scen_comb = "climate scenario combination".
* `cvar_cov_mat.csv`: covariance matrix of CVaR (P-RA) planting across species, generated from code `optimisation/fcn_cov_mat.m`
* `daigneault_timber_price_delta.csv`: Diagneault et al. (2022), Global Environmental Change: [link](https://www.sciencedirect.com/science/article/pii/S0959378022001200#s0115), processed into a tidy CSV for ingestion into the dataset.
* `ev_cov_mat.csv`: covariance matrix of EV (P-EV) planting across species, generated from code.
* `high_level_lcs.mat`: the high-level (initial) land-use in the UK for each of the 57230 SEER GRID cells, each uniquely identified by a "new2kid".
* `new2kid_crafty_gb`: a crosswalk between the new2kid in this spatial dataset and the the CRAFTY-GB dataset, and a variable indicating whether the new2kid is predicted to be urban land under the CRAFTY-GB dataset (across each RCP and SSP projection).
* `param_table.csv`: The parameter table for each of the CERs (4000 of those), with "rcp" indicating its representative concentration pathway and climate_model_member indicating the UKCP18 climate model member used. Price predictions for commodities are stored in the `price_assumptions` folder.
* `price_assumptions`: a folder containing the price projections used in the CERs. 
    * `carbon_price_assumption.csv`: time series from 2020 to 2320 (columns) for each of the 4000 CERs (rows).
    * `price_beef_assumption.csv`: 2020-2050 price projections for beef, for each of the 4000 CERs (rows).
    * `price_dairy_assumption.csv`: 2020-2050 price projections for dairy, for each of the 4000 CERs (rows).
    * `price_fert_assumption.csv`: : 2020-2050 price projections for fertiliser, for each of the 4000 CERs (rows).
    * `price_osr_assumption.csv`: : 2020-2050 price projections for oil seed rape, for each of the 4000 CERs (rows).
    * `price_pot_assumption.csv`: : 2020-2050 price projections for potato, for each of the 4000 CERs (rows).
    * `price_sb_assumption.csv`: : 2020-2050 price projections for sugar beet, for each of the 4000 CERs (rows).
    * `price_sbar_assumption.csv`: : 2020-2050 price projections for summer barley, for each of the 4000 CERs (rows).
    * `price_sheep_assumption.csv`: : 2020-2050 price projections for sheep, for each of the 4000 CERs (rows).
    * `price_timber_assumption.csv`: : 2020-2050 price projections for timber, for each of the 4000 CERs (rows).
    * `price_wbar_assumption.csv`: : 2020-2050 price projections for winter barley, for each of the 4000 CERs (rows).
    * `price_wheat_assumption.csv`: : 2020-2050 price projections for wheat, for each of the 4000 CERs (rows).
* `shapefiles`: a folder containing the following shapefiles for spatial plotting of figures:
    * `Countries__December_2017__Boundaries-shp`: boundaries of countries within the UK
    * `gb_border`: border of Great Britain
    * `SEER_GRID`: polygons of each of the SEER gridded locations, indexed by its new2kid
* `timberC`: Timber carbon
    * `TimberC_POK`: Timber carbon for pedunculate oak (POK). Each column is a yield class (1,2,3,4,5,6,7,8), and each row is the tCO2e per hectare for years starting from 2020 for 300 years
    * `TimberC_SS`: Timber carbon for Sitka spruce (SS). Each column is a yield class (1-20), and each row is the tCO2e per hectare for years starting from 2020 for 300 years.
* `bins`: the binary files produced as outputs of the National Environment Valuation (NEV) model simulations. SS denotes Sitka spruce values (conifers) and POK denote pedunculate oak (POK) values.
    * `a1_cost_carbon_forestry_SS.bin`: Sitka spruce annualised cost of tree-planting, including monetised carbon values, timber revenues, forestry management costs, and agricultural opportunity costs, in GBP. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`
    * `a1_cost_carbon_forestry_POK.bin`: pedunculate oak annualised cost of tree-planting, including monetised carbon values, timber revenues, forestry management costs, and agricultural opportunity costs, in GBP. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`
    * `a1_farm_profit_ann.bin`: annualised farm profits only (the agricultural opportunity cost) for each of the 57230 gridded locations and 4000 CERs. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`
    * `a1_forestry_ghg_ann_POK.bin`: annualised greenhouse gas sequestration/ emissions monetary value of emissions for pedunculate oak (POK) planting monetised based on its carbon value (in GBP), for the entire cell, for each of the 57230 gridded locations and 4000 CERs. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`.
    * `a1_forestry_ghg_ann_SS.bin`: annualised greenhouse gas of equestration/ emissions monetary value based on its carbon value (in GBP), for each of the 57230 gridded locations and 4000 CERs, for the entire cell. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`.
    * `a1_forestry_soil_ghg_qnt.bin`: annualised greenhouse gas (CO2 equivalent) in quantities of sequestration/ emissions of gasses in planted soil only, for each of the 57230 gridded locations and 4000 CERs, for the entire cell. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`.
    * `a1_ghg_total_qnt_POK.bin`: annualised greenhouse gas (CO2 equivalent) in quantities of sequestration/ emissions of gasses in planted soil and the tree for planting pedunculate oak (POK), for each of the 57230 gridded locations and 4000 CERs, for the entire cell. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`.
    * `a1_ghg_total_qnt_SS.bin`: annualised greenhouse gas (CO2 equivalent) in quantities of sequestration/ emissions of gasses in planted soil and the tree for planting Sitka spruce (SS), for each of the 57230 gridded locations and 4000 CERs, for the entire cell. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`.
    * `a1_cost_carbon_forestry_SS.bin`: Sitka spruce annualised cost of tree-planting, including, timber revenues, forestry management costs, and agricultural opportunity costs, but EXCLUDING monetised carbon value, in GBP. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`
    * `a1_cost_carbon_forestry_POK.bin`: pedunculate oak annualised cost of tree-planting, including timber revenues, forestry management costs, and agricultural opportunity costs, but EXCLUDING monetised carbon value, in GBP. Binary files are 57230x4000, and can be loaded into Matlab via `optimisation/fcn_load_mc_outputs.m`

### optimisation
The scripts in optimisation use mostly Matlab with a Gurobi installation.
To run the optimisation part, ensure that you specify the directory you want outputs in `SetPaths.m`. The tables carrying the optimisation decisions and returns of tree-planting will be saved in `tables_path` there. Ensure that the path to `bins_path` point to the correct directory carrying the binary files in `data/bins`.

`OptimiseDecisions.m`: The main driver script, which solves for the optimal planting decisions for the EV and CVaR (RA) planting strategies, including:

1. Optimise under each "focus" climate-economy realisations (CERs)
2. Optimise by each RCP separately
3. Exclude planting on future urban areas predicted by CRAFTY-GB (sensitivity analysis)
4. Optimise under all CERs to get the EV and CVaR (P-RA) planting strategies
5. Get the covariance matrix between planting species
6. Enable planting with an alternative greenhouse gas removal technology and vary "lambda", the risk-aversion parameter (currently only set to either 0 or 1 in the published version).
7. Enable planting with an alternative greenhouse gas removal technology, but repeat for opportunity costs (without monetised carbon values), for the comparison of risk profiles between tree planting and the alternative greenhouse gas technology.

Other functions called in `OptimiseDecisions.m` include:

`fcn_cov_mat.m`: the function to calculate the covariance matrix between planted species. Called in `OptimiseDecisions.m`

`fcn_normalise_benefits.m`: the function to convert annualised costs to total benefits. Called in `OptimiseDecisions.m`

`fcn_optim_constraints.m`: define the constraints of the optimisation, and retrieve matrices of the costs and constraints for the optimisation problem. Called in `OptimiseDecisions.m`

`fcn_optimise_all_scenarios.m`: iteratively solve for all of the optimal planting strategy across each and every one of the CERs, assuming that to be "true". Called in `OptimiseDecisions.m`

`fcn_optimise_scheme_table_mix.m`: sets up the optimisation problem to be solved using `fcn_CVaR` or `fcn_exp_cost`. Called in `OptimiseDecisions.m`.

`fcn_target_shortfall.m`: Target shortfall penalty, enabling the decision-maker to use an alternative GGR technology to substitute for tree-planting. The penalty for the shortfall is equal to the cost of the alternative GGR technology. The larger the cost, the less the decision-maker will be inclined to use the alternative GGR technology.

`optim_func/fcn_CVaR.m`: a Matlab implementation of the problem solving for the minimum conditional value-at-risk problem in a linear programming setting using Gurobi.

`optim_func/fcn_exp_cost.m`: a Matlab implementation of the problem solving for the minimum expected cost problem in a linear programming setting using Gurobi.

### plot_scripts
Scripts for plotting figures. Documented in the replication steps section.

## Computing environments
Matlab analysis done in Matlab 2022a on a Windows 10 PC with Gurobi 9.5.1.

R figure plotting completed with the following settings:

```
R version 4.4.2 (2024-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.1.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: [redacted]
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggpubr_0.6.0    ggbump_0.1.0    sf_1.0-19       patchwork_1.3.0 glue_1.8.0      lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4    
[10] purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0

loaded via a namespace (and not attached):
 [1] gtable_0.3.6       rstatix_0.7.2      tzdb_0.4.0         vctrs_0.6.5        tools_4.4.2        generics_0.1.3     parallel_4.4.2    
 [8] proxy_0.4-27       pkgconfig_2.0.3    KernSmooth_2.23-24 ggnewscale_0.5.0   lifecycle_1.0.4    compiler_4.4.2     farver_2.1.2      
[15] textshaping_0.4.1  munsell_0.5.1      carData_3.0-5      class_7.3-22       Formula_1.2-5      pillar_1.10.1      car_3.1-3         
[22] crayon_1.5.3       classInt_0.4-11    wk_0.9.4           abind_1.4-8        tidyselect_1.2.1   stringi_1.8.4      labeling_0.4.3    
[29] cowplot_1.1.3      grid_4.4.2         colorspace_2.1-1   cli_3.6.3          magrittr_2.0.3     utf8_1.2.4         broom_1.0.7       
[36] e1071_1.7-16       withr_3.0.2        scales_1.3.0       backports_1.5.0    bit64_4.6.0-1      timechange_0.3.0   bit_4.5.0.1       
[43] ggsignif_0.6.4     ragg_1.3.3         hms_1.1.3          viridisLite_0.4.2  s2_1.1.7           rlang_1.1.4        Rcpp_1.0.14       
[50] DBI_1.2.3          rstudioapi_0.17.1  vroom_1.6.5        R6_2.5.1           systemfonts_1.2.0  units_0.8-5       
```