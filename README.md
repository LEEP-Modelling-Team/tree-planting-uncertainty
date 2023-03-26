# Tree planting decision-making under uncertainty in GB
Replication code for paper on tree planting uncertainty under the ADVANCES project intended for public access.

Inputs (stored in /data): will need to contact author for a copy of the directory because this is over 10GB
1. Binary files: full matrices of output from NEV suite Monte Carlo simulations from the distribution of uncertain parameters, for each climate-economy realisation (CER)
2. a1_landuse_chg.mat: Matlab struct describing hectares of landuse change under a tree planting scenario
3. high_level_lcs.mat: landuse cover in GB
4. param_table.csv: a table describing parameter specifications for each of the CERs


Outputs
* Tables showing optimal parcels of tree planting (decision) and NPV outcomes (returns) across climate-economy realisations for reaching a 12MtCO2e carbon sequestration target excluding displaced agricultural emissions, ran under the following settings
    * Assuming each of the three "focus" climate-economy realisations (P-NH, P-ME, and P-HE), leading to 3 planting strategies with 4,000 possible NPV outcomes
    * Assuming each of all of the climate-economy realisations (4,000), leading to 4,000 planting strategies
    * Optimising the "average" NPV outcome (EV) and optimising a risk measure of NPV outcomes (conditional value-at-risk) for only using tree-planting to reach 12MtCO2e
    * Optimising EV and CVaR in the presence of a "risk-free" carbon removal target, described by a "net cost" (difference between GGR cost and social cost of carbon)
* Figures (relying on the tables above)