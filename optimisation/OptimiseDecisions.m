% OptimiseDecisions
% Author: Frankie Cho
% 
% Given input binary files (predictions from the NEV model generated by Monte 
% Carlo simulation), produces the following in CSV format:
% 1. Optimal planting decisions for the three "focus" CER (climate-economy realisation): P-NH, P-ME, and
% P-HE, solved to reach carbon sequestration target on average of all CERs,
% and obtains the decision_table (planting locations) and returns_table
% (NPV outcomes)
% 2. Optimal planting decisions and corresponding returns for all CERs
% 3. Optimal planting decisions and returns in the presence of a
% "risk-free" carbon removal technology, in increasing net cost (difference between cost of
% removal and carbon value)

%% Set up parameters
% Load high level LCS (to get array of new2kid)
SetPaths

load("data/high_level_lcs.mat")

parameters = struct();
parameters.carbon_price = 'custom'; 
parameters.S = 4000; % Number of MC scenarios
parameters.new2kid = high_level_lcs.new2kid;

% Cost type: 'no_foregone_ag_ghg_value' - foregone GHG emissions from
% agriculture not monetised; 'opp_cost' - cost from agriculture minus
% profits from timber only; 'with_foregone_ag_ghg_value' - foregone GHG
% emissions from agriculture also monetised
parameters.cost_type = 'no_foregone_ag_ghg_value'; 

% Load MC scenario costs from binary 
optim_object = fcn_load_mc_output(parameters);
param_table = readtable("data/param_table.csv");

%% Optimise under representative climate economy realisations
CER = [457, 2544, 3083]; % Three "focus" CERs
for cer_idx=CER
    optim_object.p = zeros(parameters.S, 1);
    optim_object.p(cer_idx) = 1;
    optim_object.run_cvar = false;
    optim_object.planting_target = 0;
    optim_object.ghg_target_strict = 12 * 1e6; %12MtCO2e per CCC
    [decision_table, returns_table] = fcn_optimise_scheme_table_mix(optim_object);
    suffix = ['_rs_', num2str(cer_idx)];
    writetable(decision_table, ['output/oc_decision_table',suffix,'.csv']);
    writetable([returns_table, param_table],  [tables_path, 'oc_returns_table',suffix,'.csv']);
end

%% Optimise for all scenarios to get EV and CVaR solutions
optim_object.planting_target = 0;
optim_object.ghg_target_strict = 12*1e6;
optim_object.p = ones(parameters.S, 1) / parameters.S;
[in_scenario_decision_table, in_scenario_returns_table, ~, ghg_table] = fcn_optimise_all_scenarios(optim_object);
writetable(in_scenario_decision_table, [tables_path, 'in_scenario_decision_table.csv']);
writetable(in_scenario_returns_table,[tables_path, 'in_scenario_returns_table.csv']);
writetable(ghg_table, [tables_path, 'in_scenario_ghg_table.csv']);

%% Get the covariance matrix of total costs for planting different species
optim_object.p = mod((1:parameters.S)',1)==0;
optim_object.p = optim_object.p / sum(optim_object.p);
optim_object.ghg_target_strict = 12 * 1e6;
optim_object.planting_target = 0;
optim_object.run_cvar = true;
optim_object.lambda = 1;
if (isfield(optim_object, 'abatement_cost'))
optim_object = rmfield(optim_object, 'abatement_cost');
end
[decision_table, returns_table] = fcn_optimise_scheme_table_mix(optim_object);
suffix = '';
writetable(decision_table, [tables_path , 'oc_decision_table_mix',suffix,'.csv']);
writetable([returns_table, param_table],  [tables_path, 'oc_returns_table_mix',suffix,'.csv']);

ev = decision_table.EV;
cvar = decision_table.CVaR;
species = [repelem("SS", size(cost,2)/2, 1); repelem("POK", size(cost,2)/2, 1)];

cst = [optim_object.cost_carbon_forestry_SS(optim_object.subset_cells,:); ...
    optim_object.cost_carbon_forestry_POK(optim_object.subset_cells,:)];
[ev_cov_mat , ev_cost_sums] = fcn_cov_mat(ev, cst);
[cvar_cov_mat , cvar_cost_sums] = fcn_cov_mat(cvar, cst);
writematrix(ev_cov_mat, [tables_path, 'ev_cov_mat.csv']);
writematrix(cvar_cov_mat, [tables_path, 'cvar_cov_mat.csv']);
writematrix(ev_cost_sums, [tables_path, 'ev_cost_sums.csv']);
writematrix(cvar_cost_sums, [tables_path, 'cvar_cost_sums.csv']);

%% Optimise with abatement costs and varying lambda

% Clear in_scenario_decision_table to free up memory
clear in_scenario_decision_table

for cst=linspace(0,100,41)
    optim_object.abatement_cost = cst;
    optim_object.ghg_target = 12 * 1e6;
    optim_object.ghg_target_strict = 0;
    for lambda=[0.5,1]
        optim_object.p = mod((1:parameters.S)',1)==0;
        optim_object.p = optim_object.p / sum(optim_object.p);
        optim_object.planting_target = 0;
        optim_object.lambda = lambda;
        optim_object.run_cvar = lambda > 0;
        [decision_table, returns_table] = fcn_optimise_scheme_table_mix(optim_object);
        if ismember('p', param_table.Properties.VariableNames)
            param_table = removevars(param_table, 'p');
        end
        suffix = ['lambda_', num2str(lambda*100), '_abatement_', ...
            num2str(optim_object.abatement_cost), '_ctarget_', num2str(optim_object.ghg_target/1e6)];
        writetable(decision_table, [tables_path, 'oc_decision_table_mix_',suffix,'.csv']);
        writetable([returns_table, param_table],  [ tables_path, 'oc_returns_table_mix_',suffix,'.csv']);
    end
    optim_object = rmfield(optim_object, 'abatement_cost');
    optim_object = rmfield(optim_object, 'ghg_target');
end

%% Optimise with abatement costs with opportunity costs only

% Clear in_scenario_decision_table to free up memory
clear in_scenario_decision_table

parameters_oc = parameters;
parameters_oc.cost_type = 'opp_cost';
optim_object_oc = fcn_load_mc_output(parameters_oc);

for cst=linspace(0,300,31)
    optim_object_oc.abatement_cost = cst;
    optim_object_oc.ghg_target = 12 * 1e6;
    optim_object_oc.ghg_target_strict = 0;
    for lambda=[0.5,1]
        optim_object_oc.p = mod((1:parameters.S)',1)==0;
        optim_object_oc.p = optim_object_oc.p / sum(optim_object_oc.p);
        optim_object_oc.planting_target = 0;
        optim_object_oc.lambda = lambda;
        optim_object_oc.run_cvar = lambda > 0;
        [decision_table, returns_table] = fcn_optimise_scheme_table_mix(optim_object_oc);
        if ismember('p', param_table.Properties.VariableNames)
            param_table = removevars(param_table, 'p');
        end
        suffix = ['lambda_', num2str(lambda*100), '_abatement_', ...
            num2str(optim_object_oc.abatement_cost), '_ctarget_', num2str(optim_object_oc.ghg_target/1e6)];
        writetable(decision_table, [tables_path, 'oc_decision_table_opp_cost_',suffix,'.csv']);
        writetable([returns_table, param_table],  [ tables_path, 'oc_returns_table_opp_cost_',suffix,'.csv']);
    end
    optim_object_oc = rmfield(optim_object_oc, 'abatement_cost');
    optim_object_oc = rmfield(optim_object_oc, 'ghg_target');
end