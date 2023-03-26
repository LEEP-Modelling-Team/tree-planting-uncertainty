function [cost, A, b, ghg, new2kid_subset, hectares_subset, p, cost_full, ghg_full] = ...
    fcn_optim_constraints(optim_object, get_cost)
% FCN_OPTIM_CONSTRAINTS
% Load optimisation constraints given an optim_object from
% fcn_load_mc_output for mixed planting
% First half rows for Sitka Spruce, and second half for Pedunculate Oak

if ~exist('get_cost', 'var')
    get_cost = true;
end

hectares        = optim_object.hectares;
planting_target = optim_object.planting_target;
ghg_target_strict=optim_object.ghg_target_strict;
new2kid         = optim_object.new2kid;
subset_cells    = optim_object.subset_cells;
subset_scenarios= optim_object.p > 0;
p = optim_object.p(optim_object.p > 0);

if get_cost
% Optimise planting locations allowing mix of planting species
if strcmp(optim_object.cost_type, 'opp_cost')
    cost_SS         = optim_object.opp_cost_SS';
    cost_POK        = optim_object.opp_cost_POK';
elseif strcmp(optim_object.cost_type, 'no_foregone_ag_ghg_value')
    cost_SS         = optim_object.cost_carbon_forestry_SS';
    cost_POK        = optim_object.cost_carbon_forestry_POK';
else
    cost_SS         = optim_object.cost_carbon_SS';
    cost_POK        = optim_object.cost_carbon_POK';
end

cost_SS_subset = cost_SS(subset_scenarios,subset_cells); % Remove scenarios with p=0
cost_POK_subset = cost_POK(subset_scenarios,subset_cells);
cost = [cost_SS_subset, cost_POK_subset];
cost_full = [cost_SS(:,subset_cells), cost_POK(:, subset_cells)];
clear cost_SS cost_POK
else
    cost = [];
end

ghg_SS_subset = optim_object.ghg_total_qnt_SS(subset_cells,subset_scenarios)';
ghg_POK_subset = optim_object.ghg_total_qnt_POK(subset_cells,subset_scenarios)';

ghg = [ghg_SS_subset, ghg_POK_subset];
ghg_full = [optim_object.ghg_total_qnt_SS(subset_cells,:)',...
    optim_object.ghg_total_qnt_POK(subset_cells,:)'];

hectares_subset = hectares(subset_cells)';
new2kid_subset = new2kid(subset_cells);
if planting_target>0
    A = sparse([repmat(hectares_subset, 1, 2); -repmat(hectares_subset, 1, 2); [speye(length(hectares_subset)), speye(length(hectares_subset))]]);
    b = [planting_target                     ; -planting_target; ones(length(hectares_subset), 1)];
else
    A = sparse([speye(length(hectares_subset)), speye(length(hectares_subset))]);
    b = ones(length(hectares_subset), 1);
end

if ghg_target_strict>0
    if (planting_target>0)
        warning('GHG target may be infeasible under strict planting target');
    end
    A = [A; -mean(ghg_full,1); mean(ghg_full,1)];
    b = [b; -ghg_target_strict; ghg_target_strict];
end

end