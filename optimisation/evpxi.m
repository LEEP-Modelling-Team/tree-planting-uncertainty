
% Expected value of partial perfect information
% Considering N number of uncertain elements, this estimates the value of
% infromation of sequentially resolving the first, second, third and fourth
% uncertainty, so on so forth
%
% optim_object
% scenario_group_table

%% Calculate VOI of separate elements
scenario_group_table = zeros(parameters.S, 4);
clim_scen_string_cell = cell(parameters.S,1);
clim_model_string_cell = zeros(parameters.S, 1);
carbon_price_cell = zeros(parameters.S, 1); % End-of-century carbon value

param_table = readtable("data/param_table.csv");

for i=1:parameters.S
    clim_scen_string_cell{i}  = param_table.clim_scen_string{i};
    clim_model_string_cell(i) = param_table.climate_model_member(i);
    carbon_price_cell(i) = param_table.carbon_price_path(i);
end
fcn_to_category = @(x) grp2idx(categorical(x));
scenario_group_table(:,1) = fcn_to_category(clim_scen_string_cell);
scenario_group_table(:,2) = fcn_to_category(clim_model_string_cell);
[~, ~, carbon_price_unique_id] = unique(carbon_price_cell);
scenario_group_table(:,3) = fcn_to_category(carbon_price_unique_id);
scenario_group_table(:,4) = 1:parameters.S;

%%
optim_object.planting_target = 0;
optim_object.ghg_target_strict = 12 * 1e6;
[cost, A, b] = fcn_optim_constraints(optim_object);
p = optim_object.p;
nested_table = true;
target_shortfall = [];

%% Lambda: risk aversion
lambda = 0;

%% Nesting table
scenario_group_table_nested = scenario_group_table;
fcn_to_category = @(x) grp2idx(categorical(x));

for i=2:size(scenario_group_table,2)
    scenario_group_table_nested(:,i) = fcn_to_category(cellstr(num2str(scenario_group_table(:,1:i))));
end

%% Start calculation
% Value of decision making under uncertainty (EV solution)
if lambda>0
    x0 = fcn_CVaR(cost, 0.9, lambda, A, b, p, target_shortfall);
else
    x0 = fcn_exp_cost(cost' * p, A, b, target_shortfall);
end
c0 = cost * x0;

% Value of decision making under partial information
C1 = zeros(size(scenario_group_table));
if (nested_table)
    scenario_group = scenario_group_table_nested;
else
    scenario_group = scenario_group_table;
end

n_uncertainties = 4;
voi_info_set = cell( max(scenario_group(:,4)), n_uncertainties);

for j=1:n_uncertainties
    for i=1:max(scenario_group(:,j))
        partial_info_idx = i == scenario_group(:,j);
        partial_info_pos = find(partial_info_idx);
        cost_partial_info = cost(partial_info_idx, :);
        p1 = p(partial_info_idx) / sum(p(partial_info_idx));

        voi_info_set{i,j}.partial_info_idx = partial_info_idx;
        voi_info_set{i,j}.partial_info_pos = partial_info_pos;
        voi_info_set{i,j}.p1 = p1;
        if (lambda>0)
            voi_info_set{i,j}.cost_partial_info = cost_partial_info;
        else
            voi_info_set{i,j}.exp_cost_partial_info = cost_partial_info' * p1;
        end

    end
end

for j=1:n_uncertainties
    parfor i=1:max(scenario_group(:,j))
        voi_info_set_ij = voi_info_set{i,j};
        if lambda>0
            voi_info_set{i,j}.x1 = fcn_CVaR(voi_info_set_ij.cost_partial_info, 0.9, lambda, A, b, p1, target_shortfall);
        else
            voi_info_set{i,j}.x1 = fcn_exp_cost(voi_info_set_ij.exp_cost_partial_info, A, b, target_shortfall); % action with partial info
        end
    end
end

for j=1:n_uncertainties
    for i=1:max(scenario_group(:,j))
        voi_info_set_ij = voi_info_set{i,j};
        x1 = voi_info_set_ij.x1;
        partial_info_idx = voi_info_set_ij.partial_info_idx;
        partial_info_pos = voi_info_set_ij.partial_info_pos;

        c1_sub = cost * x1; % value of action with partial information
        c1_sub = c1_sub(partial_info_idx); % select value estimates from within that scenario
        C1(partial_info_pos,j) = c1_sub;
    end
end

%% Write table
vpxi = C1 - c0;
vpxi_table = array2table([scenario_group_table, vpxi],...
    'VariableNames', {'clim_scen', 'model', 'carbon_value_group', 'index', ...
    'emissions', 'emissions_model', 'carbon_value', 'all'});
evpxi = mean(C1 - c0);
writetable(vpxi_table, ['output/tables/', 'vpxi', num2str(lambda), '.csv']);

%% Calculate certainty equivalent measures of EVPXI
CE = zeros(size(C1));
for j=1:4
    CE(:,j) = fcn_certainty_equivalent(C1(:,j), lambda) - fcn_certainty_equivalent(c0, lambda);
end
% for j=1:4
%     for i=1:max(scenario_group(:,j))
%         partial_info_idx = i == scenario_group(:,j);
%         ce_value = fcn_certainty_equivalent(C1(partial_info_idx,j),lambda) - ...
%             fcn_certainty_equivalent(c0(partial_info_idx),lambda);
%         if isnan(ce_value)
%             print('Some values are NA');
%             break;
%         end
%         CE(partial_info_idx,j) =ce_value;
%     end
% end
ce_evpxi_table = array2table([scenario_group_table, CE],...
    'VariableNames', {'clim_scen', 'model', 'carbon_value_group', 'index', ...
    'emissions', 'emissions_model', 'carbon_value', 'all'});
writetable(ce_evpxi_table, ['output/tables/', 'vpxi_ce_', num2str(lambda), '.csv']);



