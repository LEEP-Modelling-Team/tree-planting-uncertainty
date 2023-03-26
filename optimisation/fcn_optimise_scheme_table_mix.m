function [decision_table, returns_table] = fcn_optimise_scheme_table_mix(optim_object)

fcn_setup_gurobi();

[cost, A, b, ghg, new2kid, hectares, p, cost_full, ghg_full] = fcn_optim_constraints(optim_object);

if ~isfield(optim_object, 'lambda')
    optim_object.lambda = 1;
end

if ~isfield(optim_object, 'run_cvar')
    run_cvar = true;
else
    run_cvar = optim_object.run_cvar;
end

if isfield(optim_object, 'abatement_cost') && isfield(optim_object, 'ghg_target')
    target_shortfall.m = mean(ghg_full,1);
    target_shortfall.cost = optim_object.abatement_cost;
    target_shortfall.target = optim_object.ghg_target;
else
    target_shortfall = [];
end

x_ev          = fcn_exp_cost(cost' * p, A, b, target_shortfall);
%x_mm   = fcn_minimax_rowwise(cost, 1, A, b, true);
if run_cvar
    if (optim_object.lambda == 0)
        x_cvar = x_ev;
    else
        x_cvar = fcn_CVaR(cost, 0.9, optim_object.lambda, A, b, p, target_shortfall);
    end
end
%x_mv = fcn_mv_factor(cost,[],optim_object.lambda, A, b);

% Extract 
species = [repelem("SS", size(cost_full,2)/2, 1); repelem("POK", size(cost_full,2)/2, 1)];
if run_cvar
decision_array = [x_cvar, x_ev];
varnames = {'new2kid', 'cost', 'hectares', 'CVaR', 'EV', 'ghg_ann'};
decision_table = array2table([[new2kid; new2kid], mean(cost_full)',...
    [hectares'; hectares'], decision_array, mean(ghg_full,1)'], 'VariableNames', varnames);
decision_table.species = species;
returns_array = cost_full * decision_array;
ghg_array     = ghg_full * decision_array;
returns_table = array2table([returns_array, ghg_array, optim_object.p], ...
    'VariableNames', {'CVaR', 'EV', ...
    'CVaR_ghg', 'EV_ghg', 'p'});
else
    decision_array = x_ev;
    varnames = {'new2kid', 'cost', 'hectares', 'EV', 'ghg_ann'};
    decision_table = array2table([[new2kid; new2kid], mean(cost_full)',...
        [hectares'; hectares'], decision_array, mean(ghg_full,1)'], 'VariableNames', varnames);
    decision_table.species = species;
    returns_array = cost_full * decision_array;
    ghg_array     = ghg_full * decision_array;
    returns_table = array2table([returns_array, ghg_array, optim_object.p], ...
        'VariableNames', {'EV', ...
        'EV_ghg', 'p'});
end


end