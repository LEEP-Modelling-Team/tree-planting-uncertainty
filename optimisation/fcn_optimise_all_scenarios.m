function [decision_table, returns_table, scenario_returns, ghg_table, fail_ghg] = fcn_optimise_all_scenarios(optim_object)

% Optimise decisions under all scenarios
fcn_setup_gurobi();

[cost, A, b, ghg, new2kid, hectares, p, cost_full] = fcn_optim_constraints(optim_object);
X = zeros(size(cost,2), size(cost,1));
ghg_target_strict = optim_object.ghg_target_strict;
fail_ghg = zeros(size(cost,1),1);

parfor i=1:size(cost,1)
    Ai = A;
    bi = b;
    %if (ghg_target_strict > 0)
    %    ghg_cer = ghg(i,:);
    %    Ai = [A; -ghg_cer];
    %    bi = [b; -ghg_target_strict];
    %end
    x = fcn_exp_cost(cost(i,:), Ai, bi);
    % Catch cases where it is impossible to reach the GHG target
    if isempty(x)
        X(:,i) = fcn_exp_cost(cost(i,:), A, b);
        fail_ghg(i) = 1;
    else
        X(:,i) = x;
    end
end

returns_array = cost * X;
ghg_array = ghg * X;
species = [repelem("SS", size(cost_full,2)/2, 1); repelem("POK", size(cost_full,2)/2, 1)];
decision_table = array2table([[new2kid; new2kid], [hectares'; hectares'], X]);
decision_table.species = species;
returns_table  = array2table(returns_array);
ghg_table = array2table(ghg_array);
scenario_returns = diag(returns_array);
end