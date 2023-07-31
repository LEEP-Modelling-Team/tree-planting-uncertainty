function [cov_mat, cost_sums] = fcn_cov_mat(a, cost)
    % a: decision vector
    % cost: a N x S array of net annual costs of planting

    species = [repelem("SS", size(cost,1)/2, 1); repelem("POK", size(cost,1)/2, 1)];

    planted_SS = a .* strcmp(species, 'SS');
    planted_POK = a .* strcmp(species, 'POK');

    cost_SS = cost' * planted_SS;
    cost_POK = cost' * planted_POK;

    cost_sums = fcn_normalise_benefits([cost_SS, cost_POK]);
    
    % Express costs in billions
    cost_sums_b = cost_sums / 1e9;

    cov_mat=cov(cost_sums_b);
end