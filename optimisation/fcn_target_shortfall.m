% Adds a target shortfall penalty to any objective function
% The optimisation problem becomes:
% min_x f(x) + delta * cost
% such that
%     m * x + delta > target
%             delta > 0, delta < Inf
%
% The target_shortfall object should contain the following fields
%   m: metric for the decision vector x
%   target: total sum of m*x where exceedance will not lead to net costs
%   cost: shortfall cost of falling short of each unit of the target m_target

function [f, Aineq, bineq, lb, ub] = fcn_target_shortfall(target_shortfall, f, Aineq, bineq, lb, ub)
    Aineq2 = -target_shortfall.m; 
    Aineq2(1,size(Aineq,2)) = 0;
    Aineq = [Aineq; Aineq2];
    v = zeros(size(Aineq, 1),1);
    v(end) = -1;
    Aineq = [Aineq, v];
    bineq = [bineq; -target_shortfall.target];
    f = [f; target_shortfall.cost];
    lb = [lb, 0];
    ub = [ub, Inf];
end