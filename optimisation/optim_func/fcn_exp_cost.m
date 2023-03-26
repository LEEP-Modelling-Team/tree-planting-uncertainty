function [x, fval,exitflag,output] = fcn_exp_cost(f, Aineq, bineq, target_shortfall)
% fcn_exp_cost
%   Solves expected cost minimisation problem for a single-stage
%   portfolio optimisation problem.
%   min c'x
%   s.t. Ax < b
%   s.t. 0 >> x >> 1 
%
% INPUT
% ------------------
% c: expected returns in a vector of expected returns of length N.
% Aineq: matrix A of N columns
% Bineq: vector b of length N
% 
% OUTPUT
% ------------------
% x: decision vector
% fval: objective function value
% exitflag: exitflag from CPLEX
% output: structure output that contains information about the optimization
%   from CPLEX

%% CPLEX PATH
% ----------------
if ismac
    addpath(genpath('/Applications/CPLEX_Studio1210/cplex/matlab/x86-64_osx'))
elseif ispc
    addpath(genpath('C:\Program Files\IBM\ILOG\CPLEX_Studio1210\cplex\matlab\x64_win64'))
end
% ----------------

%% Solve optimisation problem
options = cplexoptimset('output.clonelog', -1);
options.Display = "off";

N = length(f);

lb = zeros(1,N);
ub = ones(1,N);

if exist('target_shortfall', 'var') && ~isempty(target_shortfall)
    [f, Aineq, bineq, lb, ub] = fcn_target_shortfall(target_shortfall, f, Aineq, bineq, lb, ub);
end

[x, fval, exitflag, output] = fcn_gurobilp(f, Aineq, bineq, [], [], lb, ub, [], options);
if (isempty(x))
    if exitflag < 0
        warning("Exitflag is less than 0. Solution may not be available")
    end
    return
end

x = x(1:N);

% model.A = sparse(Aineq);
% model.obj = c;
% model.modelsense = 'Min';
% model.rhs = bineq;
% model.lb = lb;
% model.ub = ub;
% 
% result = gurobi(model);
% x = result.x(1:N);
% fval = result.objval;
% exitflag = result.status;
% output = result;




end