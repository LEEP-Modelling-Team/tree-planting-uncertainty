function [x,fval,exitflag,output,VaR] = fcn_CVaR(J,beta,lambda,A,b,p,target_shortfall)

% fcn_CVaR
%   Conditional value-at-risk optimisation given a beta (percentile of
%   returns) and empirical realisations of cost, J.
%   min_x (1-lambda) * mean(J,1) * x + lambda * CVaR(x) + delta *
%   shortfall_cost
%   * Subject to Ax < b

%
% INPUT
% ------------------
% J: a QxN matrix storing the realisation of cost, with N being the number of assets and 
%   Q being the number of realisations.
% beta: probability that the costs are smaller than value-at-risk
% lambda: controls the trade-off between maximising expected return and minimising CVaR;
% A: additional matrix of N columns specifying the constraints
% b: additional vector of length N specifying the constraint Aineq * x
% < bineq
% p: probability of each scenario in Q
% target_shortfall: an object that describes added penalty to the objective
% function for failure to reach a target. No added reward is given for
% over-achievement of the target. This object contains the following
% fields:
%   m: a Nx1 vector of the metric for each location
%   shortfall_cost: Cost of failing to reach one unit of the objective in
%   total
% 
% OUTPUT
% ------------------
% x: decision vector
% fval: objective function value
% exitflag: exitflag from CPLEX
% output: structure output that contains information about the optimization
%   from CPLEX
% VaR: Value-at-risk of the portfolio

%% Check arguments
if (~isscalar(beta) || beta >= 1 || beta <= 0)
    error("beta must be a scalar larger than 0 and smaller than 1.");
end

if (~exist('lambda','var'))
    warning("No lambda specified. Default lambda to 1");
    lambda = 1;
end

%% Compute probability-weighted mean
[Q,N] = size(J);

if exist('p', 'var') && ~isempty(p)
    if abs(sum(p) - 1) > 1e-3
        error("Probabilities of scenarios do not sum up to 1.");
    end
    mu = (J.'*p)';
    if (size(p,1) > 1)
        p = p';
    end
else
    mu =  mean(J,1);
    p = ones(1,Q)*(1/Q);
end

%% Solve problem
options = cplexoptimset('output.clonelog', -1);
options.Display = "on";
options.Method = 1;
fprintf("CVaR problem with %i realisations of uncertain data and %i decision variables.\n", Q,N);
f = [(1-lambda)*mu, lambda * p*(1/(1-beta)), lambda * 1]'; % Minimise risk-adjusted returns

% Aineq1: sum_n x_i * y_k - u_k - alpha = 0 forall k
Aineq1_x = J; % Loss
Aineq1_u = -eye(Q);
Aineq1_alpha = -ones(Q,1);
Aineq1 = [Aineq1_x, Aineq1_u, Aineq1_alpha];
Bineq1 = zeros(Q,1);

A(1,size(Aineq1,2)) = 0;
Aineq = [Aineq1; A];
Bineq = [Bineq1; b];

% Lower bound: x_i and u_k must be larger than zero
lb = [zeros(1,N), zeros(1,Q), -inf];
ub = [ones(1,N), inf(1,Q), inf];

if exist('target_shortfall', 'var') && ~isempty(target_shortfall)
    [f, Aineq, Bineq, lb, ub] = fcn_target_shortfall(target_shortfall, f, Aineq, Bineq, lb, ub);
end

clear J Aineq1_x Aineq1_u Aineq1_alpha Aineq1 Bineq1

% Solve the problem with Gurobi due to problems with CPLEX
[x,fval,exitflag,output] = fcn_gurobilp(f, sparse(Aineq), sparse(Bineq), [], [], lb, ub, [], options);

if exitflag < 0
    warning("Exitflag is less than 0. Solution may not be available")
else
    fprintf("Solution found with VaR = %.2f and CVaR = %.2f\n", x(end), ...
    [zeros(1,N), 1/(Q*(1-beta))*ones(1,Q), 1] * x(1:(N+Q+1)));
end

VaR = x(N+Q+1);
x = x(1:N);
