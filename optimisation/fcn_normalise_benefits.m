function benefits = fcn_normalise_benefits(cost_annuity, discount_rate, years)
% Converts a annualised cost to net present values of a defined number of
% years
if ~exist('discount_rate', 'var')
    discount_rate = 0.035;
end

if ~exist('years','var')
    years = 30;
end
benefits = -cost_annuity * (1-(1/(1+discount_rate)^years))/discount_rate;
end