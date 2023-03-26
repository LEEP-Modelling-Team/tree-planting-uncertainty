%% CE function
function ce = fcn_certainty_equivalent(v, lambda)
if length(v)<=1
    ce = v;
else
    ce=(1-lambda)*mean(v) + lambda*(mean(v(v > quantile(v, 0.9))));
end
end