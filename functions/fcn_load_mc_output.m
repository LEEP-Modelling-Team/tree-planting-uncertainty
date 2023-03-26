function optim_object = fcn_load_mc_output(parameters, price_variability)
SetPaths
optim_object = struct();
optim_object.hectares = [];
optim_object.ghg = [];
optim_object.forestry_soil_ghg_qnt = [];
optim_object.cost_carbon_norec = [];
optim_object.cost_carbon_SS = [];
optim_object.cost_carbon_POK = [];
optim_object.opp_cost = [];
optim_object.opp_cost_SS = [];
optim_object.opp_cost_POK = [];
if isfield(optim_object, "planting_target")
    optim_object.planting_target = parameters.planting_target;
end
optim_object.new2kid         = parameters.new2kid;
optim_object.cost_type       = parameters.cost_type;

if exist("price_variability", "var") && ~price_variability
    prefix = 'no_price_';
else
    prefix = '';
end

fcn_load_field = @(field) fcn_readwrite_bin('r',...
    [bins_path, prefix, 'a1_', field, '.bin'],...
    [], [57230, parameters.S]);

if strcmp(parameters.cost_type, 'no_foregone_ag_ghg_value')
    optim_object.cost_carbon_forestry_SS  = fcn_load_field('cost_carbon_forestry_SS');
    optim_object.cost_carbon_forestry_POK = fcn_load_field('cost_carbon_forestry_POK');
elseif strcmp(parameters.cost_type, 'opp_cost')
    optim_object.opp_cost_SS              = fcn_load_field('opp_cost_SS');
    optim_object.opp_cost_POK             = fcn_load_field('opp_cost_POK');
else
    optim_object.cost_carbon_SS           = fcn_load_field('cost_carbon_SS');
    optim_object.cost_carbon_POK          = fcn_load_field('cost_carbon_POK');
end

optim_object.forestry_soil_ghg_qnt    = fcn_load_field('forestry_soil_ghg_qnt');

load("data/a1_landuse_chg.mat", "a1_landuse_chg");
optim_object.hectares = a1_landuse_chg.wood_ha_chg;

optim_object.ghg_total_qnt_SS = fcn_load_field('ghg_total_qnt_SS');
optim_object.ghg_total_qnt_POK = fcn_load_field('ghg_total_qnt_POK');

optim_object.subset_cells = min(optim_object.forestry_soil_ghg_qnt,[],2) >= 0;
optim_object = rmfield(optim_object, 'forestry_soil_ghg_qnt');

end