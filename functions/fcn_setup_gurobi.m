function [] = fcn_setup_gurobi()
gurobi_path = 'C:\gurobi951\win64\matlab\';
run([gurobi_path, 'gurobi_setup.m']);
end