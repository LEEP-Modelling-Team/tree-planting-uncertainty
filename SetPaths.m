% Input paths
bins_path = 'data/bins/';

% Output paths
tables_path = 'output/tables/';

if ~exist(tables_path, 'dir')
    CreateDirectories;
end

nev_path = 'D:/Github/tree-planting-uncertainty/';
addpath(genpath(nev_path));