%%% Format Ship Data %%% JPE

clear all

% set path to folder with code and output files (matfiles, figures/stats)
GDriveLoc = 'E:\';
DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\'];
dataPath = [DBPath,'output\GetShip_Timetable\ShipEvents_40km'];
outDir = [DBPath,'output\GetShip_Timetable\All_Events_40km\'];

%%% Preallocate a table, using proper format to fit the above tables

% Set start/end times
startTime = datetime(2016, 1, 1, 0, 0, 0);  % Start time
endTime = datetime(2022, 1, 1, 0, 0, 0);  % End time

% Calculate the number of rows in the table
numRows = minutes(endTime - startTime) + 1;

% Preallocate the table
newTable = table('Size', [numRows, 9], 'VariableTypes', {'datetime', 'double',...
    'double', 'double', 'double', 'double', 'double', 'double', 'double'},...
    'VariableNames', {'Time', 'minRange', 'COG_T', 'SOG_kts', 'MMSI',...
    'draft_m', 'aspect', 'n_ships', 'shipType'});

% Fill in the values
newTable.Time = startTime + minutes(0:numRows-1)';
newTable.minRange = zeros(numRows, 1);
newTable.COG_T = zeros(numRows, 1);
newTable.SOG_kts = zeros(numRows, 1);
newTable.MMSI = zeros(numRows, 1);
newTable.draft_m = zeros(numRows, 1);
newTable.aspect = zeros(numRows, 1);
newTable.n_ships = zeros(numRows, 1);
newTable.shipType = zeros(numRows, 1);

%%% Get Data %%% 
% Pick Ship Data here
[ship_files, ship_paths] = uigetfile([dataPath,'\*km.mat'],'MultiSelect','on',...
        ['Select Ship Timetable Files for Desired Years']);
fileList = fullfile(ship_paths(1,:), ship_files(1,:));


for i = 1:numel(fileList)
    load(fileList{i});
    
[~, loc] = ismember(ST1.Time, newTable.Time);
newTable.minRange(loc) = ST1.minRange;
newTable.COG_T(loc) = ST1.COG_T;
newTable.SOG_kts(loc) = ST1.SOG_kts;
newTable.MMSI(loc) = ST1.MMSI;
newTable.draft_m(loc) = ST1.draft_m;
newTable.aspect(loc) = ST1.aspect;
newTable.n_ships(loc) = ST1.n_ships;
newTable.shipType(loc) = ST1.shipType_cat;
end

%%% Can get rid of Nan's here. probably should
% newTable = fillmissing(newTable, 'constant', 0);

newShipTT = table2timetable(newTable);

save([outDir, 'All_ShipData_40km_20230627.mat'], 'newShipTT')


