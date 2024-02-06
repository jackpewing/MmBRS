%%%%% Get & Combine the Sound Measurements %%%%% JPE 06/23/2023

% set path to folder with code and output files (matfiles, figures/stats)
GDriveLoc = 'E:\';
DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\'];
dataPath = [DBPath,'output\ssLTSA_data'];
outDir = [DBPath,'output\ssLTSA_data\all_RL\'];

[SPL_files, SPL_paths] = uigetfile([dataPath,'\*.mat'],'MultiSelect','on',...
        ['Select ssLTSA files for desired years']);
fileList = fullfile(SPL_paths(1,:), SPL_files(1,:));

%%% Preallocate a table, using proper format to fit the above tables

% Set start/end times - go as datenums to work with loaded data
startTime = datetime(2016, 1, 1, 0, 0, 0);  % Start time
endTime = datetime(2022, 1, 1, 0, 0, 0);  % End time

% Calculate the number of rows in the table
numRows = minutes(endTime - startTime) + 1;

% Preallocate the table
newTable = table('Size', [numRows, 4], 'VariableTypes', {'datetime', 'double',...
    'double', 'double'}, 'VariableNames', {'Time', 'minSPL', 'meanSPL', 'maxSPL'});

% Fill in the values
newTable.Time = startTime + minutes(0:numRows-1)';
newTable.minSPL = zeros(numRows, 1);
newTable.meanSPL = zeros(numRows, 1);
newTable.maxSPL = zeros(numRows, 1);

% Temporary change to dnums
newTable.Time = datenum(newTable.Time);

for i = 1:numel(fileList)
    % find index of times in dnums
    load(fileList{i});
    [~, loc] = ismember(dnums1min, newTable.Time);
    % add to table, use same indices as 
    newTable.Time(loc) = dnums1min;
    newTable.minSPL(loc) = SPL1minBand1(:,1);
    newTable.meanSPL(loc) = SPL1minBand1(:,2);
    newTable.maxSPL(loc) = SPL1minBand1(:,3);
end

newTable.Time = datetime(newTable.Time, 'ConvertFrom', 'datenum');
newRLTT = table2timetable(newTable);

save([outDir, 'All_RL_1minData.mat'], 'newRLTT')


