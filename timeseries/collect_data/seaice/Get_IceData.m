%%% Format Ice Data %%% JPE

clear all

% set path to folder with code and output files (matfiles, figures/stats)
GDriveLoc = 'G:\';
DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\'];
dataPath = [DBPath,'_CANARC\sea ice'];
outDir = [DBPath,'JackBRS\Arctic_shiptxClicks\output\Ice\'];

%%% Preallocate a table, using proper format to fit the above tables

% Set start/end times
startTime = datetime(2016, 1, 1, 0, 0, 0);  % Start time
endTime = datetime(2022, 1, 1, 0, 0, 0);  % End time

% Calculate the number of rows in the table
numRows = minutes(endTime - startTime) + 1;

% Preallocate the table
newTable = table('Size', [numRows, 2], 'VariableTypes', {'datetime', 'double'},...
    'VariableNames', {'Time', 'pc_cover'});

% Fill in the values
newTable.Time = startTime + minutes(0:numRows-1)';
newTable.pc_cover = nan(numRows, 1);


%%% load data
[ice_files, ice_paths] = uigetfile([dataPath,'\*.csv'],'MultiSelect','on',...
        ['Select Ice Data File']);
fileList = fullfile(ice_paths(1,:), ice_files(1,:));

data = readtable(fileList);

% need to make a vector of days in the file because there is no filelist
recvec = datetime(2016,01,01):days(1):datetime(2021,12,31);

% add datetime vector
data.Date = recvec';

% make into timetable
data = table2timetable(data);

% pick median
data = removevars(data, setdiff(data.Properties.VariableNames, 'Median'));

% get indices of dates that fit our new table
timerange = startTime:endTime;
idx = find(ismember(data.Date, timerange));

%remove all other dates
data = data(idx,:);

%%%% essentially need to pull out the dates now and make them into a minute
%%%% by minute thing, then do the repelem, and add it back in. 

start = datetime(data.Date(1), 'Format', 'MM/dd/yyyy HH:mm:ss');
last = datetime(data.Date(end), 'Format', 'MM/dd/yyyy HH:mm:ss');

numRows = minutes(last - start+1) ;

% Preallocate the table
tmpTable = table('Size', [numRows, 2], 'VariableTypes', {'datetime', 'double'},...
    'VariableNames', {'Time', 'pc_cover'});

% Fill in the values
tmpTable.Time = start + minutes(0:numRows-1)';
tmpTable.pc_cover = nan(numRows, 1);

% repeat so it goes for every minute of the day
icedata = repelem(data.Median, 24*60);
    
% Add to temp table table
tmpTable.pc_cover = icedata;

%%% add to the final table
[~, loc] = ismember(tmpTable.Time, newTable.Time);
newTable.pc_cover(loc) = tmpTable.pc_cover;

newIceTT = table2timetable(newTable);

save([outDir, 'All_IceData_20km.mat'], 'newIceTT')






