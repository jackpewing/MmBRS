% 20230425 JMJ: Make timetable of click counts and RL from a set of TPWS2 and
% ID2 files. 

% NOTE: Must be an ID2 file for each TPWS2 file or timetable won't be built
% for clicks in that TPWS2.

clear all

% set path to folder with code and output files (matfiles, figures/stats)
GDriveLoc = 'G:\';
DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\'];
TPWSpath = 'I:\BRS_thesis\TPWS\TPWS2';
outDir = [DBPath,'output\Mm_data\'];
 
proj = 'CANARC';
spID = 2; % 2 for narwhal
sitenum = 4; 
    % 1 = LowIsland, 
    % 2 = Tremblay, 
    % 3 = Guys Bight 
    % 4 = PI_HARP 
    % 5 = LI_HARP 
    % 6= MI_HARP 
    % 7 = EclipseSound center (150 km r)

sites = {'ONC_LI','ONC_TS','ONC_GB','PI','LI','MI','ES'};
site = char(sites(sitenum));


% select the TPWS2 and ID2 files to include in this runs
    [TPWS_files, TPWS_paths] = uigetfile([TPWSpath,'\*TPWS2.mat'],'MultiSelect','on',...
        ['Select TPWS2 files for ',proj,' ',site,]);
    TPWSfileList = fullfile(TPWS_paths(1,:), TPWS_files(1,:));
    
    [ID_files, ID_paths] = uigetfile([TPWSpath,'\*ID2.mat'],'MultiSelect','on',...
    ['Select ID2 files for ',proj,' ',site,]);
    IDfileList = fullfile(ID_paths(1,:), ID_files(1,:));

   
    
MPPCellArray = cell(length(TPWSfileList), 1);


for iFile = 1:length(TPWSfileList)
 
    % make matfile objects for this pair of TPWS and ID files
    thisDetFile = matfile(IDfileList{iFile});
    thisTPWSfile = matfile(TPWSfileList{iFile});
    
    zID = thisDetFile.zID; % load zID from the ID2 file
    zID = zID(zID(:,2)==spID,:); %trim to zID locs where col#2 equals spID
    mySpID = thisDetFile.mySpID; % grab species ID info if there is any
    MTT = thisTPWSfile.MTT; % load just MTT from the TPWS2 file
    MPP = thisTPWSfile.MPP; % load the MPP from this TPWS2 file
    
    [~,timeInd,~] = intersect(MTT,zID);
    MPP = MPP(timeInd);
    
    MPPCellArray{iFile} = MPP;
end

data = vertcat(MPPCellArray{:});

% apply 120 db threshold
data = data(data >= 120);

% Define the number of bins for the histogram
numBins = 200;

% Create the histogram with specified bin edges
binEdges = linspace(min(data), max(data), numBins + 1);
histData = histogram(data, binEdges, 'Normalization', 'count', 'DisplayStyle', 'bar');

% Get the counts
counts = histData.Values;

% Apply log10 to the counts
logCounts = log10(counts);

% Create a new bar chart using the log10 counts
figure;
bar(logCounts);
xlabel('Peak to Peak dB');
ylabel('log10(Counts)');
title('PI06 Narwhal Click Received Levels');

% Set custom x-axis labels for every 20th bin
xticks(1:20:numBins);
% xticklabels(binEdges(1:20:end-1));
xticklabels(binEdges(1:20:end-1));

