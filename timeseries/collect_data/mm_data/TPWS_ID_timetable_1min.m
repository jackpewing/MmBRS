% JPE Make timetable of click counts and RL from a TPWS2 and
% ID2 files. 


clear all

% set path to folder with code and output files (matfiles, figures/stats)
GDriveLoc = 'E:\';
DBPath = ['I:\BRS_thesis\'];
%     LTSApath = '\\frosty\LTSA';
TPWSpath = 'I:\BRS_thesis\TPWS\TPWS2';
outDir = [DBPath,'output\Mm_data\MmData_1min_yr\'];
 
proj = 'CANARC';

% this is from other work where we might have different run params per site
% or use the deploymentInfo.mat file to get transfer functions or other
% info by site and deplyment.
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

depl = 2020; % this is the deployment year for this run
startDate = datenum(['1/01/',num2str(depl),' 00:00:00']); % this is the left edge of the first time bin
endDate = datenum(['1/01/',num2str(depl+1),' 00:00:00']); % this is the right edge of the last time bin

spID = 2; % this is the ID for your species in the ID2 file. 1=Dl, 2=Mm

% % Inuit seasons approach (may not be used if doing an annual timetable)
%     startDates = {['5/15/',depl] ['7/15/',depl] ['9/15/',depl]};
%     endDates = {['7/15/',depl] ['9/15/',depl] ['11/15/',depl]};

% setup the timetable for this period
    yr = datevec(startDate);
    yr = yr(1);

    data_start_dvec = datevec(startDate);
    startMin = [ data_start_dvec(1:5) 0];
    
% Make array of 1min tbins and preallocate for clicks data
    % This TT0 will provide the rowTimes for our final timetable output
    TT0 = datetime(datevec(startDate)):minutes(1):datetime(datevec(endDate));
    dnums1min = datenum(TT0); % heres the tbins array to work with
    
% this will become counts, and min, mean, max SPL
    clicks1min = zeros(length(dnums1min),4);
    
% select the TPWS2 and ID2 files to include in this runs
    [TPWS_files, TPWS_paths] = uigetfile([TPWSpath,'\*TPWS2.mat'],'MultiSelect','on',...
        ['Select TPWS2 files for ',proj,' ',site,' ',num2str(yr)]);
    TPWSfileList = fullfile(TPWS_paths(1,:), TPWS_files(1,:));
    
    [ID_files, ID_paths] = uigetfile([TPWSpath,'\*ID2.mat'],'MultiSelect','on',...
    ['Select ID2 files for ',proj,' ',site,' ',num2str(yr)]);
    % [ tf_file, tf_path ] = uigetfile('D:\tf_files\*.tf','Pick Transfer Function');
    IDfileList = fullfile(ID_paths(1,:), ID_files(1,:));

for iFile = 1:length(IDfileList)
 
    % make matfile objects for this pair of TPWS and ID files
    thisDetFile = matfile(IDfileList{iFile});
    thisTPWSfile = matfile(TPWSfileList{iFile});
    
    zID = thisDetFile.zID; % load zID from the ID2 file
    zID = zID(zID(:,2)==spID,:); %trim to zID locs where col#2 equals spID
    mySpID = thisDetFile.mySpID; % grab species ID info if there is any
    MTT = thisTPWSfile.MTT; % load just MTT from the TPWS2 file
    MPP = thisTPWSfile.MPP; % load the MPP from this TPWS2 file
    
    if isempty(zID)
        disp(['no spID=',mySpID{spID},' in ID2 file ',ID_files{iFile}])
        disp('skipping to next file...')
        continue
    end
    
    [~,timeInd,~] = intersect(MTT,zID);
    
    % trim MTT and MPP to include only idxs for this spID
    MTT = MTT(timeInd);
    MPP = MPP(timeInd);
    
    % find which tbin each MTT time belongs to
    Y = discretize(MTT, dnums1min);
    binIdxs = unique(Y);
    idxchk = ~isnan(binIdxs); % check to see if any ID times within date range
    binIdxs = binIdxs(idxchk); % trim tbins to only those within date range
    
    % ok fine. a for loop to get clicks info for each tbin
    if isempty(binIdxs)
        disp(['no spID=',mySpID{spID},' in this date range for ',ID_files{iFile}])
        disp('skipping to next file...')
        continue
    end
    
    for jj=1:length(binIdxs)
        thisBin = binIdxs(jj);
        MPPtmp = MPP(Y==thisBin); % get MPP values for clicks this tbin
        clicks1min(thisBin,1) = length(MPPtmp);
        clicks1min(thisBin,2:4) = [min(MPPtmp) mean(MPPtmp) max(MPPtmp)];
    end
    
end

% make the lovely timetable output. 
% first check lengths to make sure times still line up
    if length(TT0)~= size(clicks1min,1)
        disp('Error. Size of timetable does not match length of tbins generated')
    end

    TT1 = array2timetable(clicks1min,'RowTimes',TT0);

    TT1.Properties.VariableNames{1} = 'counts';
    TT1.Properties.VariableNames{2} = 'PPmin';
    TT1.Properties.VariableNames{3} = 'PPmean';
    TT1.Properties.VariableNames{4} = 'PPmax';

    %save file
    save([outDir,proj,'_',site,'_',num2str(yr),...
        '_clickTable_',mySpID{spID},'.mat'],'TT1');


