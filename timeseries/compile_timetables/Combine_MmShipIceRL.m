%%% Combine Mm, Ship, RL, & Ice Data %%% JPE
%20230606 !!! fn_idwave !!! changed blockmin to 25 temporarily for stats

GDriveLoc = 'G:\';
DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\'];

%data paths
depinfoPath = [DBPath,'matfiles'];
MmPath = ['I:\BRS_thesis\output\Mm_data'];
IcePath = [DBPath,'output\Ice'];
RLPath = [DBPath,'output\ssLTSA_data\all_RL'];
ShipPath = [DBPath,'output\GetShip_Timetable\All_Events_40km'];

%save path
outDir = [DBPath,'output\Stage_two\Combine_all\publication\10km_mask\effort'];

% Turn times with no effort into Nan's for related columns?
nanEff = 0; % 0 = no, 1 = yes

% Get rid of no effort times (based on Mm & RL effort)?
onlyeff = 0; % 0 = no, 1 = yes

% Get rid of PI 03 data?
no03 = 0; % 0 = no, 1 = yes

% Get MmData
[Mm_files, Mm_paths] = uigetfile([MmPath,'\*.mat'],'MultiSelect','on',...
        ['Select Mm Data']);
MmData = fullfile(Mm_paths(1,:), Mm_files(1,:));
%Get Ice Data
[Ice_files, Ice_paths] = uigetfile([IcePath,'\*.mat'],'MultiSelect','on',...
        ['Select Ice Data']);
IceData = fullfile(Ice_paths(1,:), Ice_files(1,:));
% Get RL data
[RL_files, RL_paths] = uigetfile([RLPath,'\*.mat'],'MultiSelect','on',...
        ['Select RL Data']);
RLData = fullfile(RL_paths(1,:), RL_files(1,:));
% Get Ship Data
[Ship_files, Ship_paths] = uigetfile([ShipPath,'\*.mat'],'MultiSelect','on',...
        ['Select Ship Data']);
ShipData = fullfile(Ship_paths(1,:), Ship_files(1,:));


% Load it up
load(MmData)
load(IceData)
load(RLData)
load(ShipData)


newMmTT = combined_TT;

% fix the ship type variable - go from nums to string
shipTypes = {'Bulk Carrier', 'General Cargo', 'Tanker', 'Icebreaker',...
    'Tug', 'Passenger', 'Pleasure Craft', 'Fishing', 'SAR',...
    'Sailing', 'Military', 'Ro-Ro Cargo','Buoy Laying', 'Dredger',...
    'Research/Survey Vessel', 'Offshore Support Vessel', 'Deck Cargo Ship',...
    'Heavy Load Carrier', 'Towing', 'Buoy-laying Vessel', 'Unknown'};



%%% Preallocate/Initialize Table %%%

% Set start/end times
startTime = datetime(2016, 1, 1, 0, 0, 0);  % Start time
endTime = datetime(2022, 1, 1, 0, 0, 0);  % End time

% Calculate the number of rows in the table
numRows = minutes(endTime - startTime) + 1;

% Get # of Columns needed
% removing some of these cols and adding some (for pres/effort)
numCols = size(newMmTT,2) + size(newIceTT,2) + size(newRLTT,2) + size(newShipTT,2)+3;

% Preallocate the table
MostData = table('Size', [numRows, numCols], 'VariableTypes', {'datetime', 'double',...
    'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double',...
    'double', 'double', 'double', 'double', 'double', 'double','double','double', 'string'}, 'VariableNames', {'Time', 'MmCounts', ...
    'MmPres', 'MmEffort', 'Ice_pc', 'IceEffort', 'minSPL', 'meanSPL', 'maxSPL', 'RLEffort','minRange', 'COG_T', 'SOG_kts', 'MMSI',...
    'draft_m', 'aspect', 'n_ships', 'ShipEffort', 'ShipType'});


MostData.Time = startTime + minutes(0:numRows-1)';
MostData.MmCounts(1:end-1) = newMmTT.counts;
MostData.Ice_pc = newIceTT.pc_cover;
MostData.minSPL = newRLTT.minSPL;
MostData.meanSPL = newRLTT.meanSPL;
MostData.maxSPL = newRLTT.maxSPL;
MostData.minRange = newShipTT.minRange;
MostData.COG_T = newShipTT.COG_T;
MostData.SOG_kts = newShipTT.SOG_kts;
MostData.MMSI = newShipTT.MMSI;
MostData.draft_m = newShipTT.draft_m;
MostData.aspect = newShipTT.aspect;
MostData.n_ships = newShipTT.n_ships;
MostData.shipType = newShipTT.shipType;


%Change ship types to their actual names



% Add Mm Presence
MostData.MmPres = MostData.MmCounts > 0;

% Add ship pres
MostData.sPres = MostData.n_ships > 0;

%%%%%%%%%%%%%% GET EFFORT %%%%%%%%%%%%%%%%%%%

%%% Get Deployment effort times 
[depinfo_files, depinfo_paths] = uigetfile([depinfoPath,'\*depinfo.mat'],'MultiSelect','on',...
        ['Select Deployment Info for PI']);
depinfo = fullfile(depinfo_paths(1,:), depinfo_files(1,:));
load(depinfo)

dplEffort = fn_get_dpleffort(MostData, recTimes);

% Find duty cycle off times (PI03), similar process to fn above
dtRecs = datetime(recTimes(:,:), 'ConvertFrom', 'datenum');
roundRecs = dateshift(dtRecs, 'start', 'minute');
% PI 03 recording minutes
recmins_idx = find(MostData.Time >= roundRecs(4,1) & MostData.Time <= roundRecs(4,2));

%get duty cycle idx's
dc_idx = find(MostData.meanSPL == 0 & MostData.Time >= roundRecs(4,1)...
    & MostData.Time <= roundRecs(4,2));
% no effort for duty cycle times 
dplEffort(dc_idx) = 0;


% Now we find the weird RL values that shouldnt be involved
% wait to talk w/ josh about it
%idxrl = find(MostData.meanSPL < 60 & MostData.meanSPL > 0 | MostData.meanSPL < 0);

% Mm Effort - same as dplEffort
MostData.MmEffort = dplEffort;

% Also same as RL effort here, check again if for certain
MostData.RLEffort = dplEffort;
    %get rid of times with RL less than 50 db RMS (plot time vs. SPL if
    %need be)
    idxtmp = find(MostData.minSPL < 50);
    MostData.RLEffort(idxtmp) = 0;
    


% now lets add some effort
MostData.IceEffort = ~isnan(MostData.Ice_pc);
MostData.ShipEffort = ones(size(MostData.MmCounts,1),1);

%%%%%%%% LAG & DURATION %%%%%%%%

% Turn ships' NaN's into 0's
for sCol = 11:17 % Ship related cols
    MostData.(sCol)(isnan(MostData.(sCol))) = 0;
end 

% ships first
MostData.slag = fn_shiplag(MostData)';
MostData.sdur = fn_shipdur(MostData)';

% now Mm
MostData.mmlag = fn_mmlag(MostData)';
MostData.mmdur = fn_mmdur(MostData)';

% start/stop times of events
a = find(MostData.sdur == 1);
b = find(MostData.slag == 1);
shiptimes = nan(size(a,1),2);
% col 1 = start, col 2 = end (indices of MostData)
shiptimes(:,1) = a;
shiptimes(:,2) = b(2:end);

clearvars a b

%%%%%% SHIP INTENSITY %%%%%%

%%% = Sum of all ship residence times / hr / km^2 %%%

% Preallocate
sIntensity = zeros(size(MostData, 1),1);
nShips = MostData.n_ships;
for i = 60:size(nShips, 1)
    % add the number of minutes of all ships for the hour previous and
    % including that minute. so if two ships in 1 row = 2 mins
    sumhr = sum(nShips(i-59:i));
    
    % not including the 40km area here
    
    sIntensity(i) = sumhr/60;
end

MostData.sIntensity = sIntensity;
clearvars nShips sumhr i sIntensity

%%%%%% SEL & cumSEL %%%%%%
% SEL = nan(size(MostData,1),1);
% sidx = find(MostData.n_ships > 0 & MostData.RLEffort == 1);
% SEL(sidx) = MostData.meanSPL(sidx) + 10*log10(60);
% MostData.SEL(sidx) = MostData.meanSPL(sidx) + 10*log10(60);

%%%%% CumSEL %%%%%

      %%%% Old Process %%%%
% % preallocate a cumintensity 
% cumIntensity = nan(length(SEL),1);
% % step 1 = convert all SEL's to intensity
% intensity = 10.^((SEL)./10);
% 
% % step 2 = make it so that the cumSEL is calculated by adding the row
% % intensity to the previous cumsel measurement (prior row) CUM INTENSITY
% 
% for i = 2:numel(intensity)
%     if ~isnan(intensity(i)) && isnan(cumIntensity(i-1))
%         cumIntensity(i) = intensity(i);
%     else if ~isnan(intensity(i)) && ~isnan(cumIntensity(i-1))
%     cumIntensity(i) = intensity(i) + cumIntensity(i-1);
%         else
%             cumIntensity(i) = nan;
%         end
%     end
% end
% 
% % Now we have cumulative intensity, time to get cumulative SEL
% 
% cumSEL = 10*log10(cumIntensity);
% MostData.cumSEL = cumSEL;
% % turn nan's into 0's
% MostData{isnan(MostData.cumSEL), 'cumSEL'} = 0 ;

[cumSEL, cumSEL_Effort, SEL] = fn_cumSEL_nondc(MostData);
MostData.cumSEL = cumSEL;
MostData.cumSEL_Effort = cumSEL_Effort;
MostData.SEL = SEL;

% Edit effort
idxtmp = find(MostData.RLEffort == 0);
MostData.cumSEL(idxtmp) = 0;
MostData.cumSEL_Effort(idxtmp) = 0;

%%%% GET AUTOCORRELATION -> WAVE + ID %%%%

% These are fake rn, the actual value is too high for stat computations
[wave, id] = fn_idwave(MostData(:,1:4));
MostData.Wave = wave;
MostData.ID = id;

%%%%% GET YEAR + jD %%%%%%%

% pull the year
MostData.year = year(MostData.Time);

% Day of year (as jd)

% Access the datetime column
timeData = MostData.Time;
% pull the day of the year for each datetime
jd = day(timeData, 'dayofyear');
% Add to TT
MostData.jd = jd;

%%%%%% FINAL FORMATTING BELOW %%%%%%

% lines below needed to be moved above for ship lag times
% % Turn ships' NaN's into 0's
% for sCol = 11:17 % Ship related cols
%     MostData.(sCol)(isnan(MostData.(sCol))) = 0;
% end

%%%%%%% 0 EFFORT to NANs %%%%%%%
% see on/off switch @ top

% For MmEff
if nanEff == 1
    idxtmp = MostData.MmEffort == 0;
    MostData.MmPres = double(MostData.MmPres);
    MostData.MmPres(idxtmp) = NaN;
    MostData.MmCounts(idxtmp) = NaN;
end
    
% For Ice
if nanEff == 1
    idxtmp = MostData.IceEffort == 0;
    MostData.Ice_pc(idxtmp) = NaN;
end

% For RL measurements
%%%%% there are a lot of wack measurements, need to look into this
if nanEff == 1
    idxtmp = MostData.RLEffort == 0;
    for RLcol = 7:9 %RL Cols
    MostData.(RLcol)(idxtmp) = NaN;
    end
end

% Ships are constant effort

%%%%%% Remove times? %%%%%%
% Or just get rid of PI03 times?
if no03 == 1
    MostData(recmins_idx,:) = [];
end

% only currently for RL & Mm effort, can do for ice too.
if onlyeff == 1
    MostData = MostData(MostData.MmEffort ~=0,:);
    MostData = MostData(MostData.RLEffort ~=0,:);
end

% Remove all other nans (which is the end of the ice period, fml)
MostData.ShipType = [];
MostData.shipType = []; %% IDK where the second one came from but abort
idx = find(any(ismissing(MostData),2));
MostData(idx,:) = [];
    
%last minute add normalized time of day (because there are nans in it)

% add normalized time of day now as well! (just for october as of now)
load('G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\Solar\publication\oct_normtod_PI_1min.mat');
MostData.tod = TT1.tod(1:end-1);


%%%% SAVE DATA %%%%

data = MostData;

%save('E:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\Combine_all\All_MmShipIceRL.mat', 'All_MmShipIceRL_nans');
% below not fucking working for some reason
save(fullfile(outDir, 'effortBinned_data_MmBRS_UTC'), 'data')

filename = 'effortBinned_data_MmBRS_UTC.csv';
writetable(data, fullfile(outDir, filename));


