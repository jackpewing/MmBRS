%%% JPE 20230621 - get all ship transits (non PI 03 duty cycle) %%%
% WITH SPL MEASURMENTS


% Same process as get_PI03_cumSEL
% use NANed data - Binned_data_MmBRS_nans_20230605

function [cumSEL, cumSEL_Effort, SEL] = fn_cumSEL_nondc(data)

GDriveLoc = 'E:\';
DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\'];
%data paths
depinfoPath = [DBPath,'matfiles'];


%%%%% get PI03 times %%%%%

%%% Get Deployment effort times 
[depinfo_files, depinfo_paths] = uigetfile([depinfoPath,'\*depinfo.mat'],'MultiSelect','on',...
        ['Select Deployment Info for PI']);
depinfo = fullfile(depinfo_paths(1,:), depinfo_files(1,:));
load(depinfo)

dplEffort = fn_get_dpleffort(data, recTimes);

% Find duty cycle off times (PI03), similar process to fn above
dtRecs = datetime(recTimes(:,:), 'ConvertFrom', 'datenum');
roundRecs = dateshift(dtRecs, 'start', 'minute');
% PI 03 recording minutes
recmins_idx = find(data.Time >= roundRecs(4,1) & data.Time <= roundRecs(4,2));
rectimes = [roundRecs(4,1):minutes(1):roundRecs(4,2)]';

%get duty cycle idx's
dc_idx = find(data.meanSPL == 0 & data.Time >= roundRecs(4,1)...
    & data.Time <= roundRecs(4,2));
% no effort for duty cycle times :(
dplEffort(dc_idx) = 0;


%%%%%%%%%%%%%%%%% READ ME
%%%%%%%%%%%%%%%%% KAIT NOTES
% use mean SPL values
% take good non duty cycle transits
% duty cycle eahc transit randomly a bunch of times
% fit a linear model for each transit
% get the error in duty cycle SPL values from the continuous data



% get transit start end times
start = data.Time(find(data.sdur == 1));
stop = data.Time(find(data.slag == 1));
% this lag is just for the beginning of the data
stop = stop(2:end,:)-minutes(1);

% Structs eek
txdata.start = start;
txdata.stop = stop;

% remove duty cycle transits

dcidx  = find(txdata.start >= roundRecs(4,1) & txdata.stop <= roundRecs(4,2));
txdata.start(dcidx) = [];
txdata.stop(dcidx) = [];

% Make timetables for each transit with mean SPL (in the commmented
% section)
for i = 1:height(txdata.start)
    z = find(data.Time >= txdata.start(i) & data.Time <= txdata.stop(i));
    txdata.events.(['tx',num2str(i)]) = data.meanSPL(z);
    t = [txdata.start(i):minutes(1):txdata.stop(i)]';
    txdata.events.(['tx',num2str(i)]) = table(t, data.meanSPL(z), 'VariableNames', {'Time', 'meanSPL'});
end

% Okay lets get cumSEL for every transit now, and set 95% as the limit


% need to turn the above into a for loop now.

numel(fieldnames(txdata.events))
for fileidx = 1:numel(fieldnames(txdata.events))
    filename = sprintf('tx%d',fileidx);
    if isfield(txdata.events,filename)
        tx = txdata.events.(filename);
    end
        SEL = table2array(tx(:,2)) + 10*log10(60);
        
        cumIntensity = nan(size(tx,1),1);
        
        intensity = 10.^((SEL)./10);    
        
    for i = 1:numel(intensity)
    if i == 1
        cumIntensity(i) = intensity(1);
    else if i ~=1
            cumIntensity(i) = intensity(i)+cumIntensity(i-1);
        end
    end
    end
    
    for i = 1:numel(cumIntensity)
    if cumIntensity(i) > cumIntensity(end)*(0.95)
        cumIntensity(i) = 0;
    end
    end
    cumSEL = 10*log10(cumIntensity);
    
    txdata.events.(filename).cumSEL = cumSEL;
    txdata.events.(filename).SEL = SEL;
    end

    

    
    clearvars -except All_MmShipIceRL data txdata
    
    
% now put all the transits into a timetable from jan 1st 2016 to jan 1st 2022
    
 % initialize, should probably rename table 
time = [datetime(2016,1,1):minutes(1):datetime(2022,1,1)]';
cseldata = table('Size', [numel(time),3], 'VariableTypes', {'datetime',...
    'double', 'double'}, 'VariableNames', {'Time', 'SEL','cumSEL'});
cseldata.Time = time;
cseldata.cumSEL = zeros(height(cseldata),1);

% Pseudocoding

% for loop going through all tx 
% find times of tx transit, get idx from the cumSEL table
% get the cumSEL values and put them in those indices
% cycle to next


numel(fieldnames(txdata.events))
for fileidx = 1:numel(fieldnames(txdata.events))
    filename = sprintf('tx%d',fileidx);
  if isfield(txdata.events,filename)
        tx = txdata.events.(filename);
  end
timeidx = find(ismember(cseldata.Time, tx.Time));

cseldata.cumSEL(timeidx) = tx.cumSEL;
cseldata.SEL(timeidx) = tx.SEL;
end

% get rid of -Inf vals within transits but outside of 95% limit
idxtmp = find(cseldata.cumSEL == -Inf);

cseldata.cumSEL(idxtmp) = 0;

% get effort of cumSEL --> when zero

idxtmp = cseldata.cumSEL ~= 0;

cseldata.cumSEL_Effort = idxtmp;

SEL = cseldata.SEL;
cumSEL = cseldata.cumSEL;
cumSEL_Effort = cseldata.cumSEL_Effort;










