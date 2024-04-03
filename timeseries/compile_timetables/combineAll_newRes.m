%%%%%% JPE 20240212 - some major problems %%%%%%
%%%%%%%%% make a new TT for worse resolution (for GEE)%%%%%%%%%

%% load data, select only relevant variables
dbPath = ['G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\Stage_two\Combine_all\publication\10km_mask\all'];
filePath = fullfile(dbPath, 'Binned_data_MmBRS_UTC.mat');

load(filePath);

% also pull in time of day
todpath = 'G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\Solar\publication\oct_normtod_PI_5min.mat';
load(todpath);
TT1 = TT1(1:end - 1,:);

data = table2timetable(data);

%% change some variable classes for now
data.MmPres = double(data.MmPres);
data.sPres = double(data.sPres);

%% begin retiming

sumdata = retime(data(:,{'MmPres';'MmEffort';'RLEffort'}), 'regular', 'sum', 'TimeStep', minutes(5));
TT = retime(data(:,{'Ice_pc';'jd';'year';'maxSPL'}), 'regular', 'max', 'TimeStep', minutes(5));
% meandata = retime(data(:,{'SOG_kts'}), 'regular', 'mean', 'TimeStep', minutes(5));
mindata = retime(data(:,{'minRange', 'n_ships','SOG_kts'}), 'regular', 'min', 'TimeStep', minutes(5));

% now combine all these things
TT.MmPres = sumdata.MmPres;
TT.MmEffort = sumdata.MmEffort;
TT.RLEffort = sumdata.RLEffort;
TT.minRange = mindata.minRange;
TT.sPres = mindata.n_ships;
TT.SOG = mindata.SOG_kts;
TT.tod = TT1.tod;

%now make it so if effort isnt full then remove. can be changed in future
%for effort adjusted but for now lets just keep it the same for a
%sensitivity analysis

% number is set by the desired time resolution
TT = TT(TT.MmEffort ~= 0,:);
TT = TT(TT.RLEffort ~= 0,:);
 
% % make narwhal presence binary
% TT.MmPres = TT.MmPres >= 1;

% reference for later if doing the pres adjusted
TT.MmEffort_perc = TT.MmEffort/5; % change what you divide by based on resolution
TT.MmPres_eff_adj = TT.MmPres./TT.MmEffort_perc;

%%

mmdata = timetable2table(TT);

% quick reference if using adjusted presence later

% idxeff = ~isnan(mmdata.MmPres_eff_adj);
% mmdata = mmdata(idxeff,:);

save(fullfile(dbPath, 'effadj_binned_5min_UTC'), 'mmdata')

filename = 'effadj_binned_5min_UTC.csv';
writetable(mmdata, fullfile(dbPath, filename));








filename = 'binned_effort_UTC.csv';
writetable(mmdata, fullfile(dbPath, filename));


