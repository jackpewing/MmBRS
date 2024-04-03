%%%%%% JPE 20240128 - some major problems %%%%%%
%%%%%%%%% but this will do for a timeseries plot for now %%%%%%%%%

%% load data, select only relevant variables
dbPath = ['G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\Stage_two\Combine_all\publication\20km_mask\all'];
filePath = fullfile(dbPath, 'Binned_data_MmBRS_UTC.mat');

load(filePath);

data = table2timetable(data);

%% Optional: Change detection radius for sPres
% data.sPres = data.minRange <= 25000 & data.minRange > 0; % 25 km detection radius

% sum of sPres for 40 km = 322,877 minutes
%                  30 km = 255,849 minutes
%                  25 km = 217,299 minutes
%                  20 km = 176,747 minutes
%                  10 km = 86,061  minutes
%% change some variable classes for now
data.MmPres = double(data.MmPres);
data.sPres = double(data.sPres);


%% Retime to hours

%countData = retime(data(:,'MmPres'), 'daily', 'sum');
countData = retime(data(:,{'MmPres';'MmEffort';'Ice_pc';'sPres'}), 'daily', 'sum');
maxData = retime(data(:,{'Ice_pc';'jd';'year'}), 'daily', 'max');
countData.Ice_pc = maxData.Ice_pc;
countData.jd = maxData.jd;
countData.year = maxData.year;
countData.MmEffort_perc = countData.MmEffort/1440;
countData.MmPres_eff_adj = countData.MmPres./countData.MmEffort_perc ;




%%

mmdata = timetable2table(countData);

idxeff = ~isnan(mmdata.MmPres_eff_adj);
mmdata = mmdata(idxeff,:);
save(fullfile(dbPath, 'dailybinned_40km_UTC'), 'mmdata')

filename = 'dailybinned_40km_UTC.csv';
writetable(mmdata, fullfile(dbPath, filename));
