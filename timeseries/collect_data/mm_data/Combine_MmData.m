%%%%% Create Timetable with All Mm Data %%%%%%

clear all

% set path to folder with code and output files (matfiles, figures/stats)
% GDriveLoc = 'E:\';
dataPath = ['I:\BRS_thesis\output\Mm_data\MmData_1min_yr'];
outDir = ['I:\BRS_thesis\output\Mm_dat\'];

[TT_files, TT_paths] = uigetfile([dataPath,'\*clickTable_Mm.mat'],'MultiSelect','on',...
        ['Select Mm Timetable Files for Desired Years']);
fileList = fullfile(TT_paths(1,:), TT_files(1,:));

load(fileList{1});
combined_TT = TT1;

for i = 1:numel(fileList)-1
    load(fileList{i+1});
    
    time_shift = combined_TT.Time(end) - TT1.Time(1);
    TT1.Time = TT1.Time+ time_shift;
    
    combined_TT = [combined_TT; TT1(2:end,:)];
end

combined_TT(end,:) = [];

save([outDir, 'All_MmData_1min.mat'], 'combined_TT')
    
    
    
    