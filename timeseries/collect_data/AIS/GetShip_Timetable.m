%%%%% GetShip_data %%%%% 

clearvars

dbPath = 'E:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\';
savePath = ['E:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\GetShip_Timetable\ShipEvents_40km\all_vars'];

proj = 'CANARC';
site = 'PI';
dpn = '02';
cluster = 'all';
deployment = [proj,' ',site, ' ',dpn];
startDate = datenum('01/00/2016 00:00:00');
endDate = datenum('01/00/2017 00:00:00');
ICImax = 1; % max ICI in sec.
rmax = 40000;
tbins = 1; % length of timetable bins (min)
effortFill = 0; % want to make a real effort array for this? yes = 1.
clicksFill = 0; % turn all counts and presence NaNs to 0? yes=1.
getLtsadata = 0; % go get new LTSA band levels for this time period (1)? Or load existing (0).

dt = tbins/24/60; % bin time step in min

shipLookup = readtable([dbPath,'Arctic_shiptxClicks\matfiles\',...
    proj,'_shipLOOKUP.xlsx']);

% for converting ship types to categorical numbered (1:16) (JPE 2023/05/10
% - NEED TO CHANGE IF THERE ARE NEW TYPES IN TxID, OTHERWISE WILL NOT RUN
shipTypes = {'Bulk Carrier', 'General Cargo', 'Tanker', 'Icebreaker',...
    'Tug', 'Passenger', 'Pleasure Craft', 'Fishing', 'SAR',...
    'Sailing', 'Military', 'Ro-Ro Cargo','Buoy Laying', 'Dredger',...
    'Research/Survey Vessel', 'Offshore Support Vessel', 'Deck Cargo Ship',...
    'Heavy Load Carrier', 'Towing', 'Buoy-laying Vessel', 'Unknown'};
 
% this is dumb. but get start year for desired timeframe. for gui getfile
 % prompt below
 yr = datevec(startDate);
yr = yr(1);

% make shipRL timetable
[shipfiles, shippath] = uigetfile([dbPath,'Arctic_shiptxClicks\matfiles\*.mat'],...
    'MultiSelect','on',['Select txID file for ',proj,' ',site,' ',num2str(yr)]);
shipfile = fullfile(shippath(1,:), shipfiles(1,:));

load(shipfile,'txID');

% loop through all ship transit events and get shipData.
% assemble into a timetable.

% Preallocate txdata
totalrows = 0;
for i = 1:size(txID,1)
    numrows = size(txID{i,8},1);
    totalrows = totalrows + numrows;
end
txdata = zeros(totalrows - size(txID,1), 7);

counter = 1;
    for ii = 1:size(txID,1) % loop through all tbinned ship transits
%         if~isempty(txID{ii,8}) % make sure there's data for this ship transit
            [cpar, cpaidx] = min(txID{ii,8}(:,8));
            for jj = 1:size(txID{ii,8},1)-1 % for each time bin in txptiles (1min)
                binstart = txID{ii,8}(jj); % get tbin start and end
                binend = txID{ii,8}(jj+1);
%                 clkidx = find(MTT>=binstart & MTT< binend); % find all clicks within that tbin
%                 count = length(clkidx);

                txdata(counter,1) = binstart; % bin start time
%                 txclicks(counter,2) = count; % n clicks in bin
                txdata(counter,2) = txID{ii,8}(jj,8); % range to ship
                txdata(counter,3) = txID{ii,8}(jj,7); % ship COG
                txdata(counter,4) = txID{ii,8}(jj,6); % ship SOG
                txdata(counter,5) = txID{ii,8}(jj,2); % ship MMSI
                txdata(counter,6) = txID{ii,8}(jj,12); % ship draft
%                txdata(counter,8) = txLtsa{ii,3}(jj); % ltsa SPLrms 5s
                
                if ~isempty(cpaidx)
                    if jj<=cpaidx
                        txdata(counter,7) = -1; % if pre-cpa, then aspect is -1 (bow)
                    else
                        txdata(counter,7) = 1; % if post-cpa, then aspect is 1 (stern)
                    end
                end
                counter = counter+1;
            end
            clear cpar cpaidx
        end
%     end

    % find any nans in the txdata array and remove them. transit bookends.
    idx = find(isnan(txdata(:,2)));
    txdata(idx,:) = []; % remove the nan entries

    % ship locs are out of time order when multiple ships are present. 
    % sort all locs by time
    [~, sortIdx] = sort(txdata(:,1),1);
    txdata = txdata(sortIdx,:);
    
    % get rid of empty transit entries
    idxtmp = find(isnan(txdata(:,3))); % find the nans
    txdata(idxtmp,:) = []; % remove those rows
    
    % convert times to datetime array                    
    ST0 = datetime(txdata(:,1),'ConvertFrom','datenum');
    
    % Assemble a binned time array for shipTransits. 
    % Make tempTime array from shipTimes, assign each time to a tbin.
    t1 = ST0;
    t1.Minute = tbins * floor(ST0.Minute/tbins);
    t1.Second = 0;
    S1 = unique(t1); % and here's the datetime array of unique t bins with clicks
  

% Preallocate txbin
txbin = zeros(size(S1,1), 9);

% wow. make a datenum array for bin ship results
txbin(:,1) = datenum(S1);
%     txbin = datenum(T1);
    % now go get some binned data from txclicks and assemble timetable
    
    for mm = 2:length(S1)
        % get idxs for current tbin
        idxtmp = find(txdata(:,1)>=txbin(mm,1) & txdata(:,1)< txbin(mm,1)+dt ...
            & txdata(:,2)<rmax);
         if isempty(idxtmp)
             txbin(mm,2:9) = nan;
             continue
        end
        txtmp = txdata(idxtmp,:); 
        [minr, idx] = min(txtmp(:,2)); % find minimum range in tbin  
        txbin(mm,2) = minr; % add min range to txbin
        txbin(mm,5) = txtmp(idx,5); % add MMSI of nearest ship
        shipidxs = find(txtmp(:,5)== txtmp(idx,5));
        txbin(mm,3) = mean(txtmp(shipidxs,3)); % add mean of closest ship COG
        txbin(mm,4) = mean(txtmp(shipidxs,4)); % add mean of closest ship SOG (kts)
        txbin(mm,6) = mean(txtmp(shipidxs,6)); % add mean of closest ship draft (m)
        txbin(mm,7) = floor(mean(txtmp(shipidxs,7))); % add mean of closest ship aspect   
        txbin(mm,8) = numel(unique(txtmp(:,5))); % number of overlapping ship transits this tbin
%        txbin(mm,10) = mean(txtmp(shipidxs,8)); % mean of RL across the bin (1min mean SPL)
%        txbin(mm,11) = max(txtmp(shipidxs,8)); % max of the 1 min RL
        
        % OK. now go get the ship type from the shipLOOKUP table
        % JPE 2023/05/10 - removed, too many types, can change in future
        % JPE 2023/06/26 - bringing back the types now !
        
        typeIdx = find(table2array(shipLookup(:,1))==txtmp(idx,5));
        if strcmp(table2array(shipLookup(typeIdx,3)),'Cargo')
            shipType = table2array(shipLookup(typeIdx,9));
        else
            shipType = table2array(shipLookup(typeIdx,3));
        end     
        txbin(mm,9) = find(strcmp(shipTypes,shipType));
    end
        
    % assemble timetable with variables  
    ST1 = array2timetable(txbin(:,2:9),'RowTimes',S1); % setup table for binMeans
%     ST2 = array2timetable(counts,'RowTimes',ST0); % setup table for binCounts
%     ST3 = array2timetable(MPP,'RowTimes',ST0); % setup table for binCounts

    % name columns in timetable
    ST1.Properties.VariableNames{1} = 'minRange';
    ST1.Properties.VariableNames{2} = 'COG_T';
    ST1.Properties.VariableNames{3} = 'SOG_kts';
    ST1.Properties.VariableNames{4} = 'MMSI';
    ST1.Properties.VariableNames{5} = 'draft_m';
    ST1.Properties.VariableNames{6} = 'aspect'; % -1=bow, 0=beam, 1=stern
    ST1.Properties.VariableNames{7} = 'n_ships'; % number of overlapping ship transit windows
    ST1.Properties.VariableNames{8} = 'shipType_cat'; % see shipTypes array for ship_categories
%    ST1.Properties.VariableNames{9} = 'SPLmean'; % this is the 1 min avg SPL 20Hz-4kHz
%    ST1.Properties.VariableNames{10} = 'SPLmax'; % 1 min max SPL 20Hz - 4 kHz


%%% Write Table %%%
   % turn into table
binDets = timetable2table(ST1);

%save it all
% filename = 'CANARC_PI_Shipdata_2020_40km.csv';
% writetable(binDets, fullfile(savePath, filename));

filename = 'CANARC_PI_Shipdata_2017_40km.mat';
save(fullfile(savePath, filename), 'ST1')

    