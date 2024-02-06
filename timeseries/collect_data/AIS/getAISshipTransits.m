function [shipEvents, txID] = getAISshipTransits(shipEvents,shipTable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get individual ship crossing events, assemble ts and interpolate between
% AIS gaps. Perform cross correlation analysis? 

yr = datevec(shipEvents.params.startDate);
yr = yr(1);
DBPath = shipEvents.params.DBPath;
proj = shipEvents.params.proj;
site = shipEvents.params.siteName;

% sort the shipEvents struct by time
[shipEvents.num, sortIdx] = sortrows(shipEvents.num,1);
shipEvents.str = shipEvents.str(sortIdx,:);

% find unique MMSI values in 'events'
eventMMSI = shipTable.num(:,1); 

% setup counter for shipTransit ID (txID)
counter = 0;
shipEvents.num(:,16) = zeros(length(shipEvents.num),1);

for ii = 1:numel(eventMMSI)
    MMSIship = eventMMSI(ii);
    idxShip = find(shipEvents.num(:,2) ==  MMSIship); % get indices for locs of ship ii
    idxTmp = shipEvents.num(idxShip,8)<=shipEvents.params.txrFltr;
    idxShip = idxShip(idxTmp);
  if isempty(idxShip)
        continue
  else
    % find duplicate times or out of order
    badLocs = diff(shipEvents.num(idxShip,1))<=0;
    idxShip(badLocs) = []; %remove duplicates
    
    % find gap times between crossing events where t(i+1)-t(i)>60 min.
    gaps = find(diff(shipEvents.num(idxShip,1))> shipEvents.params.event_gaptime/60/24);
    disp(datestr(shipEvents.num(idxShip(gaps),1)))
    disp(' ')
    disp(datestr(shipEvents.num(idxShip(gaps+1),1)))
    
    % make variable for new event start and end idxs in idxs for this mmsi
    % (idxShip)
    eventIdx = [];
    if numel(gaps)==0
        eventIdx(1,1) = 1;
        eventIdx(1,2) = length(idxShip);
    else
    % ship_idx(gaps) gives case_ts times of event gaps
    % now get start and end indices for events in case_ts
        eventIdx = zeros(length(gaps)+1,2);
        eventIdx(1,1) = 1;
        eventIdx(1,2) = gaps(1);
        eventIdx(2:end-1,2) = gaps(2:end);
        eventIdx(end,2) = length(idxShip);
        eventIdx(2:end,1) = gaps(1:end)+1;
    end
    
    % now loop through events and get all shipEvents.num idxs for ea txID
    % write the txID to column 16 in shipEvents.num array
    for jj = 1:size(eventIdx,1)
        counter = counter+1; % this is the new txID
        txLocs = idxShip(eventIdx(jj,1):eventIdx(jj,2));
        % write txID jj to all locs for mmsi ii
        shipEvents.num(txLocs,16) = counter; 
    end
  end
end

% remove duplicate shipLocs from mmsiTxID process above
    badLocs = find(shipEvents.num(:,16)==0); % col16 is txID from getShipAISdata.m
    shipEvents.num(badLocs,:) = [];
    shipEvents.str(badLocs,:) = [];
  
% create cell array for transit data (txID)
    vals = [];
    [vals(:,1),ia,ic] = unique(shipEvents.num(:,16)); 
    txID = cell(length(vals),4);
    txID(:,1) = num2cell(vals);
    
    startTimes = [];
    endTimes = [];


% loop through each transit ID and do stuff
for jj = 1:length(txID)
    txIdx = find(ic(:,1) == jj);
    if length(txIdx)>1
        txData = shipEvents.num(txIdx,:);
        txShipName = shipEvents.str(txIdx,1);
        txShipType = shipEvents.str(txIdx,2);

        % now get event durn (days)
        txID(jj,4) = num2cell(txData(end,1)-txData(1,1));
        txID(jj,5) = txShipName(1);
        txID(jj,6) = txShipType(1);
        txID(jj,7) = num2cell(txData(1,2)); % MMSI number of this ship
  
         % make event time array with handles
        dnumMin = shipEvents.params.tbin/60/24;
        % make 1-min t-res array for txID(jj) with handles of length txWin
%         txTS = txData(1,1)-shipEvents.params.txWin*dnumMin:dnumMin:txData(end,1)+ ...
%             shipEvents.params.txWin*dnumMin; % with handles
        txTS = txData(1,1):dnumMin:txData(end,1); % without handles
        txTS = txTS';

        % find start and end times for ship event and indices within
        % the event time window
        st = txData(1,1);
        stidx = nearestpoint(st,txTS); % ship start index in event_ts
        et = txData(end,1);
        etidx = nearestpoint(et,txTS); %ship end index in event_ts 

        % now interpolate missing values in the event ts
            vq = interp1(txData(:,1),txData(:,2:15),txTS(stidx:etidx,1));
            txTS(stidx:etidx,2:15) = vq;
        % now add the jj(th) transit data to the txID cell array for txID(jj)
        txID(jj,8) = {txTS};
        txID(jj,2) = num2cell(st);
        txID(jj,3) = num2cell(et);

    % OK. now check the shipLOOKUP table to see if this ship needs to
    % be added.

    % First open the shipLOOKUP table for this site. Add to it if ship is missing.
    file = [DBPath,'\matFiles\',proj,'_shipLOOKUP.xlsx'];
    shipLookup = readtable(file);   

        shipIdx = find(cell2mat(txID(jj,7))==table2array(shipLookup(:,1)));
        if isempty(shipIdx)
        %APPEND SHIP INFO TO TABLE IF NOT FOUND
            disp(['ALERT. Transit ',num2str(jj),' ship not in lookup table. adding ',...
                char(txID(jj,5))]);
            clear shipTmp
            shipTmp = cell(1,9);
            shipTmp(1:3) = [txID(jj,7),txID(jj,5) txID(jj,6)];
            shipTmp(4:5) = num2cell(txID{jj,8}(1,13:14));
            shipTmp(6:9) = num2cell([0,0,0,0]);
            shipLookup = [shipLookup;shipTmp];
    
            writetable(shipLookup,file);
            clear shipLookup
        end
    else

    end
end

% get rid of empty txID rows
idx = find(cellfun(@isempty,txID(:,2)));
txID(idx,:) = [];
% now reorganize and rename txID by startTime
st = cell2mat(txID(:,2));
[st, sortIdx] = sortrows(st,1);
txID = txID(sortIdx,:);
% renumber txID in order of start time
txID(:,1) = num2cell(1:1:size(txID,1));
% 
% txIDfieldnames = {'txID','txStart','txEnd','txDurn',...
%     'shipName','shipTypeStr','IMO','txNum'};

shipNum = [];


txcpaNum = [];
% let's get all cpa times to find nearby ships, remove empties
  for ii = 1:size(txID,1)
%     shipIdx = find(table2array(shipLookup(:,1))==cell2mat(txID(ii,7)));
    if isempty(txID{ii,8})
        continue
    end
    [cpatmp, idxtmp] = min(txID{ii,8}(:,8)); % get cpa radius and index
    dnumtmp = txID{ii,8}(idxtmp,1);
    if ~isempty(cpatmp)
        txcpaNum = [txcpaNum; [ii dnumtmp cpatmp]];
    end
  end  
  
% find only txIDs that pass within txr_fltr radius of the recorder
  idx = find(txcpaNum(:,3)<shipEvents.params.txrFltr & ...
      txcpaNum(:,2)>shipEvents.params.startDate & ...
      txcpaNum(:,2)<shipEvents.params.endDate);
  txIDcull= txID(txcpaNum(idx,1),:);
  [cnt_unique, unique_a] = hist(cell2mat(txIDcull(:,7)),unique(cell2mat(txIDcull(:,7))));

% write a ship lookup table for this site_yr
% Would be good here to make a thing to check Lloyd's for additional ship
% info
    varnames = {'mmsi','shipName','shipType','L','W','GRT',...
        'DWT','buildYr','shipType2','shipType3','n_transits'};
    for jj = 1:numel(unique_a)
        mmsi = unique_a(jj);
        [idx, ~] = find(mmsi == shipTable.num(:,1));
        shipTable.num(idx,5) = cnt_unique(jj);
    end
    
  
    file2 = ([DBPath,'\matFiles\',proj,...
        '_',site,'_',num2str(yr),'_shipTable.xlsx']);
    shipLookup = cell(height(shipTable.num)+1,11);
    shipLookup(1,:) = varnames;
    shipLookup(2:end,1) = num2cell(shipTable.num(:,1));
    shipLookup(2:end,2) = shipTable.str(:,1);
    shipLookup(2:end,3) = shipTable.str(:,2);
    shipLookup(2:end,4:5) = num2cell(shipTable.num(:,3:4));
    shipLookup(2:end,9:10) = shipTable.str(:,3:4);
    shipLookup(2:end,11) = num2cell(shipTable.num(:,5));
    
    writecell(shipLookup,file2);

end
