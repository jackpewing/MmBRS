% AIS PLOTTING CODE
% Josh Jones 3/5/2015
% Updated for Oceans North AIS data 2017037 JMJ
% updated for K Westdal mtg 20171113-20171116
% updated for .csv AIS raw data format JMJ 20180216
% Set params below
    % fileType
    % site
    % r_max, r_CPA, r_event
    % event_gaptime
    % t_bin
    % event_window

function [shipEvents, shipTable] = getAISshipData(DBPath,startDate,endDate,proj,...
        aisFileType, site, rMax)
    
yr = datevec(startDate);
yr = yr(1);
% % make sure M_Map folder is in MatlabPath
% addpath(genpath('G:\My Drive\JoshOffice\Code_repository\MATLAB_Working_Code\M_Map'))
% DBPath = 'G:\My Drive\JoshOffice\Research_projects\ARCTIC\ShipNoise';
% startDate = datenum('7/01/2018');
% endDate = datenum('11/01/2018');
% proj = 'CANARC'; % project name


%%%ENTER AIS FILE FORMAT%%%%%%%%%%%%%%%%%%%%%
%     aisFileType = 4; % enter 0=.dbf, 1=.xlsx 2, 3=gov'tCANADA data 4=daily .xls
% aisFileType = 5; % daily .csv files (2021)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%     site = 4; % 1 = LowIsland, 2 = Tremblay, 3 = Guys Bight 4 = PI_HARP
%     species = {'Ship','Icebreaker'}; % for ship detlog, (species = 'Ship' or 'Icebreaker')
%     rMax = 50000; % max radius between ships and rec sites for inclusion in analys.
%     rCPA = 3800; % filter radius for including AIS hits in CPA events
%     rEvent = 15000; % filter for radius of ship crossing events
%     eventGaptime = 180; % threshold gap time between ship AIS events (min)
%     
%     t_bin = 1; % time bins for ship and animal presence (min)
%     event_window = 180; % time window (min) around ship crossing events for ts analysis
%     e_win = event_window/t_bin; % number of time bins to include pre and post event
%     
 

%%%%%%ENTER ACOUSTIC TIME OFFSET FROM AIS%%%%
%     t_error_clock = 0; % GMT time error (in days) for acoustic data (1.08 for LI'15)
%     % 2015 LowIsland t_offset = 1.08 days
%     t_offset_GMT = 0; % time offset from local to GMT (e.g. PI time +4 = GMT) 
%     t_drift = 0; % daily clock drift in recorder (in days)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    shipEvents = struct;
    shipEvents.num = [];
    shipEvents.str = {};

    shipEvents.params.shipNumFieldnames = {'timeUTC', 'mmsi', 'imo',...
    'lat','lon','sog','cog','range_m','shipTypeNum','shipTypeNum',...
    'navstat','draught','length','width', 'txID'};   

 [ siteLocs, siteTimes, dataID,PA ] = getSiteLocs(startDate,endDate,DBPath,proj,site); 
 m_proj('lambert','lon', [siteLocs(1)-3 siteLocs(1)+3],...
        'lat', [siteLocs(2)-0.7 siteLocs(1)+0.7], 'direction', 'vertical', 'aspect', .5);
        set(gcf,'color','w')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%make variables for outputs
    timeUTC = [];
    Lat0 = [];
    Lon0 = [];
    MMSI0 = [];
    IMO0 = [];
    shipNam = [];
    SOG0 = [];
    COG0 = [];
    HDGT0 = [];
    navstat = [];
    draught = [];
    shipTypeNum = [];
    
    aisEvents = [];
    
if aisFileType == 4  
    % get AIS .xls file/s (2018-2019)
    [AIS_files, aisPath] = uigetfile([DBPath,...
        '\AISdata\EclipseSound\*.xls'],'MultiSelect','on',...
    ['Select AIS .xls files for ',proj,' ',num2str(yr)]);
    aisFiles = fullfile(aisPath(1,:),AIS_files(1,:));
    
elseif aisFileType == 5  
    % get AIS .xls file/s (2018-2019)
    [AIS_files, aisPath] = uigetfile([DBPath,...
        '\data\AISdata\EclipseSound\*.csv'],'MultiSelect','on',...
    ['Select AIS daily .csv files for ',proj,' ',num2str(yr)]);
    aisFiles = fullfile(aisPath(1,:),AIS_files(1,:));
    
elseif aisFileType == 1 
        % get AIS .xlsx file/s (2017)
    [AIS_files, aisPath] = uigetfile([DBPath,...
        '\AISdata\EclipseSound\*.xlsx'],'MultiSelect','on',...
        ['Select AIS .xlsx files for ',proj,' ',num2str(yr)]);
    aisFiles = fullfile(aisPath(1,:),AIS_files(1,:));
elseif aisFileType == 0
    [AIS_files, aisPath] = uigetfile([DBPath,...
    '\AISdata\EclipseSound\*.shx'],'MultiSelect','on',...
    ['Select AIS .shx files for ',proj,' ',num2str(yr)]);
    aisFiles = fullfile(aisPath(1,:),AIS_files(1,:));
end
 
for h = 1:numel(aisFiles)
    events = []; % make/clear array for aisData from this file 
    aisFile = aisFiles(h);

     disp(['Processing AIS file ',num2str(h),' of ',num2str(numel(aisFiles))])
%      disp(['Filename :',AIS_files{1,h}(12:20)])
%      disp(' ')
%      
    if aisFileType == 0 
        [pathstr, name, ext] = fileparts(char(aisFile));
        M = m_shaperead([pathstr(1,:),'\',name]); % creates a lovely struct of AIS data
        % Get ship positions and other meta data from struct
%         clearvars Lattmp Lontmp SOGtmp COGtmp bs
        Lat0 = cell2mat(M.latitude);
        Lon0 = cell2mat(M.longitude);
        SOG0 = cell2mat(M.sog);
        HDG0 = str2double(M.heading);
        if isnan(HDG0(1,1))
            disp('Oops. Irregular formatting of HDG data. Converting to double...')
            HDG0 = cell2mat(M.heading);
        end
        COG0 = str2double(M.cog);
        if isnan(COG0(1,1))
            disp('Oops. Irregular formatting of COG data. Converting to double...')
            COG0 = cell2mat(M.cog);
        else
        end
        MMSI0 = cell2mat(M.mmsi);
        shipNam = M.vessel_nam;
        shipTypeStr = M.vessel_typ;
        navstat = zeros(length(M.nav_status),1); % this comes as a string in data. need num.
        shipTypeNum = zeros(length(M.dbfdata),1);
        shipL = cell2mat(M.length);
        shipW = cell2mat(M.width);
        IMO = cell2mat(M.imo);
        draught = cell2mat(M.draught); % draught in 1/10 m (e.g. 55 = 5.5m)
        
        % clean up trailing whitespace in vessel names and ship type
        % first get rid of NaNs
        nans = cell2mat(cellfun(@(x)any(isnan(x)),shipNam,'UniformOutput',false));
        shipNam(nans) = {''}; %replace NaNs
        shipNam = strtrim(shipNam); % strtrim cuts the leading and trailing whitespace
        
        nans = find(cell2mat(cellfun(@(x)any(isnan(x)),shipTypeStr,'UniformOutput',false)));
        shipTypeStr(nans) = {''}; %replace NaNs
        shipTypeStr = strtrim(shipTypeStr); % strtrim cuts the leading and trailing whitespace

        % get times from AIS each struct and convert to datenums
        tmp_1 = char(M.ts_pos_utc(:,1));
        b = size(M.ts_pos_utc);
        if b>0
            clear time_str
            for i = 1:b(1,1)
                time_str(i,:) = ([tmp_1(i,1:4),'/',tmp_1(i,5:6),'/',tmp_1(i,7:8),...
                    ' ',tmp_1(i,10:11),':',tmp_1(i,12:13),':',tmp_1(i,14:15)]);
            end
            time_tmp = datenum(time_str);
            timeUTC = time_tmp;
        else
        end
        clear M
    else
        % read in .xls data for this file
%         [num,str,raw] = xlsread(char(aisFile)); % xlsread faster than readtable
        T = readtable(char(aisFile));
        T = sortrows(T,30); % sort this array by MMSI, then time  
    
    % Get ship positions and other meta data from struct
      if height(T)>1%numel(num)>1
            if aisFileType == 1
                Lat0 = table2array(T(:,17)); %num(:,17); %
                Lon0 = table2array(T(:,16)); %num(:,16); %
                SOG0 = table2array(T(:,18)); %num(:,18); %
                HDG0 = table2array(T(:,21)); %num(:,21); %
                COG0 = table2array(T(:,19)); %num(:,19); %
                MMSI0 = table2array(T(:,1)); %num(:,1); %
                if iscell(MMSI0) % dumb one-off fix
                    MMSI0 = cell2mat(MMSI0);
                end
                shipNam = table2array(T(:,3)); %str(2:end,3); %
                shipTypeStr = table2array(T(:,5)); %str(2:end,5); %
                shipTypeStr2 = table2array(T(:,29)); % vessel_type_main
                shipTypeStr3 = table2array(T(:,30)); % vessel_type_sub

                shipTypeNum = table2array(T(:,6)); %num(:,6);
                shipL = table2array(T(:,9)); %num(:,9); %
                shipW = table2array(T(:,10)); %num(:,10); %
                IMO = table2array(T(:,2)); %num(:,2); %
                if iscell(IMO) % dumb one-off fix
                    IMO = cell2mat(IMO);
                end
                navstat = table2array(T(:,23)); %num(:,23);
                draught = table2array(T(:,15)); %num(:,15);

                % clean up trailing whitespace in vessel names and ship type
                % first get rid of NaNs
                nans = find(cell2mat(cellfun(@(x)any(isnan(x)),shipNam,'UniformOutput',false)));
                shipNam(nans) = {''}; %replace NaNs
                shipNam = strtrim(shipNam); % strtrim cuts the leading and trailing whitespace

                % get times from AIS file and convert to datenums
                timeUTC = cellfun(@datenum,(table2cell(T(:,27))));
%             elseif aisFileType == 2 % need to fix this one for 2014 and prior data
%                 Lat0 = num(:,31);%table2array(T(:,20));
%                 Lon0 = num(:,30);%table2array(T(:,19));
%                 SOG0 = num(:,28);%table2array(T(:,21));
%                 HDG0 = num(:,33);%table2array(T(:,24));
%                 COG0 = num(:,32);%table2array(T(:,22));
%                 MMSI0 = num(:,2);%table2array(T(:,3));
%                 if iscell(MMSI0) % dumb one-off fix
%                     MMSI0 = cell2mat(MMSI0);
%                 end
%                 shipNam = raw(2:end,15);%table2array(T(:,5));
%                 shipTypeStr = raw(2:end,18);%table2array(T(:,7));
%                 shipTypeNum = num(:,18);%table2array(T(:,8));
%                 shipL = num(:,11);%table2array(T(:,11));
%                 shipW = num(:,12);%table2array(T(:,12));
%                 shipIMO = num(:,4);%table2array(T(:,4));
%                 if iscell(shipIMO) % dumb one-off fix
%                     shipIMO = cell2mat(shipIMO);
%                 end
%                 navstat = num(:,26);%table2array(T(:,26));
%                 draught = num(:,17);%table2array(T(:,17));
% 
%                 % clean up trailing whitespace in vessel names and ship type
%                 % first get rid of NaNs
%                 nans = find(cell2mat(cellfun(@(x)any(isnan(x)),shipNam,'UniformOutput',false)));
%                 shipNam(nans) = {''}; %replace NaNs
%                 shipNam = strtrim(shipNam); % strtrim cuts the leading and trailing whitespace
% 
%                 % get times from AIS file and convert to datenums
%                 timeUTC = datenum(str(2:end,30));%datenum(table2array(T(:,30)));
            elseif aisFileType == 4
                Lat0 = table2array(T(:,20)); %num(:,17); %
                Lon0 = table2array(T(:,19)); %num(:,16); %
                SOG0 = table2array(T(:,21)); %num(:,18); %
                HDG0 = table2array(T(:,24)); %num(:,21); %
                COG0 = table2array(T(:,22)); %num(:,19); %
                MMSI0 = table2array(T(:,3)); %num(:,1); %
                if iscell(MMSI0) % dumb one-off fix
                    MMSI0 = cell2mat(MMSI0);
                end
                shipNam = table2array(T(:,5)); %str(2:end,3); %
                shipTypeStr = table2array(T(:,7)); %str(2:end,5); %
                shipTypeStr2 = table2array(T(:,32));
                shipTypeStr3 = table2array(T(:,33));

                shipTypeNum = table2array(T(:,8)); %num(:,6);
                shipL = table2array(T(:,11)); %num(:,9); %
                shipW = table2array(T(:,12)); %num(:,10); %
                IMO = table2array(T(:,4)); %num(:,2); %
                if iscell(IMO) % dumb one-off fix
                    IMO = cell2mat(IMO);
                end
                navstat = table2array(T(:,26)); %num(:,23);
                draught = table2array(T(:,17)); %num(:,15);

                % clean up trailing whitespace in vessel names and ship type
                % first get rid of NaNs
                nans = find(cell2mat(cellfun(@(x)any(isnan(x)),shipNam,'UniformOutput',false)));
                shipNam(nans) = {''}; %replace NaNs
                shipNam = strtrim(shipNam); % strtrim cuts the leading and trailing whitespace

                % get times from AIS file and convert to datenums
                timeUTC = cellfun(@datenum,(table2cell(T(:,30))));
                
            elseif aisFileType == 5
                Lat0 = table2array(T(:,18)); %num(:,17); %
                Lon0 = table2array(T(:,17)); %num(:,16); %
                SOG0 = table2array(T(:,19)); %num(:,18); %
                HDG0 = table2array(T(:,22)); %num(:,21); %
                COG0 = table2array(T(:,20)); %num(:,19); %
                MMSI0 = table2array(T(:,2)); %num(:,1); %
                if iscell(MMSI0) % dumb one-off fix
                    MMSI0 = cell2mat(MMSI0);
                end
                shipNam = table2array(T(:,4)); %str(2:end,3); %
                shipTypeStr = table2array(T(:,6)); %str(2:end,5); %
                shipTypeStr2 = table2array(T(:,30));
                shipTypeStr3 = table2array(T(:,31));

                shipTypeNum = table2array(T(:,7)); %num(:,6);
                shipL = table2array(T(:,10)); %num(:,9); %
                shipW = table2array(T(:,11)); %num(:,10); %
                IMO = table2array(T(:,3)); %num(:,2); %
                if iscell(IMO) % dumb one-off fix
                    IMO = cell2mat(IMO);
                end
                navstat = table2array(T(:,24)); %num(:,23);
                draught = table2array(T(:,16)); %num(:,15);

                % clean up trailing whitespace in vessel names and ship type
                % first get rid of NaNs
                nans = find(cell2mat(cellfun(@(x)any(isnan(x)),shipNam,'UniformOutput',false)));
                shipNam(nans) = {''}; %replace NaNs
                shipNam = strtrim(shipNam); % strtrim cuts the leading and trailing whitespace

                % get times from AIS file and convert to datenums
                timeUTC = cellfun(@datenum,(table2cell(T(:,28))));
                
            else
            end
      else
          continue % num array from ais isempty, so jump to next ais file
      end
    end
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
% Get distances between sites and ship position 
    % M_IDIST APPROACH
    % ship distance (r) and bearing (a12) from Tremblay (T) at time t
        latchk_idx = find(Lat0(:,1)>89);
        Lat0(latchk_idx) = 89; %dumb fix for weird error with some locs having lats >90 deg
        
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%ADD THING HERE TO CHECK TimeUTC AND
        %%%%%%%%%%%%%%%%%%%%%%%%%%ASSIGN SITELOC FOR EACH SHIP POSITION
        
        % for each site loc and deployment, go get idxs in shipAIS with
        % corresponding times, calculate m_idist for thos, fill in r
        
        rRaw = nan(size(timeUTC,1),1); %preallocate for range array to match timeUTC
        for kk = 1:size(siteLocs,1) 
        
        idxtmp = find(timeUTC>=siteTimes(kk,1) & timeUTC<=siteTimes(kk,2));
            if isempty(idxtmp)
                continue
            end
            [rRaw(idxtmp), brg12, brg21] = m_idist(siteLocs(kk,2), siteLocs(kk,1),...
                Lon0(idxtmp), Lat0(idxtmp), 'wgs84');
        end
        
        % find Nans in ranges (where there was no recording time), fill in
        % with previous deployment location. Here just uses depl loc #1 as
        % the default for the 'no effort' case (times not recorded between deployments)
        idxtmp = find(isnan(rRaw));
        if ~isempty(idxtmp)
            for kk = 1:length(idxtmp) % for the ramaining AIS times with no recording
                % fill in lat, lon, r with locs from nearest deployment in
                % time
            [row,~] = ind2sub(size(siteTimes),nearestpoint(timeUTC(idxtmp(kk)),siteTimes));
            [rRaw(idxtmp(kk)), brg12, brg21] = m_idist(siteLocs(row,2), siteLocs(row,1),...
                Lon0(idxtmp(kk)), Lat0(idxtmp(kk)), 'wgs84');
            end
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        
        % find all radii less than r (m)
        idxs = find(rRaw(:,1)<=rMax); % get indices with ships within r_max
        tTmp = timeUTC(idxs); % times with ships w/in r (m) of Tremblay
        rTmp = rRaw(idxs); %distances at those times

%             if siteNum == 2 % get site (S) locs for Tremblay (T)
%                 [rRaw, brg12, brg21] = m_idist(lon_T, lat_T,...
%                     Lon0, Lat0, 'wgs84');
%                 % find all radii less than r (m)
%                 idxs = find(rRaw(:,1)<=rMax); % get indices with ships within r_max
%                 tTmp = timeUTC(idxs); % times with ships w/in r (m) of Tremblay
%                 rTmp = rRaw(idxs); %distances at those times
%             elseif siteNum == 1 
%             % same for Low Island (L)
%                 [rRaw, brg12, brg21] = m_idist(lon_L, lat_L,...
%                     Lon0, Lat0, 'wgs84');
%                 idxs = find(rRaw(:,1)<=rMax);
%                 tTmp = timeUTC(idxs); % times with ships within r (m) of site (S)
%                 rTmp = rRaw(idxs); %distances at those times
%             elseif siteNum == 4
%              % same for HARP PI (PI)
%                 [rRaw, brg12, brg21] = m_idist(lon_PI, lat_PI,...
%                     Lon0, Lat0, 'wgs84');
%                 idxs = find(rRaw(:,1)<=rMax);
%                 tTmp = timeUTC(idxs); % times with ships within r (m) of site (S)
%                 rTmp = rRaw(idxs); %distances at those times
%              elseif siteNum == 7
%              % same for EclipseSound (ES)
%                 [rRaw, brg12, brg21] = m_idist(lon_ES, lat_ES,...
%                     Lon0, Lat0, 'wgs84');
%                 idxs = find(rRaw(:,1)<=rMax);
%                 tTmp = timeUTC(idxs); % times with ships within r (m) of site (S)
%                 rTmp = rRaw(idxs); %distances at those times
%                           elseif siteNum == 5
%              % same for HARP LI (LI)
%                 [rRaw, brg12, brg21] = m_idist(lon_LI, lat_LI,...
%                     Lon0, Lat0, 'wgs84');
%                 idxs = find(rRaw(:,1)<=rMax);
%                 tTmp = timeUTC(idxs); % times with ships within r (m) of site (S)
%                 rTmp = rRaw(idxs); %distances at those times
%             end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get data for ship passing events where ships are within r_event of site
%     idxEvent = find(rRaw>0 & rRaw<=r_event);
            if ~isempty(idxs)
%                 shipName = shipNam(idxs); % ship name 'string'
%                 shipType = shipTypeStr(idxs);

                events(:,1) = timeUTC(idxs); %UTC time
                events(:,2) = MMSI0(idxs); % MMSI ship ID
                events(:,3) = IMO(idxs); % ship IMO number
                events(:,4) = Lat0(idxs); % AIS latitude
                events(:,5) = Lon0(idxs); % AIS longitude
                events(:,6) = SOG0(idxs); % AIS speed over ground (SOG)
                events(:,7) = COG0(idxs); % AIS course over ground (COG)
                events(:,8) = rRaw(idxs); % AIS radius (r; m) to recording site
                events(:,9) = shipTypeNum(idxs); % ship type
                events(:,10) = shipTypeNum(idxs);
                events(:,11) = navstat(idxs);
                events(:,12) = draught(idxs); % ship draught (m)
                events(:,13) = shipL(idxs); %ship length (m)
                events(:,14) = shipW(idxs); % ship width (m)
                events(:,15) = HDG0(idxs); % ship hdg T

            %     shipEvents = sortrows(shipEvents,[2 1]); % sort this array by MMSI, then time
                shipEvents.num = [shipEvents.num; events];
                % ok. check for empty ship type cols as double. make empty
                % cell arrays to match dimension of shipTypeStr
                if isa(shipTypeStr2,'double')
                    shipTypeStr2 = cell(length(shipTypeStr2),1,1);
                end
                if isa(shipTypeStr3,'double')
                    shipTypeStr3 = cell(length(shipTypeStr3),1,1);
                end

                shipEvents.str = [shipEvents.str; [shipNam(idxs), ...
                    shipTypeStr(idxs),shipTypeStr2(idxs), shipTypeStr3(idxs)]];
            else
            end
end

% sort the shipEvents struct by time
        [shipEvents.num, sortIdx] = sortrows(shipEvents.num,1);
        shipEvents.str = shipEvents.str(sortIdx,:);

% make shipLOOKUP table for all ships in this shipEvent file
    IMO = [];
    MMSI = [];
    shipTable = struct;
% find unique MMSI numbers in array
    [shipTable.num(:,1),ia,ic] = unique(shipEvents.num(:,2)); 
    idx = find(~isnan(shipTable.num(:,1)));
    ia = ia(idx);
% save info for these unique ships in file
    shipTable.num(:,1) = shipTable.num(idx,1);
    shipTable.num(:,2) = shipEvents.num(ia,3);
    shipTable.num(:,3) = shipEvents.num(ia,13);
    shipTable.num(:,4) = shipEvents.num(ia,14);
    shipTable.str = shipEvents.str(ia,:);
 % save fieldnames for future reference 
    shipEvents.params.shipTableNumFields = {'MMSI','IMO','length(m)','width(m)'};
    shipEvents.params.shipTableStrFields = {'shipNam','shipType1','shipType2','shipType3'};
        shipEvents.params.numFieldnames = ...
                {'datenum','mmsi','imo','lat','lon','sog','cog','r','shipTypeNum',...
                'shipTypeNum', 'navStat','draught','length','width','hdg'};
        shipEvents.params.strFieldnames = {'shipName','shipType1','shipType2','shipType3'};
end
% save([DBPath,'\output\',...
%          proj,'_',siteName,'_',num2str(yr),'_shipEvents.mat'],'shipEvents','-mat');

     