
% 20230506 JMJ: updated thing to go get AIS data from various AIS file
% types for site, find discrete ship transits, interpolate data to some
% tbin (e.g. 5 sec)
% Drawn from runShipNoiseAIS.m JMJ setup shipAIS and LTSA shipNoise


% 20221005 ADD LOITERING FILTER
clearvars
close all
%%
%%%%%%%%%%%%%SET PARAMETERS FOR THIS RUN%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    GDriveLoc = 'G:\';
    % make sure M_Map folder is in MatlabPath (need for m_idist to get
    % distance from site to ship)
    addpath(genpath([GDriveLoc,...
        'Shared drives\SWAL_Arctic\code_repository\M_Map']))
    DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\'];
    startDate = datenum('5/15/2021 00:00:00');
    endDate = datenum('11/15/2021 00:00:00');

    proj = 'CANARC';
    sitenum = 4; % 1 = LowIsland, 2 = Tremblay, 3 = Guys Bight 4 = PI_HARP 5 = LI_HARP 6= MI_HARP 7 = EclipseSound center (150 km r)
    sites = {'ONC_LI','ONC_TS','ONC_GB','PI','LI','MI','ES'};
    site = char(sites(sitenum));
    %ENTER AIS FILE FORMAT%
    aisFileType = 5; 
        % enter 0=.dbf(2015), 1=.xlsx(2016-17,20), 2=.csv, 3=gov'tCANADA data 
        % 4=.xls (2018-19) 5=.csv (2021)

  
%%%%%% SETUP RUN PARAMS %%%%%%%%%%
%     getnewXwavTimes = 0; % 1= run getXWAVTimes  
    getnewAISdata = 1; %1 = reprocess get new AIS data with these params, 0= look for existing .mat
    getnewShipTransits = 1; % rerun ship txID for new ship transit array? 1 = yes

%     species = {'Ship','Icebreaker'}; % for ship detlog, (species = 'Ship' or 'Icebreaker')
    tbin = 5/60; % time bins (min) for interpolated ship data

    %%%%%Ship Transit Parameters
 if sitenum ==4
        rMax = 100000; % max radius between ships and rec sites for inclusion in analys.
        txrFltr = 100000; % filter radius for transit events
        cpaMax = 15000; % filter radius for including AIS hits in SL CPA events
        event_gaptime = 120; % threshold gap time between ship AIS events (min)

 elseif sitenum ==5 
        rMax = 60000; % max radius between ships and rec sites for inclusion in analys.
        txrFltr = 30000; % filter radius for transit events (smaller to avoid anchorage and port ops)
        cpaMax = 10000; % filter radius for including AIS hits in SL CPA events
        event_gaptime = 120; % threshold gap time between ship AIS events (min)
        
elseif sitenum ==6 
        rMax = 60000; % max radius between ships and rec sites for inclusion in analys.
        txrFltr = 30000; % filter radius for transit events (smaller to avoid anchorage and port ops)
        cpaMax = 10000; % filter radius for including AIS hits in SL CPA events
        event_gaptime = 120; % threshold gap time between ship AIS events (min)
        
  elseif sitenum ==7 % for center of Eclipse Sound AIS data analysis.
        rMax = 300000; % max radius between ships and rec sites for inclusion in analys.
        txrFltr = 150000; % filter radius for transit events
        cpaMax = 15000; % filter radius for including AIS hits in SL CPA events
        event_gaptime = 120; % threshold gap time between ship AIS events (min)

  end
%%%%%%%%%%%%%%%%%%%END SETUP PARAMS FOR RUN%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET SITELOCS AND XWAV TIMES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

 [ siteLocs, siteTimes, dataID,PA ] = getSiteLocs(startDate,endDate,DBPath,proj,site); 
 m_proj('lambert','lon', [siteLocs(2)-3 siteLocs(2)+3],...
        'lat', [siteLocs(1)-0.7 siteLocs(1)+0.7], 'direction', 'vertical', 'aspect', .5);
        set(gcf,'color','w')

yr = datevec(startDate);
%% GET AIS DATA
% if newAIS not selected, check for existing shipEvents.mat file
% otherwise, go make a new one from aisData
    if getnewAISdata ~= 1 
        fileChk = [DBPath,'\matFiles\',...
                 proj,'_',site,'_',num2str(yr(1)),'_shipEvents.mat'];
        if exist(fileChk,'file') ~= 0
            disp(['existing shipEvents.mat file found for ',...
                proj,'_',site,'_',num2str(yr(1)),' ...loading'])
            load(fileChk);
        else % get ais data and return shipEvents and txID data
            disp('no shipEvents file found. processing aisData.')
                [shipEvents, shipTable] = getAISshipData(DBPath,startDate,endDate,proj,...
                aisFileType, site, rMax); 
            
                % add params for this run to shipEvents          
                shipEvents.params.rMax = rMax;
                shipEvents.params.proj = proj;
                shipEvents.params.siteName = site;
                shipEvents.params.siteNum = sitenum;
                shipEvents.params.DBPath = DBPath;
                shipEvents.params.shipLookup(:,1) = eventIMO(~isnan(eventIMO));
                shipEvents.params.startDate = startDate;
                shipEvents.params.endDate = endDate;
                shipEvents.params.tbin = tbin;  
                shipEvents.params.cpaMax = cpaMax;
                shipEvents.params.txrFltr = txrFltr;
                shipEvents.params.event_gaptime = event_gaptime;


                save([DBPath,'\matFiles\',proj,'_',site,'_',num2str(yr(1)),'_shipEvents.mat'],...
        'shipEvents','shipTable','-mat');           
        end


    elseif getnewAISdata ==1
        disp(['processing ais data for ',proj,' ',site,' ',num2str(yr(1))])
        [shipEvents, shipTable] = getAISshipData(DBPath,startDate,endDate,proj,...
        aisFileType, site, rMax);  

            % add params for this run to shipEvents  
            shipEvents.params.rMax = rMax;
            shipEvents.params.proj = proj;
            shipEvents.params.siteName = site;
            shipEvents.params.siteNum = sitenum;
            shipEvents.params.DBPath = DBPath;
            
            % find unique shipIDs for lookup table. Fill in missing IMO
            % numbers if possible
            [uniqueShips,idx] = unique(shipEvents.num(:,2));
            shipEvents.params.shipLookup = [uniqueShips shipEvents.num(idx,3)];
            for ii = 1:length(uniqueShips)
                idxtmp = find(shipEvents.num(:,2)==uniqueShips(ii));
                idxnum = find(shipEvents.num(idxtmp,3)>1);
                if isempty(idxnum)
                    continue
                else
                IMO = shipEvents.num(idxtmp(idxnum(1)),3);
                shipEvents.params.shipLookup(ii,2) = IMO;
                end
            end
                       
            shipEvents.params.startDate = startDate;
            shipEvents.params.endDate = endDate;
            shipEvents.params.tbin = tbin;
            shipEvents.params.cpaMax = cpaMax;
            shipEvents.params.txrFltr = txrFltr;
            shipEvents.params.event_gaptime = event_gaptime;

            
        save([DBPath,'\matFiles\',proj,'_',site,'_',num2str(yr(1)),'_shipEvents.mat'],...
        'shipEvents','shipTable','-mat');
    end
    %  
%% GET SHIP TRANSITS
% now take culled AIS data and make interpolated ship transit cases (txID)
    if getnewShipTransits ~=1
    fileChk = [DBPath,'\matFiles\',...
                 proj,'_',site,'_',num2str(yr(1)),'_txID.mat'];
        if exist(fileChk,'file') ~= 0
            disp(['existing txID.mat file found for ',...
                proj,'_',site,'_',num2str(yr(1)),' ...loading'])
            load(fileChk);
        else
            disp(['no txID.mat file found for ',...
                proj,'_',site,'_',num2str(yr(1)),' ...generating'])
            [shipEvents,txID] = getAISshipTransits(shipEvents,shipTable);  
        
            shipEvents.params.txIDfieldnames = {'txID','txStart','txEnd','txDurn',...
            'shipName','shipTypeStr','mmsi','txNum','txInterpDist','dataID',...
            'PA','ltsaData','ltsaDnum','RL_BB','ltsaFreq'}; %,
        
            save([DBPath,'\matFiles\',proj,'_',site,'_',num2str(yr(1)),'_txID.mat'],...
            'txID','-mat');
        end
    elseif getnewShipTransits ==1
        [shipEvents, txID] = getAISshipTransits(shipEvents, shipTable);
        shipEvents.params.txIDfieldnames = {'txID','txStart','txEnd','txDurn',...
        'shipName','shipTypeStr','mmsi','txNum','txInterpDist','dataID',...
        'PA','ltsaData','ltsaDnum','RL_BB','ltsaFreq'}; %,
    
        save([DBPath,'\matFiles\',proj,'_',site,'_',num2str(yr(1)),'_txID.mat'],...
        'txID','-mat');
        save([DBPath,'\matFiles\',proj,'_',site,'_',num2str(yr(1)),'_shipEvents.mat'],...
        'shipEvents','shipTable','-mat');
    end

