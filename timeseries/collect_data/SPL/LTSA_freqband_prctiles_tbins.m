
% 20221214 Get data from a set of ltsa files for a time period and df. Make ptiles
% and plots for timeperiod (SPL, SPSL) save as a .mat file.

% function [ hourly_ptiles56_70,hourly_avgs56_70, dets ] = LTSA_freqband_prctiles_20200929()

clearvars
global PARAMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. set path etc variables
% 2. don't forget to specify duty cycle durn and period
% 3. also enter the project and site info for filenames, plot titles, etc.
% 4. enter the start and end dates of interest
% 5. enter pte
% 6. make sure there's a logger output excel detFile with ship start/ends
% 7. oh also, this currently only works on LTSA files with 1Hz bins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% setup run params
    GDriveLoc = 'G:\';
    DBPath = [GDriveLoc,'Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks'];
%     LTSApath = '\\frosty\LTSA';
% make a big folder containing all ltsa files per df for each site
   LTSApath = 'D:\LTSA';

    
    proj = 'CANARC';
    sitenum = 4; % 1 = LowIsland, 2 = Tremblay, 3 = Guys Bight 4 = PI_HARP 5 = LI_HARP 6= MI_HARP 7 = EclipseSound center (150 km r)
    sites = {'ONC_LI','ONC_TS','ONC_GB','PI','LI','MI','ES'};
    site = char(sites(sitenum));
    
    proj = 'CANARC';
    site = 'PI';
    depl = '2016';
    
    %start and end datetimes for this run
% %     start and end datetimes of large time periods (e.g. months)for this run
%     startDates = {'6/01/2016' '7/01/2016' '8/01/2016' '9/01/2016' ...
%         '10/01/2016' '11/01/2016' '12/01/2016' ...
%         '1/01/2017' '2/01/2017' '3/01/2017' '4/01/2017' ...
%         '5/01/2017' '6/01/2017' '7/01/2017' '8/01/2017' '9/01/2017' ...
%         '10/01/2017' '11/01/2017' '12/01/2017' '1/01/2018'};
%     endDates = {'6/30/2016' '7/30/2016' '8/30/2016' '9/30/2016' ...
%         '10/30/2016' '11/30/2016' '12/30/2016' ...
%         '1/30/2017' '2/26/2017' '3/30/2017' '4/30/2017' ...
%         '5/30/2017' '6/30/2017' '7/30/2017' '8/30/2017' '9/30/2017' ...
%         '10/30/2017' '11/30/2017' '12/30/2017' '1/30/2018'};

% Inuit seasons approach
%     startDates = {['5/15/',depl] ['7/15/',depl] ['9/15/',depl]};
%     endDates = {['7/15/',depl] ['9/15/',depl] ['11/15/',depl]};
    startDates = {['1/1/',depl]};
    endDates = {['12/31/',depl]};

    
%     %start and end datetimes of large time periods (e.g. months)for this run
%     startDates = {'10/01/2018' '11/01/2018' '12/01/2018' ...
%         '1/01/2019' '2/01/2019' '3/01/2019' '4/01/2019' ...
%         '5/01/2019' '6/01/2019' '7/01/2019' '8/01/2019' ...
%         '9/01/2019' '10/01/2019' '11/01/2019' '12/01/2019' ...
%         '1/1/2020' '2/01/2020' '3/01/2020' '4/01/2020' ...
%         '5/01/2020' '6/01/2020' '7/01/2020' '8/01/2020'};
%     endDates = {'10/30/2018' '11/21/2018' '12/30/2018' ...
%         '1/30/2019' '2/26/2019' '3/30/2019' '4/30/2019' ...
%         '5/30/2019' '6/30/2019' '7/30/2019' '8/30/2019' ...
%         '9/21/2019' '10/30/2019' '11/21/2019' '12/30/2019' ...
%         '1/30/2020' '2/26/2020' '3/30/2020' '4/30/2020' ...
%         '5/30/2020' '6/30/2020' '7/30/2020' '8/30/2020'};
   
   % decimation factor for this run
    dfs = [1 20 100];
    % do we want to use only one tf for all ltsa files? (1=yes, 0=no)
    oneTF = 1;
    oneDutyCyc = 1; %if 0, enter duty cycle each ltsa file.
    % getLtsadata set to: get one set of ltsa files at start (2), 
    % or pick by month/season (1), or use existing outputs (0)
    getLtsadata = 1; 
    
    %%%%%%ENTER ACOUSTIC TIME OFFSET FROM AIS%%%%
    t_error_clock = 0; % GMT time error (in days) for acoustic data (1.08 for LI'15)
    % 2015 LowIsland t_offset = 1.08 days
    t_offset_GMT = 0; % time offset from local to GMT (e.g. PI time +4 = GMT) 
    t_drift = 0; % daily clock drift in recorder (in days)

    dets = [];
    threshold = -100; %before TF
    ptiles = [1 5 10 50 90 95 99]; % percentiles to calculate
    recDurn = 1; % on sec in duty cycle (s)
    recPer = 1; %rec period in duty cycle (s)
%     recDurn = 1600; % on sec in duty cycle (s)
%     recPer = 1800; %rec period in duty cycle (s)
    dty_cyc = (recDurn/recPer)*60; % recording minutes per hour 
        %e.g. 10/12 min duty cycle -> 50 rec min/hr)
        %e.g. 600/900 sec = 40 rec min/hr
    % rf_length = 470; %length of raw files for 32 kHz data (sec)
    rf_length = 75; %length of raw files for 200 kHz data (sec)
    tbins = 1; % durn of time bins for output (minutes)
    % NOTE: raw file length 75s * 4 files = 5 minutes. Nice even number for
    % time averages.
    
    SPLbounds = [60 140]; % ylims for SPL 1-min plot
    SPSLbounds = [30 110]; % ylims for SPSL plots
    LTSAbounds = [40 90]; % clims for imagesc LTSA plot
    SURFzbounds = [30 120]; % z lims for surf LTSA plot
    SURFcbounds = [40 110]; % c lim for surf LTSA plot
    FS = 16; % font size for plots
    FS2 = 24;
    
    plots = 1; % yes or no make new plots

    % set some frequency bands of interest
        N = 3; % 1/N octqave freq bands calculated
        min_f = 5; %lowest freq to calculate 1/N oct bands
        max_f = 100000; %upper freq for 1/N oct freq bands
 
    if oneTF == 1
    % select and load tf file for this ltsa
%     [ tf_file, tf_path ] = uigetfile('G:\Shared drives\MBARC_TF\*.tf',...
%         ['Pick Transfer Function for file ', proj,' ',site,' ',depl]);
    [ tf_file, tf_path ] = uigetfile('D:\tf_files\*.tf',...
        ['Pick Transfer Function for file ', proj,' ',site,' ',depl]);
    end
    
    % compute 1/N oct bands using fract_oct_freq_band.m (put in path)
    [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(N, min_f, max_f);
    
  % make a giant giant loop to run this script for each month in date range  
  for dfidx = 1:length(dfs)
      df = dfs(dfidx);
%       df = 1;
          
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % let's set some freq bands of interest for each decimation factor
   if df==100
        % interpolate between FIFO frequencies on for <4 kHz
        FIFOinterp = 1; % 1=yes, 0=no
        % set freq range for LTSA data to analyze
        frqBand1 = [5 500];
        fstep = 1; % freq bin size for this decimation factor (df100 = 1Hz)
        f2 = 63; % center of 1/3 oct band representing ship propeller
        f3 = 125; % center of 1/3 oct band representing ship noise, bowhead, and ringed seal growl
        f4 = 250; % center of 1/3 oct. band representing ringed seal bark, bowhead
   elseif df==20
        % interpolate between FIFO frequencies on for <4 kHz
        FIFOinterp = 1; % 1=yes, 0=no
        % set freq range for LTSA data to analyze
        frqBand1 = [500 4000];
        fstep = 10; % freq bin size for this decimation factor (df20 = 10Hz)
        f2 = 1000; % center freq for band 2 representing narwhal burstPulse calls
        f3 = 2500; % center freq for band 3 narwhal whistle lowe        
        f4 = 3150; % center freq for band 4 representing narwhal whistle upper
    else %df=1
        % interpolate between FIFO frequencies off for >4 kHz
        FIFOinterp = 0; % 1=yes, 0=no
        % set freq range for LTSA data to analyze
        frqBand1 = [4000 100000];
        fstep = 100; % freq bin size for this decimation factor (df1 = 100Hz)
        f2 = 6000; %  sperm whale clicks, narwhal upper whistles
        f3 = 22500; % narwhal clicks 120-130 dB peak
        f4 = 55600; % narwhal clicks 140-150 dB peak
    end

    % Now set 1/3 oct band upper, lower, center freqs for f2, f3, f4
    fcidx = nearestpoint([f2, f3, f4],fexact_l_c_u(:,2));
    frqBand2 = fnominal_l_c_u(fcidx(1),1):fstep:fnominal_l_c_u(fcidx(1),3); 
    frqBand3 = fnominal_l_c_u(fcidx(2),1):fstep:fnominal_l_c_u(fcidx(2),3); 
    frqBand4 = fnominal_l_c_u(fcidx(3),1):fstep:fnominal_l_c_u(fcidx(3),3); 
    f2 = fnominal_l_c_u(fcidx(1),2); %  f2 nominal center freq
    f3 = fnominal_l_c_u(fcidx(2),2); % f3 nominal center freq
    f4 = fnominal_l_c_u(fcidx(3),2); % f4 nominal center freq

     % interpolate between FIFO frequencies?
    fifoFreqs = 50:50:frqBand1(end)-50;
    frqBand1 = frqBand1(1):fstep:frqBand1(end);
    
    if getLtsadata == 2        
        % number of raw files per hour according to duty cycle
        rf_per_hr = dty_cyc*60/rf_length;% (e.g. 50 min/hr * 60 sec/min/75 sec/rf = 40 rf)
        % select df
        [ltsa_files, ltsa_paths] = uigetfile([LTSApath,'\',site,'\*.ltsa'],'MultiSelect','on',...
            ['Select LTSA files for ',proj,' ',site,' ',depl,...
            'df ',num2str(df)]);
        % [ tf_file, tf_path ] = uigetfile('D:\tf_files\*.tf','Pick Transfer Function');
        ltsas = fullfile(ltsa_paths(1,:), ltsa_files(1,:));
    end
    
  for month=1:numel(startDates)
%    for month=1
        startDate = datenum(startDates{month});
        endDate = datenum(endDates{month});
        
        disp(['starting month/season ',datestr(startDate)])
%         startDate = datenum(startDates{23});
%         endDate = datenum(endDates{23});
        
        % make start and end dates in ltsa trime format
            ltsa_startDate = startDate - datenum([2000 0 0 0 0 0]);
            ltsa_endDate = endDate - datenum([2000 0 0 0 0 0]);

    if getLtsadata == 1        
        % number of raw files per hour according to duty cycle
        rf_per_hr = dty_cyc*60/rf_length;% (e.g. 50 min/hr * 60 sec/min/75 sec/rf = 40 rf)

        [ltsa_files, ltsa_paths] = uigetfile([LTSApath,'\',site,'\*.ltsa'],'MultiSelect','on',...
            ['Select LTSA files for ',proj,' ',site,' ',depl,...
            ' month ',datestr(startDate,'yyyy-mm'),' df ',num2str(df)]);
        % [ tf_file, tf_path ] = uigetfile('D:\tf_files\*.tf','Pick Transfer Function');
        ltsas = fullfile(ltsa_paths(1,:), ltsa_files(1,:));
    end
        % read deployment, proj, site info from LTSA file
    %     sites = char(ltsas);
    %     proj = sites(1,end-36:end-31);
    %     site = sites(1,end-29:end-28);
    %     depl = sites(1,end-26:end-25); 
%         startdvec = datevec(T1(1)); % datevec of period start

    if ischar(ltsas)
        ltsa_files = 1; % one in length
    end

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Set up output variables
        tbinPtiles1 = [];
        tbinPtiles2 = [];
        tbinPtiles3 = [];
        tbinPtiles4 = [];
        % these will have the tbin average and max dBsum values for all tbins
        bindbBand1 = []; 
        bindbBand2 = [];
        bindbBand3 = [];
        bindbBand4 = [];

        tbinStarts = [];
        tbinDnums = []; % this will contain all tbin times for bindB measurements
        ltsaCull = [];
        dnums_ltsaCull = [];

    % open and read LTSA file/s
    for file = 1:length(ltsa_files)

        % just in case we're doing something with duty cycle and this ltsa
        % file/deployment was different than others in set
        if oneDutyCyc == 0
            recDurn = input('Enter on sec in duty cycle (s)');
            recPer = input('Enter rec period in duty cycle (s)');
            dty_cyc = (recDurn/recPer)*60; % recording minutes per hour 
            rf_per_hr = dty_cyc*60/rf_length;% (e.g. 50 min/hr * 60 sec/min/75 sec/rf = 40 rf)
        end

    %     temp_100 = [];
    %     tmp_band2 = [];
    %     tmp_band3 = [];
    %     tmp_band4 = [];
        tmp_tbinStart = [];

        if iscell(ltsa_files) % more than one
            ltsa = char(ltsas{file});
        else
            ltsa = char(ltsas);
        end
        fid = fopen(ltsa,'r');
        disp(['opening ltsa, ',ltsa])
        
        PARAMS = read_ltsahead_so(ltsa);
        
%     % SETUP TIME ARRAY FOR FINAL OUTPUT
        % get total tbins bins in ltsa file
        % make datetime array for tbins covering the whole ltsa file
            % % Get start and end times from LTSA file and create num_hours
        % change date to -2000 yrs to match ltsa dnums
        rf_starts = PARAMS.ltsa.dnumStart; 
        rf_ends = PARAMS.ltsa.dnumEnd;
        
        t1 = (datenum(startDate):tbins/60/24:datenum(endDate))';
        T1 = datetime(t1,'ConvertFrom','datenum');

        % make sure there's only one entry per minute
        T1 = unique(T1); % and here's the datetime array of unique 5min bins with clicks

    % if only one tf file option selected, skip. otherwise select tf
    % for this ltsa
        if oneTF == 0
%     % select and load tf file for this ltsa
        [ tf_file, tf_path ] = uigetfile('G:\Shared drives\MBARC_TF\*.tf',...
            ['Pick Transfer Function for file ', ltsa(end-36:end-5)]);
        end

        loadTF(fullfile(tf_path,tf_file));
        [C,ia,ic] = unique(PARAMS.tf.freq); % ia is idx of unique tf freqs in PARAMS
        if length(ia) ~= length(ic) 
            disp(['Error: TF file ',tf_file,' is not monotoically increasing'])
        end
        tf_freq = PARAMS.tf.freq(ia); % freq of tf at idx ia in PARAMS
        tf_uppc = PARAMS.tf.uppc(ia); % value of tf at idx ia in PARAMS

    % Transfer function correction vector
        %interpolate values of tf onto freq vector in ltsa file
        Ptf = interp1(tf_freq,tf_uppc,PARAMS.ltsa.freq,'linear','extrap');    
        Ftf = PARAMS.ltsa.freq;

    % let's find raw file idxs with start and end within specified date range  
         rf_idxs = find(PARAMS.ltsa.dnumStart >= ltsa_startDate & ...
              PARAMS.ltsa.dnumEnd < ltsa_endDate);

    % let's try preallocating an array bigger than we will need for ltsa data
    % within date range for this file
        tmp_band1 = nan(length(frqBand1),length(rf_idxs)*15);
        tmp_band2 = nan(length(frqBand2),length(rf_idxs)*15);
        tmp_band3 = nan(length(frqBand3),length(rf_idxs)*15);
        tmp_band4 = nan(length(frqBand4),length(rf_idxs)*15);
%         tmp_band1 = [];

        tmp_dnums = [];
%         tmp_dnums = nan(length(rf_idxs)*15);

         % skip this ltsa if no target dates in it
         if isempty(rf_idxs)
             continue
         end
         rfcounter = 1;
          for rf=rf_idxs(1):(rf_idxs(end)-1)
              fseek(fid,PARAMS.ltsa.byteloc(rf),'bof');

              % get the ltsa data for all time bins in raw file rf (e.g. 15 5
              % sec ltsa t bins per 75 s raw file
              data = fread(fid, [ PARAMS.ltsa.nf PARAMS.ltsa.nave(rf) ], '*int8');

              % get time avg for this ltsa file, conv to dnum
              rfTBinDurn = PARAMS.ltsa.tave/60/60/24; 

              %get durn of raw file and of time bins from PARAMS
              rf_tbins = PARAMS.ltsa.nave(rf); 

              % get dnum starttime of each tbin in this raw file
              rf_tbinStarts = PARAMS.ltsa.dnumStart(rf):...
                  rfTBinDurn:PARAMS.ltsa.dnumStart(rf)+(rf_tbins-1)*rfTBinDurn;

              %remove 25% of beginning time bins from rf to remove disk writes
              cullStart = round(rf_tbins*.25);   
              cullEnd = 2; % just remove 2 tbins from end of raw file (make smarter later)

              %apply TF to all time bins in data
              a = size(data); %first check to see if the hour is truncated (e.g. last hour cutoff)
              if a(1,2)>=(rf_tbins-cullStart) % if there are enough time bins in data for filter size (cull) do thing

                  % Now remove disk writes from start and end of raw file (ltsa
                  % tbin 5s rf length 75 s, 15 tbins in rf. remove first and
                  % last n=cull bins
                  datac = double(data(:,cullStart:(rf_tbins-cullEnd))) + Ptf'*ones(1,...
                      (rf_tbins-cullStart-cullEnd+1));
                    % get dnumStarts for all tbins selected for this rf
                  tbinStartc = rf_tbinStarts(cullStart:(rf_tbins-cullEnd));

                  % get start idx to fill in ltsacull array (matches
                  % tbinStarts length from prev loop plus one)
                  rfidxstart = length(tmp_dnums)+1;

                  % append dnumStarts onto ltsaDnums
                    tmp_dnums = [tmp_dnums tbinStartc];
%                     tmp_dnums(rfidxstart:rfidxend) = tbinStartc;

                  % get end idx to fill in ltsacull array (matches
                  % tbinStarts)
                  rfidxend = length(tmp_dnums);

                % Find the idxs of freqBand1 start and end freqs in the LTSA freq array
                    sfIdx1 = nearestpoint(frqBand1(1),Ftf); % band 1 start freq index in LTSA freq array
                    efIdx1 = nearestpoint(frqBand1(end),Ftf); % band 1 end freq index in LTSA freq array
                    sfIdx2 = nearestpoint(frqBand2(1),Ftf); % band 2 start freq index in LTSA freq array
                    efIdx2 = nearestpoint(frqBand2(end),Ftf); % band 2 end freq index in LTSA freq array
                    sfIdx3 = nearestpoint(frqBand3(1),Ftf); % band 3 start freq index in LTSA freq array
                    efIdx3 = nearestpoint(frqBand3(end),Ftf); % band 3 end freq index in LTSA freq array
                    sfIdx4 = nearestpoint(frqBand4(1),Ftf); % band 4 start freq index in LTSA freq array
                    efIdx4 = nearestpoint(frqBand4(end),Ftf); % band 4 end freq index in LTSA freq array
                    

                    tmp_band1(:,rfidxstart:rfidxend) = datac(sfIdx1:efIdx1,:); %
                    tmp_band2(:,rfidxstart:rfidxend) = datac(sfIdx2:efIdx2,:); %
                    tmp_band3(:,rfidxstart:rfidxend) = datac(sfIdx3:efIdx3,:); %
                    tmp_band4(:,rfidxstart:rfidxend) = datac(sfIdx4:efIdx4,:); %

                  rfcounter = rfcounter + 1;    
              elseif a(1,2)<(rf_tbins-cull) % if not enough time bins in data, skip and move on
                  continue
              end

          end
          % get rid of extra nans from tmpBand1
          [~, idxnans] = find(isnan(tmp_band1(1,:)));
          tmp_band1(:,idxnans) = [];
          ltsaCull = [ltsaCull tmp_band1];
          dnums_ltsaCull = [dnums_ltsaCull tmp_dnums];
    end
    
    % if no data/times for this month in ltsaCull, continue to next month
    if isempty(ltsaCull)
        continue
    end
    
    %%         %CORRECT FOR FIFO NOISE
    % setup to interpolate across fifo frequencies (dumb way for now. use
    % Interp1 instead). NOTE that this interpolates between adjacent
    % frequencies to the FIFO. Assumes there is bandwidth of 1Hz to FIFO
    % around ea freq.
        if FIFOinterp == 1
        [lia, fifoIdx] = find(ismember(frqBand1,fifoFreqs));
        %     ltsadataFifo = ltsadata;
            for jj = 1:size(ltsaCull,2)

                % if the first fifo freq is freq1, then set equal to f1+2
                if fifoIdx(1,1)==1
                % for the first freq, just set it equal to the one above
                ltsaCull(fifoIdx(1,1):fifoIdx(1,1)+1,jj) = ltsaCull(fifoIdx(1,1)+2,jj);

                 % for remaining freqs to end-1, interpolate between adjacent freq
                % values
                    for kk = 2:length(fifoIdx)-1
                        % get the freq index in ltsa for this one
                        idxtmp = fifoIdx(kk);

                        % linearly interpolate between values adjacent to fifo freq
                        n=length(idxtmp-1:idxtmp+1);
                        x = [ltsaCull(idxtmp-2,jj) ltsaCull(idxtmp+2,jj)];
                        y = linspace(x(1), x(2), n+2);         
                        ltsaCull(idxtmp-1:idxtmp+1,jj) = y(2:end-1);
                    end
                else
                    % if the first freq is not f1 in the ltsa, loop through all
                    % fifo freqs to end-1
                    for kk = 1:length(fifoIdx)-1
                    % get the freq index in ltsa for this one
                    idxtmp = fifoIdx(kk);

                    % linearly interpolate between values adjacent to fifo freq
                    n=length(idxtmp-1:idxtmp+1);
                    x = [ltsaCull(idxtmp-2,jj) ltsaCull(idxtmp+2,jj)];
                    y = linspace(x(1), x(2), n+2);         
                    ltsaCull(idxtmp-1:idxtmp+1,jj) = y(2:end-1);
                    end
                end

                % for last freq, set equal to the one below it
                ltsaCull(fifoIdx(1,end-1:end),jj) = ltsaCull(fifoIdx(1,end)-2,jj);
            end
        end

    % add 2000 yrs to ltsa dnums to get real datetimes for tbins
        dnums_ltsaCull = dnums_ltsaCull+datenum([2000 0 0 0 0 0]);

    %% 1-min levels section
    % now some 1min averaged spectra and SPL for shipNoise tshld RL triggers
    % Setup time array of minutes

        Dnums1min = [];
        specAvg1min = [];

    % get total hours and 1min bins in ltsa file
        numMin = floor((dnums_ltsaCull(end) - dnums_ltsaCull(1))*24*60);
        Dnums1min = inf.*ones(numMin,1);
        specAvg1min = inf.*ones(size(ltsaCull,1),numMin);
        SPL1minBand1 = inf.*ones(numMin,1);
        SPL1minBand2 = inf.*ones(numMin,1);
        SPL1minBand3 = inf.*ones(numMin,1);
        SPL1minBand4 = inf.*ones(numMin,1);

        data_start_dvec = datevec(dnums_ltsaCull(1));
        startMin = [ data_start_dvec(1:5) 0];

        currMin = startMin ; % set first hour for loop through all hours below
        for minBin = 1:numMin
            % find raw files in this hour of this ltsa
          ltsaCull_idxs = find(dnums_ltsaCull >= datenum(currMin) & ...
              dnums_ltsaCull < datenum(currMin + [ 0 0 0 0 1 0 ]));
    %       % there will be n=ltsaCull_idx start times in this min
    %       fprintf('Minute %d has %d ea. 5s ltsa slices\n', ...
    %       minBin,numel(ltsaCull_idxs));
        % get mean of ltsa bins in minute
          specAvg1min(:,minBin)= mean(ltsaCull(:,ltsaCull_idxs),2);

          % now get the 1 min mean SPL for freqBands 
          p_tmpBand1 = (10.^(ltsaCull(:,ltsaCull_idxs)./10));
          p_sumBand1 = []; 
           a = size(p_tmpBand1); % dim 1 is timebins, dim 2 is freq
    %           clear p_sumBand1 p_sumBand2 p_sumBand3
              for i = 1:a(2) % for each time

                  % sum the pressure^2 across the freq band for ltsa tbin i
                  p_sumBand1(i) = sum(p_tmpBand1(:,i)); 
%                   p_sumBand2(i) = sum(p_tmpBand2(:,i));
%                   p_sumBand3(i) = sum(p_tmpBand3(:,i));       
              end
           tmp_dbSumBand1 = 10.*log10(p_sumBand1); 
           SPL1minBand1(minBin) = mean(tmp_dbSumBand1);

          Dnums1min(minBin) = datenum(currMin);
          currMin = currMin + [0 0 0 0 1 0];
        end

    perPtiles1 = prctile(specAvg1min, ptiles,2); % ptiles for dateRange 
    p_tmpBand1 = (10.^(perPtiles1./10));
    perSPLptiles1 = 10.*log10(sum(p_tmpBand1,1));

    %hmm. gotta find nearest point in frqBand1 to frqBand(n)
    idx2 = nearestpoint([frqBand2(1) frqBand2(end)], frqBand1);
    % now pull freq columns for the band and do ptiles
    perPtiles2 = prctile(specAvg1min(idx2(1):idx2(2),:),...
        ptiles,2); % ptiles for dateRange 
    p_tmpBand2 = (10.^(perPtiles2./10));
    perSPLptiles2 = 10.*log10(sum(p_tmpBand2,1));
    
    idx3 = nearestpoint([frqBand3(1) frqBand3(end)], frqBand1);
    perPtiles3 = prctile(specAvg1min(idx3(1):idx3(2),:),...
        ptiles,2); % ptiles for dateRange 
    p_tmpBand3 = (10.^(perPtiles3./10));
    perSPLptiles3 = 10.*log10(sum(p_tmpBand3,1));
    
    idx4 = nearestpoint([frqBand4(1) frqBand4(end)], frqBand1);
    perPtiles4 = prctile(specAvg1min(idx4(1):idx4(2),:),...
        ptiles,2); % ptiles for dateRange 
    p_tmpBand4 = (10.^(perPtiles4./10));
    perSPLptiles4 = 10.*log10(sum(p_tmpBand4,1));
    % 
    % perSPLptiles = [perSPLptiles1;perSPLptiles3;perSPLptiles2;perSPLptiles4];


    %% write output files
    params.frqBand1 = frqBand1;
    params.frqBand2 = frqBand2;
    params.frqBand3 = frqBand3;
    params.frqBand4 = frqBand4;
    params.ltsas = ltsas;
    params.startDate = startDate;
    params.endDate = endDate;
    params.ptiles = ptiles;
    params.DBPath = DBPath;
    params.LTSApath = LTSApath;

    save([DBPath,'matFiles\',proj,'_',site,...
        '_',datestr(startDate,'yyyy-mm-dd'),'_ptiles_bandLvls_df',num2str(df),'.mat'],...
        'params','dnums_ltsaCull','ltsaCull', 'perSPLptiles1', 'perSPLptiles2',...
        'perSPLptiles3', 'perSPLptiles4','specAvg1min','Dnums1min', '-v7.3');
    if plots ==1
    % make ltsa plot
%     load([DBPath,'\code\LTSA_cmap.mat'])
%     figure(100)
%         set(gcf, 'Position',  [600, 270, 1500, 500])
% 
%         h =surf(Dnums1min,frqBand1,specAvg1min,'Linestyle','none');
% 
%         sgAx = get(h,'Parent');
%         view(sgAx,0,90)
%         set(sgAx,'yscale','log','ylim',[frqBand1(1) frqBand1(end)],'xlim',...
%             [datenum(startDate) datenum(endDate)])
%         set(sgAx,'TickDir','out')
%         % YTick = [10 100 1000 10000 100000];
%         % set(sgAx,'YTick',YTick)
%         set(gca,'XMinorTick','on')
%         set(gca,'FontSize',FS)
%         ylabel('Frequency [kHz]','FontSize',FS)
%         xlabel('Date', 'FontSize',FS)
%             c = colorbar;
%              set(gcf,'colormap',cmap)
%              c.Label.String = 'Sound Spectrum Level (dB re 1 \muPa^2/Hz)';
%              c.Label.FontSize = FS;
%              c.Limits = SURFcbounds;
%              zlim(SURFzbounds)
%              set(gca,'XTick', floor(dnums_ltsaCull(1):7:ceil(dnums_ltsaCull(end))));
%              datetick('x','mm/dd','keepticks','keeplimits');
%              set(gcf,'color','w')
% 
%         print(gcf,[DBPath,'output\',proj,'_',site,...
%             '_',datestr(startDate,'yyyy-mm-dd'),'_',...
%             num2str(frqBand1(1)),'-',num2str(frqBand1(end)),'Hz_LTSA_surf_df',...
%             num2str(df),'.png'],'-dpng')
% %         saveas(gcf,[DBPath,'output\',proj,'_',site,...
% %             '_',datestr(startDate,'yyyy-mm'),'_',...
% %             num2str(frqBand1(1)),'-',num2str(frqBand1(end)),'Hz_LTSA_surf_df',...
% %             num2str(df),'.fig'])
%     clf
%     close(100)

%     % plot SPLbb of each tbin across the month
    figure(888)
        set(gcf, 'Position',  [500, 50, 1440, 400])
        plot(Dnums1min,SPL1minBand1);
        ax = gca;
        set(ax,'XTick', floor(Dnums1min(1):7:ceil(Dnums1min(end))));
        datetick('x','mm/dd','keepticks','keeplimits');
        set(ax,'XLim', [ Dnums1min(1) Dnums1min(end)]);
        set(ax,'YLim', [ SPLbounds(1) SPLbounds(2) ], 'fontsize',14);
        set(ax,'YTick', SPLbounds(1):10:SPLbounds(2),...
        'xminortick','on');
        set(gcf,'color','w');
        ax.XAxis.MinorTickValues = floor(Dnums1min(1):1:ceil(Dnums1min(end)));
        xlabel('Date');
        ylabel('Sound Pressure Level (dB re 1 \muPa^2)');
        grid on
        grid minor
    %      legend('90^{th}','75^{th}', '50^{th}','25^{th}','10^{th}')
    %      title([proj,' ',site,' Band SPL (',...
    %          num2str(band1(1)),'-',num2str(band1(2)),' Hz, tAvg 5 sec)',datestr(startDate,'mmm-yyyy')]);
    %      
        print(gcf,[DBPath,'output\',proj,'_',site,...
            '_',datestr(startDate,'yyyy-mm-dd'),'_',...
            num2str(frqBand1(1)),'-',num2str(frqBand1(end)),'Hz_bandSPL_df',...
            num2str(df),'.png'],'-dpng')
        saveas(gcf,[DBPath,'output\',proj,'_',site,...
            '_',datestr(startDate,'yyyy-mm-dd'),'_',...
            num2str(frqBand1(1)),'-',num2str(frqBand1(end)),'Hz_bandSPL_df',...
            num2str(df),'.fig'])
    clf
    
    figure(777)
     set(gcf,'Position',[900,100,450,400],'color','w');
        semilogx(frqBand1,perPtiles1(:,4),...
        'linewidth',2,'color','k')
        ylim([SPSLbounds(1) SPSLbounds(2)])
        xlim([frqBand1(1) frqBand1(end)])
        xlabel('Frequency (Hz)')
        ylabel('Spectrum Level (dB re 1 \muPa^2/Hz)')

        set(gca,'fontsize',14)
        set(gcf,'color','w')
        grid(gca,'on')
        h1 = gca;
        h1.GridAlpha = 1;
        h1.GridLineStyle= ':';
        h1.LineWidth = 1;
        h1.MinorGridAlpha = 0.3;
        text(22,SPSLbounds(1)+5,['n = ',num2str(length(dnums_ltsaCull))],'fontsize',14)

    %     title([proj,' ',site,' Month-average spectrum ',datestr(startDate,'mmm-yyyy')])
        hold on
        semilogx(frqBand1,perPtiles1(:,7),'linewidth',1, 'linestyle',':',...
            'color','k')
        semilogx(frqBand1,perPtiles1(:,6),'linewidth',1,'linestyle','-.',...
            'color',[0.6 0.6 0.6])
        semilogx(frqBand1,perPtiles1(:,5),'linewidth',1,'linestyle','--',...
            'color',[0.2 0.2 0.2])
        semilogx(frqBand1,perPtiles1(:,3),'linewidth',1,'linestyle','--',...
            'color',[0.2 0.2 0.2])
        semilogx(frqBand1,perPtiles1(:,2),'linewidth',1,'linestyle','-.',...
            'color',[0.6 0.6 0.6])
        semilogx(frqBand1,perPtiles1(:,1),'linewidth',1,'linestyle',':',...
            'color','k')

        print(gcf,[DBPath,'output\',proj,'_',site,...
            '_',datestr(startDate,'yyyy-mm-dd'),'_',...
            num2str(frqBand1(1)),'-',num2str(frqBand1(end)),'Hz_ptilesSPSL_df',...
            num2str(df),'.png'],'-dpng')
        saveas(gcf,[DBPath,'output\',proj,'_',site,...
            '_',datestr(startDate,'yyyy-mm-dd'),'_',...
            num2str(frqBand1(1)),'-',num2str(frqBand1(end)),'Hz_ptilesSPSL_df',...
            num2str(df),'.fig'])
    clf
    
    end  
  end
  end
  

    