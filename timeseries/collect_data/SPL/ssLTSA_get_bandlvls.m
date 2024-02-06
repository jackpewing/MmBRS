% open soundscape ltsa files and make 1min freqBand received levels

clear all


    DBPath = ['G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks'];
    LTSApath = ['G:\Shared drives\SWAL_Arctic\Research_projects\_CANARC\ssLTSA\PI'];
    savePath = ['G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\ssLTSA_data'];
%     LTSApath = '\\frosty\LTSA';
    
    proj = 'CANARC';
    sitenum = 4; % 1 = LowIsland, 2 = Tremblay, 3 = Guys Bight 4 = PI_HARP 5 = LI_HARP 6= MI_HARP 7 = EclipseSound center (150 km r)
    sites = {'ONC_LI','ONC_TS','ONC_GB','PI','LI','MI','ES'};
    site = char(sites(sitenum));
    
    depl = '2016'; 
    
%% setup frequency bands for SPL

%specify some freqbands of interest
freqbands = [63 125 250 1000 3150 6300 20000 50000];
f1 = [20 10000]; % lower and upper bounds for broadband SPL (including ships, excluding clicks)
 % set some frequency bands of interest
        N = 3; % 1/N octave freq bands calculated
        min_f = 5; %lowest freq to calculate 1/N oct bands
        max_f = 100000; %upper freq for 1/N oct freq bands
        
% compute 1/N oct bands using fract_oct_freq_band.m (put in path)
    [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(N, min_f, max_f);
  
%%
% Inuit seasons approach
%%%% Edit, do entire year.
%     startDates = {['1/1/',depl] ['7/15/',depl] ['9/15/',depl]};
%     endDates = {['7/15/',depl] ['9/15/',depl] ['12/31/',depl]};

    startDates = {['1/1/',depl]};
    endDates = {['12/31/',depl]};

    yr = datevec(startDates(1));

SPL1minBand1 = [];
dnums1min = [];
 for ii=1:numel(startDates)
% for ii=2

startDate = startDates{ii};
endDate = endDates{ii};

% check if .mat file exists for this time period
ltsafile = [LTSApath,'\',proj,'_',site,'_',...
    datestr(startDate,'yyyy-mm-dd'),'_ptiles_bandLvls.mat'];
    if isfile(ltsafile)
        disp(['existing LTSA_cull.mat file found for ',proj,'_',site,'_',num2str(yr(1)),...
            ' ...loading'])
        
       % Go to ssLTSA matfile and get data to do stuff.   
        ltsafile = matfile(ltsafile);
        ltsavars = who(ltsafile); % reassuring display of variables in the matfile
        ltsadnums = ltsafile.ltsadnums;
        ltsadata = ltsafile.ltsadata;
%         dnums1min = ltsafile.dnums1min;
%         specAvg1min = ltsafile.specAvg1min;
        params = ltsafile.params;
        f = params.f;
        
    else
        disp('no LTSA_cull file found. Rerun LTSA_freqband_compile_df to generate.')
        continue
    end
 
% make 1/N oct freq bands to match f array
% Set 1/3 oct band upper, lower, center freqs for f2, f3, f4, ... fn
    freqcidx = nearestpoint(freqbands,fnominal_l_c_u(:,2));
    flidx = nearestpoint(fnominal_l_c_u(freqcidx,1),f);
    fcidx = nearestpoint(fnominal_l_c_u(freqcidx,2),f);
    fuidx = nearestpoint(fnominal_l_c_u(freqcidx,3),f);
    f1idx = nearestpoint(f1,f);
    frqBand1 = f(f1idx(1):f1idx(2));
    frqBand2 = f(flidx(1):fuidx(1)); 
    frqBand3 = f(flidx(2):fuidx(2)); 
    frqBand4 = f(flidx(3):fuidx(3));
    frqBand5 = f(flidx(4):fuidx(4));    
    frqBand6 = f(flidx(5):fuidx(5));
    frqBand7 = f(flidx(6):fuidx(6));
    frqBand8 = f(flidx(7):fuidx(7));   
    
    
    data_start_dvec = datevec(startDate);
    startMin = [ data_start_dvec(1:5) 0];
% how many minutes in this analysis period? we'll loop through and
% get RLs etc. then populate a 1min tbin array.
     numMin = (floor(datenum(endDate)) - floor(datenum(startDate)))*24*60;
     currMin = startMin;
     SPL1minBand1_tmp = zeros(numMin,3); % this will become min, mean, max of the SPL for ea tbin
    dnums1min_tmp = zeros(numMin,1); % preallocate time array for the tbins dnums
 for minBin = 1:numMin
            % find raw files in this hour of this ltsa
          ltsabin_idxs = find(ltsadnums >= datenum(currMin) & ...
              ltsadnums < datenum(currMin + [ 0 0 0 0 1 0 ]));
          % if not ltsa times in this minute bin, continue to next min
          if isempty(ltsabin_idxs)
              currMin = currMin + [0 0 0 0 1 0];
              dnums1min_tmp(minBin) = datenum(currMin);
              continue
          end
    %       % there will be n=ltsaCull_idx start times in this min
    %       fprintf('Minute %d has %d ea. 5s ltsa slices\n', ...
    %       minBin,numel(ltsaCull_idxs));
        % get mean of ltsa bins in minute
        SPLbin = zeros(length(ltsabin_idxs),1);

        ltsabin = ltsadata(:,ltsabin_idxs);
         p_sumBand = [];
        for kk = 1:length(ltsabin_idxs) % for all 5s bins in this minute
%               sum the pressure^2 across the freq band for ltsa tbin kk
                p_tmpBand1 = (10.^(ltsabin(f1idx(1):f1idx(2),:)./10));
                p_sumBand1(kk) = sum(p_tmpBand1(:,kk)); 
                SPLbin(kk) = 10.*log10(p_sumBand1(kk));
        end
        
        SPL1minBand1_tmp(minBin,:) = [min(SPLbin) mean(SPLbin) max(SPLbin)];
        dnums1min_tmp(minBin) = datenum(currMin);
 
          currMin = currMin + [0 0 0 0 1 0];
 end

 SPL1minBand1 = [SPL1minBand1; SPL1minBand1_tmp];
 dnums1min = [dnums1min; dnums1min_tmp];
 
 
 end
 
      save([savePath,'\',proj,'_',site,...
        '_',depl,'_SPL1minBand1.mat'],...
        'params','frqBand1','SPL1minBand1','dnums1min', '-v7.3');
    
    
    % get freqband percentile spsl for 1min tbins across the whole period
%     perPtiles1 = prctile(specAvg1min, ptiles,2); % ptiles for dateRange 
%     p_tmpBand1 = (10.^(perPtiles1(f1idx(1):f1idx(2),:)./10)); % get pressure for f1(bb)
%     perSPLptiles1 = 10.*log10(sum(p_tmpBand1,1));
%     
%     p_tmpBand2 = (10.^(perPtiles1(flidx(1):fuidx(1),:)./10)); % get pressure for f2
%     perSPLptiles2 = 10.*log10(sum(p_tmpBand2,1));
%     
%     p_tmpBand3 = (10.^(perPtiles1(flidx(2):fuidx(2),:)./10)); % get pressure for f3
%     perSPLptiles3 = 10.*log10(sum(p_tmpBand3,1));
%     
%     p_tmpBand4 = (10.^(perPtiles1(flidx(3):fuidx(3),:)./10)); % get pressure for f4
%     perSPLptiles4 = 10.*log10(sum(p_tmpBand4,1));
%     
%     p_tmpBand5 = (10.^(perPtiles1(flidx(4):fuidx(4),:)./10)); % get pressure for f5
%     perSPLptiles5 = 10.*log10(sum(p_tmpBand5,1));
%     
%     p_tmpBand6 = (10.^(perPtiles1(flidx(5):fuidx(5),:)./10)); % get pressure for f6
%     perSPLptiles6 = 10.*log10(sum(p_tmpBand6,1));
%     
%     p_tmpBand7 = (10.^(perPtiles1(flidx(6):fuidx(6),:)./10)); % get pressure for f7
%     perSPLptiles7 = 10.*log10(sum(p_tmpBand7,1));
%     
%     p_tmpBand8 = (10.^(perPtiles1(flidx(7):fuidx(7),:)./10)); % get pressure for f8
%     perSPLptiles8 = 10.*log10(sum(p_tmpBand8,1));

    
        
 