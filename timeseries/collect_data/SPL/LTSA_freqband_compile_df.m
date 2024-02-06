
% 20221209 JMJ Here's a thing that opens the three df-level ship transit
% .mat files as io opjects, then iterates through transits to pull ltsa and
% ais data to make transit plots.

clearvars 
close all

% setup run params
    GDriveLoc = 'G:\';
    DBPath = [GDriveLoc,'My Drive\JoshOffice\Research_projects\ARCTIC\ShipNoise\2023\'];
    LTSApath = '\\frosty\LTSA';
    
    proj = 'CANARC';
    sitenum = 5; % 1 = LowIsland, 2 = Tremblay, 3 = Guys Bight 4 = PI_HARP 5 = LI_HARP 6= MI_HARP 7 = EclipseSound center (150 km r)
    sites = {'ONC_LI','ONC_TS','ONC_GB','PI','LI','MI','ES'};
    site = char(sites(sitenum));
    
    depl = '2019';
    
%     proj = 'CANARC';
%     site = 'LI';
%     depl = '2016-18';
%  %start and end datetimes of large time periods (e.g. months)for this run 
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
    startDates = {['5/15/',depl] ['7/15/',depl] ['9/15/',depl]};
    endDates = {['7/15/',depl] ['9/15/',depl] ['11/15/',depl]};
    
% %start and end datetimes of large time periods (e.g. months)for this run
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
% setup plot parameters
    SPLbounds = [60 140]; % ylims for SPL 1-min plot
    SPSLbounds = [30 110]; % ylims for SPSL plots
    LTSAbounds = [40 90]; % clims for imagesc LTSA plot
    SURFzbounds = [30 120]; % z lims for surf LTSA plot
    SURFcbounds = [40 110]; % c lim for surf LTSA plot
    FS = 16; % font size for plots
    FS2 = 24;
    
    plots = 1; % yes or no make new plots


% LTSAbounds = [35 120]; % ylims for combined medians monthly
% LTSAclim = [35 120];
ptiles = [1 5 10 50 90 95 99];

%% get LTSA and tbin specAvgs data for this site and time period
for ii=1:numel(startDates)
% for ii=1:2

ltsadata = [];

startDate = startDates{ii};
endDate = endDates{ii};

% check if .mat file exists for this time period
if ~isfile([DBPath,'matFiles\',proj,'_',site,'_',...
    datestr(startDate,'yyyy-mm-dd'),'_ptiles_bandLvls_df1.mat'])
continue
end
% hard-coded here, but these are the desired shipTransit .mat files
file_df1 = [DBPath,'matFiles\',proj,'_',site,'_',...
    datestr(startDate,'yyyy-mm-dd'),'_ptiles_bandLvls_df1.mat'];
file_df20 = [DBPath,'matFiles\',proj,'_',site,'_',...
    datestr(startDate,'yyyy-mm-dd'),'_ptiles_bandLvls_df20.mat'];
file_df100 = [DBPath,'matFiles\',proj,'_',site,'_',...
    datestr(startDate,'yyyy-mm-dd'),'_ptiles_bandLvls_df100.mat'];

df1 = matfile(file_df1);
df20 = matfile(file_df20);
df100 = matfile(file_df100);
 
% get the txLtsa data (all 5s tbins) from ea df file
% assemble not-so-giant 1minavgLTSA for this time period
    ltsa_df100 = df100.ltsaCull; % df 100 for this tx
    ltsa_df20 = df20.ltsaCull; %df 20 for thei tx
    ltsa_df1 = df1.ltsaCull; % df 1 for this tx
    
% Get dnums for ltsadata
    ltsadnums = df100.dnums_ltsaCull;
    ltsadnumsdf1 = df1.dnums_ltsaCull; % this will be the time array for ltsaCull

% Check here for different length ltsa data for the dfs
% Make smarter. This assumes that the df100 will be shorter.
if size(ltsa_df100,2)<size(ltsa_df1,2)

    [~, ltsaIdx] = find(ismember(ltsadnums,ltsadnumsdf1));
    
% and the combined ltsa for the time period
    ltsadata = [ltsa_df100(1:end-1,:); ltsa_df20(:,ltsaIdx); ltsa_df1(2:end,ltsaIdx)];
    
elseif size(ltsa_df100,2)>size(ltsa_df1,2)

    [~, ltsaIdx] = find(ismember(ltsadnumsdf1,ltsadnums));
    
% and the combined ltsa for the time period
    ltsadata = [ltsa_df100(1:end-1,ltsaIdx); ltsa_df20(:,ltsaIdx); ltsa_df1(2:end,ltsaIdx)];
else
    ltsadata = [ltsa_df100(1:end-1,:); ltsa_df20(:,:); ltsa_df1(2:end,:)];
end

    clear ltsa_df100 ltsa_df20 ltsa_df1
    

% get the 1min avg spsl data (all 5s tbins) for this tx from ea df file
% assemble giant LTSA for this time period
    sp1min_df100 = df100.specAvg1min; % df 100 for this tx
    sp1min_df20 = df20.specAvg1min; %df 20 for thei tx
    sp1min_df1 = df1.specAvg1min; % df 1 for this tx
    
    
% Check here for different length ltsa data for the dfs
% Make smarter. This assumes that the df100 will be shorter.
    % Get dnums for binned ltsa data
    % get dnums for ltsadata
    dnums1min = df100.Dnums1min;
    dnums1mindf1 = df1.Dnums1min; % this will be the time array for binned ltsa
    [binsIdx,~] = find(ismember(dnums1min,dnums1mindf1));

    % and the combined ltsa for the transit
    specAvg1min = [sp1min_df100(1:end-1,binsIdx); sp1min_df20(:,binsIdx);...
        sp1min_df1(2:end,binsIdx)];

    clear sp1min_df100 sp1min_df20 sp1min_df1
 
% get percentile spsl for 1min tbins across the whole period
    perPtiles1 = prctile(specAvg1min, ptiles,2); % ptiles for dateRange 
    p_tmpBand1 = (10.^(perPtiles1./10));
    perSPLptiles1 = 10.*log10(sum(p_tmpBand1,1));
    
    % assemble a freq array
        f100 = df100.params;
        f100 = f100.frqBand1;
        f20 = df20.params;
        f20 = f20.frqBand1;
        f1 = df1.params;
        f1 = f1.frqBand1;

        f = [f100(1,1:end-1) f20 f1(1,2:end)];
        f = f';
        
   %% write output files
    params.f = f;
    params.files = {file_df1 file_df1 file_df1};
    params.startDate = startDate;
    params.endDate = endDate;
    params.ptiles = ptiles;
    params.DBPath = DBPath;
    params.proj = proj;
    params.site = site;

        
    save([DBPath,'matFiles\',proj,'_',site,...
        '_',datestr(startDate,'yyyy-mm-dd'),'_ptiles_bandLvls.mat'],...
        'params','ltsadnums','ltsadata', 'perPtiles1', 'perSPLptiles1',...
        'specAvg1min','dnums1min', '-v7.3');
%% LTSA plots        

    %setup a meshgrid from 1min tbins and freq
        [Xt,Yt] = meshgrid(dnums1min,f);

%     %now make a cool full bandwidth ltsa of the time period.
    figure(888)
    clf
    set(gcf, 'Position',   [600, 270, 1500, 500])
        sf = surf(Xt,Yt,specAvg1min);
        ylabel('Frequency (Hz)')
        xlabel('Date', 'FontSize',FS)
        xlim([min(dnums1min) max(dnums1min)]);
        zlim(SURFzbounds)
        caxis(SURFcbounds)
        sf.EdgeColor = 'none'; %turn off edge markers
        view(2) % plot in 2D view from 'top'
        c = colorbar;
        c.Limits = SURFcbounds;
        colormap('jet')
        c.Label.String = 'Sound Spectrum Level (dB re 1 \muPa^2/Hz)';
        c.Label.FontSize = 14;
        set(gca,'fontsize',16,'yscale','log','ylim',[5 100000])
        set(gca,'XTick', floor(dnums1min(1):7:ceil(dnums1min(end))));
        datetick('x','mm/dd','keepticks','keeplimits');
        set(gcf,'color','w')
        
        print(gcf,[DBPath,'output\',proj,'_',site,...
            '_',datestr(startDate,'yyyy-mm-dd'),'_',...
            num2str(f(1)),'-',num2str(f(end)),...
            'Hz_LTSA_surf.png'],'-dpng')
    clf


%% SPSL plots

   figure(777)
     set(gcf,'Position',[900,100,650,500],'color','w');
        semilogx(f,perPtiles1(:,4),...
        'linewidth',2,'color','k')
        ylim([SPSLbounds(1) SPSLbounds(2)])
        xlim([f(1) f(end)])
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
        text(22,SPSLbounds(1)+5,['n = ',num2str(length(dnums1min))],'fontsize',14)

    %     title([proj,' ',site,' Month-average spectrum ',datestr(startDate,'mmm-yyyy')])
        hold on
         semilogx(f,perPtiles1(:,7),'linewidth',1, 'linestyle',':',...
            'color','k')
        semilogx(f,perPtiles1(:,6),'linewidth',1,'linestyle','-.',...
            'color',[0.6 0.6 0.6])
        semilogx(f,perPtiles1(:,5),'linewidth',1,'linestyle','--',...
            'color',[0.2 0.2 0.2])
        semilogx(f,perPtiles1(:,3),'linewidth',1,'linestyle','--',...
            'color',[0.2 0.2 0.2])
        semilogx(f,perPtiles1(:,2),'linewidth',1,'linestyle','-.',...
            'color',[0.6 0.6 0.6])
        semilogx(f,perPtiles1(:,1),'linewidth',1,'linestyle',':',...
            'color','k')

        print(gcf,[DBPath,'output\',proj,'_',site,...
            '_',datestr(startDate,'yyyy-mm-dd'),'_',...
            num2str(f(1)),'-',num2str(f(end)),'Hz_ptilesSPSL.png'],'-dpng')
        saveas(gcf,[DBPath,'output\',proj,'_',site,...
            '_',datestr(startDate,'yyyy-mm-dd'),'_',...
            num2str(f(1)),'-',num2str(f(end)),'Hz_ptilesSPSL.fig'])
    clf
    
end
        
        
%         
%         df1RowC = df1.txLtsa(1,2);
% df1RowD = df1.txLtsa(1,3);
% 
% 
% df20RowC = df20.txLtsa(1,2);
% df20RowD = df20.txLtsa(1,3);
% 
% df100RowC = df100.txLtsa(1,2);
% df100RowD = df100.txLtsa(1,3);
% 
% df20 = struct;
% df20.params = params;
% df20.txLtsa = txLtsa;
% df20.txptilesbin = txptilesbin;
% 
% 
% df1 = struct;
% df1.params = params;
% df1.txLtsa = txLtsa;
% df1.txptilesbin = txptilesbin;
% 
% 
% df100 = struct;
% df100.params = params;
% df100.txLtsa = txLtsa;
% df100.txptilesbin = txptilesbin;
% 
% 
