function [ siteLocs, siteTimes, dataID,PA ] = getSiteLocs(startDate,endDate,DBPath,proj,site)

% JMJ20200307 modifed slightly from SMW=>BJT shipNoise code set

% nominal site B location is CINMS_B_30_00	
% ~7.6 km from edge of southbound side of the shipping lane
% ~3.5 km from edge of northbound lane
% need to add in code that looks up site location based on time period!
siteLocs = [];
dataID = {};
PA = '';
siteTimes = [];

%load('D:\Projects\ShippingCINMS\code\matlab\CINMS_B_depInfo.mat');
% load('G:\ShippingCINMS\code\matlab\CINMS_B_depInfo.mat');
load([DBPath,'\matFiles\',proj,'_',site,'_depInfo.mat']);

siteLocs = [];
dataID = [];
PA = [];
siteTimes = [];

for ii = 1:size(recTimes,1)
    if startDate<=recTimes(ii,1)&& endDate > recTimes(ii,2) ...
            || startDate>=recTimes(ii,1)&& startDate < recTimes(ii,2) ...
            || startDate<recTimes(ii,2) && endDate>=recTimes(ii,2)
        siteLocs = [siteLocs; lats(ii), lons(ii)];
        dataID = [dataID; names(ii)];
        PA = [PA; preAmp(ii)];
        siteTimes = [siteTimes; recTimes(ii,:)];
    else
    continue
    end
end


%%%%%%%
% note that this was commented out when it didn't work for the last
% deployment in the depinfo array. Might need to add a condition in line 25
% above for case where start time is before end of rec and endtime goes
% beytond end of rec.
% 
% si = find(recTimes(:,1) >= startDate,1,'first');
% ei = find(recTimes(:,2) >= endDate,1,'first');
% siteLocs = [ lats(si:ei), lons(si:ei) ];
% 
% dataID = names(si:ei);
% PA = preAmp(si:ei);
% siteTimes = recTimes(si:ei,:);


% if isempty(si) && isempty (ei)
%     fprintf('No HARP deployment found for this time period...skipping\n');
%     return;
% elseif isempty(si) || isempty(ei)
%     fprintf('Partial HARP deployment found for this time period...skipping\n'); 
%     return;
% elseif si<ei 
%     fprintf('Multiple HARP deployment found for this time period...skipping!!!!\n');
%     return;
% end

1;