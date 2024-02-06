function [wave, ID] = fn_idwave(data)

% indices of effort  = 1
idxeff = data.MmEffort == 1;
idxdata = data.MmPres(idxeff);

% run autocorrelation
[acf, lags, bounds] = autocorr(idxdata,60*24*365);

% get the first lag time within the bounds
acfidx = find(acf <= bounds(1) & acf >= bounds(2));
% !!! change blockmin for now for time testing !!!
% based on baffinland report average of the median dive durations. shitty
%blockMin = 6;
blockMin = lags(acfidx(1));


block = (data.Time(1):minutes(blockMin):data.Time(end))';
clusterIdx = (1:length(block))';
repBlock = ones(length(block),1)*blockMin;
index = zeros(1,height(data));
index([1; cumsum(repBlock(1:end-1))+1]) = 1;
ID = clusterIdx(cumsum(index));

obsCluster = (1:blockMin)';
obsIdx = repmat(obsCluster,height(block),1);
wave = obsIdx(1:height(data));