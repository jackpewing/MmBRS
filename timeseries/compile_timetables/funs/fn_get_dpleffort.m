function dplEffort = fn_get_dpleffort(timetable, recTimes)

% Round recording times to nearest minute

% recTimes = datetime(recTimes(:,:),'ConvertFrom', 'datenum');

% Convert to datenums, its already in format but needs to be said
dtRecs = datetime(recTimes(:,:), 'ConvertFrom', 'datenum');

% now round to nearest minute
roundRecs = dateshift(dtRecs, 'start', 'minute');

% COMMENT OUT/CHANGE DATES FOR THIS SECTION
% Enter end date
roundRecs(8,1) = datetime(2022,1,1);

% COMMENT OUT THIS SECTION IF NOT PI
% PI_01 rec times wrong, start @ 22:00
roundRecs(2,1) = datetime(2016, 05, 28, 22, 0, 0);

% initialize new effort 
rec_effort = ones(size(timetable,1),1);

% pull out recording times, not including the first row of weird dates
for i = 1:size(roundRecs, 1) - 1
    if roundRecs(i,2) < roundRecs(i+1,1)
% make a 1 min binned TT of times with no effort
    times_noeff = (roundRecs(i,2):minutes(1):roundRecs(i+1,1))';
    end
% get indices of these times in the overall TT, then make a effort table
    [~,idxtmp] = ismember(timetable.Time, times_noeff);
    x = idxtmp > 0;
    rec_effort(x) = 0;
end
    
dplEffort = rec_effort;