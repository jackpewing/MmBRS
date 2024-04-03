
% pull in this piece of shit table
data = sunsetsunrisePI20162022;
% make variable daytime and nightime
data.rise = datetime(data.Sunrise_UTC, 'ConvertFrom', 'datenum');
data.rise = dateshift(data.rise, 'start','minute','nearest');

data.set = datetime(data.Sunset_UTC, 'ConvertFrom', 'datenum');
data.set = dateshift(data.set, 'start','minute','nearest');


 

data.daytime = minutes(data.set - data.rise);

data.nighttime = NaN(size(data,1),1);
data.nighttime(2:end) = minutes(data.rise(2:end) - data.set(1:end-1));


%make timetable for each october of each year
dt2016 = datetime(2016,10,1,0,0,0):minutes(1):datetime(2016,11,1,0,0,0);
dt2017 = datetime(2017,10,1,0,0,0):minutes(1):datetime(2017,11,1,0,0,0);
dt2018 = datetime(2018,10,1,0,0,0):minutes(1):datetime(2018,11,1,0,0,0);
dt2019 = datetime(2019,10,1,0,0,0):minutes(1):datetime(2019,11,1,0,0,0);
dt2020 = datetime(2020,10,1,0,0,0):minutes(1):datetime(2020,11,1,0,0,0);

time = [dt2016, dt2017, dt2018, dt2019, dt2020]';

tod = NaN(size(time,1),1);

tt = timetable(time, tod);

%just getting the date
dt = dateshift(tt.time, 'start','day');

%now I want to get a variable with the idx's of the date from the
%sunrise/sunset table
[~,tt.idx] = ismember(dt,data.Date);


% okay so now:
% 1. find the first sunrise or sunset and set to 

for i = 1:size(tt,1)
   event = tt.time(i);
   if sum(event>= data.rise(tt.idx(i)) & event < data.set(tt.idx(i)))
%        idxday = find(event>= data.rise(tt.idx(i)) & event < data.set(tt.idx(i)));
       tt.tod(i) = - minutes(data.set(tt.idx(i)) - event)/data.daytime(tt.idx(i));
       
   elseif sum(event>= data.set(tt.idx(i)) & event < data.rise(tt.idx(i)+1))
       %problem is here because the way I did it makes it not do the night
       %before AGH
       tt.tod(i) = minutes(event - data.set(tt.idx(i)))/data.nighttime(tt.idx(i+1));
   
   elseif sum(event>= data.set(tt.idx(i)-1) & event < data.rise(tt.idx(i)))
       tt.tod(i) = minutes(event - data.set(tt.idx(i)-1))/data.nighttime(tt.idx(i));

   else
       disp('fuck my life')
   end
end


%now make a full timetable of all years, nan'sw
start= datetime(2016,1,1,0,0,0);
stop = datetime(2022,1,1,0,0,0);

time = [start:minutes(1):stop]';
ntod = NaN(height(time),1);

tt1 = timetable(time,ntod);


TT1 = synchronize(tt1, tt, 'union');

TT1 = removevars(TT1, {'ntod','idx'});
       
outdir = 'G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\Solar\publication';

fullpath = fullfile(outdir, 'oct_normtod_PI_1min.mat');
save(fullpath,'TT1');

TT1 = timetable2table(TT1);
writetable('TT1', fullfile(outdir, 'oct_normtod_PI_min.csv'));

       
       
       
       
       
       