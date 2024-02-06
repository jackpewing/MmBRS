%%% Get Transit Summary Data %%% JPE 2023 11 08

ST2 = timetable2table(ST1);

% First row sucks
ST2(1,:) = [];

% Step 1: Identify all rows with any NaN entries
rowsWithNaN = any(isnan(ST2.minRange), 2);

% Step 2: Identify all non-NaN periods (consecutive rows without NaN)
nonNaNPeriods = ~rowsWithNaN;

% Step 3: Find start and end indices of non-NaN periods
diffNonNaN = diff([0; nonNaNPeriods; 0]);
startIndex = find(diffNonNaN == 1);
endIndex = find(diffNonNaN == -1) - 1;

% Step 4: Calculate the lengths of non-NaN periods and their average
lengthsOfNonNaNPeriods = endIndex - startIndex + 1;
averageLength = mean(lengthsOfNonNaNPeriods);

% If you need to extract these segments and store them
nonNanDataSegments = cell(length(startIndex), 1);
for i = 1:length(startIndex)
    nonNanDataSegments{i} = ST2(startIndex(i):endIndex(i), :);
end

% Output the average length
disp(averageLength/60);

% hours of ship presence
disp(sum(lengthsOfNonNaNPeriods)/60);

% find start and end times, length of ship season
t1 = nonNanDataSegments{1, 1}(1,1);
t1 = t1{1,1}

t2 = nonNanDataSegments{end, 1}(end,1);
t2 = t2{1,1}

hours(t2-t1)
days(t2-t1)
