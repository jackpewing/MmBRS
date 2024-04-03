%%%% JPE 20230125 / timeseries plot of narwhal presence (mins/day) and sea ice %%%%

depinfoPath = ['G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\matfiles'];
[depinfo_files, depinfo_paths] = uigetfile([depinfoPath,'\*depinfo.mat'],'MultiSelect','on',...
        ['Select Deployment Info for PI']);
depinfo = fullfile(depinfo_paths(1,:), depinfo_files(1,:));
load(depinfo)

% Convert the recTimes datenums to datetime
effort_start = datetime(recTimes(:,1), 'ConvertFrom', 'datenum');
effort_end = datetime(recTimes(:,2), 'ConvertFrom', 'datenum');

% Identify gaps (no effort periods)
no_effort_start = [datetime(2016, 1, 1); effort_end(1:end-1)]; % Including the start of the dataset as a potential no-effort start
no_effort_end = effort_start;

% Load the new data file
dpath = 'G:\Shared drives\SWAL_Arctic\Research_projects\JackBRS\Arctic_shiptxClicks\output\Stage_two\Combine_all\publication\5km_mask\daily';
load(fullfile(dpath, 'dailybinned_UTC.mat'));

tt = table2timetable(mmdata);

% Define the start and end of the period of interest
start_date = datetime(2016, 1, 1);
end_date = datetime(2021, 12, 31);

% Generate a list of all daily timestamps from start_date to end_date
all_days = (start_date:days(1):end_date)';

% Find the missing hours by comparing with tt
missing_days = setdiff(all_days, tt.Time);

% Identify contiguous blocks of missing hours
diff_missing = [days(2); diff(missing_days)];
breaks = find(diff_missing > hours(1));

% Determine start and end of these blocks
missing_starts = missing_days(breaks);
missing_ends = [missing_days(breaks(2:end)-1); missing_days(end)];

unique_years = unique(year(tt.Time));

x = figure;
clf; % remove existing y axis labels

totalAxesHeight = 0.9; % This is a fraction of figure height; adjust as needed
subplotHeight = totalAxesHeight / length(unique_years);
gapHeight = 0.025; % Adjust this to set the gap between subplots

for i = 1:length(unique_years)
    year_data = tt(year(tt.Time) == unique_years(i), :);
    
    
    ax = subplot(length(unique_years), 1, i);
    
    % Set position
    currentPosition = get(ax, 'Position');
    set(ax, 'Position', [currentPosition(1), 1 - i * subplotHeight, currentPosition(3), subplotHeight - gapHeight]);

    hold on;
    
    % Gray out no-effort times
    yyaxis right
    for j = 1:length(no_effort_start)
        if year(no_effort_start(j)) == unique_years(i)
            fill([no_effort_start(j) no_effort_end(j) no_effort_end(j) no_effort_start(j)], ...
                 [1440 1440 0 0], [0.7 0.7 0.7], 'EdgeColor', 'none', 'FaceAlpha', 0.5); % Gray color with 50% transparency
        end
    end

        % Gray out area after October 4th, 2021
    if unique_years(i) == 2021
        hold on;
        yyaxis right;
        oct4 = datetime(2021, 10, 4);
        endOfYear = datetime(2021, 12, 31);
        fill([oct4 endOfYear endOfYear oct4], [0 0 100 100], [0.7 0.7 0.7], 'EdgeColor', 'none', 'FaceAlpha', 0.5);
    end
    
    % Plotting the Ice_pc with better filling underneath
    hold on
    yyaxis right
    ax.YColor = 'k';
    
    ylim([0 100]);
    ax.YAxis(2).FontSize = 11; % Set font size for left Y-axis
    


    nonNanIndices = ~isnan(year_data.Ice_pc);
    x_vals = [year_data.Time(nonNanIndices); flipud(year_data.Time(nonNanIndices))];
    y_vals = [year_data.Ice_pc(nonNanIndices); zeros(sum(nonNanIndices), 1)];
    fill(x_vals, y_vals, [0.7 0.9 1], 'EdgeColor', 'none','FaceAlpha',0.7); % Fill area

    % Adding narwhal presence as a solid black line
    hold on;
    yyaxis left;
    ax.YColor = 'k';

        % Set the limits for the left y-axis (Narwhal presence in minutes)
    ylim([0 1440]);
    ax.YAxis(1).FontSize = 11; % Set font size for left Y-axis

    % Plot the narwhal presence data
    bar(year_data.Time, year_data.MmPres_eff_adj, 'k', 'EdgeColor', 'none');

    % Configure the axes and labels
    if i == 1
        ldg = legend({'Percent Ice Cover', 'Narwhal Presence', 'No Effort'}, ...
               'Location', 'west');
        legend('boxoff');
        ldg.FontSize = 14;
        ldg.Box = 'on';
        ldg.Color = 'white';
    end
    
    % Adjusting X-axis labels
    firstOfMonth = datetime(unique_years(i), 1, 1);
    lastOfMonth = datetime(unique_years(i), 12, 31);
    monthTicks = firstOfMonth:calmonths(1):lastOfMonth;

    xticks(monthTicks);

    if i == length(unique_years) % If it's the last subplot
        xticklabels({'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'});
    else
        xticklabels({}); % Empty xticklabels for all subplots except the last
    end
    
    % x tick label size
    ax.XAxis.FontSize = 13;

    xlim([datetime(unique_years(i), 1, 1), datetime(unique_years(i), 12, 31)]);
    title([num2str(unique_years(i))], 'FontSize', 14);
    grid on;

    if i == length(unique_years)
        xlb = xlabel('Month');
        xlb.FontSize = 16;
    end
    hold off
end

% Left y-axis label for Ice Cover
axes('Position',[0 0 1 1],'Visible','off');
text(0.1, 0.5, 'Narwhal Presence (Minutes per Day)', 'Units', 'Normalized', ...
     'Rotation', 90, 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', 'FontSize', 16);

% Right y-axis label for Narwhal Presence
text(0.93, 0.5, 'Percent Ice Cover', 'Units', 'Normalized', ...
     'Rotation', -90, 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', 'FontSize', 16);


