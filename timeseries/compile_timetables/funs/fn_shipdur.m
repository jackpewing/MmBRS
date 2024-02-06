function sdur = fn_shipdur(data)

duration = 0;
for i = 1:size(data,1)
    if data.n_ships(i) > 0
        duration = duration + 1;
    else
        duration = 0;
    end
    sdur(i) = duration;
end
end