function slag = fn_shiplag(data)

lag = 0;
for i = 1:size(data,1)
    if data.n_ships(i) == 0
        lag = lag + 1;
    else
        lag = 0;
    end
    slag(i) = lag;
end
end


