function mmlag = fn_mmlag(data)

lag = 0;
for i = 1:size(data,1)
    if data.MmPres(i) == 0
        lag = lag + 1;
    else
        lag = 0;
    end
    mmlag(i) = lag;
end
end

