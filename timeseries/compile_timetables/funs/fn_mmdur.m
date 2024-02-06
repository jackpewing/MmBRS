function mmdur = fn_mmdur(data)

duration = 0;
for i = 1:size(data,1)
    if data.MmPres(i) > 0
        duration = duration + 1;
    else
        duration = 0;
    end
    mmdur(i) = duration;
end
end