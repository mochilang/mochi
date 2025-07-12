function __exists(v)
    if type(v) == 'table' then
        if v.items ~= nil then return #v.items > 0 end
        if v[1] ~= nil or #v > 0 then return #v > 0 end
        return next(v) ~= nil
    elseif type(v) == 'string' then
        return #v > 0
    else
        return false
    end
end
data = {1, 2}
flag = __exists((function()
    local _res = {}
    for _, x in ipairs(data) do
        if (x == 1) then
            _res[#_res+1] = x
        end
    end
    return _res
end)())
print(flag)
