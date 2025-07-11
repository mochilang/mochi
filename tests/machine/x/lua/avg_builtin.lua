function __avg(v)
    local items
    if type(v) == 'table' and v.items ~= nil then
        items = v.items
    elseif type(v) == 'table' then
        items = v
    else
        error('avg() expects list or group')
    end
    if #items == 0 then return 0 end
    local sum = 0
    for _, it in ipairs(items) do sum = sum + it end
    local res = sum / #items
    if res == math.floor(res) then return math.floor(res) end
    return res
end
print(__avg({1, 2, 3}))
