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
function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
__print(__avg({1, 2, 3}))
