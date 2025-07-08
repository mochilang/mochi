function __print(...)
    local args = {...}
    local function to_str(v)
        if v == nil then return '<nil>' end
        if type(v) == 'table' then
            if v[1] ~= nil or #v > 0 then
                local parts = {}
                for i,x in ipairs(v) do parts[#parts+1] = tostring(x) end
                return table.concat(parts, ' ')
            end
        end
        if type(v) == 'number' and v == math.floor(v) then
            return tostring(math.floor(v))
        end
        return tostring(v)
    end
    for i, a in ipairs(args) do
        if i > 1 then io.write(' ') end
        io.write(to_str(a))
    end
    io.write('\n')
end
items = {{["n"]=1, ["v"]="a"}, {["n"]=1, ["v"]="b"}, {["n"]=2, ["v"]="c"}}
result = (function()
    local _res = {}
    for _, i in ipairs(items) do
        _res[#_res+1] = {__key = i.n, __val = i.v}
    end
    local items = _res
    table.sort(items, function(a,b) return a.__key < b.__key end)
    local tmp = {}
    for _, it in ipairs(items) do tmp[#tmp+1] = it.__val end
    items = tmp
    _res = items
    return _res
end)()
__print(result)
