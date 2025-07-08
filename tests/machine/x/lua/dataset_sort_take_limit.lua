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
products = {{["name"]="Laptop", ["price"]=1500}, {["name"]="Smartphone", ["price"]=900}, {["name"]="Tablet", ["price"]=600}, {["name"]="Monitor", ["price"]=300}, {["name"]="Keyboard", ["price"]=100}, {["name"]="Mouse", ["price"]=50}, {["name"]="Headphones", ["price"]=200}}
expensive = (function()
    local _res = {}
    for _, p in ipairs(products) do
        _res[#_res+1] = {__key = -p.price, __val = p}
    end
    local items = _res
    table.sort(items, function(a,b) return a.__key < b.__key end)
    local tmp = {}
    for _, it in ipairs(items) do tmp[#tmp+1] = it.__val end
    items = tmp
    local skip = 1
    if skip < #items then
        for i=1,skip do table.remove(items,1) end
    else
        items = {}
    end
    local take = 3
    if take < #items then
        for i=#items, take+1, -1 do table.remove(items) end
    end
    _res = items
    return _res
end)()
__print("--- Top products (excluding most expensive) ---")
for _, item in ipairs(expensive) do
    __print(item.name, "costs $", item.price)
    ::__continue0::
end
