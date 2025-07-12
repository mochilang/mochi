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
print(table.concat(result, " "))
