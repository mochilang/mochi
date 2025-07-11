function __values(m)
    if type(m) ~= 'table' then error('values() expects map') end
    local keys = {}
    for k in pairs(m) do keys[#keys+1] = k end
    table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)
    local out = {}
    for _, k in ipairs(keys) do out[#out+1] = m[k] end
    return out
end
m = {["a"]=1, ["b"]=2, ["c"]=3}
print(table.concat(__values(m), " "))
