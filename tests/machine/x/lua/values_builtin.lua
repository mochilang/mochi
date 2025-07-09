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
(function(lst) for i,v in ipairs(lst) do io.write(v) if i < #lst then io.write(" ") end end io.write("\n") end)(__values(m))
