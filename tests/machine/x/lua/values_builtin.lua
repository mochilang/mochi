function __values(m)
    if type(m) ~= 'table' then error('values() expects map') end
    local out = {}
    for _, v in pairs(m) do out[#out+1] = v end
    return out
end
m = {["a"]=1, ["b"]=2, ["c"]=3}
(function()
    local _tmp0 = __values(m)
    for i, v in ipairs(_tmp0) do
        io.write(tostring(v))
        if i < #_tmp0 then io.write(" ") end
    end
    io.write("\n")
end)()
