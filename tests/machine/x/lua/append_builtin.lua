function __append(lst, v)
    local out = {}
    if lst then for i = 1, #lst do out[#out+1] = lst[i] end end
    out[#out+1] = v
    return out
end
a = {1, 2}
(function()
    local _tmp0 = __append(a, 3)
    for i, v in ipairs(_tmp0) do
        io.write(tostring(v))
        if i < #_tmp0 then io.write(" ") end
    end
    io.write("\n")
end)()
