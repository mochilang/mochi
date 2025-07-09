function __slice(obj, i, j)
    if i == nil then i = 0 end
    if type(obj) == 'string' then
        local len = #obj
        if j == nil then j = len end
        if i < 0 then i = len + i end
        if j < 0 then j = len + j end
        if i < 0 then i = 0 end
        if j > len then j = len end
        return string.sub(obj, i+1, j)
    elseif type(obj) == 'table' then
        local len = #obj
        if j == nil then j = len end
        if i < 0 then i = len + i end
        if j < 0 then j = len + j end
        if i < 0 then i = 0 end
        if j > len then j = len end
        local out = {}
        for k = i+1, j do
            out[#out+1] = obj[k]
        end
        return out
    else
        return {}
    end
end
(function()
    local _tmp0 = __slice({1, 2, 3}, 1, 3)
    for i, v in ipairs(_tmp0) do
        io.write(tostring(v))
        if i < #_tmp0 then io.write(" ") end
    end
    io.write("\n")
end)()
(function()
    local _tmp1 = __slice({1, 2, 3}, 0, 2)
    for i, v in ipairs(_tmp1) do
        io.write(tostring(v))
        if i < #_tmp1 then io.write(" ") end
    end
    io.write("\n")
end)()
print(__slice("hello", 1, 4))
