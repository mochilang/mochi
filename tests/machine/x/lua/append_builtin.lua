function __append(lst, v)
    local out = {}
    if lst then for i = 1, #lst do out[#out+1] = lst[i] end end
    out[#out+1] = v
    return out
end
a = {1, 2}
(function(lst) for i,v in ipairs(lst) do io.write(v) if i < #lst then io.write(" ") end end io.write("\n") end)(__append(a, 3))
