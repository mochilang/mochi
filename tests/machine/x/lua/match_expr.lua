x = 2
label = (function()
    local _t0 = x
    if _t0 == 1 then return "one" end
    if _t0 == 2 then return "two" end
    if _t0 == 3 then return "three" end
    return "unknown"
end)()
print(label)
