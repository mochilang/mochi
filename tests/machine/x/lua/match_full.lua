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
function classify(n)
    return (function()
    local _t0 = n
    if _t0 == 0 then return "zero" end
    if _t0 == 1 then return "one" end
    return "many"
end)()
end

x = 2
label = (function()
    local _t1 = x
    if _t1 == 1 then return "one" end
    if _t1 == 2 then return "two" end
    if _t1 == 3 then return "three" end
    return "unknown"
end)()
__print(label)
day = "sun"
mood = (function()
    local _t2 = day
    if _t2 == "mon" then return "tired" end
    if _t2 == "fri" then return "excited" end
    if _t2 == "sun" then return "relaxed" end
    return "normal"
end)()
__print(mood)
ok = true
status = (function()
    local _t3 = ok
    if _t3 == true then return "confirmed" end
    if _t3 == false then return "denied" end
    return nil
end)()
__print(status)
__print(classify(0))
__print(classify(5))
