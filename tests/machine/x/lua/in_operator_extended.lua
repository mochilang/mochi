function __contains(container, item)
    if type(container) == 'table' then
        if container[1] ~= nil or #container > 0 then
            for _, v in ipairs(container) do
                if v == item then return true end
            end
            return false
        else
            return container[item] ~= nil
        end
    elseif type(container) == 'string' then
        return string.find(container, item, 1, true) ~= nil
    else
        return false
    end
end
function __eq(a, b)
    if type(a) ~= type(b) then return false end
    if type(a) == 'number' then return math.abs(a-b) < 1e-9 end
    if type(a) ~= 'table' then return a == b end
    if (a[1] ~= nil or #a > 0) and (b[1] ~= nil or #b > 0) then
        if #a ~= #b then return false end
        for i = 1, #a do if not __eq(a[i], b[i]) then return false end end
        return true
    end
    for k, v in pairs(a) do if not __eq(v, b[k]) then return false end end
    for k, _ in pairs(b) do if a[k] == nil then return false end end
    return true
end
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
xs = {1, 2, 3}
ys = (function()
    local _res = {}
    for _, x in ipairs(xs) do
        if __eq((x % 2), 1) then
            _res[#_res+1] = x
        end
    end
    return _res
end)()
__print(__contains(ys, 1))
__print(__contains(ys, 2))
m = {["a"]=1}
__print((m["a"] ~= nil))
__print((m["b"] ~= nil))
s = "hello"
__print(__contains(s, "ell"))
__print(__contains(s, "foo"))
