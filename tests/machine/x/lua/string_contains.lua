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
s = "catch"
__print(__contains(s, "cat"))
__print(__contains(s, "dog"))
