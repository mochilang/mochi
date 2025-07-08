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
function boom()
    __print("boom")
    return true
end

__print(((((1 < 2)) and ((2 < 3))) and ((3 < 4))))
__print(((((1 < 2)) and ((2 > 3))) and boom()))
__print((((((1 < 2)) and ((2 < 3))) and ((3 > 4))) and boom()))
