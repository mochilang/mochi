function __div(a, b)
    if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
        return a // b
    end
    return a / b
end
function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
__print((6 * 7))
__print(__div(7, 2))
__print((7 % 2))
