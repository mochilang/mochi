function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
m = {[1]="a", [2]="b"}
__print((m[1] ~= nil))
__print((m[3] ~= nil))
