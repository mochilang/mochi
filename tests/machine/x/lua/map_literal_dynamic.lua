function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
x = 3
y = 4
m = {["a"]=x, ["b"]=y}
__print(m["a"], m["b"])
