function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
data = {["outer"]={["inner"]=1}}
data["outer"]["inner"] = 2
__print(data["outer"]["inner"])
