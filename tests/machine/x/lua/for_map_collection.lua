function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
m = {["a"]=1, ["b"]=2}
for k in pairs(m) do
  __print(k)
  ::__continue0::
end
