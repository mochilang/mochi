function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
function boom(a, b)
  __print("boom")
  return true
end

__print((false and boom(1, 2)))
__print((true or boom(1, 2)))
