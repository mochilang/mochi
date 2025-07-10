function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
function boom()
  __print("boom")
  return true
end

__print(((((1 < 2)) and ((2 < 3))) and ((3 < 4))))
__print(((((1 < 2)) and ((2 > 3))) and boom()))
__print((((((1 < 2)) and ((2 < 3))) and ((3 > 4))) and boom()))
