function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
x = 2
label = (function()
  local _t0 = x
  if _t0 == 1 then return "one" end
  if _t0 == 2 then return "two" end
  if _t0 == 3 then return "three" end
  return "unknown"
end)()
__print(label)
