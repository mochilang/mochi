function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
x = 12
msg = (function()
  if (x > 10) then
    return "yes"
  else
    return "no"
  end
end)()
__print(msg)
