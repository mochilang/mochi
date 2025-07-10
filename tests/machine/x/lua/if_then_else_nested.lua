function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
x = 8
msg = (function()
  if (x > 10) then
    return "big"
  else
    return (function()
  if (x > 5) then
    return "medium"
  else
    return "small"
  end
end)()
  end
end)()
__print(msg)
