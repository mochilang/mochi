print((function(v)
  if type(v) == 'table' and v.items ~= nil then
    return #v.items
  elseif type(v) == 'table' and (v[1] == nil) then
    local c = 0
    for _ in pairs(v) do
      c = c + 1
    end
    return c
  else
    return #v
  end
end)({1, 2, 3}))
