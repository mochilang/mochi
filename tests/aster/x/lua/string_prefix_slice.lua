prefix = "fore";
s1 = "forest";
print((string.sub(s1, (0 + 1), (function(v)
  if type(v) == 'table' and v.items ~= nil then
    return #v.items;
  elseif type(v) == 'table' and (v[1] == nil) then
    local c = 0;
    for _ in pairs(v) do
      c = c + 1;
    end;
    return c;
  else
    return #v;
  end;
end)(prefix)) == prefix));
s2 = "desert";
print((string.sub(s2, (0 + 1), (function(v)
  if type(v) == 'table' and v.items ~= nil then
    return #v.items;
  elseif type(v) == 'table' and (v[1] == nil) then
    local c = 0;
    for _ in pairs(v) do
      c = c + 1;
    end;
    return c;
  else
    return #v;
  end;
end)(prefix)) == prefix));
