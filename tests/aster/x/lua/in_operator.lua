xs = {1, 2, 3};
print((function(lst, v)
  for _, x in ipairs(lst) do
    if x == v then
      return true;
    end;
  end;
  return false;
end)(xs, 2));
print((((function(lst, v)
  for _, x in ipairs(lst) do
    if x == v then
      return true;
    end;
  end;
  return false;
end)(xs, 5)) and (0) or (1)));
