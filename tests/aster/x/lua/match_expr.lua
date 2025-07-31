x = 2;
label = (function(_m)
  if _m == 1 then
    return "one";
  elseif _m == 2 then
    return "two";
  elseif _m == 3 then
    return "three";
  elseif true then
    return "unknown";
  end;
end)(x);
print(label);
