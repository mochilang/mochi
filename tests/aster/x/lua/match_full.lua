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
day = "sun";
mood = (function(_m)
  if _m == "mon" then
    return "tired";
  elseif _m == "fri" then
    return "excited";
  elseif _m == "sun" then
    return "relaxed";
  elseif true then
    return "normal";
  end;
end)(day);
print(mood);
ok = true;
status = (function(_m)
  if _m == true then
    return "confirmed";
  elseif _m == false then
    return "denied";
  end;
end)(ok);
print(status);
function classify(n);
  return (function(_m)
    if _m == 0 then
      return "zero";
    elseif _m == 1 then
      return "one";
    elseif true then
      return "many";
    end;
  end)(n);
end;
print(classify(0));
print(classify(5));
