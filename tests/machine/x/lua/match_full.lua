function classify(n)
  return (function()
  local _t0 = n
  if _t0 == 0 then return "zero" end
  if _t0 == 1 then return "one" end
  return "many"
end)()
end

x = 2
label = (function()
  local _t1 = x
  if _t1 == 1 then return "one" end
  if _t1 == 2 then return "two" end
  if _t1 == 3 then return "three" end
  return "unknown"
end)()
print(label)
day = "sun"
mood = (function()
  local _t2 = day
  if _t2 == "mon" then return "tired" end
  if _t2 == "fri" then return "excited" end
  if _t2 == "sun" then return "relaxed" end
  return "normal"
end)()
print(mood)
ok = true
status = (function()
  local _t3 = ok
  if _t3 == true then return "confirmed" end
  if _t3 == false then return "denied" end
  return nil
end)()
print(status)
print(classify(0))
print(classify(5))
