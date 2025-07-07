local x = 2
local label
if x == 1 then
  label = "one"
elseif x == 2 then
  label = "two"
elseif x == 3 then
  label = "three"
else
  label = "unknown"
end
print(label)

local day = "sun"
local mood
if day == "mon" then
  mood = "tired"
elseif day == "fri" then
  mood = "excited"
elseif day == "sun" then
  mood = "relaxed"
else
  mood = "normal"
end
print(mood)

local ok = true
local status = ok and "confirmed" or "denied"
print(status)

local function classify(n)
  if n == 0 then return "zero" elseif n == 1 then return "one" else return "many" end
end
print(classify(0))
print(classify(5))
