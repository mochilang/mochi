local xs = {1, 2, 3}
local ys = {}
for _, x in ipairs(xs) do
  if x % 2 == 1 then
    table.insert(ys, x)
  end
end

local function contains(list, value)
  for _, v in ipairs(list) do
    if v == value then return true end
  end
  return false
end

print(contains(ys, 1))
print(contains(ys, 2))

local m = {a = 1}
print(m["a"] ~= nil)
print(m["b"] ~= nil)

local s = "hello"
local function str_contains(str, sub)
  return string.find(str, sub, 1, true) ~= nil
end
print(str_contains(s, "ell"))
print(str_contains(s, "foo"))
