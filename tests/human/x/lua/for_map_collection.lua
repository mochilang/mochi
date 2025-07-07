local m = {a = 1, b = 2}
-- gather keys in sorted order for deterministic output
local keys = {}
for k in pairs(m) do table.insert(keys, k) end
table.sort(keys)
for _, k in ipairs(keys) do
  print(k)
end
