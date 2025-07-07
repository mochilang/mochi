local m = {a=1, b=2}
local count = 0
for _ in pairs(m) do count = count + 1 end
print(count)
