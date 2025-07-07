local a = {1,2,3}
local sum = 0
for _, v in ipairs(a) do
  sum = sum + v
end
local avg = sum / #a
print(avg == math.floor(avg) and math.floor(avg) or avg)
