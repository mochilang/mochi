local data = {1,2}
local flag = false
for _, x in ipairs(data) do
  if x == 1 then
    flag = true
    break
  end
end
print(flag)
