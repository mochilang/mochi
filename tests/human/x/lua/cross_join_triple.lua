local nums = {1,2}
local letters = {"A","B"}
local bools = {true, false}
print("--- Cross Join of three lists ---")
for _, n in ipairs(nums) do
  for _, l in ipairs(letters) do
    for _, b in ipairs(bools) do
      print(n .. " " .. l .. " " .. tostring(b))
    end
  end
end
