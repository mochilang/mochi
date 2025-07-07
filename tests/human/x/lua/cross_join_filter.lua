local nums = {1,2,3}
local letters = {"A","B"}
print("--- Even pairs ---")
for _, n in ipairs(nums) do
  if n % 2 == 0 then
    for _, l in ipairs(letters) do
      print(n .. " " .. l)
    end
  end
end
