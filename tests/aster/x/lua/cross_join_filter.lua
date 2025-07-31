nums = {1, 2, 3}
letters = {"A", "B"}
pairs = {}
for _, n in ipairs(nums) do
  for _, l in ipairs(letters) do
    if ((n % 2) == 0) then
      table.insert(pairs, {n = n, l = l})
    end
  end
end
print("--- Even pairs ---")
for _, p in ipairs(pairs) do
  print(string.format("%s %s", p.n, p.l))
end
