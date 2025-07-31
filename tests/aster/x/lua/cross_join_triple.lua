nums = {1, 2};
letters = {"A", "B"};
bools = {true, false};
combos = {};
for _, n in ipairs(nums) do
  for _, l in ipairs(letters) do
    for _, b in ipairs(bools) do
      table.insert(combos, {n = n, l = l, b = b});
    end;
  end;
end;
print("--- Cross Join of three lists ---");
for _, c in ipairs(combos) do
  print(string.format("%s %s %s", c.n, c.l, c.b));
end;
