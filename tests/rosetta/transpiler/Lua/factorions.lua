-- Generated by Mochi v0.10.42 on 2025-07-27 17:23 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}
facts = {1};

n = 1;

while (n < 12) do
  facts = (function(lst, item)
  local res = {table.unpack(lst)}
  table.insert(res, item)
  return res
end)(facts, (facts[(n - 1) + 1] * n))
n = (n + 1)
end;

for b = 9, 13 - 1 do
  print((("The factorions for base " .. tostring(b)) .. " are:"))
  line = ""
  i = 1
  while (i < 1500000) do
    m = i
    sum = 0
    while (m > 0) do
      d = (m % b)
      sum = (sum + facts[d + 1])
      m = (m // b)
    end
    if (sum == i) then
      line = ((line .. tostring(i)) .. " ")
    end
    i = (i + 1)
  end
  print(line)
  print("")
end;
