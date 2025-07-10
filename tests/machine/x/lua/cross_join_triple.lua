function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
nums = {1, 2}
letters = {"A", "B"}
bools = {true, false}
combos = (function()
  local _res = {}
  for _, n in ipairs(nums) do
    for _, l in ipairs(letters) do
      for _, b in ipairs(bools) do
        _res[#_res+1] = {["n"]=n, ["l"]=l, ["b"]=b}
      end
    end
  end
  return _res
end)()
__print("--- Cross Join of three lists ---")
for _, c in ipairs(combos) do
  __print(c.n, c.l, c.b)
  ::__continue0::
end
