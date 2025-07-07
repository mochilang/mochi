local xs = {1, 2, 3}

local function contains(list, value)
  for _, v in ipairs(list) do
    if v == value then
      return true
    end
  end
  return false
end

print(contains(xs, 2))
print(not contains(xs, 5))
