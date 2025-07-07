local function makeAdder(n)
  return function(x)
    return x + n
  end
end
local add10 = makeAdder(10)
print(add10(7))
