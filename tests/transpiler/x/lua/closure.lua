-- Generated by Mochi v0.10.33 on 2025-07-21 16:57 GMT+7
function makeAdder(n)
  return function(x)
  return (x + n)
end
end

add10 = makeAdder(10)

print(add10(7))
