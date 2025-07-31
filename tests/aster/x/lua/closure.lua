function makeAdder(n)
  return function(x)
    return (x + n)
  end
end
add10 = makeAdder(10)
print(add10(7))
