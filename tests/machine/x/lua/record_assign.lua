Counter = {}
Counter.__index = Counter
function Counter.new(o)
  o = o or {}
  setmetatable(o, Counter)
  return o
end

function inc(c)
  c.n = (c.n + 1)
end

c = {n=0}
inc(c)
print(c.n)
