function __add(a, b)
    if type(a) == 'table' and type(b) == 'table' then
        local out = {}
        for i = 1, #a do out[#out+1] = a[i] end
        for i = 1, #b do out[#out+1] = b[i] end
        return out
    elseif type(a) == 'string' or type(b) == 'string' then
        return tostring(a) .. tostring(b)
    else
        return a + b
    end
end
Counter = {}
Counter.__index = Counter
function Counter.new(o)
  o = o or {}
  setmetatable(o, Counter)
  return o
end

function inc(c)
  c.n = __add(c.n, 1)
end

c = {n=0}
inc(c)
print(c.n)
