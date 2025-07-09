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
function outer(x)
  local function inner(y)
    return __add(x, y)
  end
  return inner(5)
end

print(outer(3))
