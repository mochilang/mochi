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
function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
function outer(x)
  local function inner(y)
    return __add(x, y)
  end
  return inner(5)
end

__print(outer(3))
