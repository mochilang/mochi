function __eq(a, b)
    if type(a) ~= type(b) then return false end
    if type(a) == 'number' then return math.abs(a-b) < 1e-9 end
    if type(a) ~= 'table' then return a == b end
    if (a[1] ~= nil or #a > 0) and (b[1] ~= nil or #b > 0) then
        if #a ~= #b then return false end
        for i = 1, #a do if not __eq(a[i], b[i]) then return false end end
        return true
    end
    for k, v in pairs(a) do if not __eq(v, b[k]) then return false end end
    for k, _ in pairs(b) do if a[k] == nil then return false end end
    return true
end
numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9}
for _, n in ipairs(numbers) do
    if __eq((n % 2), 0) then
        goto __continue0
    end
    if (n > 7) then
        break
    end
    print("odd number:", n)
    ::__continue0::
end
