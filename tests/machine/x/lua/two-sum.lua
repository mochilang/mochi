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
function __index(obj, i)
    if type(obj) == 'string' then
        return __indexString(obj, i)
    elseif type(obj) == 'table' then
        if obj[1] ~= nil or #obj > 0 then
            return obj[(i)+1]
        else
            return obj[i]
        end
    else
        error('cannot index')
    end
end
function __indexString(s, i)
    local len = #s
    if i < 0 then
        i = len + i + 1
    else
        i = i + 1
    end
    if i < 1 or i > len then error('index out of range') end
    return string.sub(s, i, i)
end
function twoSum(nums, target)
    local n = #nums
    for i = 0, (n)-1 do
        for j = __add(i, 1), (n)-1 do
            if __eq(__add(__index(nums, i), __index(nums, j)), target) then
                return {i, j}
            end
        end
    end
    return {-1, -1}
end

result = twoSum({2, 7, 11, 15}, 9)
print(__index(result, 0))
print(__index(result, 1))
