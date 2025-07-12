function __contains(container, item)
    if type(container) == 'table' then
        if container[1] ~= nil or #container > 0 then
            for _, v in ipairs(container) do
                if v == item then return true end
            end
            return false
        else
            return container[item] ~= nil
        end
    elseif type(container) == 'string' then
        return string.find(container, item, 1, true) ~= nil
    else
        return false
    end
end
xs = {1, 2, 3}
ys = (function()
    local _res = {}
    for _, x in ipairs(xs) do
        if ((x % 2) == 1) then
            _res[#_res+1] = x
        end
    end
    return _res
end)()
print(__contains(ys, 1))
print(__contains(ys, 2))
m = {["a"]=1}
print((m["a"] ~= nil))
print((m["b"] ~= nil))
s = "hello"
print(__contains(s, "ell"))
print(__contains(s, "foo"))
