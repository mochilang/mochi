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
function __except(a, b)
    local res = {}
    if a then
        for _, v in ipairs(a) do
            local found = false
            if b then
                for _, w in ipairs(b) do
                    if __eq(v, w) then found = true break end
                end
            end
            if not found then res[#res+1] = v end
        end
    end
    return res
end
function __intersect(a, b)
    local res = {}
    if a and b then
        for _, v in ipairs(a) do
            for _, w in ipairs(b) do
                if __eq(v, w) then
                    local dup = false
                    for _, r in ipairs(res) do if __eq(r, v) then dup = true break end end
                    if not dup then res[#res+1] = v end
                    break
                end
            end
        end
    end
    return res
end
function __print(...)
    local args = {...}
    local function to_str(v)
        if v == nil then return '<nil>' end
        if type(v) == 'table' then
            if v[1] ~= nil or #v > 0 then
                local parts = {}
                for i,x in ipairs(v) do parts[#parts+1] = tostring(x) end
                return table.concat(parts, ' ')
            end
        end
        if type(v) == 'number' and v == math.floor(v) then
            return tostring(math.floor(v))
        end
        return tostring(v)
    end
    for i, a in ipairs(args) do
        if i > 1 then io.write(' ') end
        io.write(to_str(a))
    end
    io.write('\n')
end
function __union(a, b)
    local res = {}
    local function add(lst)
        if lst then
            for _, v in ipairs(lst) do
                local dup = false
                for _, w in ipairs(res) do
                    if __eq(v, w) then dup = true break end
                end
                if not dup then res[#res+1] = v end
            end
        end
    end
    add(a); add(b);
    return res
end
function __union_all(a, b)
    local res = {}
    if a then for _, v in ipairs(a) do res[#res+1] = v end end
    if b then for _, v in ipairs(b) do res[#res+1] = v end end
    return res
end
__print(__union({1, 2}, {2, 3}))
__print(__except({1, 2, 3}, {2}))
__print(__intersect({1, 2, 3}, {2, 4}))
__print(#__union_all({1, 2}, {2, 3}))
