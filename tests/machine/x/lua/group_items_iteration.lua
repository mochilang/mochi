_Group = {}
function _Group.new(k)
    return {key = k, items = {}}
end
function __group_by(src, keyfn)
    local groups = {}
    local order = {}
    for _, it in ipairs(src) do
        local key = keyfn(it)
        local ks
        if type(key) == 'table' then
            local fields = {}
            for k,_ in pairs(key) do fields[#fields+1] = k end
            table.sort(fields)
            local parts = {}
            for _,k in ipairs(fields) do parts[#parts+1] = tostring(k)..'='..tostring(key[k]) end
            ks = table.concat(parts, ',')
        else
            ks = tostring(key)
        end
        local g = groups[ks]
        if not g then
            g = _Group.new(key)
            groups[ks] = g
            order[#order+1] = ks
        end
        table.insert(g.items, it)
    end
    local res = {}
    for _, ks in ipairs(order) do
        res[#res+1] = groups[ks]
    end
    return res
end
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
function __append(lst, v)
    local out = {}
    if lst then for i = 1, #lst do out[#out+1] = lst[i] end end
    out[#out+1] = v
    return out
end
function __iter(obj)
    if type(obj) == 'table' then
        if obj[1] ~= nil or #obj > 0 then
            local i = 0
            local n = #obj
            return function()
                i = i + 1
                if i <= n then return i, obj[i] end
            end
        else
            return pairs(obj)
        end
    elseif type(obj) == 'string' then
        local i = 0
        local n = #obj
        return function()
            i = i + 1
            if i <= n then return i, string.sub(obj, i, i) end
        end
    else
        return function() return nil end
    end
end
data = {{["tag"]="a", ["val"]=1}, {["tag"]="a", ["val"]=2}, {["tag"]="b", ["val"]=3}}
groups = (function()
  local _groups = __group_by(data, function(d) return d.tag end)
  local _res = {}
  for _, g in ipairs(_groups) do
    _res[#_res+1] = g
  end
  return _res
end)()
tmp = {}
for _, g in ipairs(groups) do
  local total = 0
  for _, x in __iter(g.items) do
    total = __add(total, x.val)
    ::__continue1::
  end
  tmp = __append(tmp, {["tag"]=g.key, ["total"]=total})
  ::__continue0::
end
result = (function()
  local _res = {}
  for _, r in ipairs(tmp) do
    _res[#_res+1] = {__key = r.tag, __val = r}
  end
  local items = _res
  table.sort(items, function(a,b) return a.__key < b.__key end)
  local tmp = {}
  for _, it in ipairs(items) do tmp[#tmp+1] = it.__val end
  items = tmp
  _res = items
  return _res
end)()
print(table.concat(result, " "))
