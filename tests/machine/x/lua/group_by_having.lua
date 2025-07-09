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
function __count(v)
    if type(v) == 'table' then
        if v.items ~= nil then return #v.items end
        if v[1] ~= nil or #v > 0 then return #v end
        local n = 0
        for _ in pairs(v) do n = n + 1 end
        return n
    elseif type(v) == 'string' then
        return #v
    else
        error('count() expects list or group')
    end
end
function __json(v)
    if type(v) == 'table' and next(v) == nil then print('[]'); return end
    local function sort(x)
        if type(x) ~= 'table' then return x end
        if x[1] ~= nil or #x > 0 then
            local out = {}
            for i=1,#x do out[i] = sort(x[i]) end
            return out
        end
        local keys = {}
        for k in pairs(x) do keys[#keys+1] = k end
        table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)
        local out = {}
        for _,k in ipairs(keys) do out[k] = sort(x[k]) end
        return out
    end
    local ok, json = pcall(require, 'json')
    if not ok then ok, json = pcall(require, 'cjson') end
    if ok then
        print(json.encode(sort(v)))
        return
    end
    local function enc(x)
        local t = type(x)
        if t == 'nil' then
            return 'null'
        elseif t == 'boolean' or t == 'number' then
            return tostring(x)
        elseif t == 'string' then
            return string.format('%q', x)
        elseif t == 'table' then
            if x[1] ~= nil or #x > 0 then
                local parts = {}
                for i=1,#x do parts[#parts+1] = enc(x[i]) end
                return '['..table.concat(parts, ',')..']'
            else
                local keys = {}
                for k in pairs(x) do keys[#keys+1] = k end
                table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)
                local parts = {}
                for _,k in ipairs(keys) do parts[#parts+1] = enc(k)..':'..enc(x[k]) end
                return '{'..table.concat(parts, ',')..'}'
            end
        else
            return 'null'
        end
    end
    print(enc(sort(v)))
end
people = {{["name"]="Alice", ["city"]="Paris"}, {["name"]="Bob", ["city"]="Hanoi"}, {["name"]="Charlie", ["city"]="Paris"}, {["name"]="Diana", ["city"]="Hanoi"}, {["name"]="Eve", ["city"]="Paris"}, {["name"]="Frank", ["city"]="Hanoi"}, {["name"]="George", ["city"]="Paris"}}
big = (function()
  local _groups = __group_by(people, function(p) return p.city end)
  local _res = {}
  for _, g in ipairs(_groups) do
    if (__count(g) >= 4) then
      _res[#_res+1] = {["city"]=g.key, ["num"]=__count(g)}
    end
  end
  return _res
end)()
__json(big)
