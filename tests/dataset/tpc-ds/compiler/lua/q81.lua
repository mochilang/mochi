-- Generated by Mochi compiler v0.10.26 on 2025-07-15T07:50:43Z
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
function __avg(v)
    local items
    if type(v) == 'table' and v.items ~= nil then
        items = v.items
    elseif type(v) == 'table' then
        items = v
    else
        error('avg() expects list or group')
    end
    if #items == 0 then return 0 end
    local sum = 0
    for _, it in ipairs(items) do sum = sum + it end
    local res = sum / #items
    if res == math.floor(res) then return math.floor(res) end
    return res
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
function __first(v)
    if type(v) == 'table' then
        if v.items ~= nil then
            if #v.items == 0 then return nil end
            return v.items[1]
        end
        if #v == 0 then return nil end
        return v[1]
    end
    return nil
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
function __run_tests(tests)
    local function format_duration(d)
        if d < 1e-6 then return string.format('%dns', math.floor(d*1e9)) end
        if d < 1e-3 then return string.format('%.1fµs', d*1e6) end
        if d < 1 then return string.format('%.1fms', d*1e3) end
        return string.format('%.2fs', d)
    end
    local failures = 0
    for _, t in ipairs(tests) do
        io.write('   test ' .. t.name .. ' ...')
        local start = os.clock()
        local ok, err = pcall(t.fn)
        local dur = os.clock() - start
        if ok then
            io.write(' ok (' .. format_duration(dur) .. ')\n')
        else
            io.write(' fail ' .. tostring(err) .. ' (' .. format_duration(dur) .. ')\n')
            failures = failures + 1
        end
    end
    if failures > 0 then
        io.write('\n[FAIL] ' .. failures .. ' test(s) failed.\n')
    end
end
function test_TPCDS_Q81_sample()
    if not (__eq(result, 81.0)) then error('expect failed') end
end

catalog_returns = {{["cust"]=1, ["state"]="CA", ["amt"]=40.0}, {["cust"]=2, ["state"]="CA", ["amt"]=50.0}, {["cust"]=3, ["state"]="CA", ["amt"]=81.0}, {["cust"]=4, ["state"]="TX", ["amt"]=30.0}, {["cust"]=5, ["state"]="TX", ["amt"]=20.0}}
avg_list = (function()
    local _groups = __group_by(catalog_returns, function(r) return r.state end)
    local _res = {}
    for _, g in ipairs(_groups) do
        _res[#_res+1] = {["state"]=g.key, ["avg_amt"]=__avg((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.amt
    end
    return _res
end)())}
    end
    return _res
end)()
avg_state = __first((function()
    local _res = {}
    for _, a in ipairs(avg_list) do
        if __eq(a.state, "CA") then
            _res[#_res+1] = a
        end
    end
    return _res
end)())
result_list = (function()
    local _res = {}
    for _, r in ipairs(catalog_returns) do
        if (__eq(r.state, "CA") and (r.amt > (avg_state.avg_amt * 1.2))) then
            _res[#_res+1] = r.amt
        end
    end
    return _res
end)()
result = __first(result_list)
__json(result)
local __tests = {
    {name="TPCDS Q81 sample", fn=test_TPCDS_Q81_sample},
}
