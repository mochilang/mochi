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
function __sum(v)
    local items
    if type(v) == 'table' and v.items ~= nil then
        items = v.items
    elseif type(v) == 'table' then
        items = v
    else
        error('sum() expects list or group')
    end
    local sum = 0
    for _, it in ipairs(items) do sum = sum + it end
    return sum
end
function test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus()
    if not (__eq(result, {{["returnflag"]="N", ["linestatus"]="O", ["sum_qty"]=53, ["sum_base_price"]=3000, ["sum_disc_price"]=(950.0 + 1800.0), ["sum_charge"]=(((950.0 * 1.07)) + ((1800.0 * 1.05))), ["avg_qty"]=26.5, ["avg_price"]=1500, ["avg_disc"]=0.07500000000000001, ["count_order"]=2}})) then error('expect failed') end
end

lineitem = {{["l_quantity"]=17, ["l_extendedprice"]=1000.0, ["l_discount"]=0.05, ["l_tax"]=0.07, ["l_returnflag"]="N", ["l_linestatus"]="O", ["l_shipdate"]="1998-08-01"}, {["l_quantity"]=36, ["l_extendedprice"]=2000.0, ["l_discount"]=0.1, ["l_tax"]=0.05, ["l_returnflag"]="N", ["l_linestatus"]="O", ["l_shipdate"]="1998-09-01"}, {["l_quantity"]=25, ["l_extendedprice"]=1500.0, ["l_discount"]=0.0, ["l_tax"]=0.08, ["l_returnflag"]="R", ["l_linestatus"]="F", ["l_shipdate"]="1998-09-03"}}
result = (function()
    local _items = {}
    for _, row in ipairs(lineitem) do
        if (row.l_shipdate <= "1998-09-02") then _items[#_items+1] = row end
    end
    local _groups = __group_by(_items, function(row) return {["returnflag"]=row.l_returnflag, ["linestatus"]=row.l_linestatus} end)
    local _res = {}
    for _, g in ipairs(_groups) do
        _res[#_res+1] = {["returnflag"]=g.key.returnflag, ["linestatus"]=g.key.linestatus, ["sum_qty"]=__sum((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.l_quantity
    end
    return _res
end)()), ["sum_base_price"]=__sum((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.l_extendedprice
    end
    return _res
end)()), ["sum_disc_price"]=__sum((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = (x.l_extendedprice * ((1 - x.l_discount)))
    end
    return _res
end)()), ["sum_charge"]=__sum((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = ((x.l_extendedprice * ((1 - x.l_discount))) * (__add(1, x.l_tax)))
    end
    return _res
end)()), ["avg_qty"]=__avg((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.l_quantity
    end
    return _res
end)()), ["avg_price"]=__avg((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.l_extendedprice
    end
    return _res
end)()), ["avg_disc"]=__avg((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.l_discount
    end
    return _res
end)()), ["count_order"]=__count(g)}
    end
    return _res
end)()
__json(result)
local __tests = {
    {name="Q1 aggregates revenue and quantity by returnflag + linestatus", fn=test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus},
}
