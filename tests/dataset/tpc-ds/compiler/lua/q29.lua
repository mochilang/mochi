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
function __query(src, joins, opts)
    local whereFn = opts.where
    local items = {}
    if #joins == 0 and whereFn then
        for _, v in ipairs(src) do if whereFn(v) then items[#items+1] = {v, n=1} end end
    else
        for _, v in ipairs(src) do items[#items+1] = {v, n=1} end
    end
    for ji, j in ipairs(joins) do
        local joined = {}
        local jitems = j.items or {}
        if j.right and j.left then
            local matched = {}
            for _, left in ipairs(items) do
                local m = false
                for ri, right in ipairs(jitems) do
                    local leftLen = left.n or #left
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left,1,leftLen)}
                        args[leftLen+1] = right
                        keep = j.on(table.unpack(args,1,leftLen+1))
                    end
                    if keep then
                        m = true; matched[ri] = true
                        local row = {table.unpack(left,1,leftLen)}
                        row[leftLen+1] = right
                        row.n = leftLen + 1
                        if ji == #joins and whereFn and not whereFn(table.unpack(row,1,row.n or #row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if not m then
                    local leftLen = left.n or #left
                    local row = {table.unpack(left,1,leftLen)}
                    row[leftLen+1] = nil
                    row.n = leftLen + 1
                    if ji == #joins and whereFn and not whereFn(table.unpack(row,1,row.n or #row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
            for ri, right in ipairs(jitems) do
                if not matched[ri] then
                    local row = {}
                    for _=1,ji do row[#row+1] = nil end
                    row[#row+1] = right
                    if ji == #joins and whereFn and not whereFn(table.unpack(row,1,row.n or #row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        elseif j.right then
            for _, right in ipairs(jitems) do
                local m = false
                for _, left in ipairs(items) do
                    local leftLen = left.n or #left
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left,1,leftLen)}
                        args[leftLen+1] = right
                        keep = j.on(table.unpack(args,1,leftLen+1))
                    end
                    if keep then
                        m = true
                        local row = {table.unpack(left,1,leftLen)}
                        row[leftLen+1] = right
                        row.n = leftLen + 1
                        if ji == #joins and whereFn and not whereFn(table.unpack(row,1,row.n or #row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if not m then
                    local row = {}
                    for _=1,ji do row[#row+1] = nil end
                    row[#row+1] = right
                    row.n = ji + 1
                    if ji == #joins and whereFn and not whereFn(table.unpack(row,1,row.n or #row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        else
            for _, left in ipairs(items) do
                local m = false
                for _, right in ipairs(jitems) do
                    local leftLen = left.n or #left
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left,1,leftLen)}
                        args[leftLen+1] = right
                        keep = j.on(table.unpack(args,1,leftLen+1))
                    end
                    if keep then
                        m = true
                        local row = {table.unpack(left,1,leftLen)}
                        row[leftLen+1] = right
                        row.n = leftLen + 1
                        if ji == #joins and whereFn and not whereFn(table.unpack(row,1,row.n or #row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if j.left and not m then
                    local leftLen = left.n or #left
                    local row = {table.unpack(left,1,leftLen)}
                    row[leftLen+1] = nil
                    row.n = leftLen + 1
                    if ji == #joins and whereFn and not whereFn(table.unpack(row,1,row.n or #row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        end
        items = joined
    end
    if opts.sortKey then
        local pairs = {}
        for _, it in ipairs(items) do pairs[#pairs+1] = {item=it, key=opts.sortKey(table.unpack(it,1,it.n or #it))} end
        table.sort(pairs, function(a,b)
            local ak, bk = a.key, b.key
            if type(ak)=='number' and type(bk)=='number' then return ak < bk end
            if type(ak)=='string' and type(bk)=='string' then return ak < bk end
            return tostring(ak) < tostring(bk)
        end)
        items = {}
        for i,p in ipairs(pairs) do items[i] = p.item end
    end
    if opts.skip ~= nil then
        local n = opts.skip
        if n < #items then
            for i=1,n do table.remove(items,1) end
        else
            items = {}
        end
    end
    if opts.take ~= nil then
        local n = opts.take
        if n < #items then
            for i=#items, n+1, -1 do table.remove(items) end
        end
    end
    local res = {}
    for _, r in ipairs(items) do res[#res+1] = opts.selectFn(table.unpack(r,1,r.n or #r)) end
    return res
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
StoreSale = {}
StoreSale.__index = StoreSale
function StoreSale.new(o)
    o = o or {}
    setmetatable(o, StoreSale)
    return o
end

StoreReturn = {}
StoreReturn.__index = StoreReturn
function StoreReturn.new(o)
    o = o or {}
    setmetatable(o, StoreReturn)
    return o
end

CatalogSale = {}
CatalogSale.__index = CatalogSale
function CatalogSale.new(o)
    o = o or {}
    setmetatable(o, CatalogSale)
    return o
end

DateDim = {}
DateDim.__index = DateDim
function DateDim.new(o)
    o = o or {}
    setmetatable(o, DateDim)
    return o
end

Store = {}
Store.__index = Store
function Store.new(o)
    o = o or {}
    setmetatable(o, Store)
    return o
end

Item = {}
Item.__index = Item
function Item.new(o)
    o = o or {}
    setmetatable(o, Item)
    return o
end

function test_TPCDS_Q29_quantity_summary()
    if not (__eq(result, {{["i_item_id"]="ITEM1", ["i_item_desc"]="Desc1", ["s_store_id"]="S1", ["s_store_name"]="Store1", ["store_sales_quantity"]=10, ["store_returns_quantity"]=2, ["catalog_sales_quantity"]=5}})) then error('expect failed') end
end

store_sales = {{["ss_sold_date_sk"]=1, ["ss_item_sk"]=1, ["ss_store_sk"]=1, ["ss_customer_sk"]=1, ["ss_quantity"]=10, ["ss_ticket_number"]=1}}
store_returns = {{["sr_returned_date_sk"]=2, ["sr_item_sk"]=1, ["sr_customer_sk"]=1, ["sr_ticket_number"]=1, ["sr_return_quantity"]=2}}
catalog_sales = {{["cs_sold_date_sk"]=3, ["cs_item_sk"]=1, ["cs_bill_customer_sk"]=1, ["cs_quantity"]=5}}
date_dim = {{["d_date_sk"]=1, ["d_moy"]=4, ["d_year"]=1999}, {["d_date_sk"]=2, ["d_moy"]=5, ["d_year"]=1999}, {["d_date_sk"]=3, ["d_moy"]=5, ["d_year"]=2000}}
store = {{["s_store_sk"]=1, ["s_store_id"]="S1", ["s_store_name"]="Store1"}}
item = {{["i_item_sk"]=1, ["i_item_id"]="ITEM1", ["i_item_desc"]="Desc1"}}
base = (function()
    local _src = store_sales
    return __query(_src, {
        { items = store_returns, on = function(ss, sr) return ((ss.ss_ticket_number == sr.sr_ticket_number) and (ss.ss_item_sk == sr.sr_item_sk)) end },
        { items = catalog_sales, on = function(ss, sr, cs) return ((sr.sr_customer_sk == cs.cs_bill_customer_sk) and (sr.sr_item_sk == cs.cs_item_sk)) end },
        { items = date_dim, on = function(ss, sr, cs, d1) return (d1.d_date_sk == ss.ss_sold_date_sk) end },
        { items = date_dim, on = function(ss, sr, cs, d1, d2) return (d2.d_date_sk == sr.sr_returned_date_sk) end },
        { items = date_dim, on = function(ss, sr, cs, d1, d2, d3) return (d3.d_date_sk == cs.cs_sold_date_sk) end },
        { items = store, on = function(ss, sr, cs, d1, d2, d3, s) return __eq(s.s_store_sk, ss.ss_store_sk) end },
        { items = item, on = function(ss, sr, cs, d1, d2, d3, s, i) return __eq(i.i_item_sk, ss.ss_item_sk) end }
    }, { selectFn = function(ss, sr, cs, d1, d2, d3, s, i) return {["ss_quantity"]=ss.ss_quantity, ["sr_return_quantity"]=sr.sr_return_quantity, ["cs_quantity"]=cs.cs_quantity, ["i_item_id"]=i.i_item_id, ["i_item_desc"]=i.i_item_desc, ["s_store_id"]=s.s_store_id, ["s_store_name"]=s.s_store_name} end, where = function(ss, sr, cs, d1, d2, d3, s, i) return ((((((d1.d_moy == 4) and (d1.d_year == 1999)) and (d2.d_moy >= 4)) and (d2.d_moy <= 7)) and __contains({1999, 2000, 2001}, d3.d_year))) end })
end)()
result = (function()
    local _groups = __group_by(base, function(b) return {["item_id"]=b.i_item_id, ["item_desc"]=b.i_item_desc, ["s_store_id"]=b.s_store_id, ["s_store_name"]=b.s_store_name} end)
    local _res = {}
    for _, g in ipairs(_groups) do
        _res[#_res+1] = {["i_item_id"]=g.key.item_id, ["i_item_desc"]=g.key.item_desc, ["s_store_id"]=g.key.s_store_id, ["s_store_name"]=g.key.s_store_name, ["store_sales_quantity"]=__sum((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.ss_quantity
    end
    return _res
end)()), ["store_returns_quantity"]=__sum((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.sr_return_quantity
    end
    return _res
end)()), ["catalog_sales_quantity"]=__sum((function()
    local _res = {}
    for _, x in ipairs(g.items) do
        _res[#_res+1] = x.cs_quantity
    end
    return _res
end)())}
    end
    return _res
end)()
__json(result)
local __tests = {
    {name="TPCDS Q29 quantity summary", fn=test_TPCDS_Q29_quantity_summary},
}
