-- Generated by Mochi compiler v0.10.26 on 2025-07-15T07:50:43Z
_Group = {}
function _Group.new(k)
    return {key = k, items = {}}
end
function __group_by_rows(rows, keyfn, valfn)
    local groups = {}
    local order = {}
    for _, r in ipairs(rows) do
        local key = keyfn(table.unpack(r,1,r.n or #r))
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
        table.insert(g.items, valfn(table.unpack(r,1,r.n or #r)))
    end
    local res = {}
    for _, ks in ipairs(order) do
        res[#res+1] = groups[ks]
    end
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
function __merge(...)
    local res = {}
    for i=1,select('#', ...) do
        local t = select(i, ...)
        if type(t) == 'table' then
            for k,v in pairs(t) do res[k] = v end
        end
    end
    return res
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
function test_TPCDS_Q37_simplified()
    if not (__eq(result, {{["i_item_id"]="I1", ["i_item_desc"]="Item1", ["i_current_price"]=30.0}})) then error('expect failed') end
end

item = {{["i_item_sk"]=1, ["i_item_id"]="I1", ["i_item_desc"]="Item1", ["i_current_price"]=30.0, ["i_manufact_id"]=800}, {["i_item_sk"]=2, ["i_item_id"]="I2", ["i_item_desc"]="Item2", ["i_current_price"]=60.0, ["i_manufact_id"]=801}}
inventory = {{["inv_item_sk"]=1, ["inv_warehouse_sk"]=1, ["inv_date_sk"]=1, ["inv_quantity_on_hand"]=200}, {["inv_item_sk"]=2, ["inv_warehouse_sk"]=1, ["inv_date_sk"]=1, ["inv_quantity_on_hand"]=300}}
date_dim = {{["d_date_sk"]=1, ["d_date"]="2000-01-15"}}
catalog_sales = {{["cs_item_sk"]=1, ["cs_sold_date_sk"]=1}}
result = (function()
    local _src = item
    local _rows = __query(_src, {
        { items = inventory, on = function(i, inv) return __eq(i.i_item_sk, inv.inv_item_sk) end },
        { items = date_dim, on = function(i, inv, d) return __eq(inv.inv_date_sk, d.d_date_sk) end },
        { items = catalog_sales, on = function(i, inv, d, cs) return __eq(cs.cs_item_sk, i.i_item_sk) end }
    }, { selectFn = function(i, inv, d, cs) return {i, inv, d, cs} end, where = function(i, inv, d, cs) return (((((((i.i_current_price >= 20) and (i.i_current_price <= 50)) and (i.i_manufact_id >= 800)) and (i.i_manufact_id <= 803)) and (inv.inv_quantity_on_hand >= 100)) and (inv.inv_quantity_on_hand <= 500))) end })
    local _groups = __group_by_rows(_rows, function(i, inv, d, cs) return {["id"]=i.i_item_id, ["desc"]=i.i_item_desc, ["price"]=i.i_current_price} end, function(i, inv, d, cs) local _row = __merge(i, inv, d, cs); _row.i = i; _row.inv = inv; _row.d = d; _row.cs = cs; return _row end)
    local _res = {}
    for _, g in ipairs(_groups) do
        _res[#_res+1] = {["i_item_id"]=g.key.id, ["i_item_desc"]=g.key.desc, ["i_current_price"]=g.key.price}
    end
    return _res
end)()
__json(result)
local __tests = {
    {name="TPCDS Q37 simplified", fn=test_TPCDS_Q37_simplified},
}
