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
function __div(a, b)
    if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
        return a // b
    end
    return a / b
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
function test_TPCDS_Q34_simplified()
    if not (__eq(result, {{["c_last_name"]="Smith", ["c_first_name"]="John", ["c_salutation"]="Mr.", ["c_preferred_cust_flag"]="Y", ["ss_ticket_number"]=1, ["cnt"]=16}})) then error('expect failed') end
end

store_sales = {{["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=1, ["ss_customer_sk"]=1, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=1}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}, {["ss_ticket_number"]=2, ["ss_customer_sk"]=2, ["ss_sold_date_sk"]=1, ["ss_store_sk"]=1, ["ss_hdemo_sk"]=2}}
date_dim = {{["d_date_sk"]=1, ["d_dom"]=2, ["d_year"]=2000}}
store = {{["s_store_sk"]=1, ["s_county"]="A"}}
household_demographics = {{["hd_demo_sk"]=1, ["hd_buy_potential"]=">10000", ["hd_vehicle_count"]=2, ["hd_dep_count"]=3}, {["hd_demo_sk"]=2, ["hd_buy_potential"]=">10000", ["hd_vehicle_count"]=2, ["hd_dep_count"]=1}}
customer = {{["c_customer_sk"]=1, ["c_last_name"]="Smith", ["c_first_name"]="John", ["c_salutation"]="Mr.", ["c_preferred_cust_flag"]="Y"}, {["c_customer_sk"]=2, ["c_last_name"]="Jones", ["c_first_name"]="Alice", ["c_salutation"]="Ms.", ["c_preferred_cust_flag"]="N"}}
dn = (function()
    local _src = store_sales
    local _rows = __query(_src, {
        { items = date_dim, on = function(ss, d) return __eq(ss.ss_sold_date_sk, d.d_date_sk) end },
        { items = store, on = function(ss, d, s) return __eq(ss.ss_store_sk, s.s_store_sk) end },
        { items = household_demographics, on = function(ss, d, s, hd) return __eq(ss.ss_hdemo_sk, hd.hd_demo_sk) end }
    }, { selectFn = function(ss, d, s, hd) return {ss, d, s, hd} end, where = function(ss, d, s, hd) return (((((((((d.d_dom >= 1) and (d.d_dom <= 3))) and __eq(hd.hd_buy_potential, ">10000")) and (hd.hd_vehicle_count > 0)) and ((__div(hd.hd_dep_count, hd.hd_vehicle_count)) > 1.2)) and __eq(d.d_year, 2000)) and __eq(s.s_county, "A"))) end })
    local _groups = __group_by_rows(_rows, function(ss, d, s, hd) return {["ticket"]=ss.ss_ticket_number, ["cust"]=ss.ss_customer_sk} end, function(ss, d, s, hd) local _row = __merge(ss, d, s, hd); _row.ss = ss; _row.d = d; _row.s = s; _row.hd = hd; return _row end)
    local _res = {}
    for _, g in ipairs(_groups) do
        _res[#_res+1] = {["ss_ticket_number"]=g.key.ticket, ["ss_customer_sk"]=g.key.cust, ["cnt"]=__count(g)}
    end
    return _res
end)()
result = (function()
    local _src = dn
    return __query(_src, {
        { items = customer, on = function(dn1, c) return __eq(dn1.ss_customer_sk, c.c_customer_sk) end }
    }, { selectFn = function(dn1, c) return {["c_last_name"]=c.c_last_name, ["c_first_name"]=c.c_first_name, ["c_salutation"]=c.c_salutation, ["c_preferred_cust_flag"]=c.c_preferred_cust_flag, ["ss_ticket_number"]=dn1.ss_ticket_number, ["cnt"]=dn1.cnt} end, where = function(dn1, c) return (((dn1.cnt >= 15) and (dn1.cnt <= 20))) end, sortKey = function(dn1, c) return (c.c_last_name) end })
end)()
__json(result)
local __tests = {
    {name="TPCDS Q34 simplified", fn=test_TPCDS_Q34_simplified},
}
