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
function __min(v)
    local items
    if type(v) == 'table' and v.items ~= nil then
        items = v.items
    elseif type(v) == 'table' then
        items = v
    else
        error('min() expects list or group')
    end
    if #items == 0 then return 0 end
    local m = items[1]
    if type(m) == 'string' then
        for i=2,#items do
            local it = items[i]
            if type(it) == 'string' and it < m then m = it end
        end
        return m
    else
        m = tonumber(m)
        for i=2,#items do
            local n = tonumber(items[i])
            if n < m then m = n end
        end
        return m
    end
end
function __query(src, joins, opts)
    local whereFn = opts.where
    local items = {}
    if #joins == 0 and whereFn then
        for _, v in ipairs(src) do if whereFn(v) then items[#items+1] = {v} end end
    else
        for _, v in ipairs(src) do items[#items+1] = {v} end
    end
    for ji, j in ipairs(joins) do
        local joined = {}
        local jitems = j.items or {}
        if j.right and j.left then
            local matched = {}
            for _, left in ipairs(items) do
                local m = false
                for ri, right in ipairs(jitems) do
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left)}
                        args[#args+1] = right
                        keep = j.on(table.unpack(args))
                    end
                    if keep then
                        m = true; matched[ri] = true
                        local row = {table.unpack(left)}
                        row[#row+1] = right
                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if not m then
                    local row = {table.unpack(left)}
                    row[#row+1] = nil
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
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
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        elseif j.right then
            for _, right in ipairs(jitems) do
                local m = false
                for _, left in ipairs(items) do
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left)}
                        args[#args+1] = right
                        keep = j.on(table.unpack(args))
                    end
                    if keep then
                        m = true
                        local row = {table.unpack(left)}
                        row[#row+1] = right
                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if not m then
                    local row = {}
                    for _=1,ji do row[#row+1] = nil end
                    row[#row+1] = right
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        else
            for _, left in ipairs(items) do
                local m = false
                for _, right in ipairs(jitems) do
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left)}
                        args[#args+1] = right
                        keep = j.on(table.unpack(args))
                    end
                    if keep then
                        m = true
                        local row = {table.unpack(left)}
                        row[#row+1] = right
                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if j.left and not m then
                    local row = {table.unpack(left)}
                    row[#row+1] = nil
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
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
        for _, it in ipairs(items) do pairs[#pairs+1] = {item=it, key=opts.sortKey(table.unpack(it))} end
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
    for _, r in ipairs(items) do res[#res+1] = opts.selectFn(table.unpack(r)) end
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
function test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part()
    if not (__eq(result, {{["s_acctbal"]=1000.0, ["s_name"]="BestSupplier", ["n_name"]="FRANCE", ["p_partkey"]=1000, ["p_mfgr"]="M1", ["s_address"]="123 Rue", ["s_phone"]="123", ["s_comment"]="Fast and reliable", ["ps_supplycost"]=10.0}})) then error('expect failed') end
end

region = {{["r_regionkey"]=1, ["r_name"]="EUROPE"}, {["r_regionkey"]=2, ["r_name"]="ASIA"}}
nation = {{["n_nationkey"]=10, ["n_regionkey"]=1, ["n_name"]="FRANCE"}, {["n_nationkey"]=20, ["n_regionkey"]=2, ["n_name"]="CHINA"}}
supplier = {{["s_suppkey"]=100, ["s_name"]="BestSupplier", ["s_address"]="123 Rue", ["s_nationkey"]=10, ["s_phone"]="123", ["s_acctbal"]=1000.0, ["s_comment"]="Fast and reliable"}, {["s_suppkey"]=200, ["s_name"]="AltSupplier", ["s_address"]="456 Way", ["s_nationkey"]=20, ["s_phone"]="456", ["s_acctbal"]=500.0, ["s_comment"]="Slow"}}
part = {{["p_partkey"]=1000, ["p_type"]="LARGE BRASS", ["p_size"]=15, ["p_mfgr"]="M1"}, {["p_partkey"]=2000, ["p_type"]="SMALL COPPER", ["p_size"]=15, ["p_mfgr"]="M2"}}
partsupp = {{["ps_partkey"]=1000, ["ps_suppkey"]=100, ["ps_supplycost"]=10.0}, {["ps_partkey"]=1000, ["ps_suppkey"]=200, ["ps_supplycost"]=15.0}}
europe_nations = (function()
    local _src = region
    return __query(_src, {
        { items = nation, on = function(r, n) return __eq(n.n_regionkey, r.r_regionkey) end }
    }, { selectFn = function(r, n) return n end, where = function(r, n) return (__eq(r.r_name, "EUROPE")) end })
end)()
europe_suppliers = (function()
    local _src = supplier
    return __query(_src, {
        { items = europe_nations, on = function(s, n) return __eq(s.s_nationkey, n.n_nationkey) end }
    }, { selectFn = function(s, n) return {["s"]=s, ["n"]=n} end })
end)()
target_parts = (function()
    local _res = {}
    for _, p in ipairs(part) do
        if (__eq(p.p_size, 15) and __eq(p.p_type, "LARGE BRASS")) then
            _res[#_res+1] = p
        end
    end
    return _res
end)()
target_partsupp = (function()
    local _src = partsupp
    return __query(_src, {
        { items = target_parts, on = function(ps, p) return __eq(ps.ps_partkey, p.p_partkey) end },
        { items = europe_suppliers, on = function(ps, p, s) return __eq(ps.ps_suppkey, s.s.s_suppkey) end }
    }, { selectFn = function(ps, p, s) return {["s_acctbal"]=s.s.s_acctbal, ["s_name"]=s.s.s_name, ["n_name"]=s.n.n_name, ["p_partkey"]=p.p_partkey, ["p_mfgr"]=p.p_mfgr, ["s_address"]=s.s.s_address, ["s_phone"]=s.s.s_phone, ["s_comment"]=s.s.s_comment, ["ps_supplycost"]=ps.ps_supplycost} end })
end)()
costs = (function()
    local _res = {}
    for _, x in ipairs(target_partsupp) do
        _res[#_res+1] = x.ps_supplycost
    end
    return _res
end)()
min_cost = __min(costs)
result = (function()
    local _res = {}
    for _, x in ipairs(target_partsupp) do
        if __eq(x.ps_supplycost, min_cost) then
            _res[#_res+1] = {__key = -x.s_acctbal, __val = x}
        end
    end
    local items = _res
    table.sort(items, function(a,b) return a.__key < b.__key end)
    local tmp = {}
    for _, it in ipairs(items) do tmp[#tmp+1] = it.__val end
    items = tmp
    _res = items
    return _res
end)()
__json(result)
local __tests = {
    {name="Q2 returns only supplier with min cost in Europe for brass part", fn=test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part},
}
