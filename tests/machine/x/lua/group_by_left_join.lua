_Group = {}
function _Group.new(k)
    return {key = k, items = {}}
end
function __group_by_rows(rows, keyfn, valfn)
    local groups = {}
    local order = {}
    for _, r in ipairs(rows) do
        local key = keyfn(table.unpack(r))
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
        table.insert(g.items, valfn(table.unpack(r)))
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
                    local undef = {}
                    if #items > 0 then for _=1,#items[1] do undef[#undef+1]=nil end end
                    local row = {table.unpack(undef)}
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
                    local undef = {}
                    if #items > 0 then for _=1,#items[1] do undef[#undef+1]=nil end end
                    local row = {table.unpack(undef)}
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
customers = {{["id"]=1, ["name"]="Alice"}, {["id"]=2, ["name"]="Bob"}, {["id"]=3, ["name"]="Charlie"}}
orders = {{["id"]=100, ["customerId"]=1}, {["id"]=101, ["customerId"]=1}, {["id"]=102, ["customerId"]=2}}
stats = (function()
    local _src = customers
    local _rows = __query(_src, {
        { items = orders, on = function(c, o) return __eq(o.customerId, c.id) end, left = true }
    }, { selectFn = function(c, o) return {c, o} end })
    local _groups = __group_by_rows(_rows, function(c, o) return c.name end, function(c, o) local _row = __merge(c, o); _row.c = c; _row.o = o; return _row end)
    local _res = {}
    for _, g in ipairs(_groups) do
        _res[#_res+1] = {["name"]=g.key, ["count"]=__count((function()
    local _res = {}
    for _, r in ipairs(g.items) do
        if r.o then
            _res[#_res+1] = r
        end
    end
    return _res
end)())}
    end
    return _res
end)()
print("--- Group Left Join ---")
for _, s in ipairs(stats) do
    print(s.name, "orders:", s.count)
    ::__continue0::
end
