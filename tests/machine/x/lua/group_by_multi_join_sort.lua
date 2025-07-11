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
nation = {{["n_nationkey"]=1, ["n_name"]="BRAZIL"}}
customer = {{["c_custkey"]=1, ["c_name"]="Alice", ["c_acctbal"]=100.0, ["c_nationkey"]=1, ["c_address"]="123 St", ["c_phone"]="123-456", ["c_comment"]="Loyal"}}
orders = {{["o_orderkey"]=1000, ["o_custkey"]=1, ["o_orderdate"]="1993-10-15"}, {["o_orderkey"]=2000, ["o_custkey"]=1, ["o_orderdate"]="1994-01-02"}}
lineitem = {{["l_orderkey"]=1000, ["l_returnflag"]="R", ["l_extendedprice"]=1000.0, ["l_discount"]=0.1}, {["l_orderkey"]=2000, ["l_returnflag"]="N", ["l_extendedprice"]=500.0, ["l_discount"]=0.0}}
start_date = "1993-10-01"
end_date = "1994-01-01"
result = (function()
  local _src = customer
  local _rows = __query(_src, {
    { items = orders, on = function(c, o) return __eq(o.o_custkey, c.c_custkey) end },
    { items = lineitem, on = function(c, o, l) return __eq(l.l_orderkey, o.o_orderkey) end },
    { items = nation, on = function(c, o, l, n) return __eq(n.n_nationkey, c.c_nationkey) end }
  }, { selectFn = function(c, o, l, n) return {c, o, l, n} end, where = function(c, o, l, n) return ((((o.o_orderdate >= start_date) and (o.o_orderdate < end_date)) and __eq(l.l_returnflag, "R"))) end })
  local _groups = __group_by_rows(_rows, function(c, o, l, n) return {["c_custkey"]=c.c_custkey, ["c_name"]=c.c_name, ["c_acctbal"]=c.c_acctbal, ["c_address"]=c.c_address, ["c_phone"]=c.c_phone, ["c_comment"]=c.c_comment, ["n_name"]=n.n_name} end, function(c, o, l, n) local _row = __merge(c, o, l, n); _row.c = c; _row.o = o; _row.l = l; _row.n = n; return _row end)
  local _res = {}
  for _, g in ipairs(_groups) do
    _res[#_res+1] = {["c_custkey"]=g.key.c_custkey, ["c_name"]=g.key.c_name, ["revenue"]=__sum((function()
  local _res = {}
  for _, x in ipairs(g.items) do
    _res[#_res+1] = (x.l.l_extendedprice * ((1 - x.l.l_discount)))
  end
  return _res
end)()), ["c_acctbal"]=g.key.c_acctbal, ["n_name"]=g.key.n_name, ["c_address"]=g.key.c_address, ["c_phone"]=g.key.c_phone, ["c_comment"]=g.key.c_comment}
  end
  return _res
end)()
print(table.concat(result, " "))
