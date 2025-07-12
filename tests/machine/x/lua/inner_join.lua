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
customers = {{["id"]=1, ["name"]="Alice"}, {["id"]=2, ["name"]="Bob"}, {["id"]=3, ["name"]="Charlie"}}
orders = {{["id"]=100, ["customerId"]=1, ["total"]=250}, {["id"]=101, ["customerId"]=2, ["total"]=125}, {["id"]=102, ["customerId"]=1, ["total"]=300}, {["id"]=103, ["customerId"]=4, ["total"]=80}}
result = (function()
    local _src = orders
    return __query(_src, {
        { items = customers, on = function(o, c) return __eq(o.customerId, c.id) end }
    }, { selectFn = function(o, c) return {["orderId"]=o.id, ["customerName"]=c.name, ["total"]=o.total} end })
end)()
print("--- Orders with customer info ---")
for _, entry in ipairs(result) do
    print("Order", entry.orderId, "by", entry.customerName, "- $", entry.total)
    ::__continue0::
end
