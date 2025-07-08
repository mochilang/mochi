function __print(...)
    local args = {...}
    local function to_str(v)
        if v == nil then return '<nil>' end
        if type(v) == 'table' then
            if v[1] ~= nil or #v > 0 then
                local parts = {}
                for i,x in ipairs(v) do parts[#parts+1] = tostring(x) end
                return table.concat(parts, ' ')
            end
        end
        if type(v) == 'number' and v == math.floor(v) then
            return tostring(math.floor(v))
        end
        return tostring(v)
    end
    for i, a in ipairs(args) do
        if i > 1 then io.write(' ') end
        io.write(to_str(a))
    end
    io.write('\n')
end
customers = {{["id"]=1, ["name"]="Alice"}, {["id"]=2, ["name"]="Bob"}, {["id"]=3, ["name"]="Charlie"}}
orders = {{["id"]=100, ["customerId"]=1, ["total"]=250}, {["id"]=101, ["customerId"]=2, ["total"]=125}, {["id"]=102, ["customerId"]=1, ["total"]=300}}
result = (function()
    local _res = {}
    for _, o in ipairs(orders) do
        for _, c in ipairs(customers) do
            _res[#_res+1] = {["orderId"]=o.id, ["orderCustomerId"]=o.customerId, ["pairedCustomerName"]=c.name, ["orderTotal"]=o.total}
        end
    end
    return _res
end)()
__print("--- Cross Join: All order-customer pairs ---")
for _, entry in ipairs(result) do
    __print("Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName)
    ::__continue0::
end
