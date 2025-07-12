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
print("--- Cross Join: All order-customer pairs ---")
for _, entry in ipairs(result) do
    print("Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName)
    ::__continue0::
end
