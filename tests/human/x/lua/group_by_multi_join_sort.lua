local nation = {
  {n_nationkey = 1, n_name = "BRAZIL"}
}
local customer = {
  {c_custkey = 1, c_name = "Alice", c_acctbal = 100.0, c_nationkey = 1,
   c_address = "123 St", c_phone = "123-456", c_comment = "Loyal"}
}
local orders = {
  {o_orderkey = 1000, o_custkey = 1, o_orderdate = "1993-10-15"},
  {o_orderkey = 2000, o_custkey = 1, o_orderdate = "1994-01-02"}
}
local lineitem = {
  {l_orderkey = 1000, l_returnflag = "R", l_extendedprice = 1000.0, l_discount = 0.1},
  {l_orderkey = 2000, l_returnflag = "N", l_extendedprice = 500.0,  l_discount = 0.0}
}

local start_date = "1993-10-01"
local end_date = "1994-01-01"

local temp = {}
for _, c in ipairs(customer) do
  for _, o in ipairs(orders) do
    if o.o_custkey == c.c_custkey and o.o_orderdate >= start_date and o.o_orderdate < end_date then
      for _, l in ipairs(lineitem) do
        if l.l_orderkey == o.o_orderkey and l.l_returnflag == "R" then
          local n
          for _, nn in ipairs(nation) do if nn.n_nationkey == c.c_nationkey then n = nn break end end
          if n then
            local key = table.concat({c.c_custkey,c.c_name,c.c_acctbal,c.c_address,c.c_phone,c.c_comment,n.n_name}, "|")
            local entry = temp[key]
            if not entry then
              entry = {key={
                c_custkey=c.c_custkey,
                c_name=c.c_name,
                c_acctbal=c.c_acctbal,
                c_address=c.c_address,
                c_phone=c.c_phone,
                c_comment=c.c_comment,
                n_name=n.n_name
              }, revenue=0}
              temp[key]=entry
            end
            entry.revenue = entry.revenue + l.l_extendedprice * (1 - l.l_discount)
          end
        end
      end
    end
  end
end

local result = {}
for _, v in pairs(temp) do
  table.insert(result, {c_custkey=v.key.c_custkey, c_name=v.key.c_name, revenue=v.revenue,
    c_acctbal=v.key.c_acctbal, n_name=v.key.n_name,
    c_address=v.key.c_address, c_phone=v.key.c_phone, c_comment=v.key.c_comment})
end

table.sort(result, function(a,b) return a.revenue > b.revenue end)

for _, r in ipairs(result) do
  print(r.c_name .. " revenue=" .. r.revenue)
end
