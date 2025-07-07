require 'date'

nation = [
  {n_nationkey: 1, n_name: 'BRAZIL'}
]
customer = [
  {
    c_custkey: 1,
    c_name: 'Alice',
    c_acctbal: 100.0,
    c_nationkey: 1,
    c_address: '123 St',
    c_phone: '123-456',
    c_comment: 'Loyal'
  }
]
orders = [
  {o_orderkey: 1000, o_custkey: 1, o_orderdate: '1993-10-15'},
  {o_orderkey: 2000, o_custkey: 1, o_orderdate: '1994-01-02'}
]
lineitem = [
  {l_orderkey: 1000, l_returnflag: 'R', l_extendedprice: 1000.0, l_discount: 0.1},
  {l_orderkey: 2000, l_returnflag: 'N', l_extendedprice: 500.0, l_discount: 0.0}
]

start_date = Date.parse('1993-10-01')
end_date = Date.parse('1994-01-01')
records = []
customer.each do |c|
  orders.each do |o|
    next unless o[:o_custkey] == c[:c_custkey]
    d = Date.parse(o[:o_orderdate])
    next unless d >= start_date && d < end_date
    lineitem.each do |l|
      next unless l[:l_orderkey] == o[:o_orderkey]
      next unless l[:l_returnflag] == 'R'
      nation.each do |n|
        next unless n[:n_nationkey] == c[:c_nationkey]
        records << {c: c, o: o, l: l, n: n}
      end
    end
  end
end

groups = records.group_by do |x|
  {
    c_custkey: x[:c][:c_custkey],
    c_name: x[:c][:c_name],
    c_acctbal: x[:c][:c_acctbal],
    c_address: x[:c][:c_address],
    c_phone: x[:c][:c_phone],
    c_comment: x[:c][:c_comment],
    n_name: x[:n][:n_name]
  }
end

result = groups.map do |key, arr|
  revenue = arr.sum { |x| x[:l][:l_extendedprice] * (1 - x[:l][:l_discount]) }
  key.merge(revenue: revenue)
end
result.sort_by! { |r| -r[:revenue] }

p result
