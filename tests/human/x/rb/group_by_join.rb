customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'}
]
orders = [
  {id: 100, customerId: 1},
  {id: 101, customerId: 1},
  {id: 102, customerId: 2}
]

pairs = orders.map do |o|
  c = customers.find { |cust| cust[:id] == o[:customerId] }
  c ? {name: c[:name]} : nil
end.compact
groups = pairs.group_by { |x| x[:name] }
stats = groups.map { |name, g| { name: name, count: g.length } }
puts '--- Orders per customer ---'
stats.each { |s| puts "#{s[:name]} orders: #{s[:count]}" }
