customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'}
]
orders = [
  {id: 100, customerId: 1, total: 250},
  {id: 101, customerId: 3, total: 80}
]

result = orders.map do |o|
  c = customers.find { |cust| cust[:id] == o[:customerId] }
  {orderId: o[:id], customer: c, total: o[:total]}
end

puts '--- Left Join ---'
result.each do |e|
  puts "Order #{e[:orderId]} customer #{e[:customer]} total #{e[:total]}"
end
