customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'},
  {id: 3, name: 'Charlie'}
]
orders = [
  {id: 100, customerId: 1, total: 250},
  {id: 101, customerId: 2, total: 125},
  {id: 102, customerId: 1, total: 300},
  {id: 103, customerId: 4, total: 80}
]

result = []
orders.each do |o|
  c = customers.find { |cust| cust[:id] == o[:customerId] }
  next unless c
  result << {orderId: o[:id], customerName: c[:name], total: o[:total]}
end

puts '--- Orders with customer info ---'
result.each do |e|
  puts "Order #{e[:orderId]} by #{e[:customerName]} - $#{e[:total]}"
end
