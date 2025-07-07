customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'}
]
orders = [
  {id: 100, customerId: 1},
  {id: 101, customerId: 2}
]
items = [
  {orderId: 100, sku: 'a'},
  {orderId: 101, sku: 'b'}
]

result = []
orders.each do |o|
  c = customers.find { |cust| cust[:id] == o[:customerId] }
  next unless c
  items.each do |i|
    next unless i[:orderId] == o[:id]
    result << {name: c[:name], sku: i[:sku]}
  end
end

puts '--- Multi Join ---'
result.each { |r| puts "#{r[:name]} bought item #{r[:sku]}" }
