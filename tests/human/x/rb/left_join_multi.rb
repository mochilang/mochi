customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'}
]
orders = [
  {id: 100, customerId: 1},
  {id: 101, customerId: 2}
]
items = [
  {orderId: 100, sku: 'a'}
]

result = []
orders.each do |o|
  c = customers.find { |cust| cust[:id] == o[:customerId] }
  its = items.select { |i| i[:orderId] == o[:id] }
  if its.empty?
    result << {orderId: o[:id], name: c[:name], item: nil}
  else
    its.each { |i| result << {orderId: o[:id], name: c[:name], item: i} }
  end
end

puts '--- Left Join Multi ---'
result.each { |r| puts "#{r[:orderId]} #{r[:name]} #{r[:item]}" }
