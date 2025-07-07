customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'},
  {id: 3, name: 'Charlie'}
]
orders = [
  {id: 100, customerId: 1, total: 250},
  {id: 101, customerId: 2, total: 125},
  {id: 102, customerId: 1, total: 300}
]
result = []
orders.each do |o|
  customers.each do |c|
    result << {
      orderId: o[:id],
      orderCustomerId: o[:customerId],
      pairedCustomerName: c[:name],
      orderTotal: o[:total]
    }
  end
end
puts '--- Cross Join: All order-customer pairs ---'
result.each do |entry|
  puts "Order #{entry[:orderId]} (customerId: #{entry[:orderCustomerId]}, total: $#{entry[:orderTotal]}) paired with #{entry[:pairedCustomerName]}"
end
