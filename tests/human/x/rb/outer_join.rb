customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'},
  {id: 3, name: 'Charlie'},
  {id: 4, name: 'Diana'}
]
orders = [
  {id: 100, customerId: 1, total: 250},
  {id: 101, customerId: 2, total: 125},
  {id: 102, customerId: 1, total: 300},
  {id: 103, customerId: 5, total: 80}
]

result = []
orders.each do |o|
  c = customers.find { |cust| cust[:id] == o[:customerId] }
  result << {order: o, customer: c}
end
customers.each do |c|
  unless orders.any? { |o| o[:customerId] == c[:id] }
    result << {order: nil, customer: c}
  end
end

puts '--- Outer Join using syntax ---'
result.each do |row|
  if row[:order]
    if row[:customer]
      puts "Order #{row[:order][:id]} by #{row[:customer][:name]} - $#{row[:order][:total]}"
    else
      puts "Order #{row[:order][:id]} by Unknown - $#{row[:order][:total]}"
    end
  else
    puts "Customer #{row[:customer][:name]} has no orders"
  end
end
