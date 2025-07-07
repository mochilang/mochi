customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'},
  {id: 3, name: 'Charlie'},
  {id: 4, name: 'Diana'}
]
orders = [
  {id: 100, customerId: 1, total: 250},
  {id: 101, customerId: 2, total: 125},
  {id: 102, customerId: 1, total: 300}
]

result = customers.map do |c|
  o = orders.find { |ord| ord[:customerId] == c[:id] }
  {customerName: c[:name], order: o}
end

puts '--- Right Join using syntax ---'
result.each do |entry|
  if entry[:order]
    puts "Customer #{entry[:customerName]} has order #{entry[:order][:id]} - $#{entry[:order][:total]}"
  else
    puts "Customer #{entry[:customerName]} has no orders"
  end
end
