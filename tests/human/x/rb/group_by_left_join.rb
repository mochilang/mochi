customers = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'},
  {id: 3, name: 'Charlie'}
]
orders = [
  {id: 100, customerId: 1},
  {id: 101, customerId: 1},
  {id: 102, customerId: 2}
]

pairs = []
customers.each do |c|
  matched = orders.select { |o| o[:customerId] == c[:id] }
  if matched.empty?
    pairs << {name: c[:name], o: nil}
  else
    matched.each { |o| pairs << {name: c[:name], o: o} }
  end
end

groups = pairs.group_by { |x| x[:name] }
stats = groups.map do |name, rows|
  count = rows.count { |r| r[:o] }
  { name: name, count: count }
end

puts '--- Group Left Join ---'
stats.each { |s| puts "#{s[:name]} orders: #{s[:count]}" }
