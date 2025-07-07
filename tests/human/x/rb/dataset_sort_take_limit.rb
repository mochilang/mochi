products = [
  {name: 'Laptop', price: 1500},
  {name: 'Smartphone', price: 900},
  {name: 'Tablet', price: 600},
  {name: 'Monitor', price: 300},
  {name: 'Keyboard', price: 100},
  {name: 'Mouse', price: 50},
  {name: 'Headphones', price: 200}
]
expensive = products.sort_by { |p| -p[:price] }[1,3]
puts '--- Top products (excluding most expensive) ---'
expensive.each do |item|
  puts "#{item[:name]} costs $#{item[:price]}"
end
