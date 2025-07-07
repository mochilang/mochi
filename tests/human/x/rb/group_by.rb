people = [
  {name: 'Alice', age: 30, city: 'Paris'},
  {name: 'Bob', age: 15, city: 'Hanoi'},
  {name: 'Charlie', age: 65, city: 'Paris'},
  {name: 'Diana', age: 45, city: 'Hanoi'},
  {name: 'Eve', age: 70, city: 'Paris'},
  {name: 'Frank', age: 22, city: 'Hanoi'}
]

stats = people.group_by { |p| p[:city] }.map do |city, group|
  ages = group.map { |p| p[:age] }
  { city: city, count: group.length, avg_age: ages.sum.to_f / ages.length }
end

puts '--- People grouped by city ---'
stats.each do |s|
  puts "#{s[:city]}: count = #{s[:count]}, avg_age = #{s[:avg_age]}"
end
