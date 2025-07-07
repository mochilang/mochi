people = [
  {name: 'Alice', age: 30},
  {name: 'Bob', age: 15},
  {name: 'Charlie', age: 65},
  {name: 'Diana', age: 45}
]
adults = people.select { |p| p[:age] >= 18 }.map do |p|
  {
    name: p[:name],
    age: p[:age],
    is_senior: p[:age] >= 60
  }
end
puts '--- Adults ---'
adults.each do |person|
  suffix = person[:is_senior] ? ' (senior)' : ''
  puts "#{person[:name]} is #{person[:age]}#{suffix}"
end
