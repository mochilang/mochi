require 'json'
people = [
  {name: 'Alice', city: 'Paris'},
  {name: 'Bob', city: 'Hanoi'},
  {name: 'Charlie', city: 'Paris'},
  {name: 'Diana', city: 'Hanoi'},
  {name: 'Eve', city: 'Paris'},
  {name: 'Frank', city: 'Hanoi'},
  {name: 'George', city: 'Paris'}
]

groups = people.group_by { |p| p[:city] }
big = groups.select { |_, g| g.length >= 4 }.map { |city, g| { city: city, num: g.length } }
puts JSON.generate(big)
