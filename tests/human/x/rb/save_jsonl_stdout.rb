require 'json'
people = [
  {name: 'Alice', age: 30},
  {name: 'Bob', age: 25}
]
people.each { |p| puts JSON.generate(p) }
