people = [
  {name: 'Alice', age: 17, status: 'minor'},
  {name: 'Bob', age: 25, status: 'unknown'},
  {name: 'Charlie', age: 18, status: 'unknown'},
  {name: 'Diana', age: 16, status: 'minor'}
]

people.each do |p|
  if p[:age] >= 18
    p[:status] = 'adult'
    p[:age] += 1
  end
end

expected = [
  {name: 'Alice', age: 17, status: 'minor'},
  {name: 'Bob', age: 26, status: 'adult'},
  {name: 'Charlie', age: 19, status: 'adult'},
  {name: 'Diana', age: 16, status: 'minor'}
]

if people == expected
  puts 'ok'
else
  puts 'mismatch'
end
