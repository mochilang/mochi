require 'ostruct'

$people = [OpenStruct.new(name: "Alice", age: 30), OpenStruct.new(name: "Bob", age: 15), OpenStruct.new(name: "Charlie", age: 65), OpenStruct.new(name: "Diana", age: 45)]
$adults = ((($people)).select { |person| (person.age >= 18) }).map { |person| OpenStruct.new(name: person.name, age: person.age, is_senior: (person.age >= 60)) }
puts("--- Adults ---")
for person in $adults
	puts([person.name, "is", person.age, (person.is_senior ? " (senior)" : "")].join(" "))
end
