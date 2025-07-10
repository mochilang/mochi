Person = Struct.new(:name, :age, :status, keyword_init: true)

$people = [Person.new(name: "Alice", age: 17, status: "minor"), Person.new(name: "Bob", age: 25, status: "unknown"), Person.new(name: "Charlie", age: 18, status: "unknown"), Person.new(name: "Diana", age: 16, status: "minor")]
$people.each_with_index do |_t1, _t0|
  age = _t1.age
  status = _t1.status
  next unless (age >= 18)
  _t1.status = "adult"
  _t1.age = (age + 1)
end
puts("ok")
raise "expect failed" unless ($people == [Person.new(name: "Alice", age: 17, status: "minor"), Person.new(name: "Bob", age: 26, status: "adult"), Person.new(name: "Charlie", age: 19, status: "adult"), Person.new(name: "Diana", age: 16, status: "minor")])
