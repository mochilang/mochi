Person = Struct.new(:name, :age, :status, keyword_init: true)

people = [Person.new(name: "Alice", age: 17, status: "minor"), Person.new(name: "Bob", age: 25, status: "unknown"), Person.new(name: "Charlie", age: 18, status: "unknown"), Person.new(name: "Diana", age: 16, status: "minor")]
puts(["ok"].join(" "))
raise "expect failed" unless (people == [Person.new(name: "Alice", age: 17, status: "minor"), Person.new(name: "Bob", age: 26, status: "adult"), Person.new(name: "Charlie", age: 19, status: "adult"), Person.new(name: "Diana", age: 16, status: "minor")])
