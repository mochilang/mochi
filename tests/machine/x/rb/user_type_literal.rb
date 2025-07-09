Person = Struct.new(:name, :age, keyword_init: true)

Book = Struct.new(:title, :author, keyword_init: true)

$book = Book.new(title: "Go", author: Person.new(name: "Bob", age: 42))
puts($book.author.name)
