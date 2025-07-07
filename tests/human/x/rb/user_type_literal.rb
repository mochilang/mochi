Person = Struct.new(:name, :age)
Book = Struct.new(:title, :author)

book = Book.new('Go', Person.new('Bob', 42))
puts book.author.name
