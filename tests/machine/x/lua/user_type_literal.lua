Person = {}
Person.__index = Person
function Person.new(o)
    o = o or {}
    setmetatable(o, Person)
    return o
end

Book = {}
Book.__index = Book
function Book.new(o)
    o = o or {}
    setmetatable(o, Book)
    return o
end

book = {title="Go", author={name="Bob", age=42}}
print(book.author.name)
