function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
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
__print(book.author.name)
