from dataclasses import dataclass

@dataclass
class Person:
    name: str
    age: int

@dataclass
class Book:
    title: str
    author: Person

book = Book(title="Go", author=Person(name="Bob", age=42))
print(book.author.name)
