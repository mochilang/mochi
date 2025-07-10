from __future__ import annotations
import dataclasses


@dataclasses.dataclass
class Person:
    name: str
    age: int


@dataclasses.dataclass
class Book:
    title: str
    author: Person


book = Book(title="Go", author=Person(name="Bob", age=42))
print(book.author.name)
