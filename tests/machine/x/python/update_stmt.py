class Person:
    def __init__(self, name: str, age: int, status: str):
        self.name = name
        self.age = age
        self.status = status

    def __eq__(self, other):
        return (
            self.name == other.name
            and self.age == other.age
            and self.status == other.status
        )

    def __repr__(self):
        return f"Person(name={self.name!r}, age={self.age}, status={self.status!r})"

people = [
    Person("Alice", 17, "minor"),
    Person("Bob", 25, "unknown"),
    Person("Charlie", 18, "unknown"),
    Person("Diana", 16, "minor"),
]

for p in people:
    if p.age >= 18:
        p.status = "adult"
        p.age += 1

expected = [
    Person("Alice", 17, "minor"),
    Person("Bob", 26, "adult"),
    Person("Charlie", 19, "adult"),
    Person("Diana", 16, "minor"),
]

assert all(a == b for a, b in zip(people, expected))
print("ok")
