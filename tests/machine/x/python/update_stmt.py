from dataclasses import dataclass

@dataclass
class Person:
    name: str
    age: int
    status: str

people = [Person(name="Alice", age=17, status="minor"), Person(name="Bob", age=25, status="unknown"), Person(name="Charlie", age=18, status="unknown"), Person(name="Diana", age=16, status="minor")]
for __tmp1, __tmp2 in enumerate(people):
    name = __tmp2.name
    age = __tmp2.age
    status = __tmp2.status
    if age >= 18:
        __tmp2.status = "adult"
        __tmp2.age = age + 1
    people[__tmp1] = __tmp2
# test "update adult status"
assert people == [Person(name="Alice", age=17, status="minor"), Person(name="Bob", age=26, status="adult"), Person(name="Charlie", age=19, status="adult"), Person(name="Diana", age=16, status="minor")]
print("ok")
