class Person {
  String name;
  int age;
  String status;
  Person(this.name, this.age, this.status);
  @override
  String toString() => 'Person{name: $name, age: $age, status: $status}';
}

void main() {
  var people = [
    Person('Alice', 17, 'minor'),
    Person('Bob', 25, 'unknown'),
    Person('Charlie', 18, 'unknown'),
    Person('Diana', 16, 'minor'),
  ];

  for (var p in people) {
    if (p.age >= 18) {
      p.status = 'adult';
      p.age = p.age + 1;
    }
  }

  print('ok');
  print(people);
}
