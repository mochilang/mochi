class Person {
  String name;
  int age;
  String status;
  Person(this.name, this.age, this.status);
}

void main() {
  List<Person> people = [Person('Alice', 17, 'minor'), Person('Bob', 25, 'unknown'), Person('Charlie', 18, 'unknown'), Person('Diana', 16, 'minor')];
  for (var _i0 = 0; _i0 < people.length; _i0++) {
    var _it1 = people[_i0];
    var name = _it1.name;
    var age = _it1.age;
    var status = _it1.status;
    if (age >= 18) {
      _it1.status = 'adult';
      _it1.age = age + 1;
    }
    people[_i0] = _it1;
  }
  print('ok');
}
