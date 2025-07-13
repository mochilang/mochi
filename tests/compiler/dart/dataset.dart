Map<String, Function> _structParsers = {};

class Person {
  String name;
  int age;
  Person({required this.name, required this.age});
  factory Person.fromJson(Map<String,dynamic> m) {
    return Person(name: m['name'] as String, age: m['age'] as int);
  }
}

List<Person> people = [Person(name: "Alice", age: 30), Person(name: "Bob", age: 15), Person(name: "Charlie", age: 65)];

List<String> names = (() {
  var _res = [];
  var _src = (people).where((p) => (p.age >= 18)).toList();
  for (var p in _src) {
    _res.add(p.name);
  }
  return _res;
})();

void main() {
  _structParsers['Person'] = (m) => Person.fromJson(m);
  
  for (var n in names) {
    print(n);
  }
}
