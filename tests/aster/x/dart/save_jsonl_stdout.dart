// Generated by Mochi transpiler
import 'dart:convert';
class People {
  final String name;
  final int age;
  const People({required this.name, required this.age});
}
void main() {
  final List<People> people = [People(name: "Alice", age: 30), People(name: "Bob", age: 25)];
  for (var _row in people) {
    var _tmp = {, };
    print(jsonEncode(_tmp));
  }
}
