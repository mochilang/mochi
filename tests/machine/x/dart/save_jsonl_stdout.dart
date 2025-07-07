import 'dart:convert';

void main() {
  var people = [
    {'name': 'Alice', 'age': 30},
    {'name': 'Bob', 'age': 25}
  ];
  for (var p in people) {
    print(jsonEncode(p));
  }
}
