import 'dart:io';
import 'dart:convert';

var people = [
  {'name': 'Alice', 'age': 30},
  {'name': 'Bob', 'age': 25},
];

void main() {
  File('-').writeAsStringSync(jsonEncode(people));
}
