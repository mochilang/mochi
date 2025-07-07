import 'dart:convert';

void main() {
  var people = [
    {'name': 'Alice', 'city': 'Paris'},
    {'name': 'Bob', 'city': 'Hanoi'},
    {'name': 'Charlie', 'city': 'Paris'},
    {'name': 'Diana', 'city': 'Hanoi'},
    {'name': 'Eve', 'city': 'Paris'},
    {'name': 'Frank', 'city': 'Hanoi'},
    {'name': 'George', 'city': 'Paris'},
  ];

  var groups = <String, List<Map<String, dynamic>>>{};
  for (var p in people) {
    groups.putIfAbsent(p['city'], () => []).add(p);
  }

  var big = [];
  groups.forEach((city, list) {
    if (list.length >= 4) {
      big.add({'city': city, 'num': list.length});
    }
  });

  print(jsonEncode(big));
}
