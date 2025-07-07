void main() {
  var people = [
    {'name': 'Alice', 'age': 30, 'city': 'Paris'},
    {'name': 'Bob', 'age': 15, 'city': 'Hanoi'},
    {'name': 'Charlie', 'age': 65, 'city': 'Paris'},
    {'name': 'Diana', 'age': 45, 'city': 'Hanoi'},
    {'name': 'Eve', 'age': 70, 'city': 'Paris'},
    {'name': 'Frank', 'age': 22, 'city': 'Hanoi'},
  ];

  var groups = <String, List<Map<String, dynamic>>>{};
  for (var p in people) {
    groups.putIfAbsent(p['city'], () => []).add(p);
  }

  var stats = [];
  groups.forEach((city, list) {
    var totalAge = list.fold(0, (a, b) => a + (b['age'] as int));
    stats.add({'city': city, 'count': list.length, 'avg_age': totalAge / list.length});
  });

  print('--- People grouped by city ---');
  for (var s in stats) {
    print('${s['city']}: count = ${s['count']}, avg_age = ${s['avg_age']}');
  }
}
