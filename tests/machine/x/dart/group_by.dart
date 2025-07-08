void main() {
  var people = [{'name': 'Alice', 'age': 30, 'city': 'Paris'}, {'name': 'Bob', 'age': 15, 'city': 'Hanoi'}, {'name': 'Charlie', 'age': 65, 'city': 'Paris'}, {'name': 'Diana', 'age': 45, 'city': 'Hanoi'}, {'name': 'Eve', 'age': 70, 'city': 'Paris'}, {'name': 'Frank', 'age': 22, 'city': 'Hanoi'}];
  var stats = (() {
  var _q0 = <dynamic>[];
  for (var person in people) {
    _q0.add({'city': g.key, 'count': g.length, 'avg_age': ((() {
  var _q1 = <dynamic>[];
  for (var p in g) {
    _q1.add(p.age);
  }
  return _q1;
})().isEmpty ? 0 : (() {
  var _q1 = <dynamic>[];
  for (var p in g) {
    _q1.add(p.age);
  }
  return _q1;
})().reduce((a, b) => a + b) / (() {
  var _q1 = <dynamic>[];
  for (var p in g) {
    _q1.add(p.age);
  }
  return _q1;
})().length)});
  }
  return _q0;
})();
  print('--- People grouped by city ---');
  for (var s in stats) {
    print([s.city, ': count =', s.count, ', avg_age =', s.avg_age].join(' '));
  }
}
