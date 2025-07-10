var people = [
  {'name': 'Alice', 'age': 30, 'city': 'Paris'},
  {'name': 'Bob', 'age': 15, 'city': 'Hanoi'},
  {'name': 'Charlie', 'age': 65, 'city': 'Paris'},
  {'name': 'Diana', 'age': 45, 'city': 'Hanoi'},
  {'name': 'Eve', 'age': 70, 'city': 'Paris'},
  {'name': 'Frank', 'age': 22, 'city': 'Hanoi'},
];

var stats = (() {
  var _q0 = <dynamic>[];
  var _g1 = <dynamic, List<dynamic>>{};
  for (var person in people) {
    var _k3 = person['city'];
    _g1.putIfAbsent(_k3, () => <dynamic>[]).add({'person': person});
  }
  for (var entry in _g1.entries) {
    var g = entry.value;
    var _k3 = entry.key;
    _q0.add({
      'city': _k3,
      'count': g.length,
      'avg_age':
          ((() {
            var _q4 = <dynamic>[];
            for (var p in g) {
              _q4.add(p['age']);
            }
            return _q4;
          })().isEmpty
          ? 0
          : (() {
                  var _q4 = <dynamic>[];
                  for (var p in g) {
                    _q4.add(p['age']);
                  }
                  return _q4;
                })().reduce((a, b) => a + b) /
                (() {
                  var _q4 = <dynamic>[];
                  for (var p in g) {
                    _q4.add(p['age']);
                  }
                  return _q4;
                })().length),
    });
  }
  return _q0;
})();

void main() {
  print('--- People grouped by city ---');
  var _iter5 = stats;
  for (var s in (_iter5 is Map ? (_iter5 as Map).keys : _iter5) as Iterable) {
    print(
      [
        s['city'],
        ': count =',
        s['count'],
        ', avg_age =',
        s['avg_age'],
      ].join(' '),
    );
  }
}
