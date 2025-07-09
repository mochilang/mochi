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
  var big = (() {
    var _q0 = <dynamic>[];
    var _g1 = <dynamic, List<dynamic>>{};
    for (var p in people) {
      var _k2 = p['city'];
      _g1.putIfAbsent(_k2, () => <dynamic>[]).add(p);
    }
    for (var entry in _g1.entries) {
      var g = entry.value;
      var _k2 = entry.key;
      if (!(g.length >= 4)) continue;
      _q0.add({'city': _k2, 'num': g.length});
    }
    return _q0;
  })();
  print(jsonEncode(big));
}
