import 'dart:convert';

var data = [
  {'a': 1, 'b': 2},
  {'a': 1, 'b': 1},
  {'a': 0, 'b': 5},
];

var sorted = (() {
  var _q0 = <dynamic>[];
  for (var x in data) {
    _q0.add([
      {'a': x['a'], 'b': x['b']},
      x,
    ]);
  }
  _q0.sort(
    (a, b) => (jsonEncode(a[0]) as Comparable).compareTo(jsonEncode(b[0])),
  );
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();

void main() {
  print(sorted);
}
