import 'dart:convert';

var items = [
  {'n': 1, 'v': 'a'},
  {'n': 1, 'v': 'b'},
  {'n': 2, 'v': 'c'},
];

var result = (() {
  var _q0 = <dynamic>[];
  for (var i in items) {
    _q0.add([i['n'], i['v']]);
  }
  _q0.sort(
    (a, b) => (jsonEncode(a[0]) as Comparable).compareTo(jsonEncode(b[0])),
  );
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();

void main() {
  print(result);
}
