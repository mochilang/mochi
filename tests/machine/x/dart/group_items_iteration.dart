import 'dart:convert';

var data = [
  {'tag': 'a', 'val': 1},
  {'tag': 'a', 'val': 2},
  {'tag': 'b', 'val': 3},
];

var groups = (() {
  var _q0 = <dynamic>[];
  var _g1 = <dynamic, List<dynamic>>{};
  for (var d in data) {
    var _k2 = d['tag'];
    _g1.putIfAbsent(_k2, () => <dynamic>[]).add(d);
  }
  for (var entry in _g1.entries) {
    var g = entry.value;
    var _k2 = entry.key;
    _q0.add({'key': _k2, 'items': g});
  }
  return _q0;
})();

var tmp = [];

var result = (() {
  var _q3 = <dynamic>[];
  for (var r in tmp) {
    _q3.add([r.tag, r]);
  }
  _q3.sort(
    (a, b) => (jsonEncode(a[0]) as Comparable).compareTo(jsonEncode(b[0])),
  );
  _q3 = [for (var x in _q3) x[1]];
  return _q3;
})();

void main() {
  for (var g in groups) {
    var total = 0;
    for (var x in g['items']) {
      total = (total as num) + (x['val'] as num);
    }
    tmp = List.from(tmp)..add({'tag': g['key'], 'total': total});
  }
  print(result);
}
