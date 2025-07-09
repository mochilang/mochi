void main() {
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
      _q0.add(g);
    }
    return _q0;
  })();
  var tmp = [];
  var _iter3 = groups;
  for (var g in (_iter3 is Map ? (_iter3 as Map).keys : _iter3) as Iterable) {
    var total = 0;
    var _iter4 = g['items'];
    for (var x in (_iter4 is Map ? (_iter4 as Map).keys : _iter4) as Iterable) {
      total = (total as num) + (x.val as num);
    }
    tmp = List.from(tmp)..add({'tag': _k2, 'total': total});
  }
  var result = (() {
    var _q5 = <dynamic>[];
    for (var r in tmp) {
      _q5.add([r.tag, r]);
    }
    _q5.sort((a, b) => (a[0] as Comparable).compareTo(b[0]));
    _q5 = [for (var x in _q5) x[1]];
    return _q5;
  })();
  print(result);
}
