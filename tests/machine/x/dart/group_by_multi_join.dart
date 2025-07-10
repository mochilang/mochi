var nations = [
  {'id': 1, 'name': 'A'},
  {'id': 2, 'name': 'B'},
];

var suppliers = [
  {'id': 1, 'nation': 1},
  {'id': 2, 'nation': 2},
];

var partsupp = [
  {'part': 100, 'supplier': 1, 'cost': 10, 'qty': 2},
  {'part': 100, 'supplier': 2, 'cost': 20, 'qty': 1},
  {'part': 200, 'supplier': 1, 'cost': 5, 'qty': 3},
];

var filtered = (() {
  var _q0 = <dynamic>[];
  for (var ps in partsupp) {
    for (var s in suppliers) {
      if (!(s['id'] == ps['supplier'])) continue;
      for (var n in nations) {
        if (!(n['id'] == s['nation'])) continue;
        if (!(n['name'] == 'A')) continue;
        _q0.add({
          'part': ps['part'],
          'value': (ps['cost'] as num) * (ps['qty'] as num),
        });
      }
    }
  }
  return _q0;
})();

var grouped = (() {
  var _q1 = <dynamic>[];
  var _g2 = <dynamic, List<dynamic>>{};
  for (var x in filtered) {
    var _k4 = x['part'];
    _g2.putIfAbsent(_k4, () => <dynamic>[]).add({'x': x});
  }
  for (var entry in _g2.entries) {
    var g = entry.value;
    var _k4 = entry.key;
    _q1.add({
      'part': _k4,
      'total': (() {
        var _q5 = <dynamic>[];
        for (var r in g) {
          _q5.add(r['value']);
        }
        return _q5;
      })().reduce((a, b) => a + b),
    });
  }
  return _q1;
})();

void main() {
  print(grouped);
}
